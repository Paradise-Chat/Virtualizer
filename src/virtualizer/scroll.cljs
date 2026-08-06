(ns virtualizer.scroll
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [cljs.core.async :as async :refer [put! chan sliding-buffer close! <! go-loop]]
            [virtualizer.layout :as layout]))

(defn create-scroll-state []
  {:!anchor                  (atom {:id nil :offset 0 :anchor-bottom 0 :total-height 0})
   :!current-height          (atom 0)
   :!current-positioned      (atom [])
   :!current-focus           (atom false)
   :!current-was-loading-fwd (atom false)
   :!prev-loading-fwd        (atom false)
   :!dist-bottom             (r/atom 0)
   :!at-bottom?              (r/atom true)
   :!show-jump?              (r/atom false)
   :!initialized?            (r/atom false)
   :!scroll-top              (atom 0)
   :!client-height           (atom 800)
   :!expected-scroll-top     (atom nil)
   :scroll-ch                (chan (sliding-buffer 1))})


(defn start-scroll-machine! [scroll-state]
  (let [ch (:scroll-ch scroll-state)
        !at-bottom? (:!at-bottom? scroll-state)
        !show-jump? (:!show-jump? scroll-state)
        !last-load  (atom 0)]
    (go-loop []
      (when-let [{:keys [ctx at-bottom? should-show-jump?
                         trigger-older? trigger-newer?]} (<! ch)]
        (let [{:keys [on-load-older on-load-newer loading-older? has-items? initialized?]} ctx
              now (js/Date.now)]
          (when (and has-items? initialized?)
            (when (not loading-older?)
              (when (not= @!at-bottom? at-bottom?) (reset! !at-bottom? at-bottom?)))
            (when (not= @!show-jump? should-show-jump?) (reset! !show-jump? should-show-jump?))

            (cond
              (and trigger-older? (> (- now @!last-load) 500))
              (do (reset! !last-load now) (when on-load-older (on-load-older)))

              (and trigger-newer? (> (- now @!last-load) 500))
              (do (reset! !last-load now) (when on-load-newer (on-load-newer))))))
        (recur)))))

(defn get-dist [st _max-s]
  (js/Math.abs (js/Math.round st)))

(defn apply-scroll-anchoring [el state snapshot]
  (let [{:keys [!anchor !current-height !current-positioned !current-focus
                !current-was-loading-fwd !scroll-top !client-height !expected-scroll-top]} state
        anchor-val   @!anchor
        total-height @!current-height
        client-h     @!client-height
        positioned   @!current-positioned
        focus-mode?  @!current-focus
        was-loading? @!current-was-loading-fwd
        current-st   (if snapshot (:scroll-top snapshot) (.-scrollTop el))
        max-s        (max 0 (- total-height client-h))]

    (when (and (:id anchor-val) (not= total-height (:total-height anchor-val)))
      (let [old-anchor-item (some #(when (= (:id %) (:id anchor-val)) %) positioned)]
        (if old-anchor-item
          (let [target-dist  (+ (:bottom old-anchor-item) (:offset anchor-val))
                clamped-dist (max 0 (min max-s target-dist))
                is-neg       (<= current-st 0)
                new-st       (if is-neg (- clamped-dist) clamped-dist)]

            (when (> (js/Math.abs (- current-st new-st)) 0.5)
              (let [sync-dist (get-dist current-st max-s)]
                (when-not (and (<= sync-dist 5) (not was-loading?) (not focus-mode?))
                  (reset! !expected-scroll-top new-st)
                  (set! (.-scrollTop el) new-st)
                  (reset! !scroll-top new-st)))))
          (js/console.warn "--> JS ANCHOR LOST..."))))

    (let [final-st         (.-scrollTop el)
          dist-from-bottom (get-dist final-st max-s)
          viewport-top     (+ dist-from-bottom client-h)
          idx              (layout/binary-search-start-index positioned viewport-top)
          new-anchor       (get positioned idx)]
      (when new-anchor
        (reset! !anchor
                {:id            (:id new-anchor)
                 :offset        (- dist-from-bottom (:bottom new-anchor))
                 :anchor-bottom (:bottom new-anchor)
                 :total-height  total-height})))))

(defn evaluate-scroll-position! [target scroll-state ctx]
  (let [st       (.-scrollTop target)
        expected @(:!expected-scroll-top scroll-state)]

    (if (and expected (= (js/Math.round st) (js/Math.round expected)))
      (do
        (reset! (:!expected-scroll-top scroll-state) nil)
        (reset! (:!scroll-top scroll-state) st))

      (let [_                (reset! (:!scroll-top scroll-state) st)
            {:keys [!dist-bottom !anchor !current-height !current-positioned scroll-ch !client-height]} scroll-state
            ch               @!client-height
            th               @!current-height
            max-scroll       (max 0 (- th ch))
            dist-from-bottom (get-dist st max-scroll)
            dist-from-top    (max 0 (- max-scroll dist-from-bottom))
            at-bottom?       (<= dist-from-bottom 30)
            should-show-jump? (> dist-from-bottom 600)
            {:keys [initialized?]} ctx]

        (when (not= @!dist-bottom dist-from-bottom)
          (reset! !dist-bottom dist-from-bottom))

        (when (and initialized? (pos? th))
          (let [positioned   @!current-positioned
                viewport-top (+ dist-from-bottom ch)
                idx          (layout/binary-search-start-index positioned viewport-top)
                new-anchor   (get positioned idx)]
            (when new-anchor
              (reset! !anchor
                      {:id            (:id new-anchor)
                       :offset        (- dist-from-bottom (:bottom new-anchor))
                       :anchor-bottom (:bottom new-anchor)
                       :total-height  th}))))

        (let [{:keys [loading-older? older-dead? chunking? focus-mode? loading-newer?]} ctx
              threshold       (max 600 (* ch 2))
              trigger-older?  (and (<= dist-from-top threshold) (not loading-older?) (not older-dead?) (not chunking?))
              trigger-newer?  (and focus-mode? (<= dist-from-bottom threshold) (not loading-newer?) (not chunking?))
              bottom-changed? (not= @(:!at-bottom? scroll-state) at-bottom?)
              jump-changed?   (not= @(:!show-jump? scroll-state) should-show-jump?)]
          (when (or trigger-older? trigger-newer? bottom-changed? jump-changed?)
            (put! scroll-ch {:ctx ctx
                             :dist-from-top dist-from-top
                             :dist-from-bottom dist-from-bottom
                             :at-bottom? at-bottom?
                             :should-show-jump? should-show-jump?
                             :trigger-older? trigger-older?
                             :trigger-newer? trigger-newer?})))))))



