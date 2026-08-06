(ns virtualizer.core
  (:require [reagent.core :as r]
            ["react" :as react]
            [cljs.core.async :as async :refer [put! close! chan sliding-buffer <! timeout go-loop]]
            [virtualizer.metrics :as metrics]
            [virtualizer.layout :as layout]
            [virtualizer.scroll :as scroll]))

(defonce !last-visible-ids (atom []))
(defonce !is-scrolling? (atom false))
(defonce !scroll-timer (atom nil))

(defn VirtualItemWrapper [^js props]
  (let [id              (.-id props)
        item            (.-item props)
        layout-height   (.-layoutHeight props)
        jump-target-id  (.-jumpTargetId props)
        render-item     (.-renderItem props)
        item-resize-obs (.-itemResizeObs props)
        el-ref          (react/useRef nil)]

    (react/useLayoutEffect
     (fn []
       (let [el (.-current el-ref)]
         (when el (metrics/observe! item-resize-obs el))
         (fn [] (when el (metrics/unobserve! item-resize-obs el)))))
     #js [id item-resize-obs])

    (react/createElement "div"
                         #js {"data-item-id" id
                              "data-layout-height" layout-height
                              "ref" el-ref
                              "className" (if (= id jump-target-id) "is-jump-target" "")
                              "style" #js {"width" "100%"
                                           "flexShrink" 0
                         ;;                  "height" (str layout-height "px")
                         ;;                  "overflow" "hidden"

                                           }}
                         (render-item item))
    ))

(def MemoVirtualItemWrapper
  (react/memo VirtualItemWrapper
              (fn [^js prev ^js next]
                (and (identical? (.-item prev) (.-item next))
                     (= (.-layoutHeight prev) (.-layoutHeight next))))))



(defn VirtualItemsRenderer [^js props]
  (let [visible-window  (.-visibleWindow props)
        items-map       (.-itemsMap props)
        total-height    (.-totalHeight props)
        jump-target-id  (.-jumpTargetId props)
        render-item     (.-renderItem props)
        item-resize-obs (.-itemResizeObs props)
        first-vis       (first visible-window)
        last-vis        (last visible-window)
        bottom-gap      (if first-vis (:bottom first-vis) total-height)
        top-gap         (if last-vis (max 0 (- total-height (+ (:bottom last-vis) (:height last-vis)))) 0)
        children        #js []]

    (.push children (react/createElement "div" #js {"key" "bottom-gap" "style" #js {"height" (str bottom-gap "px") "flexShrink" 0 "width" "100%"}}))

    (doseq [{:keys [id height] :as layout-item} visible-window]
      (let [real-item (or (get items-map id) layout-item)]
        (.push children (react/createElement MemoVirtualItemWrapper
                                             #js {"key" (str id)
                                                  "id" id
                                                  "item" real-item
                                                  "layoutHeight" height
                                                  "jumpTargetId" jump-target-id
                                                  "renderItem" render-item
                                                  "itemResizeObs" item-resize-obs}))))

    (.push children (react/createElement "div" #js {"key" "top-gap" "style" #js {"height" (str top-gap "px") "overflowAnchor" "none" "flexShrink" 0 "width" "100%"}}))

    (react/createElement react/Fragment nil children)))

(def MemoVirtualItemsRenderer
  (react/memo VirtualItemsRenderer
              (fn [^js prev ^js next]
                (and (= (.-totalHeight prev) (.-totalHeight next))
                     (= (.-jumpTargetId prev) (.-jumpTargetId next))
                     (identical? (.-visibleWindow prev) (.-visibleWindow next))
                     (identical? (.-itemsMap prev) (.-itemsMap next))))))

(defn virtualized-list [initial-props]
  (let [!scroll-ref          (atom nil)
        !container-width     (r/atom 400)
        !measured            (r/atom {})
        !latest-items        (atom (:items-map initial-props {}))
        !theme-metrics       (r/atom (:default-theme-metrics initial-props {}))
        !last-context        (atom nil)
        scroll-state         (scroll/create-scroll-state)
        !next-layout         (atom nil)
        !last-item-count     (atom 0)
        !last-first-id       (atom nil)
        !deferred-front      (atom 0)
        !chunk-tick          (r/atom 0)
        chunk-ch             (chan (sliding-buffer 1))
        !quantized-dist      (r/atom 0)

        metrics-obs          (when (:extract-metrics-fn initial-props)
                               (metrics/create-theme-observer !theme-metrics (:extract-metrics-fn initial-props)))

        item-resize-obs      (metrics/create-item-observer !latest-items !measured)
        container-obs        (metrics/create-container-observer !container-width (:!client-height scroll-state))

         container-ref-fn
         (fn [el]
                               (let [old-el @!scroll-ref]
                                 (when (not= old-el el)
                                   (when old-el (metrics/unobserve! container-obs old-el))
                                   (reset! !scroll-ref el)
                                   (when el
                                     (reset! (:!client-height scroll-state) (.-clientHeight el))
                                     (reset! (:!scroll-top scroll-state) (.-scrollTop el))
                                     (metrics/observe! container-obs el)))))]

    (r/create-class
     {:component-did-mount
      (fn [this]
        (let [props (r/props this)
              items (:items props)]
          (reset! !latest-items (:items-map props))
          (reset! !last-item-count (count items))
          (when-let [first-id (if (seq items) (:id (first items)) nil)]
            (reset! !last-first-id first-id))
          (when-let [on-change (:on-layout-context-change props)]
            (let [w @!container-width
                  t @!theme-metrics
                  m @!measured]
              (reset! !last-context {:width w :theme t :measured m})
              (on-change w t m))))

        (scroll/start-scroll-machine! scroll-state)
        (go-loop []
          (when-let [_ (<! chunk-ch)]
            (<! (timeout 16))
            (let [current-df @!deferred-front]
              (when (pos? current-df)
                (js/requestAnimationFrame
                 (fn []
                   (swap! !deferred-front #(max 0 (- % 2)))
                   (swap! !chunk-tick inc)))))
            (recur))))

      :get-snapshot-before-update
      (fn [this old-argv old-state]
        (when-let [el @!scroll-ref]
          {:scroll-top (.-scrollTop el)}))

      :component-did-update
      (fn [this old-argv old-state snapshot]
        (let [props         (r/props this)
              items         (:items props)
              current-count (count items)
              last-count    @!last-item-count
              first-id      (if (seq items) (:id (first items)) nil)
              old-first     @!last-first-id

              front-added?  (and (> current-count 0) (> last-count 0)
                                 (> current-count last-count) old-first (not= first-id old-first))
              diff          (if front-added? (- current-count last-count) 0)]
          (when-let [on-change (:on-layout-context-change props)]
            (let [w @!container-width
                  t @!theme-metrics
                  m @!measured
                  ctx @!last-context]
              (when (or (not= w (:width ctx))
                        (not= t (:theme ctx))
                        (not= m (:measured ctx)))
                (reset! !last-context {:width w :theme t :measured m})
                (on-change w t m))))

          (when front-added?
            (swap! !deferred-front + diff)
            (swap! !chunk-tick inc))

          (reset! !latest-items (:items-map props))
          (reset! !last-item-count current-count)
          (when first-id
            (reset! !last-first-id first-id))

          (when-let [layout @!next-layout]
            (reset! (:!current-height scroll-state) (:th layout))
            (reset! (:!current-positioned scroll-state) (:positioned layout))
            (reset! (:!current-focus scroll-state) (:focus-mode layout))
            (reset! (:!current-was-loading-fwd scroll-state) (:was-loading layout))
            (reset! (:!prev-loading-fwd scroll-state) (:loading-newer layout))

            (when (:chunking? layout)
              (put! chunk-ch true))

            (js/console.log (str "[DEBUG: Layout Update] Next Layout processed."
                                 " | Total Height: " (:th layout)
                                 " | Anchor Valid? " (some? @(:!anchor scroll-state))))

            (reset! !next-layout nil))

          (when-let [el @!scroll-ref]
            (let [positioned   @(:!current-positioned scroll-state)
                  cnt          (count positioned)
                  initialized? @(:!initialized? scroll-state)]
              (if (and (pos? cnt) (not initialized?))
                (let [jump-target-id (:jump-target-id props)
                      target-item    (if jump-target-id
                                       (some #(when (= (:id %) jump-target-id) %) positioned)
                                       nil)
                      st             (if target-item (- (:bottom target-item)) 0)]
                  (set! (.-scrollTop el) st)
                  (reset! (:!scroll-top scroll-state) st)

                  (let [anchor-item (or target-item (first positioned))]
                    (when anchor-item
                      (reset! (:!anchor scroll-state)
                              {:id             (:id anchor-item)
                               :offset         0
                               :anchor-bottom  (:bottom anchor-item)
                               :total-height   @(:!current-height scroll-state)})))

                  (reset! (:!initialized? scroll-state) true)
                  (reset! (:!at-bottom? scroll-state) (not jump-target-id)))

                (scroll/apply-scroll-anchoring el scroll-state snapshot))



))))

      :component-will-unmount
      (fn [this]
        (metrics/disconnect-all! container-obs)
        (metrics/disconnect-all! item-resize-obs)
        (when metrics-obs (metrics/disconnect-all! metrics-obs))
        (when-let [ch (:scroll-ch scroll-state)]
          (close! ch))
        (close! chunk-ch))

      :reagent-render
      (fn [{:keys [items items-map loading-older? loading-newer?
                   older-dead? jump-target-id focus-mode?
                   on-load-older on-load-newer on-jump-live
                   render-item render-measuring-sticks render-empty-state
                   render-jump-button render-loading-overlay
                   on-layout-context-change
                   on-viewport-change
                   on-scroll-state-change
                   wrapper-class scroll-container-class]
            :or {wrapper-class          "virtual-list-wrapper"
                 scroll-container-class "virtual-list-scroll-container"}}]

        @!chunk-tick
        @!measured

        (let [current-count  (count items)
              last-count     @!last-item-count
              first-id       (if (seq items) (:id (first items)) nil)
              old-first      @!last-first-id]

          (let [front-added?   (and (> current-count 0) (> last-count 0) (> current-count last-count) old-first (not= first-id old-first))
                diff           (if front-added? (- current-count last-count) 0)
                effective-df   (min (+ @!deferred-front diff) current-count)
                chunking?      (pos? effective-df)

                items-to-process (if chunking?
                                   (if (vector? items)
                                     (subvec items effective-df)
                                     (drop effective-df items))
                                   items)

                positioned     (vec (rseq (vec items-to-process)))
                total-height   (if (seq positioned) (+ (:bottom (last positioned)) (:height (last positioned))) 0)
                cnt            (count positioned)

                dist-bottom    @!quantized-dist
                vh             @(:!client-height scroll-state)
                overscan       (max 800 (* vh 1.5))

                w-start        (- dist-bottom overscan)
                w-end          (+ dist-bottom vh overscan)

                start-idx      (layout/binary-search-start-index positioned w-start)
                safe-start     (min cnt start-idx)

                visible-window (->> (subvec positioned safe-start)
                                    (take-while #(<= (:bottom %) w-end)))

                vis-ids        (mapv :id visible-window)
                _              (when (not= @!last-visible-ids vis-ids)
                                 (reset! !last-visible-ids vis-ids)
                                 (when on-viewport-change (on-viewport-change vis-ids)))

                do-jump!       (fn []
                                 (reset! (:!at-bottom? scroll-state) true)
                                 (if focus-mode?
                                   (do
                                     (reset! (:!initialized? scroll-state) false)
                                     (when on-jump-live (on-jump-live)))
                                   (when-let [el @!scroll-ref]
                                     (set! (.-scrollTop el) 0)
                                     (reset! (:!scroll-top scroll-state) 0)

                                     (let [positioned @(:!current-positioned scroll-state)
                                           th         @(:!current-height scroll-state)
                                           newest     (first positioned)]
                                       (when newest
                                         (reset! (:!anchor scroll-state)
                                                 {:id             (:id newest)
                                                  :offset         0
                                                  :anchor-bottom  (:bottom newest)
                                                  :total-height   th}))))))]

            (reset! !next-layout
                    {:th             total-height
                     :positioned     positioned
                     :focus-mode     focus-mode?
                     :was-loading    @(:!prev-loading-fwd scroll-state)
                     :loading-newer  loading-newer?
                     :chunking?      chunking?})

            [:div {:class wrapper-class :style {:min-height "0" :display "flex" :flex-direction "column"}}
             (when render-measuring-sticks
               (render-measuring-sticks (fn [el] (when (and el metrics-obs) (metrics/observe! metrics-obs el)))))

             [:div {:class (str scroll-container-class (when jump-target-id " jumping-animation"))
                    :ref container-ref-fn
                    :style {:overflow-anchor "none" :overflow-y "auto" :min-height "0"}
                    :on-scroll (fn [e]
                                 (let [el               (.-currentTarget e)
                                       st               (.-scrollTop el)
                                       ch               (.-clientHeight el)
                                       synced-th        @(:!current-height scroll-state)
                                       max-scroll       (max 0 (- synced-th ch))
                                       dist-from-bottom (scroll/get-dist st max-scroll)
                                       dist-from-top    (max 0 (- max-scroll dist-from-bottom))
                                       q-step           (max 200 (quot ch 2))
                                       q-dist           (if dist-from-bottom (- dist-from-bottom (mod dist-from-bottom q-step)) 0)]

                                   (when-let [timer @!scroll-timer]
                                     (js/clearTimeout timer))

                                   (when-not @!is-scrolling?
                                     (reset! !is-scrolling? true)
                                     (when el (.add (.-classList el) "is-scrolling"))
                                     (when on-scroll-state-change
                                       (on-scroll-state-change true)))

                                   (reset! !scroll-timer
                                           (js/setTimeout
                                            #(do
                                               (reset! !is-scrolling? false)
                                               (when-let [scroll-el @!scroll-ref]
                                                 (.remove (.-classList scroll-el) "is-scrolling"))
                                               (when on-scroll-state-change
                                                 (on-scroll-state-change false)))
                                            150))

                                   (when (not= @!quantized-dist q-dist)
                                     (reset! !quantized-dist q-dist))

                                   (scroll/evaluate-scroll-position!
                                    el scroll-state
                                    {:on-load-older    on-load-older
                                     :on-load-newer    on-load-newer
                                     :loading-older?   loading-older?
                                     :older-dead?      older-dead?
                                     :focus-mode?      focus-mode?
                                     :loading-newer?   loading-newer?
                                     :has-items?       (pos? cnt)
                                     :chunking?        chunking?
                                     :initialized?     @(:!initialized? scroll-state)})))}

              (when (or loading-older? chunking?)
                [:div {:style {:position "absolute" :top "10px" :left 0 :right 0 :z-index 10 :display "flex" :justify-content "center" :pointer-events "none"}}
                 (if render-loading-overlay (render-loading-overlay) "Loading...")])

              (if (zero? cnt)
                (when render-empty-state (render-empty-state))
                (react/createElement MemoVirtualItemsRenderer
                                     #js {"visibleWindow" visible-window
                                          "itemsMap" items-map
                                          "totalHeight" total-height
                                          "renderItem" render-item
                                          "itemResizeObs" item-resize-obs}))

              (when (and focus-mode? loading-newer?)
                [:div {:style {:position "absolute" :bottom "10px" :left 0 :right 0 :z-index 10 :display "flex" :justify-content "center" :pointer-events "none"}}
                 (if render-loading-overlay (render-loading-overlay) "Loading...")])]

             (when (and render-jump-button (or @(:!show-jump? scroll-state) (not @(:!at-bottom? scroll-state))))
               (render-jump-button do-jump! focus-mode?))])))})))
