(ns virtualizer.metrics
  (:require [cljs.core.async :as async :refer [put! chan sliding-buffer close! <! go-loop]]
            [reagent.core :as r]))

(defn create-item-observer [!latest-items !measured]
  (let [!pending-updates (atom {})
        !flush-timer     (atom nil)
        flush-updates!   (fn []
                           (let [updates @!pending-updates]
                             (when (seq updates)
                               (reset! !pending-updates {})
                               (swap! !measured merge updates)
                               (r/flush))))
        obs (js/ResizeObserver.
             (fn [entries]
               (let [measured-cache @!measured
                     updates (reduce
                              (fn [acc entry]
                                (let [el    (.-target entry)
                                      id    (or (.getAttribute el "data-item-id")
                                                (.getAttribute el "data-event-id"))
                                      dom-h (.-offsetHeight el)]
                                  (if (and id (pos? dom-h))
                                    (let [current-measured (get measured-cache id)
                                          raw-layout-h     (.getAttribute el "data-layout-height")
                                          layout-h         (if raw-layout-h (js/parseFloat raw-layout-h) 0)
                                          used-h           (or current-measured layout-h)
                                          diff             (js/Math.abs (- dom-h used-h))
                                          dom-h-round      (js/Math.round dom-h)]
                                      (if (and (> diff 2) (not= current-measured dom-h-round))
                                        (assoc acc id dom-h-round)
                                        acc))

                                    acc)))
                              {} entries)]
                 (when (seq updates)
                   (swap! !pending-updates merge updates)
                   (when-let [timer @!flush-timer] (js/cancelAnimationFrame timer))
                   (reset! !flush-timer (js/requestAnimationFrame flush-updates!))))))]
    {:observer obs :channel nil}))

(defn create-theme-observer [!theme-metrics extract-metrics-fn]
  (if-not extract-metrics-fn
    nil
    (let [ch  (chan (sliding-buffer 1))
          obs (js/ResizeObserver. (fn [entries] (put! ch entries)))]
      (go-loop []
        (when-let [entries (<! ch)]
          (let [new-metrics (reduce extract-metrics-fn @!theme-metrics entries)]
            (when (not= new-metrics @!theme-metrics)
              (js/requestAnimationFrame
               (fn [] (reset! !theme-metrics new-metrics)))))
          (recur)))
      {:observer obs :channel ch})))





(defn create-container-observer
  ([!container-width]
   (create-container-observer !container-width nil))
  ([!container-width !client-height]
   (let [ch  (chan (sliding-buffer 1))
         obs (js/ResizeObserver. (fn [entries] (put! ch entries)))]
     (go-loop []
       (when-let [entries (<! ch)]
         (js/requestAnimationFrame
          (fn []
            (let [el (.-target (aget entries 0))]
              (reset! !container-width (.-clientWidth el))
              (when !client-height
                (reset! !client-height (.-clientHeight el))))))
         (recur)))
     {:observer obs :channel ch})))

(defn observe! [obs-map el]
  (when (and obs-map el (:observer obs-map))
    (.observe (:observer obs-map) el)))

(defn unobserve! [obs-map el]
  (when (and obs-map el (:observer obs-map))
    (.unobserve (:observer obs-map) el)))

(defn disconnect-all! [obs-map]
  (when obs-map
    (when-let [o (:observer obs-map)] (.disconnect o))
    (when-let [c (:channel obs-map)] (close! c))))
