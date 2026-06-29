(ns virtualizer.metrics
  (:require [cljs.core.async :as async :refer [put! chan sliding-buffer close! <! go-loop]]
            [reagent.core :as r]))

(defn create-item-observer [!latest-items !measured]
  (let [obs (js/ResizeObserver.
             (fn [entries]
               (let [latest-items    @!latest-items
                     measured-cache  @!measured
                     updates (reduce
                              (fn [acc entry]
                                (let [el    (.-target entry)
                                      id    (.getAttribute el "data-item-id")
                                      dom-h (.-offsetHeight el)]
                                  (if (and id (pos? dom-h))
                                    (let [item             (or (get latest-items id) {:id id})
                                          current-measured (get measured-cache id)
                                          used-h           (or current-measured (:height item))
                                          diff             (if used-h (js/Math.abs (- dom-h used-h)) 1000)
                                          dom-h-round      (js/Math.round dom-h)]
                                      (if (and used-h (> diff 4) (not= current-measured dom-h-round))
                                        (assoc acc id dom-h-round)
                                        acc))
                                    acc)))
                              {} entries)]
                 (when (seq updates)
                   (swap! !measured merge updates)
                   (r/flush)))))]
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
