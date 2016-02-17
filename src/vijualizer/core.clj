(ns vijualizer.core
  (:require [incanter.charts :as p]) ; plots
  (:import [ddf.minim Minim]
           [ddf.minim.analysis BeatDetect FFT]))


(def FRAME_RATE 43)


(defn plot [l]
  (let [chart (p/xy-plot (map #(* FRAME_RATE %)
                              (range (count l))) l)]
    (p/set-axis chart :x (p/log-axis :base 2))
    (incanter.core/view chart)))


;;; Minim

(definterface MinimInput
  (^String sketchPath [^String filename])
  (^java.io.InputStream createInput [^String filename]))


(def MinimDummyService
  (proxy [MinimInput] []
    (sketchPath [filename] "")
    (createInput [filename]
      nil)))


(defn plot1 [fft]
  (let [x (->> (range (.avgSize fft))
               (map #(.getAverageCenterFrequency fft %)))
        y (->> (range (.avgSize fft))
               (map #(.getAvg fft %)))
        chart (p/xy-plot x y)]
    (println (count x))
    (print x)
    (print y)
    (p/set-axis chart :x (p/log-axis :base 2))
    (incanter.core/view chart)))


(defn init-stuff []
  (let [frame-size 1024
        minim (Minim. MinimDummyService)
        input (.getLineIn minim Minim/MONO, frame-size)]
    {:frame-size frame-size
     :minim minim
     :input input}))


(defn stop-stuff [stuff]
  (.close (:input stuff))
  (.stop (:minim stuff)))


(defn minim-test [args]
  (let [fft (FFT. (:frame-size args) 44100.0)
        get-bands (fn [] (->> (range (.specSize fft))
                              (map #(.getBand fft %))))
        get-avgs (fn [] (->> (range (.avgSize fft))
                             (map #(.getAverageBandWidth fft %))))
        get-bnds get-bands]
    (.logAverages fft 11 11)
    (.forward fft (.-mix (:input args)))
    (Thread/sleep 50)
    (.forward fft (.-mix (:input args)))
    (def bands (get-bnds))
    (println (count bands))
                                        ;(plot bands)
    (plot1 fft)
    nil
    #_(while true
      (.forward fft (.-mix input))
      (->> (get-bands) plot)
      (Thread/sleep 2000)
      #_(.detect beat-detect (.-mix input))
      #_(cond (.isHat beat-detect) (println "HAT")
              (.isSnare beat-detect) (println "SNARE")
              (.isKick beat-detect) (println "KICK")))))
