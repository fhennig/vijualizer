(ns vijualizer.core
  (:require [incanter.charts :as p]
            [clojure.core.async :as a]
            [quil.core :as q]
            [quil.middleware :as qm])
  (:import [ddf.minim Minim]
           [ddf.minim.analysis BeatDetect FFT]))

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


#_(def state (atom {:stop false
                  :xs [] :ys []}))


(defn make-init-state []
  (atom {:stop false
         :xs [] :ys []}))


(defn update-state-once [state fft]
  (-> state
      (assoc :xs (->> (range (.avgSize fft))
                      (map #(.getAverageCenterFrequency fft %))))
      (assoc :ys (->> (range (.avgSize fft))
                      (map #(.getAvg fft %))))))



(def A 45)
(def B 15)

(defn stop-update [state]
  (swap! state assoc :stop true))

(defn update-state [state args]
  (stop-update state)
  (Thread/sleep 400)
  (swap! state assoc :stop false)
  (let [fft (FFT. (:frame-size args) 44100.0)]
    (.logAverages fft A B)
    (a/go (while (not (:stop @state))
            (.forward fft (.-mix (:input args)))
            (swap! state update-state-once fft)
            (Thread/sleep 10)))))




#_(defn test []
  (def a (init-stuff))
  (update-state state a)
  (Thread/sleep 1000)
  (swap! state assoc :stop true)
  (stop-stuff a)
  (swap! state assoc :stop false)
  nil)


(defn minim-test [args]
  (let [fft (FFT. (:frame-size args) 44100.0)
        get-bands (fn [] (->> (range (.specSize fft))
                              (map #(.getBand fft %))))
        get-avgs (fn [] (->> (range (.avgSize fft))
                             (map #(.getAverageBandWidth fft %))))
        get-bnds get-bands]
    (.logAverages fft A B)
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



;;; quil


(def CONF
  {:w 600
   :h 400})


(defn quil-setup []
  (q/frame-rate 50)
  (q/color-mode :rgb 1.0))


(defn draw-state [state]
  "state is an atom"
  (q/background 0.2)
  (let [s @state]
;    (q/text (str (:ys s)) 100 100 300 300)
    (doseq [i (range (count (:xs s)))
            :let [x (* i 4)
                  h (+ 2 (nth (:ys s) i))]]
      #_(q/rect x 0 4 (* 10 (/ (Math/log (* 1 h))
                             (Math/log (+ 1.1 (- 1 (/ i 135)))))))
      (q/rect x 0 4 h)
      #_(q/rect x 0 4 (Math/pow h (nth (:xs s) i)))
      #_(q/rect x 0 4 (* (+ 1 (/ (+ 1 i) 136)) h)))))


(defn quil-plot [state]
  (q/sketch
   :title "plot"
   :size [(:w CONF) (:h CONF)]
   :setup quil-setup
   :update identity
   :draw (fn [_] (draw-state state))
   :middleware [qm/fun-mode]))



;; in repl:
;; (def a (init-stuff)) ;; a dict
;; (def state (make-init-state)) ;; an atom
;; (update-state state a)
;; ... (quil-plot state)
;; (swap! state assoc :stop true)
;; (stop-stuff a)
