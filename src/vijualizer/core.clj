(ns vijualizer.core
  (:require [incanter.charts :as p]) ; plots
  (:import [java.io File]
           [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat AudioFormat$Encoding
            AudioInputStream AudioSystem
            DataLine$Info TargetDataLine]
           [org.jtransforms.fft FloatFFT_1D]))


; http://www.codejava.net/coding/capture-and-record-sound-into-wav-file-with-java-sound-api


(def wav-file (File. "test-recording.wav"))

(def file-type (AudioFileFormat$Type/WAVE))
;  (AudioSystem/write ais file-type wav-file)


(def audio-format
  (AudioFormat.
   44100  ; sampleRate
   8      ; sampleSizeInBits
   1      ; channels
   true   ; signed
   true)) ; bigEndian


(def info
  (DataLine$Info. TargetDataLine audio-format))


(def line (atom nil))


(def FRAME_RATE 30)


(defn record []
  (if (not (AudioSystem/isLineSupported info))
    (System/exit 1))
  (reset! line (AudioSystem/getLine info))
  (.open @line audio-format)
  (.start @line)
  (println "Start capturing ...")
  (def ais (AudioInputStream. @line))
  (println "Start recording ...")
  (AudioSystem/write ais file-type wav-file))


(defn start []
  (if (not (AudioSystem/isLineSupported info))
    (System/exit 1))
  (reset! line (AudioSystem/getLine info))
  (.open @line audio-format)
  (.start @line)
  (println "Start capturing ...")
  (def ais (AudioInputStream. @line))
  (println "Start recording ...")
;  (AudioSystem/write ais file-type wav-file)
  (def frame-count (Math/floor (/ 44100 FRAME_RATE)))
  (def data (byte-array frame-count))
  (.read @line data 0 frame-count)
  (.stop @line)
  (.close @line)
  (println "Finished")
  (def array (->> data
                  (apply list)
                  (map #(-> % (/ 128.0) float))
                  (map #(list % (float 0)))
                  (apply concat)
                  float-array))
  (def fft (FloatFFT_1D. frame-count))
  (.complexForward fft array)
  (apply list array))


(defn magic
  "interpret FFT output"
  [array]
  (let [re (keep-indexed #(if (even? %1) %2) array)
        im (keep-indexed #(if (odd?  %1) %2) array)
        mag (map (fn [r i]
                   (Math/sqrt (+ (* r r) (* i i)))) re im)]
    (take (Math/floor  (/ (count mag) 2)) mag)))


(defn stop []
  (.stop @line)
  (.close @line)
  (println "Finished"))


(defn plot [l]
  (let [chart (p/xy-plot (map #(* FRAME_RATE %)
                              (range (count l))) l)]
    (p/set-axis chart :x (p/log-axis :base 10))
    (incanter.core/view chart)))
