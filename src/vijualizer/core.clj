(ns vijualizer.core
  (:import [java.io File]
           [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat AudioFormat$Encoding
            AudioInputStream AudioSystem
            DataLine$Info TargetDataLine]))


; http://www.codejava.net/coding/capture-and-record-sound-into-wav-file-with-java-sound-api


(def wav-file (File. "test-recording.wav"))

(def file-type (AudioFileFormat$Type/WAVE))


(def audio-format
  (AudioFormat.
   16000  ; sampleRate
   8      ; sampleSizeInBits
   1      ; channels
   true   ; signed
   true)) ; bigEndian


(def info
  (DataLine$Info. TargetDataLine audio-format))


(def line (atom nil))


(defn start []
  (if (not (AudioSystem/isLineSupported info))
    (System/exit 1))
  (reset! line (AudioSystem/getLine info))
  (.open @line audio-format)
  (.start @line)
  (println "Start capturing ...")
  (def ais (AudioInputStream. @line))
  (println "Start recording ...")
  (AudioSystem/write ais file-type wav-file))


(defn stop []
  (.stop @line)
  (.close @line)
  (println "Finished"))
