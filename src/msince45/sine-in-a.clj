(ns msince45.sine-in-a
  (:use [overtone.live]))

(def harmonic-series-ratios
  (map inc (range 12)))

(def default-vols
  (vec (for [_ (range (count harmonic-series-ratios))] 1.0)))

(defcgen sin-harmonics
  [fundamental {:default 55.0}]
  (:ar
   (let [freqs (map #(* fundamental %) harmonic-series-ratios)
         vols (take 12 (iterate #(/ % 2) 1.0))]
     (apply +
            (map
             (fn [freq vol]
               (* (sin-osc freq) vol))
             freqs vols)))))

(definst sin-event
  [freq 55.0 vol 1.0]
  (sin-harmonics))

(defn cool-chord
  []
  (map sin-event
       (map #(* 55.0 %) harmonic-series-ratios)
       (take 12 (iterate #(/ % 2) 1.0))))

(definst cool-inst
  []
  (apply +
         (map #(sin-osc %)
              [55 110 165 220 275 330 385 440 495 550 605 660])))

(definst sin-inst
  [freq 440 vol 1]
  (* (sin-osc freq)
     vol))

(defn sins []
  (map #(sin-inst %)
       [55 110 165 220 275 330 385 440 495 550 605 660]))

(definst two-freqs [freq1 220 freq2 440]
  (apply +
         (map #(sin-osc % 0) [freq1 freq2])))