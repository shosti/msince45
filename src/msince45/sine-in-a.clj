(ns msince45.sine-in-a
  (:use [overtone.live]))

(def harmonic-series-ratios
  (map inc (range 12)))

(def ^:dynamic *volumes*
  (take 12 (iterate #(/ % 2) 1.0)))

(defcgen sin-sound
  "A composite generator that takes as input a fundamental, a
  duration, and a time step `t` and returns a stochastic sound event
  with the volumes of harmonics determined by the `current-vols`
  function."
  [fundamental {:default 55.0}
   duration    {:default 1.0}]
  (:ar
   (let [envelope (env-gen (sine duration))
         frequencies (map #(* fundamental %) harmonic-series-ratios)]
     (* envelope
        (apply +
               (map
                (fn [freq vol]
                  (* vol (sin-osc freq)))
                frequencies *volumes*))))))

(defsynth sin-event
  [fundamental 55.0
   duration    1.0
   volume      1.0
   balance     0.5]
  (out 0
       (* volume
          [balance (- 1 balance)]
          (sin-sound fundamental duration))))
