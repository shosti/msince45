(ns msince45.sine-in-a
  (:use [overtone.live]))

(def harmonic-series-ratios
  (map inc (range 12)))

(def default-volumes
  (take 12 (iterate #(/ % 2) 1.0)))

(defcgen sine-event
  [fundamental
   {:default 55 :doc "The fundamental frequency for the sine composite."}
   duration
   {:default 1.0 :doc "The duration of the sine composite, in seconds."}
   volumes default-volumes
   {:doc "A vector of the volumes for the respective overtones."}]
  (:ar
   (let [envelope (env-gen (sine duration) :action FREE)
         frequencies (map #(* fundamental %) harmonic-series-ratios)]
     (* envelope
        (apply +
               (map #(* %1 (sin-osc %2))
                    volumes frequencies))))))

(defn sine-synth
  [fundamental duration balance volumes]
  (synth []
         (out 0
              (* [balance (- 1 balance)]
                 (sine-event
                  fundamental duration volumes)))))

;;##Runner

(def sine-metronome (metronome 300))

(def all-freqs (cycle (map #(* 55.0 %) harmonic-series-ratios)))

(defn play-sines
  [beat freqs]
  (let [next-beat (inc beat)
        player (sine-synth (first freqs) 1 0.5 default-volumes)]
    (at (sine-metronome beat)
      (player))
    (apply-at (sine-metronome next-beat) #'play-sines [next-beat (rest freqs)])))

;(play-sines (sine-metronome) all-freqs)
;(stop)