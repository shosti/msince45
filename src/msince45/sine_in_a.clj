(ns msince45.sine-in-a
  (:use [overtone.live]))

;; ## Basic constants

(def a-fundamental
  "The fundamental frequency for A, upon which the piece is based."
  55.0)

(def number-of-partials
  "The number of partials to be played in each sine event, as well as
  the number of sine events to be played in each sine group."
  12)

(def group-duration
  "The duration, in seconds, of each sine group."
  1.0)

(def volume-max
  "A maximum overall volume to prevent clipping."
  0.1)

(def harmonic-series-ratios
  "The numbers 1 through 12 (multiplied by the fundamental to obtain
the first 12 harmonics)."
  (map inc (range number-of-partials)))

(def fundamental-frequencies
  "The first 12 harmonics of A, which will be the basis of each sine
  group."
  (map #(* a-fundamental %) harmonic-series-ratios))

(def fundamental-volumes
  "The fundamental volume sequence that partials are multiplied by in
their most consonant state: `[1, 0.5, 0.25 ...]`"
  (take number-of-partials (iterate #(/ % 2) 1.0)))

;; ## Sine generators

(defcgen sine-event
  [fundamental
   {:doc "The fundamental frequency for the sine composite."}
   partials
   {:doc "A vector of 12 numbers that comprise the ratios for the partials.
For harmonics, these numbers will be whole numbers."}
   volumes
   {:doc "A vector of the volumes for the respective partials."}]
  (:ar
   (let [frequencies (map #(* fundamental %) partials)]
     (apply +
            (map #(* %1 (sin-osc %2))
                 volumes frequencies)))))

(defcgen sine-group
  [fundamentals
   {:doc "The fundamentals for the group."}
   duration
   {:doc "The duration of the group, in seconds."}
   partials-list
   {:doc "A vector of 12 vectors, each with 12 partial frequencies."}
   volumes-list
   {:doc "A vector of 12 vectors, each with 12 respective volumes for the partials."}]
  (:ar
   (let [env (env-gen (sine duration) :action FREE)
         events (map #(sine-event %1 %2 %3)
                     fundamentals partials-list volumes-list)]
     (* env
        (apply + events)))))

(defn sine-group-synth
  [{:keys [fundamentals
           duration
           partials-list
           volumes-list
           balance
           overall-volume]}]
  (synth []
         (out 0
              (* overall-volume
                 [balance (- 1 balance)]
                 (sine-group fundamentals
                             duration
                             partials-list
                             volumes-list)))))
;; ## Deterministic functions

(defn time-func
  "The basic function of time upon which the piece is based: a sine
wave offset by π and normalized so that with π time events per second
a full cycle will take 2 minutes."
  [t]
  (Math/sin (+ Math/PI (/ t 60.0))))

(defn overall-volume
  "The overall volume as a function of time.  Rises as the piece
  enters maximally consonant and dissonant zones."
  [t]
  (* volume-max
     (Math/abs (time-func t))))

(defn frequency-variance
  "The frequency variance as a function of time.  As the piece
  approaches consonance, the frequencies will approach the harmonic
  series. Bounded by 0 and 0.5."
  [t]
  (/ (+ (- (time-func t)) 1) 4))

(defn volume-variance
  "The partial volume variance as a function of time.  As the piece
  approaches consonance, the partial volumes will approach the
  fundamental volumes. Bounded by 0 and 1."
  [t]
  (/ (+ (- (time-func t)) 1) 2))

(defn balance-variance
  "The balance variance as a function of time.  As the piece
  approaches consonance, the balance approaches the center.
  Equivalent to the frequency variance."
  [t]
  (frequency-variance t))

;; ## Stochastic functions

(defn stochastic-partial
  "The frequency of a partial based on the consonant harmonic and fundamental."
  [harmonic t]
  (let [variance (frequency-variance t)
        offset (ranged-rand (- variance) variance)]
    (+ harmonic offset)))

(defn stochastic-partials
  "A list of 12 stochastic partials, as a function of time."
  [t]
  (map #(stochastic-partial % t) harmonic-series-ratios))

(defn stochastic-volume
  "The volume of a partial based on variance and harmonic.  Cannot be
  greater than 1."
  [harmonic t]
  (let [variance (volume-variance t)
        offset (ranged-rand (- variance) variance)]
    (min 1
         (Math/abs (+ (nth fundamental-volumes (- harmonic 1))
                      offset)))))

(defn stochastic-volumes
  "A list of 12 stochastic volumes (for the respective partials), as a
  function of time."
  [t]
  (map #(stochastic-volume % t) harmonic-series-ratios))

(defn stochastic-balance
  "The balance based on variance.  Between 0 and 1."
  [t]
  (let [variance (balance-variance t)
        offset (ranged-rand (- variance) variance)]
    (+ 0.5 offset)))

;; ## Running the piece

(def sine-metronome
  "A metronome with π beats per second.  Will be set once the runner starts."
  nil)

(defn play-sine-group
  "Plays the current sine group (with characteristics determined as a
  function of time) and recursively schedules the next sine group."
  [t]
  (let [next-t         (inc t)
        partials-list  (for [_ (range number-of-partials)]
                       (stochastic-partials t))
        volumes-list   (for [_ (range number-of-partials)]
                       (stochastic-volumes t))
        balance        (stochastic-balance t)
        overall-volume (overall-volume t)
        player         (sine-group-synth
                        {:fundamentals fundamental-frequencies
                         :duration group-duration
                         :partials-list partials-list
                         :volumes-list volumes-list
                         :balance balance
                         :overall-volume overall-volume})]
    (at (sine-metronome t)
      (player))
    (apply-at (sine-metronome next-t) #'play-sine-group [next-t])))

(defn sine-in-a
  []
  (intern *ns* 'sine-metronome (metronome (* Math/PI 60)))
  (play-sine-group (sine-metronome)))


;;(sine-in-a)
;;(stop)

;; ## Demo

(defn consonant-chord []
  (let [fundamentals (map #(* 55.0 %) harmonic-series-ratios)
        volumes-list (take 12 (cycle [fundamental-volumes]))
        partials-list (take 12 (cycle [harmonic-series-ratios]))]
    ((sine-group-synth {:fundamentals fundamentals
                        :duration 5.0
                        :partials-list partials-list
                        :volumes-list volumes-list
                        :balance 0.5
                        :overall-volume 0.1}))))

;;(consonant-chord)
;;(stop)
