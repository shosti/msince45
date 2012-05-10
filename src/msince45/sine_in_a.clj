;; # Sine in A #
;; ### Emanuel Evans ###

;; **Sine in A** is an aleatoric electronic piece exploring
;; harmonicity and inharmonicity.  It is loosely inspired by various
;; techniques of Stockhausen, as well as ideas from minimalism and
;; just intonation composers.  The general process involves groups of
;; 144 sine waves (12 tones with 12 sine waves each) at varying
;; frequencies and pitches.  Each group lasts for one second; there
;; are π groups per second, so approximately three groups will be
;; playing at any given time.  When the piece is at its most harmonic,
;; each sine tone consists of pure harmonics, with the tones'
;; fundamentals aligned with the harmonic series on top of A (55 hz).
;; At its most inharmonic, the pitches, balance, volume, and internal
;; volumes of sine waves are determined almost completely at random.
;; Harmonicity varies as a function of time according to a large-scale
;; sine wave (thus the piece is self-similar at the macro and micro
;; levels).

(ns msince45.sine-in-a
  (:use [overtone.live]))

;; ## Basic constants

(def a-fundamental
  "**Sine in A** is fundamentally based on the frequency of 55 hz (a low
  A).  All sine groups are based on this pitch."
  55.0)

(def number-of-partials
  "Each sine tone is comprised of 12 partials, and each sine group is
  comprised of 12 sine tones."
  12)

(def group-duration
  "Each sine group lasts exactly 1 second."
  1.0)

(def volume-max
  "To prevent clipping due to the additive sine waves, overall volume
  is scaled by a factor of \\\\(\\frac{1}{10}\\\\)."
  0.1)

(def harmonic-series-ratios
  "The harmonic series ratios are simply the numbers 1 through 12."
  (map inc (range number-of-partials)))

(def fundamental-frequencies
  "The fundamentals for the sine tones in each group are centered
around the harmonic series of 55 hz (55 hz, 110 hz, 165 hz, etc)."
  (map #(* a-fundamental %) harmonic-series-ratios))

(def fundamental-volumes
  "At times of harmonicity, each successive partial in a sine tone
will be halved in volume (so the respective volumes will be 1, 0.5,
0.25, etc)."
  (take number-of-partials (iterate #(/ % 2) 1.0)))

;; ## Sound generators

;; Sound generation in **Sine in A** is relatively simple: there are two
;; composite generators, one for sine tones and one for sine groups,
;; and one synthesizer generator for sine groups with given parameters.

(defcgen sine-tone
  "A sine tone is created through a composite generator that simply
  adds together 12 sine tones.  The frequencies are determined by
  multiplying the fundamental by the partials ratios (the resulting
  sine tones are then multiplied by their respective volumes).  Note
  that if the first partial ratio is not 1, the fundamental of the
  resulting tone will technically be the first partial ratio
  multiplied by `fundamental` parameter."
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
  "A sine group is created by adding together 12 sine tones (for a
total of 144 sine waves).  The resulting tones are multiplied by a
1-second sound envelope (using Overtone's built-in `sine` envelope)."
  [fundamentals
   {:doc "The fundamentals for the group."}
   duration
   {:doc "The duration of the group, in seconds."}
   partials-list
   {:doc "A vector of 12 vectors, each with 12 partial frequencies."}
   volumes-list
   {:doc "A vector of 12 vectors, each with 12 respective
          volumes for the partials."}]
  (:ar
   (let [env (env-gen (sine duration) :action FREE)
         events (map #(sine-tone %1 %2 %3)
                     fundamentals partials-list volumes-list)]
     (* env
        (apply + events)))))

(defn sine-group-synth
  "`sine-group-synth` returns a synthesizer for a sine group with the
given balance and overall group volume."
  [{:keys [fundamentals
           duration
           partials-list
           volumes-list
           balance
           group-volume]}]
  (synth []
         (out 0
              (* group-volume
                 [balance (- 1 balance)]
                 (sine-group fundamentals
                             duration
                             partials-list
                             volumes-list)))))

;; ## Deterministic functions

;; The deterministic functions in **Sine in A** form the core of the
;; changing sounds over time.  The essential technique is to determine
;; variance for random variables as a function of time.  The four
;; variances are: group volume variance; frequency variance; partial
;; volume variance; and balance variance.  Each of these variances
;; decreases as the piece approaches harmonicity.

;; $$ f(t) = \frac{\sin(\frac{t}{60} - \frac{\pi}{2}) + 1}{2} $$
(defn time-func
  "The fundamental time function (\\\\(f(t)\\\\)) is essentially a
measure of harmonicity.  It is a sine function scaled so that the
result is between 0 and 1, and a full cycle lasts 120π beats (2
minutes at π beats per second):"
  [t]
  (/ (+ (Math/sin (- (/ t 60) (/ Math/PI 2))) 1) 2))

;; $$ 1 - f(t) $$
(defn group-volume-variance
  "The volume of each group varies when the piece is inharmonic.  The
maximum variance is 1, and the variance is inversely proportional to
the time function:"
  [t]
  (- 1 (time-func t)))

;; $$ \frac{1 - f(t)}{4} $$
(defn frequency-variance
  "As the piece becomes inharmonic, the partial ratio variance will
  approach 0.25 (for all frequencies to be possible, ratios would have
  to vary by 0.5, but in practice it seems difficult to distinguish
  variances above 0.25):"
  [t]
  (/ (- 1 (time-func t)) 4))

;; $$ 1-f(t) $$
(defn partial-volume-variance
  "The volumes of the partials will vary by a maximum of 1 (they are
clipped at 1):"
  [t]
  (- 1 (time-func t)))

;; $$ \frac{1 - f(t)}{2} $$
(defn balance-variance
  "As the piece approaches harmonicity, the balance is at the center,
while during inharmonic periods it varies up to 0.5 (a balance of 0.5
indicates centered sound):"
  [t]
  (/ (- 1 (time-func t)) 2))


(defn overall-volume
  "For balance and safety reasons, the overall volume of the piece
decreases during periods of inharmonicity to a minimum of
\\\\(\\frac{1}{2}\\\\) (otherwise inharmonic periods seem to be
louder)."
  [t]
  (+ 0.5 (/ (time-func t) 2)))

;; ## Stochastic functions

;; The stochastic functions in **Sine in A** are very simple: a random
;; offset bounded by the variance is added to each variable.

(defn stochastic-group-volume
  "The group volume will be decreased by an amount up to the variance
as a function of time.  For safety reasons, the volume is also scaled
according to the maximum volume and the overall volume."
  [t]
  (let [variance (group-volume-variance t)
        offset (ranged-rand 0 variance)]
    (* volume-max (overall-volume t) (- 1 offset))))

(defn stochastic-partial
  "Each partial can vary by ± the partial variance."
  [harmonic t]
  (let [variance (frequency-variance t)
        offset (ranged-rand (- variance) variance)]
    (+ harmonic offset)))

(defn stochastic-partials
  "The `stochastic-partial` function is mapped over the harmonic
series ratios to return a list of random partial ratios."
  [t]
  (map #(stochastic-partial % t) harmonic-series-ratios))

(defn stochastic-partial-volume
  "Partial volume determination is similar to partial ratio
determination, but resulting volumes are bounded above by 1 and
cannot be negative."
  [harmonic t]
  (let [variance (partial-volume-variance t)
        offset (ranged-rand (- variance) variance)]
    (min 1
         (Math/abs (+ (nth fundamental-volumes (- harmonic 1))
                      offset)))))

(defn stochastic-partial-volumes
  "The mapping process for partial volumes is identical to that of
partial ratios."
  [t]
  (map #(stochastic-partial-volume % t) harmonic-series-ratios))

(defn stochastic-balance
  "Balance varies between 0 and 1; a balance of 0.5 indicates center
balance."
  [t]
  (let [variance (balance-variance t)
        offset (ranged-rand (- variance) variance)]
    (+ 0.5 offset)))

;; ## Running the piece

;; The process for running **Sine in A** involves scheduling π sine
;; group synthesizers to play per second.

(def sine-metronome
  "The metronome is set at 60π BPM (π beats per second).  For
implementation reasons, the metronome will be created once the piece
begins (otherwise the piece might not begin at time 0)."
  nil)

(defn play-sine-group
  "A new sine group synthesizer is dynamically created and scheduled
at each metronome beat, with characteristics determined by the
aleatoric process.  The next synthesizer is recursively scheduled for
the next metronome beat using `apply-at`."
  [t]
  (let [next-t         (inc t)
        partials-list  (for [_ (range number-of-partials)]
                       (stochastic-partials t))
        volumes-list   (for [_ (range number-of-partials)]
                       (stochastic-partial-volumes t))
        balance        (stochastic-balance t)
        group-volume   (stochastic-group-volume t)
        player         (sine-group-synth
                        {:fundamentals fundamental-frequencies
                         :duration group-duration
                         :partials-list partials-list
                         :volumes-list volumes-list
                         :balance balance
                         :group-volume group-volume})]
    (at (sine-metronome t)
      (player))
    (apply-at (sine-metronome next-t) #'play-sine-group [next-t])))

(defn sine-in-a
  "To play the piece, a metronome is initialized (using `intern`) and
the first sine group is scheduled."
  []
  (intern *ns* 'sine-metronome (metronome (* Math/PI 60)))
  (play-sine-group (sine-metronome)))

(defn record-sine-in-a
  "Any duration of **Sine in A** can also be recorded to a wav file."
  [fname duration]
  (recording-start fname)
  (sine-in-a)
  (Thread/sleep duration)
  (recording-stop)
  (stop))