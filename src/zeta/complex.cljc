(ns zeta.complex
  "Complex arithmetic over double-precision floats.

  Complex numbers are represented as plain maps `{:re x :im y}` so the
  code is portable across Clojure (JVM), ClojureScript, and Clojurust
  (https://github.com/csm/clojurust).  Only `Math/*` host functions that
  exist on all three platforms are used, so this namespace contains no
  reader conditionals.")

(defn complex
  "Construct a complex number from real and imaginary parts."
  ([re] (complex re 0.0))
  ([re im] {:re (double re) :im (double im)}))

(def ZERO (complex 0.0 0.0))
(def ONE  (complex 1.0 0.0))
(def I    (complex 0.0 1.0))

(defn re "Real part." [z] (:re z))
(defn im "Imaginary part." [z] (:im z))

(defn ->complex
  "Coerce a real number (or pass through a complex map) to a complex value."
  [x]
  (if (map? x) x (complex x 0.0)))

(defn add
  ([a b]
   (complex (+ (:re a) (:re b)) (+ (:im a) (:im b))))
  ([a b & more]
   (reduce add (add a b) more)))

(defn sub [a b]
  (complex (- (:re a) (:re b)) (- (:im a) (:im b))))

(defn neg [z]
  (complex (- (:re z)) (- (:im z))))

(defn conjugate [z]
  (complex (:re z) (- (:im z))))

(defn scale
  "Multiply complex z by real number k."
  [z k]
  (complex (* (:re z) k) (* (:im z) k)))

(defn mul
  ([a b]
   (let [ar (:re a) ai (:im a) br (:re b) bi (:im b)]
     (complex (- (* ar br) (* ai bi))
              (+ (* ar bi) (* ai br)))))
  ([a b & more]
   (reduce mul (mul a b) more)))

(defn magnitude
  "Absolute value |z|."
  [z]
  (Math/hypot (:re z) (:im z)))

(defn abs2
  "Squared absolute value |z|^2."
  [z]
  (let [x (:re z) y (:im z)]
    (+ (* x x) (* y y))))

(defn arg
  "Principal argument of z, in (-pi, pi]."
  [z]
  (Math/atan2 (:im z) (:re z)))

(defn div
  "Complex division a/b (Smith's algorithm for numerical robustness)."
  [a b]
  (let [ar (:re a) ai (:im a) br (:re b) bi (:im b)]
    (if (>= (Math/abs br) (Math/abs bi))
      (let [r (/ bi br) d (+ br (* bi r))]
        (complex (/ (+ ar (* ai r)) d)
                 (/ (- ai (* ar r)) d)))
      (let [r (/ br bi) d (+ (* br r) bi)]
        (complex (/ (+ (* ar r) ai) d)
                 (/ (- (* ai r) ar) d))))))

(defn inv
  "Reciprocal 1/z."
  [z]
  (div ONE z))

(defn exp
  "Complex exponential e^z."
  [z]
  (let [ex (Math/exp (:re z)) y (:im z)]
    (complex (* ex (Math/cos y)) (* ex (Math/sin y)))))

(defn log
  "Principal branch of the complex logarithm."
  [z]
  (complex (Math/log (magnitude z)) (arg z)))

(defn sin
  "Complex sine."
  [z]
  (let [x (:re z) y (:im z)]
    (complex (* (Math/sin x) (Math/cosh y))
             (* (Math/cos x) (Math/sinh y)))))

(defn cos
  "Complex cosine."
  [z]
  (let [x (:re z) y (:im z)]
    (complex (* (Math/cos x) (Math/cosh y))
             (- (* (Math/sin x) (Math/sinh y))))))

(defn pow
  "Complex power z^w = exp(w log z) (principal branch).  0^w = 0."
  [z w]
  (if (and (zero? (:re z)) (zero? (:im z)))
    ZERO
    (exp (mul w (log z)))))

(defn rpow
  "Real positive base raised to a complex power: r^z = exp(z ln r), r > 0."
  [r z]
  (let [lr (Math/log (double r))]
    (exp (complex (* (:re z) lr) (* (:im z) lr)))))

(defn sqrt
  "Principal complex square root."
  [z]
  (let [m (magnitude z) a (* 0.5 (arg z)) s (Math/sqrt m)]
    (complex (* s (Math/cos a)) (* s (Math/sin a)))))

(defn close?
  "True when a and b agree within absolute tolerance eps (componentwise)."
  [a b eps]
  (and (< (Math/abs (- (:re a) (:re b))) eps)
       (< (Math/abs (- (:im a) (:im b))) eps)))
