(ns zeta.core
  "The Riemann zeta function on the complex plane, via analytic continuation.

  Strategy
  --------
  * For Re(s) >= 1/2 we compute the Dirichlet eta function with Borwein's
    accelerated alternating-series algorithm and use

        zeta(s) = eta(s) / (1 - 2^(1-s))

    which analytically continues zeta to Re(s) > 0 (and in fact converges
    for the whole half-plane we use it on).

  * For Re(s) < 1/2 we apply Riemann's functional equation

        zeta(s) = 2^s pi^(s-1) sin(pi s / 2) Gamma(1-s) zeta(1-s)

    reducing to the previous case.  Together these give the full analytic
    continuation on C \\ {1}.

  Precision is bounded: everything is double-precision floating point.
  Expect ~1e-13 relative accuracy for moderate arguments, degrading as
  |Im s| grows; the Borwein path is reliable up to |Im s| of roughly 340
  (the term count adapts automatically), beyond which `big-z` switches to
  the Riemann-Siegel formula on the critical line.

  Caveats: s = 1 is a pole (returns nil); at the measure-zero points
  s = 1 + 2*pi*i*k/ln 2 the eta/zeta conversion factor vanishes and the
  result may be NaN."
  (:require [zeta.complex :as c]
            [zeta.gamma :as g]))

(def ^:private two-pi 6.283185307179586)

;; ---------------------------------------------------------------------------
;; Dirichlet eta via Borwein's algorithm
;; ---------------------------------------------------------------------------

(defn- borwein-n
  "Number of series terms for ~1e-15 absolute error at height t.
  Error bound: 3/(3+sqrt 8)^n * (1 + 2|t|) e^(pi |t| / 2), so n grows
  linearly with |t|.  Capped where the d_k coefficients would overflow."
  [t]
  (int (min 380.0 (+ 24.0 (* 0.95 (Math/abs (double t)))))))

(defn- borwein-ds
  "Borwein d_0..d_n:  d_k = n * sum_{j=0..k} (n+j-1)! 4^j / ((n-j)! (2j)!)."
  [n]
  (loop [j 1
         term (/ 1.0 n)
         sum (/ 1.0 n)
         ds [1.0]]
    (if (> j n)
      ds
      (let [term (* term (/ (* 4.0 (+ n j -1.0) (+ (- n j) 1.0))
                            (* 2.0 j (- (* 2.0 j) 1.0))))
            sum (+ sum term)]
        (recur (inc j) term sum (conj ds (* n sum)))))))

(defn eta
  "Dirichlet eta (alternating zeta) function at complex (or real) s,
  computed with Borwein's algorithm.  Optional second argument overrides
  the number of series terms (the precision bound)."
  ([s] (let [s (c/->complex s)] (eta s (borwein-n (:im s)))))
  ([s n]
   (let [s (c/->complex s)
         ds (borwein-ds n)
         dn (nth ds n)]
     (loop [k 0
            sign 1.0
            acc c/ZERO]
       (if (= k n)
         (c/scale acc (/ -1.0 dn))
         (recur (inc k)
                (- sign)
                (c/add acc
                       (c/scale (c/rpow (inc k) (c/neg s))
                                (* sign (- (nth ds k) dn))))))))))

;; ---------------------------------------------------------------------------
;; Zeta via eta + functional equation
;; ---------------------------------------------------------------------------

(declare zeta)

(defn- zeta-right
  "zeta(s) for Re(s) >= 1/2, via eta."
  [s n]
  (let [denom (c/sub c/ONE (c/rpow 2.0 (c/sub c/ONE s)))]
    (c/div (if n (eta s n) (eta s)) denom)))

(defn- zeta-left
  "zeta(s) for Re(s) < 1/2, via the functional equation."
  [s n]
  (let [one-minus-s (c/sub c/ONE s)]
    (c/mul (c/rpow 2.0 s)
           (c/rpow Math/PI (c/sub s c/ONE))
           (c/sin (c/scale s (* 0.5 Math/PI)))
           (g/gamma one-minus-s)
           (zeta one-minus-s n))))

(defn zeta
  "Riemann zeta function of a complex (or real) argument, analytically
  continued to all of C except the pole at s = 1 (where nil is returned).
  Optional second argument bounds the eta-series term count (precision)."
  ([s] (zeta s nil))
  ([s n]
   (let [s (c/->complex s)]
     (cond
       (and (= 1.0 (:re s)) (= 0.0 (:im s))) nil
       ;; s = 0 would send the functional equation into the pole at 1
       (and (= 0.0 (:re s)) (= 0.0 (:im s))) (c/complex -0.5 0.0)
       (>= (:re s) 0.5) (zeta-right s n)
       :else (zeta-left s n)))))

(defn zeta-critical
  "zeta(1/2 + it) — the zeta function on the critical line."
  [t]
  (zeta (c/complex 0.5 t)))

;; ---------------------------------------------------------------------------
;; Riemann-Siegel theta and Z
;; ---------------------------------------------------------------------------

(defn theta
  "Riemann-Siegel theta function (asymptotic expansion; good above t ~ 2)."
  [t]
  (let [t (double t)]
    (+ (* 0.5 t (Math/log (/ t two-pi)))
       (* -0.5 t)
       (- (/ Math/PI 8.0))
       (/ 1.0 (* 48.0 t))
       (/ 7.0 (* 5760.0 t t t)))))

(defn- z-via-eta
  "Z(t) = Re(e^(i theta(t)) zeta(1/2 + it)), accurate while Borwein holds."
  [t]
  (let [zt (zeta-critical t)
        th (theta t)]
    (- (* (Math/cos th) (:re zt))
       (* (Math/sin th) (:im zt)))))

(defn riemann-siegel-z
  "Riemann-Siegel formula for Z(t): main sum plus the first (C0) remainder
  term.  Error is O(t^(-3/4)) — coarse for values, fine for zero hunting
  at large t."
  [t]
  (let [t (double t)
        a (Math/sqrt (/ t two-pi))
        n (int (Math/floor a))
        p (- a n)
        th (theta t)
        main (loop [k 1 acc 0.0]
               (if (> k n)
                 (* 2.0 acc)
                 (recur (inc k)
                        (+ acc (/ (Math/cos (- th (* t (Math/log k))))
                                  (Math/sqrt k))))))
        psi (/ (Math/cos (* two-pi (- (* p p) p 0.0625)))
               (Math/cos (* two-pi p)))
        r (* (if (odd? n) 1.0 -1.0)
             (Math/pow (/ two-pi t) 0.25)
             psi)]
    (+ main r)))

(defn big-z
  "Hardy's Z function: real-valued on the real line, |Z(t)| = |zeta(1/2+it)|,
  and Z changes sign exactly at the critical-line zeros.  Uses the accurate
  eta-based evaluation up to t = 340, Riemann-Siegel beyond."
  [t]
  (let [at (Math/abs (double t))]
    (if (<= at 340.0)
      (z-via-eta at)
      (riemann-siegel-z at))))

;; ---------------------------------------------------------------------------
;; Zeros on the critical line
;; ---------------------------------------------------------------------------

(defn- bisect-zero
  "Refine a sign change of f in [lo, hi] by bisection."
  [f lo hi]
  (loop [lo (double lo) hi (double hi) flo (double (f lo)) i 0]
    (let [mid (* 0.5 (+ lo hi))]
      (if (>= i 80)
        mid
        (let [fm (double (f mid))]
          (cond
            (= 0.0 fm) mid
            (< (* flo fm) 0.0) (recur lo mid flo (inc i))
            :else (recur mid hi fm (inc i))))))))

(defn zeros
  "Zeros of zeta on the critical line with imaginary part in [t0, t1],
  found as sign changes of Z(t) and refined by bisection.  Returns a
  vector of t values.  Options: :step — scan grid spacing (default 0.25;
  make it smaller at large heights, where zeros crowd together)."
  ([t0 t1] (zeros t0 t1 {}))
  ([t0 t1 {:keys [step] :or {step 0.25}}]
   (let [t0 (max 1.0 (double t0))
         t1 (double t1)]
     (loop [t t0
            prev (big-z t0)
            found []]
       (if (>= t t1)
         found
         (let [t' (min t1 (+ t step))
               cur (big-z t')
               found (if (< (* prev cur) 0.0)
                       (conj found (bisect-zero big-z t t'))
                       found)]
           (recur t' cur found)))))))

;; ---------------------------------------------------------------------------
;; Partial sums (for the classic Dirichlet-series spiral)
;; ---------------------------------------------------------------------------

(defn partial-sums
  "Partial sums S_m = sum_{k=1..m} k^(-s) for m = 0..n, as a vector of
  complex values.  Plotted in the plane these trace the well-known
  spiral-of-spirals for s on the critical line."
  [s n]
  (let [s (c/->complex s)
        neg-s (c/neg s)]
    (loop [k 1
           acc c/ZERO
           out [c/ZERO]]
      (if (> k n)
        out
        (let [acc (c/add acc (c/rpow k neg-s))]
          (recur (inc k) acc (conj out acc)))))))
