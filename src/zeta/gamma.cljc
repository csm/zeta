(ns zeta.gamma
  "Complex gamma function via the Lanczos approximation (g = 7, n = 9),
  with the Euler reflection formula for Re(z) < 1/2.

  Accuracy is roughly 13-15 significant digits for arguments of moderate
  size, which is plenty for driving the functional equation of the zeta
  function at double precision."
  (:require [zeta.complex :as c]))

(def ^:private lanczos-g 7.0)

(def ^:private lanczos-coeffs
  [0.99999999999980993
   676.5203681218851
   -1259.1392167224028
   771.32342877765313
   -176.61502916214059
   12.507343278686905
   -0.13857109526572012
   9.9843695780195716e-6
   1.5056327351493116e-7])

(def ^:private sqrt-2pi 2.5066282746310002)

(declare gamma)

(defn- gamma-positive
  "Lanczos evaluation, valid for Re(z) >= 1/2."
  [z]
  (let [z (c/sub z c/ONE)
        series (loop [i 1
                      acc (c/complex (first lanczos-coeffs) 0.0)]
                 (if (< i (count lanczos-coeffs))
                   (recur (inc i)
                          (c/add acc
                                 (c/div (c/complex (nth lanczos-coeffs i) 0.0)
                                        (c/add z (c/complex i 0.0)))))
                   acc))
        t (c/add z (c/complex (+ lanczos-g 0.5) 0.0))]
    ;; sqrt(2*pi) * t^(z + 1/2) * e^-t * series
    (c/scale (c/mul (c/pow t (c/add z (c/complex 0.5 0.0)))
                    (c/mul (c/exp (c/neg t)) series))
             sqrt-2pi)))

(defn gamma
  "Gamma function of a complex (or real) argument.
  Poles at 0, -1, -2, ... yield non-finite components."
  [z]
  (let [z (c/->complex z)]
    (if (< (:re z) 0.5)
      ;; Reflection: gamma(z) = pi / (sin(pi z) * gamma(1 - z))
      (c/div (c/complex Math/PI 0.0)
             (c/mul (c/sin (c/scale z Math/PI))
                    (gamma-positive (c/sub c/ONE z))))
      (gamma-positive z))))

(defn log-gamma-abs
  "log |gamma(z)| — convenient for scale checks without overflow in the
  final multiply (the Lanczos pieces themselves stay in double range for
  |Im z| up to several hundred)."
  [z]
  (Math/log (c/magnitude (gamma z))))
