(ns zeta.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [zeta.complex :as c]
            [zeta.gamma :as g]
            [zeta.core :as z]))

(defn- close-r?
  "Real value zv (complex) close to expected x?"
  [zv x eps]
  (and (< (Math/abs (- (:re zv) (double x))) eps)
       (< (Math/abs (:im zv)) eps)))

(deftest known-real-values
  (testing "zeta(2) = pi^2/6, zeta(4) = pi^4/90"
    (is (close-r? (z/zeta 2) (/ (* Math/PI Math/PI) 6.0) 1.0e-13))
    (is (close-r? (z/zeta 4) (/ (Math/pow Math/PI 4) 90.0) 1.0e-13)))
  (testing "Apery's constant"
    (is (close-r? (z/zeta 3) 1.2020569031595943 1.0e-13)))
  (testing "eta(1) = ln 2"
    (is (close-r? (z/eta 1) (Math/log 2.0) 1.0e-13))))

(deftest analytic-continuation
  (testing "zeta(0) = -1/2"
    (is (close-r? (z/zeta 0) -0.5 1.0e-13)))
  (testing "zeta(-1) = -1/12"
    (is (close-r? (z/zeta -1) (/ -1.0 12.0) 1.0e-12)))
  (testing "zeta(-3) = 1/120"
    (is (close-r? (z/zeta -3) (/ 1.0 120.0) 1.0e-12)))
  (testing "trivial zeros at -2, -4, -6"
    (is (close-r? (z/zeta -2) 0.0 1.0e-12))
    (is (close-r? (z/zeta -4) 0.0 1.0e-12))
    (is (close-r? (z/zeta -6) 0.0 1.0e-12)))
  (testing "zeta(1/2)"
    (is (close-r? (z/zeta 0.5) -1.4603545088095868 1.0e-12)))
  (testing "pole at s=1 returns nil"
    (is (nil? (z/zeta 1)))
    (is (nil? (z/zeta (c/complex 1 0))))))

(deftest functional-equation-consistency
  (testing "chi(s) relation holds across the critical line"
    (doseq [s [(c/complex -2.5 3.0) (c/complex -0.3 -7.5) (c/complex -5.0 11.0)]]
      (let [lhs (z/zeta s)
            one-minus-s (c/sub c/ONE s)
            chi (c/mul (c/rpow 2.0 s)
                       (c/rpow Math/PI (c/sub s c/ONE))
                       (c/sin (c/scale s (* 0.5 Math/PI))))
            rhs (c/mul chi
                       (g/gamma one-minus-s)
                       (z/zeta one-minus-s))]
        (is (c/close? lhs rhs (* 1.0e-10 (max 1.0 (c/magnitude lhs)))))))))

(deftest critical-line
  (testing "zeta vanishes at the first nontrivial zero"
    (is (< (c/magnitude (z/zeta-critical 14.134725141734693)) 1.0e-10)))
  (testing "Z(t) matches |zeta(1/2+it)|"
    (doseq [t [5.0 20.0 33.3]]
      (is (< (Math/abs (- (Math/abs (z/big-z t))
                          (c/magnitude (z/zeta-critical t))))
             1.0e-10))))
  (testing "Riemann-Siegel agrees with eta-based Z at moderate height"
    (doseq [t [100.0 250.0 340.0]]
      (is (< (Math/abs (- (z/riemann-siegel-z t) (z/big-z t))) 0.05))))
  (testing "big-z defaults its second arg to sigma = 1/2"
    (doseq [t [5.0 20.0 33.3]]
      (is (= (z/big-z t) (z/big-z t 0.5)))))
  (testing "off the critical line, Z(t) = Re(e^(i theta(t)) zeta(sigma+it))"
    (doseq [sigma [0.75 1.0 0.3] t [5.0 20.0 33.3]]
      (let [zt (z/zeta (c/complex sigma t))
            th (z/theta t)
            expected (- (* (Math/cos th) (:re zt)) (* (Math/sin th) (:im zt)))]
        (is (< (Math/abs (- (z/big-z t sigma) expected)) 1.0e-10))))))

(deftest zero-finding
  (testing "first five nontrivial zeros"
    (let [expected [14.134725141734693 21.022039638771554 25.010857580145688
                    30.424876125859513 32.935061587739190]
          found (z/zeros 1 33.5)]
      (is (= (count expected) (count found)))
      (doseq [[a b] (map vector expected found)]
        (is (< (Math/abs (- a b)) 1.0e-9))))))

(deftest partial-sums
  (testing "partial sums at s=2 approach pi^2/6"
    (let [sums (z/partial-sums (c/complex 2 0) 2000)]
      (is (< (Math/abs (- (:re (peek sums)) (/ (* Math/PI Math/PI) 6.0)))
             1.0e-3)))))
