(ns zeta.complex-test
  (:require [clojure.test :refer [deftest is testing]]
            [zeta.complex :as c]))

(def eps 1.0e-12)

(deftest arithmetic
  (testing "add/sub/mul/div"
    (is (c/close? (c/add (c/complex 1 2) (c/complex 3 -5)) (c/complex 4 -3) eps))
    (is (c/close? (c/sub (c/complex 1 2) (c/complex 3 -5)) (c/complex -2 7) eps))
    (is (c/close? (c/mul (c/complex 1 2) (c/complex 3 4)) (c/complex -5 10) eps))
    (is (c/close? (c/div (c/complex -5 10) (c/complex 3 4)) (c/complex 1 2) eps))
    (is (c/close? (c/mul c/I c/I) (c/complex -1 0) eps))))

(deftest magnitude-and-arg
  (is (< (Math/abs (- 5.0 (c/magnitude (c/complex 3 4)))) eps))
  (is (< (Math/abs (- (/ Math/PI 2.0) (c/arg c/I))) eps)))

(deftest transcendental
  (testing "exp(i pi) = -1"
    (is (c/close? (c/exp (c/complex 0 Math/PI)) (c/complex -1 0) 1.0e-12)))
  (testing "log(e) = 1"
    (is (c/close? (c/log (c/complex Math/E 0)) c/ONE eps)))
  (testing "exp(log z) = z"
    (let [zv (c/complex -2.5 3.75)]
      (is (c/close? (c/exp (c/log zv)) zv 1.0e-12))))
  (testing "sqrt"
    (is (c/close? (c/sqrt (c/complex -1 0)) c/I eps))
    (is (c/close? (c/mul (c/sqrt (c/complex 3 4)) (c/sqrt (c/complex 3 4)))
                  (c/complex 3 4) 1.0e-12)))
  (testing "powers"
    (is (c/close? (c/pow (c/complex 0 1) (c/complex 2 0)) (c/complex -1 0) 1.0e-12))
    (is (c/close? (c/rpow 2.0 (c/complex 3 0)) (c/complex 8 0) 1.0e-11))
    ;; i^i = e^(-pi/2)
    (is (c/close? (c/pow c/I c/I)
                  (c/complex (Math/exp (* -0.5 Math/PI)) 0) 1.0e-12)))
  (testing "sin/cos identity sin^2 + cos^2 = 1"
    (let [zv (c/complex 1.3 -0.7)
          s (c/sin zv) co (c/cos zv)]
      (is (c/close? (c/add (c/mul s s) (c/mul co co)) c/ONE 1.0e-12)))))
