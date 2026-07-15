(ns zeta.gamma-test
  (:require [clojure.test :refer [deftest is testing]]
            [zeta.complex :as c]
            [zeta.gamma :as g]))

(deftest real-values
  (testing "integers: Gamma(n) = (n-1)!"
    (is (c/close? (g/gamma 1) (c/complex 1 0) 1.0e-12))
    (is (c/close? (g/gamma 5) (c/complex 24 0) 1.0e-9))
    (is (c/close? (g/gamma 10) (c/complex 362880 0) 1.0e-4)))
  (testing "half-integers"
    (is (c/close? (g/gamma 0.5) (c/complex (Math/sqrt Math/PI) 0) 1.0e-12))
    (is (c/close? (g/gamma 1.5) (c/complex (* 0.5 (Math/sqrt Math/PI)) 0) 1.0e-12)))
  (testing "reflection into negative reals: Gamma(-0.5) = -2 sqrt(pi)"
    (is (c/close? (g/gamma -0.5) (c/complex (* -2.0 (Math/sqrt Math/PI)) 0) 1.0e-11))))

(deftest complex-values
  (testing "|Gamma(i)|^2 = pi / sinh(pi)"
    (let [gv (g/gamma c/I)]
      (is (< (Math/abs (- (c/abs2 gv) (/ Math/PI (Math/sinh Math/PI)))) 1.0e-12))))
  (testing "recurrence Gamma(z+1) = z Gamma(z)"
    (let [zv (c/complex 0.3 2.2)]
      (is (c/close? (g/gamma (c/add zv c/ONE))
                    (c/mul zv (g/gamma zv))
                    1.0e-12)))))
