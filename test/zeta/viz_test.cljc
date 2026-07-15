(ns zeta.viz-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [zeta.viz :as v]))

(defn- occurrences [s sub]
  (loop [from 0 n 0]
    (let [i (str/index-of s sub from)]
      (if i (recur (+ i (count sub)) (inc n)) n))))

(deftest spiral
  (let [svg (v/spiral-svg {:t0 0 :t1 30 :samples 300})]
    (testing "well-formed SVG with plotted segments"
      (is (str/starts-with? svg "<svg"))
      (is (str/ends-with? svg "</svg>"))
      (is (pos? (occurrences svg "<polyline"))))))

(deftest critical
  (let [svg (v/critical-svg {:t0 0 :t1 30 :samples 300})]
    (is (str/starts-with? svg "<svg"))
    ;; three zeros below t=30, each marked with a circle (plus none extra
    ;; beyond origin/start/end markers used by other plots)
    (is (>= (occurrences svg "<circle") 3))))

(deftest domain
  (let [nx 8 ny 6
        svg (v/domain-svg {:re0 -4 :re1 4 :im0 -4 :im1 4 :nx nx :ny ny})]
    (testing "one rect per grid cell"
      (is (= (* nx ny) (occurrences svg "<rect"))))))

(deftest psums
  (let [svg (v/partial-sums-svg {:t 20 :n 50})]
    (is (str/starts-with? svg "<svg"))
    (is (pos? (occurrences svg "<polyline")))))

(deftest json
  (is (= "[[1.5,-2.0],[3.0,4.25]]"
         (v/points->json [[1.5 -2.0] [3.0 4.25]]))))
