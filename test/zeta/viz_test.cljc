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

(deftest spiral-arbitrary-real-part
  (let [pts (v/spiral-points 0.75 0 4 5)
        svg (v/spiral-svg {:re 0.75 :t0 0 :t1 4 :samples 5})]
    (is (= 5 (count pts)))
    (is (every? #(= 0.75 (:sigma %)) pts))
    (is (str/includes? svg "&#950;(0.75 + it)"))))

(deftest spiral-multiple-lines
  (let [svg (v/spiral-svg {:t0 0 :t1 10 :samples 80
                           :lines [{:re 0.5 :stroke "#111111"}
                                   {:re 0.75 :stroke "#222222"}]})]
    (is (str/includes? svg "#111111"))
    (is (str/includes? svg "#222222"))
    (is (>= (occurrences svg "<polyline") 2))))

(deftest critical
  (let [svg (v/critical-svg {:t0 0 :t1 30 :samples 300})]
    (is (str/starts-with? svg "<svg"))
    ;; three zeros below t=30, each marked with a circle (plus none extra
    ;; beyond origin/start/end markers used by other plots)
    (is (>= (occurrences svg "<circle") 3))
    (is (str/includes? svg "Re(s) = 0.5"))))

(deftest critical-arbitrary-real-part
  (let [svg (v/critical-svg {:re 0.75 :t0 0 :t1 30 :samples 300})]
    (is (str/starts-with? svg "<svg"))
    (is (str/includes? svg "Re(s) = 0.75"))
    ;; off the critical line, zero markers default to off
    (is (zero? (occurrences svg "<circle")))
    (testing "explicit :mark-zeros? true still marks the critical-line zeros"
      (let [svg (v/critical-svg {:re 0.75 :t0 0 :t1 30 :samples 300 :mark-zeros? true})]
        (is (>= (occurrences svg "<circle") 3))))))

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
