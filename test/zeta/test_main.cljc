(ns zeta.test-main
  "Portable test entry point: `clojure -M:test -m zeta.test-main` on the
  JVM, or `cljrs test --src-path src --src-path test zeta.complex-test
  zeta.gamma-test zeta.core-test` under Clojurust."
  (:require [clojure.test :as t]
            [zeta.complex-test]
            [zeta.gamma-test]
            [zeta.core-test]
            [zeta.viz-test]))

(defn -main [& _]
  (let [result (t/run-tests 'zeta.complex-test
                            'zeta.gamma-test
                            'zeta.core-test
                            'zeta.viz-test)
        failures (+ (:fail result 0) (:error result 0))]
    #?(:clj (System/exit (if (pos? failures) 1 0))
       :default (when (pos? failures)
                  (println "FAILURES:" failures)))))
