(ns zeta.cli
  "Command-line interface for the zeta library.

  JVM:        clojure -M -m zeta.cli <command> [args...]
  Clojurust:  cljrs run --src-path src main.cljrs -- <command> [args...]

  Commands:
    value RE [IM]                  zeta(s) at s = RE + IM*i
    eta RE [IM]                    Dirichlet eta at s
    gamma RE [IM]                  Gamma(s)
    z T                            Hardy Z(T)
    theta T                        Riemann-Siegel theta(T)
    zeros T0 T1 [STEP]             critical-line zeros with Im in [T0,T1]
    spiral T0 T1 [FILE.svg]        the zeta(1/2+it) spiral
    critical T0 T1 [FILE.svg]      Z(t) + |zeta| plot with zeros marked
    domain RE0 RE1 IM0 IM1 [FILE.svg] [N]   domain coloring (N x N grid)
    psums T N [FILE.svg]           partial-sum spiral at s = 1/2 + iT
    help                           this text

  When FILE.svg is omitted the SVG is printed to stdout, so you can
  always redirect instead of relying on file I/O."
  (:require [zeta.core :as z]
            [zeta.complex :as c]
            [zeta.gamma :as g]
            [zeta.viz :as v]))

(defn- ->num
  "Parse a number from a string (also accepts numbers as-is)."
  [x]
  (if (string? x)
    #?(:clj (Double/parseDouble x)
       :cljs (js/parseFloat x)
       :default (* 1.0 (parse-double x)))
    (* 1.0 x)))

(defn- complex-str [zv]
  (if (nil? zv)
    "pole (s = 1)"
    (str (:re zv) (if (< (:im zv) 0.0) " - " " + ") (Math/abs (:im zv)) "i")))

(defn- emit
  "Write SVG to a file when a path is given, else print it."
  [svg file]
  (if file
    #?(:cljs (println svg)
       :default (do (spit file svg)
                    (println "wrote" file)))
    (println svg)))

(def ^:private help-text
  (str "zeta — play with the Riemann zeta function\n\n"
       "  value RE [IM]                zeta(RE + IM*i)\n"
       "  eta RE [IM]                  Dirichlet eta\n"
       "  gamma RE [IM]                Gamma function\n"
       "  z T                          Hardy Z(T)\n"
       "  theta T                      Riemann-Siegel theta(T)\n"
       "  zeros T0 T1 [STEP]           critical-line zeros\n"
       "  spiral T0 T1 [FILE.svg]      zeta(1/2+it) spiral\n"
       "  critical T0 T1 [FILE.svg]    Z(t) plot with zeros\n"
       "  domain RE0 RE1 IM0 IM1 [FILE.svg] [N]  domain coloring\n"
       "  psums T N [FILE.svg]         partial-sum spiral\n"))

(defn -main [& args]
  (let [[cmd & more] args
        arg (fn [i] (nth (vec more) i nil))
        num (fn [i] (->num (arg i)))]
    (case cmd
      "value"
      (println (complex-str (z/zeta (c/complex (num 0) (if (arg 1) (num 1) 0.0)))))

      "eta"
      (println (complex-str (z/eta (c/complex (num 0) (if (arg 1) (num 1) 0.0)))))

      "gamma"
      (println (complex-str (g/gamma (c/complex (num 0) (if (arg 1) (num 1) 0.0)))))

      "z"
      (println (z/big-z (num 0)))

      "theta"
      (println (z/theta (num 0)))

      "zeros"
      (let [opts (if (arg 2) {:step (num 2)} {})
            zs (z/zeros (num 0) (num 1) opts)]
        (doseq [t zs] (println t))
        (println ";" (count zs) "zeros"))

      "spiral"
      (emit (v/spiral-svg {:t0 (num 0) :t1 (num 1)}) (arg 2))

      "critical"
      (emit (v/critical-svg {:t0 (num 0) :t1 (num 1)}) (arg 2))

      "domain"
      (let [n (if (arg 5) (int (->num (arg 5))) 96)]
        (emit (v/domain-svg {:re0 (num 0) :re1 (num 1)
                             :im0 (num 2) :im1 (num 3)
                             :nx n :ny n})
              (arg 4)))

      "psums"
      (emit (v/partial-sums-svg {:t (num 0) :n (int (->num (arg 1)))}) (arg 2))

      (println help-text))))
