(ns zeta.viz
  "Visualizations of the Riemann zeta function, rendered as SVG strings.

  Everything here returns a plain SVG document string, which makes the
  namespace fully portable: save it to a file from the JVM or Clojurust
  CLI, or inject it straight into the DOM from the Clojurust wasm REPL.

  Highlights:
  * `spiral-svg`   — the curve t -> zeta(1/2 + it).  Every nontrivial
                     zero sends the curve through the origin, so the
                     spiral repeatedly threads the crosshair.
  * `critical-svg` — Hardy's Z(t) and |zeta(1/2+it)| along the line.
  * `domain-svg`   — domain coloring of zeta over a rectangle.
  * `partial-sums-svg` — the spiral-of-spirals of Dirichlet partial sums."
  (:require [zeta.complex :as c]
            [zeta.core :as z]))

;; ---------------------------------------------------------------------------
;; Small formatting / SVG helpers (no `format`: Clojurust's format is
;; Rust-style, so we stick to arithmetic rounding + str)
;; ---------------------------------------------------------------------------

(defn fmt
  "Render a double rounded to 3 decimals; non-finite values become 0."
  [x]
  (let [x (double x)]
    (if (or (not (= x x)) (> (Math/abs x) 1.0e300))
      "0"
      (str (/ (Math/round (* x 1000.0)) 1000.0)))))

(defn- join-str [sep xs]
  (apply str (interpose sep xs)))

(defn- attrs->str [attrs]
  (apply str
         (map (fn [[k v]] (str " " (name k) "=\"" v "\"")) attrs)))

(defn- tag
  ([nm attrs] (str "<" nm (attrs->str attrs) "/>"))
  ([nm attrs & body] (str "<" nm (attrs->str attrs) ">" (apply str body) "</" nm ">")))

(defn- svg-doc [w h & body]
  (str "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" w "\" height=\"" h
       "\" viewBox=\"0 0 " w " " h "\">"
       (apply str body)
       "</svg>"))

(defn- polyline [pts attrs]
  (tag "polyline"
       (assoc attrs
              :fill "none"
              :points (join-str " " (map (fn [[x y]] (str (fmt x) "," (fmt y))) pts)))))

(defn points->json
  "Serialize a seq of [x y] pairs as a JSON string — handy for handing
  data from the wasm REPL to JavaScript."
  [pts]
  (str "[" (join-str "," (map (fn [[x y]] (str "[" (fmt x) "," (fmt y) "]")) pts)) "]"))

(defn- linspace [a b n]
  (let [a (double a) b (double b) step (/ (- b a) (dec n))]
    (mapv (fn [i] (+ a (* i step))) (range n))))

;; ---------------------------------------------------------------------------
;; The critical-line spiral: t -> zeta(1/2 + it)
;; ---------------------------------------------------------------------------

(defn spiral-points
  "Sample zeta(1/2+it) for t in [t0, t1]; returns [{:t t :re x :im y} ...]."
  [t0 t1 samples]
  (mapv (fn [t]
          (let [zt (z/zeta-critical t)]
            {:t t :re (:re zt) :im (:im zt)}))
        (linspace t0 t1 samples)))

(defn spiral-svg
  "SVG of the curve t -> zeta(1/2 + it) in the complex plane.

  The curve passes exactly through the origin at every nontrivial zero
  (marked by the crosshair).  Segments are colored by t so you can follow
  the motion.

  Options (all optional):
    :t0 :t1     — t range (default 0..50)
    :samples    — number of sample points (default 1500)
    :width :height — pixel size (default 720x720)
    :rainbow?   — color segments by t (default true)"
  ([] (spiral-svg {}))
  ([{:keys [t0 t1 samples width height rainbow?]
     :or {t0 0.0 t1 50.0 samples 1500 width 720 height 720 rainbow? true}}]
   (let [pts (spiral-points t0 t1 samples)
         finite (filter (fn [p] (and (= (:re p) (:re p)) (= (:im p) (:im p)))) pts)
         extent (reduce (fn [m p] (max m (Math/abs (:re p)) (Math/abs (:im p))))
                        1.0e-6 finite)
         extent (* 1.08 (min extent 50.0))
         cx (* 0.5 width) cy (* 0.5 height)
         k (/ (* 0.5 (min width height)) extent)
         ->x (fn [re] (+ cx (* k re)))
         ->y (fn [im] (- cy (* k im)))
         xy (fn [p] [(->x (:re p)) (->y (:im p))])
         buckets 72
         per (max 2 (int (Math/ceil (/ (double samples) buckets))))
         segs (if rainbow?
                (loop [i 0 out ""]
                  (let [start (* i per)]
                    (if (>= start (dec (count pts)))
                      out
                      (let [chunk (subvec pts start (min (count pts) (+ start per 1)))
                            hue (Math/round (* 300.0 (/ (double start) samples)))]
                        (recur (inc i)
                               (str out
                                    (polyline (map xy chunk)
                                              {:stroke (str "hsl(" hue ",75%,45%)")
                                               :stroke-width 1.4})))))))
                (polyline (map xy pts) {:stroke "#3366cc" :stroke-width 1.4}))]
     (svg-doc width height
              (tag "rect" {:x 0 :y 0 :width width :height height :fill "#ffffff"})
              ;; axes
              (tag "line" {:x1 0 :y1 (fmt cy) :x2 width :y2 (fmt cy)
                           :stroke "#cccccc" :stroke-width 1})
              (tag "line" {:x1 (fmt cx) :y1 0 :x2 (fmt cx) :y2 height
                           :stroke "#cccccc" :stroke-width 1})
              ;; origin crosshair — the zeros thread this point
              (tag "circle" {:cx (fmt cx) :cy (fmt cy) :r 4
                             :fill "none" :stroke "#000000" :stroke-width 1.2})
              (tag "circle" {:cx (fmt cx) :cy (fmt cy) :r 1.2 :fill "#000000"})
              segs
              ;; start / end markers
              (let [p0 (xy (first pts)) p1 (xy (last pts))]
                (str (tag "circle" {:cx (fmt (first p0)) :cy (fmt (second p0))
                                    :r 3.5 :fill "#22aa44"})
                     (tag "circle" {:cx (fmt (first p1)) :cy (fmt (second p1))
                                    :r 3.5 :fill "#cc3333"})))
              (tag "text" {:x 10 :y 22 :font-family "sans-serif" :font-size 13
                           :fill "#555555"}
                   (str "&#950;(1/2 + it),  t &#8712; [" (fmt t0) ", " (fmt t1) "]"))))))

;; ---------------------------------------------------------------------------
;; Z(t) and |zeta| along the critical line
;; ---------------------------------------------------------------------------

(defn critical-svg
  "SVG plot of Hardy's Z(t) (blue) and |zeta(1/2+it)| (grey) for t in
  [t0, t1], with the located zeros marked on the axis.

  Options: :t0 :t1 (default 0..60), :samples (default 1200),
  :width :height (default 900x360), :mark-zeros? (default true)."
  ([] (critical-svg {}))
  ([{:keys [t0 t1 samples width height mark-zeros?]
     :or {t0 0.0 t1 60.0 samples 1200 width 900 height 360 mark-zeros? true}}]
   (let [ts (linspace (max 0.05 t0) t1 samples)
         zs (mapv z/big-z ts)
         mag (mapv #(Math/abs (double %)) zs)
         top (reduce max 1.0 (map #(min (Math/abs (double %)) 25.0) zs))
         top (* 1.1 top)
         pad-l 40.0 pad-r 10.0 pad-t 10.0 pad-b 26.0
         ->x (fn [t] (+ pad-l (* (- width pad-l pad-r) (/ (- t t0) (- t1 t0)))))
         ->y (fn [v] (let [v (max (- top) (min top (double v)))]
                       (+ pad-t (* (- height pad-t pad-b)
                                   (- 1.0 (/ (+ v top) (* 2.0 top)))))))
         axis-y (->y 0.0)
         zeros (when mark-zeros? (z/zeros (max 1.0 t0) t1))]
     (svg-doc width height
              (tag "rect" {:x 0 :y 0 :width width :height height :fill "#ffffff"})
              (tag "line" {:x1 (fmt pad-l) :y1 (fmt axis-y)
                           :x2 (fmt (- width pad-r)) :y2 (fmt axis-y)
                           :stroke "#bbbbbb" :stroke-width 1})
              ;; |zeta| envelope
              (polyline (map (fn [t m] [(->x t) (->y m)]) ts mag)
                        {:stroke "#c9c9c9" :stroke-width 1})
              ;; Z(t)
              (polyline (map (fn [t v] [(->x t) (->y v)]) ts zs)
                        {:stroke "#2255bb" :stroke-width 1.5})
              (apply str
                     (map (fn [t]
                            (tag "circle" {:cx (fmt (->x t)) :cy (fmt axis-y)
                                           :r 3 :fill "#cc3333"}))
                          (or zeros [])))
              (tag "text" {:x (fmt pad-l) :y (fmt (- height 8))
                           :font-family "sans-serif" :font-size 12 :fill "#555555"}
                   (str "Z(t) on t &#8712; [" (fmt t0) ", " (fmt t1) "]"
                        (if (seq zeros)
                          (str " — " (count zeros) " zeros marked")
                          "")))))))

;; ---------------------------------------------------------------------------
;; Domain coloring
;; ---------------------------------------------------------------------------

(defn- domain-color
  "Standard domain coloring: hue from arg(z), lightness from |z|
  (zeros go black, the pole goes white)."
  [zv]
  (if (nil? zv)
    "hsl(0,0%,100%)"
    (let [m (c/magnitude zv)]
      (if (or (not (= m m)) (> m 1.0e300))
        "hsl(0,0%,100%)"
        (let [hue (Math/round (mod (+ 360.0 (/ (* 180.0 (c/arg zv)) Math/PI)) 360.0))
              light (Math/round (* 100.0 (- 1.0 (Math/pow 2.0 (- (Math/sqrt m))))))]
          (str "hsl(" hue ",85%," light "%)"))))))

(defn domain-svg
  "Domain coloring of zeta over the rectangle [re0,re1] x [im0,im1].
  Hue encodes arg(zeta(s)); brightness encodes |zeta(s)| — nontrivial
  zeros appear as black points on the critical line, trivial zeros on the
  negative real axis, and the pole at s=1 as a white flare.

  Options: :re0 :re1 :im0 :im1 (default -8..8 x -20..20),
  :nx :ny — grid resolution (default 96x96; SVG size grows with this),
  :width :height (default 640x640)."
  ([] (domain-svg {}))
  ([{:keys [re0 re1 im0 im1 nx ny width height]
     :or {re0 -8.0 re1 8.0 im0 -20.0 im1 20.0 nx 96 ny 96 width 640 height 640}}]
   (let [dx (/ (- re1 re0) nx)
         dy (/ (- im1 im0) ny)
         cw (/ (double width) nx)
         ch (/ (double height) ny)
         ;; nested map/mapcat rather than a multi-binding `for`
         ;; (Clojurust's `for` supports a single binding pair)
         cells (mapcat
                (fn [j]
                  (map (fn [i]
                         (let [sre (+ re0 (* (+ i 0.5) dx))
                               sim (+ im0 (* (+ j 0.5) dy))
                               zv (z/zeta (c/complex sre sim))]
                           (tag "rect" {:x (fmt (* i cw))
                                        :y (fmt (* (- ny 1 j) ch))
                                        :width (fmt (+ cw 0.5))
                                        :height (fmt (+ ch 0.5))
                                        :fill (domain-color zv)})))
                       (range nx)))
                (range ny))]
     (svg-doc width height
              (apply str cells)
              ;; critical line marker
              (let [x (fmt (* width (/ (- 0.5 re0) (- re1 re0))))]
                (tag "line" {:x1 x :y1 0 :x2 x :y2 height
                             :stroke "#ffffff" :stroke-width 0.6
                             :stroke-dasharray "4,4" :opacity 0.7}))))))

;; ---------------------------------------------------------------------------
;; Partial-sum spiral (Dirichlet series on the critical line)
;; ---------------------------------------------------------------------------

(defn partial-sums-svg
  "SVG of the partial sums of sum k^(-s) — for s = 1/2 + it these trace
  the classic unfolding double-spiral.

  Options: :t — imaginary part of s on the critical line (default the
  height of the first zero), :s — full complex s (overrides :t),
  :n — number of terms (default 400), :width :height (default 720x720)."
  ([] (partial-sums-svg {}))
  ([{:keys [t s n width height]
     :or {t 14.134725141734693 n 400 width 720 height 720}}]
   (let [s (or s (c/complex 0.5 t))
         sums (z/partial-sums s n)
         xs (map :re sums) ys (map :im sums)
         minx (reduce min xs) maxx (reduce max xs)
         miny (reduce min ys) maxy (reduce max ys)
         spanx (max 1.0e-6 (- maxx minx))
         spany (max 1.0e-6 (- maxy miny))
         k (* 0.9 (min (/ width spanx) (/ height spany)))
         cx (* 0.5 (+ minx maxx)) cy (* 0.5 (+ miny maxy))
         ->x (fn [x] (+ (* 0.5 width) (* k (- x cx))))
         ->y (fn [y] (- (* 0.5 height) (* k (- y cy))))]
     (svg-doc width height
              (tag "rect" {:x 0 :y 0 :width width :height height :fill "#ffffff"})
              (polyline (map (fn [x y] [(->x x) (->y y)]) xs ys)
                        {:stroke "#7733aa" :stroke-width 1.1})
              (tag "circle" {:cx (fmt (->x 0.0)) :cy (fmt (->y 0.0)) :r 3
                             :fill "#22aa44"})
              (tag "text" {:x 10 :y 22 :font-family "sans-serif" :font-size 13
                           :fill "#555555"}
                   (str "Partial sums of &#931; k^(-s), s = "
                        (fmt (:re s)) " + " (fmt (:im s)) "i, n = " n))))))
