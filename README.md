# zeta

A small, portable Clojure library for playing with the Riemann zeta
function and its analytic continuation — values anywhere on ℂ \ {1},
zeros on the critical line, and SVG visualizations, including the
famous "spiral" of ζ(½ + it) threading the origin at every nontrivial
zero.

All library code is plain `.cljc` and runs unchanged on:

* **Clojure (JVM)**
* **[Clojurust](https://github.com/csm/clojurust)** (`cljrs`, platform key `:rust`) — natively or compiled to
  WebAssembly for the interactive web page in [`www/`](www/)
* ClojureScript should also work for the math/viz namespaces (only
  `Math/*` interop is used), though it isn't a primary target

## How values are computed

* For `Re(s) ≥ ½`: Borwein's accelerated alternating-series algorithm
  for the Dirichlet eta function, then `ζ(s) = η(s) / (1 − 2^(1−s))`.
* For `Re(s) < ½`: Riemann's functional equation
  `ζ(s) = 2^s π^(s−1) sin(πs/2) Γ(1−s) ζ(1−s)`, with Γ computed by the
  Lanczos approximation.

Together these give the full analytic continuation. **Precision is
bounded**: everything is IEEE double arithmetic. Expect ~13 significant
digits for moderate arguments; the Borwein term count adapts with
`|Im s|` and stays reliable up to heights around 340. Above that,
`zeta.core/big-z` switches to the Riemann–Siegel formula (main sum +
first correction term, error `O(t^(−3/4))`) — coarse for values, fine
for locating zeros. The number of eta-series terms (the precision
bound) can also be passed explicitly: `(zeta.core/zeta s 60)`.

Known rough edges, by design: `ζ(1)` returns `nil` (the pole); at the
measure-zero points `s = 1 + 2πik/ln 2` the eta→zeta conversion factor
vanishes and you may see NaN.

## Namespaces

| namespace | what's in it |
|---|---|
| `zeta.complex` | complex doubles as `{:re x :im y}`: `add sub mul div exp log pow rpow sin cos sqrt magnitude arg …` |
| `zeta.gamma` | complex `gamma` (Lanczos g=7, reflection formula) |
| `zeta.core` | `zeta`, `eta`, `zeta-critical`, `theta`, `big-z`, `riemann-siegel-z`, `zeros`, `partial-sums` |
| `zeta.viz` | SVG strings: `spiral-svg`, `critical-svg`, `domain-svg`, `partial-sums-svg`, `spiral-points`, `points->json` |
| `zeta.cli` | command-line interface (see below) |

```clojure
(require '[zeta.core :as z] '[zeta.complex :as c] '[zeta.viz :as v])

(z/zeta 2)                        ;=> {:re 1.6449340668482264 :im -0.0}  (π²/6)
(z/zeta -1)                       ;=> {:re -0.0833333333333…}            (-1/12)
(z/zeta (c/complex 0.5 14.13473)) ;≈> 0 — first nontrivial zero
(z/zeros 1 50)                    ;=> [14.134725141734695 21.02203963877156 …]
(z/big-z 25.0)                    ;=> Hardy Z(t): real, vanishes at the zeros

(spit "spiral.svg" (v/spiral-svg {:t0 0 :t1 60}))
(spit "off-critical.svg" (v/spiral-svg {:re 0.75 :t0 0 :t1 60}))
(spit "multi-spiral.svg"
      (v/spiral-svg {:lines [{:re 0.5 :t0 0 :t1 60}
                             {:re 0.75 :t0 0 :t1 60}]}))
(spit "domain.svg" (v/domain-svg {:re0 -8 :re1 8 :im0 -20 :im1 20}))
```

## The spiral

`(v/spiral-svg {:t0 0 :t1 50})` draws the curve `t ↦ ζ(½ + it)` in the
complex plane. The curve starts at `ζ(½) ≈ −1.46`, loops around, and
passes **exactly through the origin** each time `t` hits the imaginary
part of a nontrivial zero — the crosshair in the middle of the picture.
Pass `:re` (or `:sigma`) to draw `t ↦ ζ(σ + it)` on any vertical line, and
pass `:lines` with several line option maps to compose multiple spirals into
one SVG with a shared scale. Segments are colored by `t` for single-line
plots. `critical-svg` shows the same story as
Hardy's real function `Z(t)` with the zeros dotted on the axis, and
`partial-sums-svg` draws the classic unfolding spiral of the Dirichlet
partial sums.

## Running it

### JVM

```sh
clojure -M:cli zeros 1 50
clojure -M:cli spiral 0 60 spiral.svg
clojure -M:cli value 0.5 14.134725
clojure -M:test -m zeta.test-main     # run the test suite
```

### Clojurust

```sh
cljrs run --src-path src main.cljrs -- zeros 1 50
cljrs run --src-path src main.cljrs -- spiral 0 60 spiral.svg
cljrs test --src-path src --src-path test zeta.complex-test zeta.gamma-test zeta.core-test zeta.viz-test
```

### CLI commands

```
value RE [IM]                  ζ(RE + IM·i)
eta RE [IM]                    Dirichlet η
gamma RE [IM]                  Γ(s)
z T                            Hardy Z(T)
theta T                        Riemann–Siegel θ(T)
zeros T0 T1 [STEP]             critical-line zeros in [T0, T1]
spiral T0 T1 [FILE.svg]        the ζ(½+it) spiral
critical T0 T1 [FILE.svg]      Z(t) plot with zeros marked
domain RE0 RE1 IM0 IM1 [FILE.svg] [N]   domain coloring, N×N grid
psums T N [FILE.svg]           partial-sum spiral at s = ½ + iT
```

When `FILE.svg` is omitted the SVG goes to stdout.

## Interactive web page (Clojurust as wasm)

`www/index.html` runs the *same* `.cljc` sources in the browser on
Clojurust's wasm interpreter: draw and animate the spiral (with a live
readout of `t` and `ζ(½+it)`), plot `Z(t)`, render domain colorings,
hunt zeros, and poke at the library in an embedded REPL.

Build the wasm bundle once and serve the repo root:

```sh
git clone https://github.com/csm/clojurust && cd clojurust
wasm-pack build crates/cljrs-wasm --target web
cp -r crates/cljrs-wasm/pkg ../zeta/www/pkg   # adjust paths to taste

cd ../zeta && python3 -m http.server 8000
# open http://localhost:8000/www/
```

The page fetches `src/zeta/*.cljc`, evaluates them in wasm, and drives
everything through `repl.eval` — the SVG you see is generated by the
Clojure code, not by JavaScript.  To keep larger demos from accumulating
interpreter state, each non-REPL demo runs in a short-lived worker/runtime
that is terminated after returning its result.  The embedded REPL keeps its
own persistent worker so interactive definitions survive between evals.

## Verified

The test suite (16 tests, 63 assertions — known values like ζ(2) = π²/6,
ζ(−1) = −1/12, trivial zeros, the functional equation, Γ identities, the
first five nontrivial zeros to 1e-9, and SVG structure) passes
identically on:

* Clojure 1.12 on the JVM (`clojure -M:test -m zeta.test-main`)
* Clojurust native (`cljrs test …`), which also JIT-compiles the hot
  eta loops
* the web page was exercised end-to-end against a real
  `wasm-pack` build of `cljrs-wasm` in headless Chromium (spiral,
  animation, Z(t) plot, domain coloring, zero hunting, REPL)

## Notes on portability

* The math and viz namespaces contain **no reader conditionals** — they
  use only `Math/*` host functions present on the JVM, JS, and
  Clojurust, plus core Clojure. `zeta.cli` is the only namespace with
  platform-specific bits (number parsing, `spit`).
* `format` is deliberately avoided (Clojurust's `format` is
  Rust-style rather than printf-style); SVG numbers are rounded
  arithmetically instead.
* Complex numbers are plain maps, so results print readably at any
  REPL and destructure everywhere.
