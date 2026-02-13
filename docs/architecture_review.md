# Architecture Review: ClojurePHP Compiler

**Date:** 2026-01-14
**Rating:** A- (Strong)

## Executive Summary

The compiler architecture is remarkably mature for its stage. It adopts a "Highway" model (Orchestrator -> IR levels -> Backend) that successfully decouples the compilation pipeline from target-specific concerns. This is a robust foundation for a multi-target Clojure implementation (PHP, JS, etc.).

## Strengths

1.  **Orchestrator Pattern (`clojure.compiler.orchestrator`)**
    *   **Verdict:** Excellent.
    *   **Reasoning:** Explicitly managing "Delivery Axes" (Output Shape, Runtime Mode, Caching) separate from the compilation logic is a key differentiator. It allows "Zero Friction" delivery (e.g., single-file PHP scripts vs full Composer projects) without changing the core compiler.

2.  **Source Path Tracking (`clojure.compiler.source-path`)**
    *   **Verdict:** Best-in-Class (SBCL-style).
    *   **Reasoning:** Most transpilers (Babel, chemically-pure ClojureScript) only track line/column. Tracking the *transformation history* (e.g., "this code came from macro expansion X -> inline Y") is critical for the "SLIME-like DX" goal. It enables superior error messages and debugging.

3.  **Analyzer Depth (`clojure.compiler.analyzer`)**
    *   **Verdict:** Complete.
    *   **Reasoning:** The analyzer is not a toy. It correctly handles complex Clojure semantics (recur targets, closure capturing, `deftype*`/`defrecord*`) and maps them to a rich HIR (High-Level Intermediate Representation).

4.  **Host Abstraction (`clojure.php.host`)**
    *   **Verdict:** Correct.
    *   **Reasoning:** Reifying the `Host` protocol allows the core library to be written in portable Clojure while delegating performant primitives to the native platform.

## Weaknesses & Risks

1.  **Emitter Duality (Technical Debt)**
    *   **Issue:** We currently have two competing emitter implementations:
        *   `clojure.php.emit-pure`: Functional, manual string concatenation.
        *   `clojure.compiler.emit` (Generic): Buffer-based, data-driven, O(n) performance.
    *   **Risk:** Maintaining two backends splits engineering focus. `emit-pure` is simple but won't scale to source maps efficiently or support other C-family languages easily.
    *   **Recommendation:** **Deprecate `emit-pure`**. Port the remaining functionality (Namespace/File generation) to the Generic Emitter and make it the primary backend.

2.  **IR Complexity**
    *   **Issue:** The pipeline defines IR1, IR2, and IR3.
    *   **Risk:** While powerful, this requires lowering logic (`lower.cljc`). Ensure the generic-emitter stays at **IR1** (HIR) for high-level targets like PHP/JS to avoid unnecessary complexity (re-implementing basic blocks for languages that already have control flow).

## Conclusion

The architecture is sound. The move to the "Generic Emitter" was the right choice to solve the O(n^2) performance risk of string concatenation and to enable robust source map generation.

**Next Immediate Step:** Finish porting `emit-file` / Namespace logic from `emit-pure` to `clojure.php.emit` and officially switch the Orchestrator to use the Generic Emitter.
