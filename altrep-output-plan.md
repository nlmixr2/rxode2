# ALTREP expansion plan for rxode2 solve outputs

## Problem

rxode2 already uses ALTREP for predictable `id` and `sim.id` output
columns, but the solve output still materializes many other columns
that are repeated across rows or simulations. The main missed cases
are:

- homogeneous/grouped event tables, where event-table-derived columns
  can be reconstructed from one representative event block plus group
  metadata instead of storing full per-id copies;
- repeated simulations from one event table (`nsim`, `nSub`,
  `nStud`, and related population-simulation paths), where event
  columns, individual metadata, covariates, and translated keep
  columns are duplicated across solve replicas;
- mixed solve paths that share the same event structure but differ
  only in solved states or sampled parameters.

The plan should preserve current data-frame behavior while avoiding
compression whenever runtime event-related logic makes the output no
longer safely reconstructable, especially `evid_()`/`obs()` pushes and
other dynamic dosing or dynamic observation changes.

## Proposed approach

1. Build a column eligibility matrix for solve outputs so each column
   is classified as:
   - always materialized;
   - ALTREP from a simple sequence/repetition pattern;
   - ALTREP from a representative event block plus replication
     metadata;
   - ALTREP from subject/study-level metadata repeated over a row
     block;
   - forced materialized because runtime event-related logic changed
     the event data in a way that should not be reconstructed lazily.
2. Push compression metadata into solve setup so `rxode2_df()` knows
   whether the output is backed by:
   - homogeneous grouped events (`rxHomGroups`);
   - shared per-subject event blocks reused across simulations;
   - shared keep/covariate blocks copied only because the output
     currently expects a fully expanded vector.
3. Extend the ALTREP layer from `rx_seqrep` to a small family of
   descriptor-driven output classes for integer, real, logical, and
   factor-backed repeated columns.
4. Make `rxode2_df()` emit ALTREP vectors for eligible event,
   covariate, and keep columns instead of allocating/filling expanded
   vectors, with a strict fallback to materialized vectors for
   runtime-mutated event solves.

## Implementation todos

1. **Audit output columns and repetition patterns**
   - Trace every output column created in `src/rxode2_df.cpp`.
   - Map each column back to its source: bookkeeping (`id`,
     `sim.id`), event-table fields (`time`, `evid`, `amt`, `cmt`,
     `ss`, `rate`, `dur`, `ii`), translated event-table
     keep/covariate columns, and solved-state columns.
   - Document which patterns are repeated by homogeneous groups, by
     repeated subjects, and by repeated studies.

2. **Add solve-time compression metadata**
   - Reuse existing grouped-event metadata from `rxHomGroups` and the
     shared-subject setup in `src/rxData.cpp`.
   - Add compact descriptors that tell the output builder when rows
     come from one representative event block reused across
     ids/simulations.
   - Record invalidation flags so ALTREP is disabled when
     `nPushedExtra > 0`, `indOwnAlloc` is active for runtime-grown
     event arrays, or any other runtime event-related logic
     (`evid_()`, `obs()`, dynamic `rate()`, `dur()`, `alag()`,
     `mtime()`) makes lazy reconstruction unsafe.

3. **Generalize the ALTREP implementation**
   - Keep `rx_seqrep` for trivial sequence columns.
   - Add descriptor-based ALTREP classes for:
     - repeated block integers/reals/logicals;
     - factor/integer columns with shared levels;
     - event-derived numeric columns computed from representative
       event rows and replication descriptors.
   - Ensure the classes support fast `Length`, `Elt`,
     `Dataptr_or_null`, materialization, and simple summary
     operations where it is cheap to do so.

4. **Wire ALTREP into `rxode2_df()`**
   - Replace eager allocation for eligible columns with ALTREP
     factories.
   - Skip writing to ALTREP-backed columns inside the parallel/serial
     fill loops, the same way current `id`/`sim.id` handling nulls out
     `colI`.
   - Keep solved states and any genuinely time-varying per-row values
     materialized.

5. **Handle keep/covariate and simulation-specific cases explicitly**
   - Remove the current keep-vector expansion done for homogeneous
     groups when the output can instead reference grouped
     representative rows lazily.
   - Cover repeated outputs from `nSub`/`nsim` and `nStud` paths where
     the event schedule is reused but only subject/study metadata
     changes.
   - Distinguish columns that repeat per subject from columns that
     repeat per study so the right descriptor is used.

6. **Test and document the fallback rules**
   - Add regression tests for grouped homogeneous solves, grouped
     solves with `nsim`, `nSub`, and `nStud`, and mixed keep/covariate
     outputs.
   - Add negative tests showing that dynamic dosing/observation cases
     (`evid_()`, pushed observations, runtime event growth, and other
     event-related runtime changes) fall back to materialized columns.
   - Update `NEWS.md` once implemented.

## Notes and design constraints

- The existing code already has the right hooks:
  - `src/rxode2_altrep.c` / `src/rxode2_altrep.h` contain the current
    ALTREP registration and factories;
  - `src/rxode2_df.cpp` is the only place that assembles the final
    output columns;
  - `src/rxData.cpp` already preserves grouped homogeneous metadata
    via `rxHomGroups` and reuses subject event blocks across
    simulations.
- Current keep handling still expands grouped rows in `src/rxData.cpp`;
  that is the clearest short-term place to stop copying and start
  passing descriptors instead.
- Dynamic exceptions should be conservative. If runtime event-related
  logic changes event values, event timing, row counts, or event-table
  interpretation, prefer materialized output for the affected columns
  rather than risking stale reconstructed values.
- The first implementation should stay internal and behavior-
  preserving: no new user-facing API unless profiling shows a need for
  an opt-out/debug switch.
