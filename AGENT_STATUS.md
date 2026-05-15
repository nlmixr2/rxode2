# Agent Resume Status

## Objective
Implement homogeneous/grouped `rxEt` support end-to-end so large ID sets stay compressed in R, while solve/replay/memory outputs remain parity-safe with expanded event data.

## Plan (active tracks)
1. Finalize homogeneous/grouped storage invariants.
2. Complete `et()` ID mutation/resizing/deletion behavior.
3. Remove remaining implicit R-side materialization from solve-adjacent flows.
4. Finish homogeneous solve/replay paths that still assume per-subject translated IDs.
5. Finish memory-estimation + serialization/state-restore parity for grouped event representations.
6. Expand regression/scale coverage for all above surfaces.

## Current Todo Status

### In progress
- `add-homogeneous-et-tests` — expand regression and scale coverage.
- `design-homogeneous-rxet-layout` — finalize remaining invariants and edge behavior.
- `replace-r-materialization-paths` — remove remaining implicit materialization paths.
- `update-et-id-handling` — complete ID handling edge cases.
- `update-memory-and-serialization` — close remaining grouped memory/serialization parity edges.
- `wire-homogeneous-solve-path` — finish remaining solve/replay branches.

### Done (recent slices)
- `fix-grouped-id-resize-added-ids`
- `fix-grouped-id-resize-disjoint-ids`
- `memory-solve-layout-no-model-icov`
- `optimize-grouped-icov-splitting`
- `rewrite-partial-icov-keep-warning`
- `suppress-icov-keep-false-warning`

## Recent Commits
- `46fa38769` fix: keep grouped events on disjoint id resize
- `e04edd8a7` fix: preserve added ids in grouped resize
- `981c43f08` fix: refine iCov keep missing warnings
- `5c7ebef29` feat: split grouped iCov memory path without model
- `bf3c97147` fix: silence false iCov keep warnings
- `29a2f7f42` perf: avoid over-splitting grouped iCov prep

## Current Findings
- Grouped ID resize edge cases were fixed for both added IDs and fully disjoint replacement IDs.
- Memory estimate parity is good between `rxEt`, grouped data.frame, and expanded data.frame for tested controls.
- Apparent divergence between serialized state inputs and solve-object inputs was due to comparing different solve setups; when derived from the same solve scenario, file/bundle/solve memory estimates are consistent.
- Added regression coverage to lock file/bundle/solve parity for dose-only grouped scenarios.
- Added scale regression for large homogeneous ID replacement (`1:70000` -> disjoint `70001:140000`) to lock grouped storage/no-chunk-cloning behavior.
- Fixed grouped id-only resize with `id=integer(0)` so grouped tables are truly cleared (ids/groups/chunks/counts all reset) instead of retaining stale grouped event rows.
- Added regression coverage for the grouped clear-to-empty resize case.

## Next Step (now)
Continue remaining solve-adjacent completion work, focusing next on residual materialization-sensitive solve/replay branches and parity tests.
