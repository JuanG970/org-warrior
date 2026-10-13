# org-warrior roadmap

Planning candidates only; priority indicates investigation order, not a delivery commitment. Any implementation needs its own scoped issue and review. Evidence below is from the public repository and issue tracker; do not use personal Org files as fixtures.

## 1. Restore a trustworthy full-suite result

**Why first:** a dependable test signal makes later behavior work safer.

**Evidence:** [Issue #5](https://github.com/JuanG970/org-warrior/issues/5) reports 173 passing and 17 failing tests on unchanged `master`. It identifies eleven formatter assertions that may be stale and six Emacs integration timeouts; a focused run excluding those two files passed 136 tests. The issue explicitly asks to distinguish assertion drift from product defects and isolate or clearly gate daemon-dependent tests.

**Candidate work:** reproduce from a clean checkout with the frozen lockfile; classify each formatter mismatch against the intended output contract; give Emacs integration tests an isolated, explicit daemon/socket or a documented skip/gate when that prerequisite is unavailable.

**Acceptance criteria:** record the exact full and focused commands and results; resolve formatter failures only after the output contract is checked; make the integration prerequisite explicit and isolated; ensure CI reports a full-suite result or clearly identifies the gated tests rather than presenting a partial run as full coverage.

## 2. Make the stable task-identity contract consistent

**Evidence:** `README.md` promises that the same Org ID always generates the same handle. `feature-requests/org-id-requirments.org` says Org ID is canonical, requires deterministic handle mapping, and retains numeric-index compatibility. [Issue #6](https://github.com/JuanG970/org-warrior/issues/6) also calls out reconciling those guarantees with the historical scheduling report.

**Candidate work:** state which values are persistent identity (Org ID and its resolved handle) versus display position; compare scheduling and task-operation behavior with the historical report; add regression coverage for identity when task order or schedule changes.

**Acceptance criteria:** the documented contract distinguishes stable identity from list position; tests show an unchanged Org ID resolves consistently across relevant schedule/order changes (with the same wordlist); scheduling examples and output follow that contract, with any intentional compatibility exception documented.

## 3. Repair the quick-start command path

**Evidence:** `README.md` and `README.org-warrior.md` direct readers to `./src/org-warrior --help`, while the repository has no `src/` tree. `pyproject.toml` defines the installed `org-warrior` entry point as `org_warrior.main:app`.

**Candidate work:** choose one supported install/development invocation and align the quick-start and nearby examples with it.

**Acceptance criteria:** every first-run command in the quick-start works from the stated setup on a clean checkout or documented installation; no example points at a nonexistent path; the README names any Emacs/org-ql prerequisite before commands that need it.

## 4. Define safe boundaries for Org-ID onboarding

**Evidence:** `README.md` presents a save hook that creates IDs for headings in an Org buffer. `docs/doom-emacs-setup.md` describes Org-ID tracking and a batch script over agenda files. `feature-requests/org-id-requirments.org` calls for assigning IDs to legacy headings; [Issue #5](https://github.com/JuanG970/org-warrior/issues/5) explicitly rules out using the owner's personal Org files as test fixtures.

**Candidate work:** make the scope and effects of each onboarding route clear (manual, save-hook, and batch); prefer explicitly selected files and an opt-in operation for bulk changes; document preview/backup and recovery expectations before IDs are written.

**Acceptance criteria:** setup documentation states which files/headings each route can modify and how to restrict that scope; bulk onboarding is not described as harmless or silently applied to an unspecified personal collection; tests use temporary Org fixtures and verify selected-file scope, preservation of existing IDs, and repeatable results.
