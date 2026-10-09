# org-warrior: prioritized follow-up roadmap

Planning-only backlog notes based on the open GitHub issues and the repository state on `master`. Priorities are relative ordering, not dates or commitments. Each item needs its own implementation scope and review; this document authorizes no code changes, remote Hermes deployment, or access to personal Org files.

## 1. Restore a trustworthy full-suite result

**Priority:** 1 — unblock reliable maintenance.

**Evidence:** [Issue #5](https://github.com/JuanG970/org-warrior/issues/5) records 17 failures in a 190-test run: 11 formatter assertions disagree with current output/return behavior, and six note-integration tests time out while updating Org ID locations. The issue also records a passing 136-test subset that excludes both files. `tests/test_note_integration.py` uses an Emacs server and creates temporary Org files; `org_warrior/TaskFormatter.py` currently writes task details to output and returns `None`.

**Next steps:** Reproduce from a clean checkout with the frozen lockfile; decide and document the intended formatter contract before changing assertions or implementation. Make Emacs integration tests use an explicit isolated daemon/socket and temporary test data, or state a deterministic prerequisite/skip boundary. Keep owner files and the default personal Org tree out of fixtures.

**Acceptance criteria:** Record exact full-suite and focused results; formatter tests agree with the chosen public output/return contract; integration tests either pass against an isolated test daemon or skip with a precise reason when that prerequisite is absent. A partial pass is not reported as a green full suite.

## 2. Reconcile task-identity guarantees for mutations

**Priority:** 2 — prevent a displayed task from resolving to a different target.

**Evidence:** `feature-requests/bugs.org` describes a historical scheduling workflow where filtered numeric positions changed between commands. The current `org_warrior/main.py` mutation commands accept a task ID and resolve handles before calling Org operations; `feature-requests/org-id-requirments.org` defines Org IDs as canonical and handles as stable aliases. The historical report and current interface therefore need an explicit compatibility decision.

**Next steps:** Verify which numeric/index forms are still supported, then make command help and examples unambiguous. Add regression coverage for the supported handle/Org-ID paths and for any retained index path under a changed or filtered task list; if indices are unsupported, give a clear error rather than implying they are stable identifiers.

**Acceptance criteria:** A user can determine supported identifiers from CLI help; tests demonstrate that a mutation targets the same Org ID after list filtering/reordering, or that an unsupported positional form fails clearly. No test uses a personal Org tree.

## 3. Repair the quick-start command path

**Priority:** 3 — make first-run instructions executable from this checkout.

**Evidence:** `README.md` points to `./src/org-warrior --help`, while `AGENTS.md` says that path does not exist and identifies `org_warrior/main.py`; `pyproject.toml` declares the `org-warrior` entry point. The contributor guide documents `uv run --frozen org-warrior --help`.

**Next steps:** Update the README's install/run instructions and examples to match the package layout and declared entry point. Check each documented command against the actual CLI help rather than carrying forward stale paths.

**Acceptance criteria:** A clean environment following the documented setup can run the documented help command; every command example names an existing CLI command and uses its current argument shape.

## 4. Make legacy Org-ID onboarding explicitly safe

**Priority:** 4 — reduce surprise before bulk changes to user data.

**Evidence:** `README.md` says handles depend on existing Org IDs and points users to `handles-assign`; `feature-requests/org-id-requirments.org` calls for batch assignment on legacy headings. `AGENTS.md` requires explicitly selected file scope and prohibits using the owner's personal Org files as agent fixtures.

**Next steps:** Specify a conservative onboarding flow for legacy files: show the selected file set and proposed changes first, make mutation opt-in, and document backup/recovery expectations. Keep automation away from implicit `~/org` scope.

**Acceptance criteria:** The documented workflow makes its target files visible before writes; any future batch mutation has a non-mutating preview and tests using disposable fixtures; no default or test path can silently scan or edit a personal Org tree.
