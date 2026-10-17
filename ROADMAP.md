# org-warrior roadmap

Planning candidates only; this document is not approval to implement them. Each
candidate needs its own scoped issue and human review. Priorities reflect the
repository evidence, not dates or commitments. Tracked by the existing
[planning issue #6](https://github.com/JuanG970/org-warrior/issues/6).

## 1. Restore a trustworthy full-suite result

**Evidence:** Open [issue #5](https://github.com/JuanG970/org-warrior/issues/5)
reports 173 passing and 17 failing tests on unchanged code: 11 formatter
expectation failures and six note-integration timeouts waiting for Emacs. It
also reports a 136-test focused pass excluding those files. These are the
issue's recorded results, not a fresh run. `AGENTS.md` documents the frozen
lockfile test command and the Emacs-daemon dependency.

**Candidate:** Reproduce from a clean checkout, distinguish stale formatter
assertions from product defects, and make integration-test daemon/socket setup
explicit and isolated. Keep personal Org files out of fixtures and test scope.

**Acceptance:** Record exact commands and outcomes; classify and resolve each
failure; provide a repeatable full-suite result, or an explicit, documented
gate for environment-dependent integration tests that does not hide failures.

## 2. Resolve the task-identity and selector contract

**Evidence:** `AGENTS.md` identifies Org `:ID:` values as task handles' source
and says positional indices are not stable identifiers. In contrast,
`feature-requests/org-id-requirments.org` requires compatibility with numeric
indices as well as handles and Org IDs. The planning issue also references a
historical scheduling report; no such report is present in the current tracked
tree.

**Candidate:** Locate the referenced report or record that it is unavailable,
then decide and document which selectors are supported for reads versus
mutating commands. Preserve compatibility deliberately rather than treating a
list position as a durable task identity.

**Acceptance:** A public contract covers Org IDs, handles/prefixes, and numeric
indices (including mutation behavior); any compatibility change has an explicit
migration note and focused regression coverage for resolution and ambiguity.

## 3. Repair the quick-start command path

**Evidence:** `README.md` and `README.org-warrior.md` direct readers to
`./src/org-warrior`, but that path is absent from the tracked tree. The current
entry point is `org_warrior/main.py`, and `pyproject.toml` declares the
`org-warrior` console script.

**Candidate:** Replace the obsolete invocation with a clean-checkout install
and smoke-test path, and align the two README files so users are not sent to a
nonexistent script.

**Acceptance:** Following the documented setup from a clean checkout exposes
the CLI and the documented help command succeeds; no quick-start step points
to an absent path.

## 4. Make Org-ID onboarding explicitly scoped and safe

**Evidence:** `README.md` recommends automatic ID creation on save.
`docs/emacs-setup.el` refreshes Org-ID locations over `org-directory` and
includes examples that generate IDs in a buffer or on save. `AGENTS.md` keeps
the owner's private Org files outside agent work and warns against unscoped
access.

**Candidate:** Clarify which setup steps only index IDs and which modify Org
files; make file scope, opt-in behavior, and the effect of assigning IDs
explicit. Keep examples usable with a disposable, repository-local fixture,
not anyone's personal Org tree.

**Acceptance:** The onboarding guide identifies every mutating step and its
scope, defaults examples to an explicitly selected test file or buffer, and
includes a safe verification path that never requires private notes.
