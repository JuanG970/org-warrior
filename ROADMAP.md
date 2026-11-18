# org-warrior roadmap

Planning-only document for the existing tracking issue
[#6](https://github.com/JuanG970/org-warrior/issues/6). It proposes an order for
the next work; it is not approval to implement anything. Every candidate needs
its own scoped issue, branch, tests, and human review. Priorities reflect the
repository as it stands, not dates or commitments.

## Verified state (2026-11-18, `master` @ `ee659b6`)

- Full suite, as `AGENTS.md` defines it:
  `uv run --frozen --group dev pytest tests/ -q`
  → **11 failed, 173 passed, 6 skipped** in 1.24 s.
- All 11 failures are in `tests/test_formatter.py` (detail/JSON/table
  formatting). All 6 skips are `tests/test_note_integration.py`, reason
  `Emacs daemon not running`.
- The daemon-free subset (`--ignore=tests/test_note_integration.py
  --ignore=tests/test_formatter.py`) → **136 passed**.
- `.github/` contains no workflows, only `copilot-instructions.md`, so nothing
  runs these tests automatically.
- Open PR #1 (subtask support, since 2026-02) is unrelated to the items below.
  No open PR covers this document.

## 1. Settle the task-list output contract (P1)

**Evidence.** `TaskFormatter.task_to_dict` writes `index` and `handle`
(`org_warrior/TaskFormatter.py:77`, `:79`), while the rich table it feeds uses a
`#` column (`:183`) and `tests/test_formatter.py:174` asserts `d["#"]`. That
single disagreement explains all 11 failures. The dict is the documented
machine-readable export path (JSON/CSV), so its key names are a contract for
scripts, not an internal detail, yet no README section defines them.

**Candidate.** Choose one schema for the list output — `#`/`index`, handle
inclusion, and the JSON body — write it down, then align the formatter and the
tests to it. Do not "fix" the tests first and call the schema settled.

**Acceptance.**
- The chosen keys and their meanings appear in `README.md` with a small JSON
  example, including what `--no-handles` removes.
- Table columns and dict keys derive from the same field list, so they cannot
  drift again.
- The 11 formatter tests pass against the chosen schema; a deliberate rename
  breaks them.

## 2. Make index-based task selection explicit or unsupported (P1)

**Evidence.** `feature-requests/org-id-requirments.org:11` promises that
"existing Org ID and index-based usage continue to work", but
`HandleCache.resolve_id_token` passes any token containing a digit straight
through as an Org ID (`org_warrior/HandleCache.py:158-161`), and every mutating
command now advertises only "Org ID or handle" (`org_warrior/main.py:421`).
There is no test for numeric tokens. The 2026-02-02 report in
`feature-requests/bugs.org` records the consequence: `schedule ... today 3`
hit `Task ID 9 out of range (1-7)` and scheduled tasks under unexpected IDs.
Numeric indices are still displayed in list output, so users will keep typing
them.

**Candidate.** Decide the selector contract once: drop numeric indices and
reject digit-only tokens with a clear message, or support them only against a
named, stable ordering. Update the requirements file to match the decision.

**Acceptance.**
- Digit-only tokens either resolve deterministically or fail before any Org
  write, with a message naming handles and Org IDs as the supported forms.
- Tests cover exact handle, unique prefix, ambiguous prefix, Org ID, and a
  digit-only token, including that ambiguous and rejected tokens cause no file
  mutation.
- README examples use the supported form only.

## 3. Make the test suite trustworthy and CI-gated (P1)

**Evidence.** `AGENTS.md:13` says not to treat a partial pass as green, but the
full suite currently mixes 11 hard failures with 6 environment skips, and a
green run is impossible on a machine without an Emacs daemon. Nothing runs on
push or pull request: `.github/` has no `workflows/` directory.

**Candidate.** Separate the tests that need a live Emacs/org-ql from those that
do not, so the daemon-free suite is a complete, meaningful gate; make the
daemon-dependent set fail or skip loudly and document how to run it. Add a
no-secret CI job for the daemon-free suite and the CLI `--help` smoke test.

**Acceptance.**
- `uv run --frozen --group dev pytest tests/ -q` is green on a clean checkout
  without an Emacs daemon, or fails with a message that names the exact missing
  prerequisite; no silent skips in the default run.
- A CI workflow runs the daemon-free suite plus `org-warrior --help` on pushes
  and pull requests, with read-only permissions and no secrets.
- The PR template or `AGENTS.md` states which suite is authoritative.

## 4. Repair the quick-start and example commands (P2)

**Evidence.** `README.md:16` and `README.org-warrior.md:9` send readers to
`./src/org-warrior`, which is absent from the tree; `AGENTS.md` already says so.
`README.md:83` shows `org-warrior schedule "tomorrow" paper-salmon`, the reverse
of the real signature `schedule(task_id, date)` (`org_warrior/main.py:420-421`).

**Candidate.** Rewrite the quick start around the supported install path
(`uv run --frozen org-warrior ...`, or `pipx`/`poetry` if that is preferred),
and correct every example to the current CLI signature. Keep one README, or
make the second file point at the first instead of duplicating commands.

**Acceptance.**
- Following the quick start from a clean checkout produces a working
  `org-warrior --help` without referring to an absent path.
- Every documented example matches a current command signature; a script
  extracts the fenced commands and checks them against `--help`.
- Only one file owns the command list, or the duplicate is generated.

## 5. Scope Org-ID onboarding writes (P2)

**Evidence.** `README.md:29-33` says org-warrior does not create IDs but then
tells users IDs are created automatically on save, via a `before-save-hook`
that maps over every heading in the buffer (`README.md:60-65`).
`docs/emacs-setup.el:13` refreshes ID locations over all of `org-directory`,
and `:45-61` generates IDs for a whole buffer and saves it. `AGENTS.md` warns
that the owner's private Org tree is out of scope for agents, and a
repository-local fixture is the only safe demonstration.

**Candidate.** Split read-only indexing from mutating ID creation in the
onboarding guide, make the scope (buffer, file, directory) explicit at each
step, and give a disposable fixture walkthrough instead of instructions that
touch a personal `~/org`.

**Acceptance.**
- Every mutating step names its scope and whether it writes to disk.
- The worked example uses a repository-local or temporary Org file and verifies
  the result without reading private notes.
- The README no longer claims both "does not create IDs" and "creates them on
  save" without explaining the division of labour.

## Suggested order and boundaries

1. Item 1, because it is the only way to get a green suite without guessing.
2. Item 2, because a wrong selector can mutate the wrong task.
3. Item 3, so later PRs have a gate.
4. Items 4 and 5 as documentation fixes that need no design decision.

Out of scope for all of the above: changing the handle algorithm or wordlist,
adding new CLI commands, touching the owner's Org files or Emacs config, and
merging or releasing anything without review. Item 3's CI job needs an explicit
decision before it is added; until then, local commands and their exact output
are the evidence.
