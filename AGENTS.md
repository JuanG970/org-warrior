# org-warrior agent guide

## Scope and source of truth

- GitHub Issues in `JuanG970/org-warrior` are the canonical project backlog. Work only on the explicitly assigned issue; read its current body and comments with `gh issue view <number> --repo JuanG970/org-warrior --comments` before coding. An issue in another repository, an Org mirror, or a local TODO is not an assignment.
- The owner's personal Org files and Emacs configuration are not part of this repository and are not required for a remote coding agent. Do not search, copy, or commit them. Do not put credentials or private notes in issues, PRs, logs, or this repository; issues in this repository are public.
- The Org mirror is a local view of GitHub Issues, not another backlog. Changes to a local Org file do not reach GitHub without an explicit, verified sync. Hermes issue dispatch is not installed by this guide: a label or issue alone does not launch an agent.

## Development

- The CLI is a Python package: `org_warrior/main.py` defines the Typer entry point (`org-warrior`); `org_warrior/Org.py` invokes Emacs/org-ql. The `src/org-warrior` path in older guides does not exist in this checkout. Reads query org-ql; writes use Emacs/Org APIs. Task handles come from Org `:ID:` properties; positional indices are not stable task identifiers.
- `ORG_WARRIOR_FILES` chooses file scope (default `~/org`); `ORG_WARRIOR_SERVER` selects the Emacs socket (default `edit`); `ORG_WARRIOR_LOAD_PATH` and `EMACS_CMD` customize the client. The configured Emacs timeout is 15 seconds. Inspect the target file and daemon before a mutating CLI command.
- Use the repository's `pyproject.toml`/`uv.lock` for dependencies. Run `uv run --frozen --group dev pytest tests/ -q` for the full suite; do not treat a partial pass as a green suite. For a daemon-free focused check, use `uv run --frozen --group dev pytest tests/ --ignore=tests/test_note_integration.py --ignore=tests/test_formatter.py -q`; report skipped files and full-suite failures explicitly. Integration tests that use `emacsclient` require an appropriately configured local Emacs daemon and org-ql. Run `uv run --frozen org-warrior --help` for a CLI smoke test.
- Keep arbitrary user text out of unescaped Elisp evaluations. Limit task queries to explicitly selected files; `ORG_WARRIOR_FILES` defaults to `~/org` on a developer machine and should not be pointed at the owner's full private notes tree for an agent. For writes, confirm the target Org file and avoid `org-warrior` auto-commit outside the intended repository (`--no-git` where supported).

## Delivery and review

- Work on a feature branch, open a PR against this repository, link the assigned issue without an auto-closing keyword, and report the exact tests run and any tests not run. Post the PR link and evidence to the issue.
- Do not merge a PR or close an issue on behalf of the owner. Human review and an explicit decision are required. If requirements conflict or a task needs private files, stop and ask for a scoped, public handoff instead of guessing.
