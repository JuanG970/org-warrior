Org-warrior extensions (draft)

Overview
- email_import.py: import a notmuch message into ~/org/inbox.org as a TODO with properties.
- remind_scheduler.py: scan org files for :remind:telegram and schedule reminders (daily digest, meeting reminders) and manage deep-work sessions (50/10 check-ins).

Usage
- python3 email_import.py <message-id>
- python3 remind_scheduler.py --dry-run
- python3 remind_scheduler.py deep-start --task-id <ORG-ID>
- python3 remind_scheduler.py deep-end --session-id <SESSION>

Notes
- This is a draft. It edits org files directly; backups are created before changes.
- It uses the `notmuch` CLI and simple org file string edits. For more robust edits we can switch to orgparse or python-org-mode libraries.
