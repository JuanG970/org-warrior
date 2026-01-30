#!/usr/bin/env python3
"""Import a notmuch message into ~/org/inbox.org as a TODO
"""
import sys
import subprocess
import shlex
from datetime import datetime
from pathlib import Path

ORG_INBOX = Path.home() / 'org' / 'inbox.org'
BACKUP_SUFFIX = '.bak'


def get_notmuch_metadata(msg_id):
    cmd = ['notmuch', 'show', '--format=sexp', msg_id]
    p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if p.returncode != 0:
        raise RuntimeError(f"notmuch show failed: {p.stderr.strip()}")
    out = p.stdout
    # crude parsing: find subject, from
    subj = ''
    frm = ''
    for line in out.splitlines():
        if line.strip().startswith('(subject'):
            subj = line.split(None, 1)[1].strip().strip('"')
        if line.strip().startswith('(from'):
            frm = line.split(None, 1)[1].strip().strip('"')
    return {'subject': subj, 'from': frm}


def make_org_entry(metadata, msg_id):
    ts = datetime.now().strftime('%Y-%m-%d %a %H:%M')
    title = metadata.get('subject') or f'Email {msg_id}'
    frm = metadata.get('from') or ''
    props = f":MAIL-ID: {msg_id}\n:FROM: {frm}\n:END:\n"
    entry = f"* TODO {title} :email:\n:PROPERTIES:\n{props}\nCaptured: [{ts}]\nLink: notmuch:{msg_id}\n\n"
    return entry


def backup_file(path: Path):
    bp = path.with_suffix(path.suffix + BACKUP_SUFFIX)
    path.write_bytes(path.read_bytes())
    # also write a timestamped copy
    ts = datetime.now().strftime('%Y%m%dT%H%M%S')
    stamp = path.with_name(path.name + f'.{ts}.bak')
    stamp.write_bytes(path.read_bytes())
    return bp


def append_to_inbox(entry: str):
    ORG_INBOX.parent.mkdir(parents=True, exist_ok=True)
    if not ORG_INBOX.exists():
        ORG_INBOX.write_text('#+TITLE: Inbox\n\n')
    backup_file(ORG_INBOX)
    with ORG_INBOX.open('a', encoding='utf-8') as f:
        f.write(entry)


def main():
    if len(sys.argv) < 2:
        print('Usage: email_import.py <notmuch-message-id>')
        sys.exit(2)
    msg_id = sys.argv[1]
    meta = get_notmuch_metadata(msg_id)
    entry = make_org_entry(meta, msg_id)
    append_to_inbox(entry)
    print(f'Imported message {msg_id} into {ORG_INBOX}')


if __name__ == '__main__':
    main()
