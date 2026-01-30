#!/usr/bin/env python3
"""Remind scheduler for org-warrior (draft)
- daily digest at 09:00
- meeting reminders 15 min before (based on SCHEDULED or DEADLINE)
- deep-work sessions (deep-start / deep-end) with 50/10 check-ins

This draft operates in dry-run mode by default. It prints scheduled actions instead of creating OpenClaw cron jobs.
"""
import argparse
import re
import uuid
from datetime import datetime, timedelta, time
from pathlib import Path

ORG_DIR = Path.home() / 'org'
DAILY_HOUR = 9


def find_remindable_tasks():
    tasks = []
    pattern = re.compile(r"^\*+\s+(TODO|STRT).*", re.MULTILINE)
    for p in ORG_DIR.glob('**/*.org'):
        text = p.read_text(encoding='utf-8')
        # crude: any headline with :remind:telegram: or scheduled: or deadline:
        if ':remind:telegram:' in text or 'SCHEDULED:' in text or 'DEADLINE:' in text:
            for m in pattern.finditer(text):
                # extract heading line
                start = m.start()
                line = text[start:text.find('\n', start)]
                tasks.append({'file': str(p), 'heading': line.strip()})
    return tasks


def daily_digest():
    tasks = find_remindable_tasks()
    now = datetime.now()
    print(f"Daily digest for {now.date()} ({len(tasks)} items):")
    for t in tasks:
        print(f" - {t['heading']} ({t['file']})")


# Deep-work session manager (simple in-process scheduler)
SESSIONS = {}


def deep_start(task_id=None):
    session_id = str(uuid.uuid4())
    start = datetime.now()
    SESSIONS[session_id] = {'task_id': task_id, 'start': start, 'state': 'running'}
    print(f"Started deep-work session {session_id} for task {task_id} at {start}")
    print("Schedule: 50min work / 10min break (check-ins every 50 min)")
    return session_id


def deep_end(session_id):
    now = datetime.now()
    if session_id not in SESSIONS:
        print(f"Unknown session {session_id}")
        return
    SESSIONS[session_id]['state'] = 'ended'
    SESSIONS[session_id]['end'] = now
    print(f"Ended session {session_id} at {now}")


def cli():
    ap = argparse.ArgumentParser()
    ap.add_argument('--dry-run', action='store_true')
    sub = ap.add_subparsers(dest='cmd')
    sub.add_parser('daily')
    deep = sub.add_parser('deep-start')
    deep.add_argument('--task-id')
    sub.add_parser('deep-end').add_argument('session_id')
    sub.add_parser('list')
    args = ap.parse_args()
    if args.cmd == 'daily':
        daily_digest()
    elif args.cmd == 'deep-start':
        deep_start(task_id=args.task_id)
    elif args.cmd == 'deep-end':
        deep_end(args.session_id)
    elif args.cmd == 'list':
        print('Active sessions:')
        for k,v in SESSIONS.items():
            print(k, v)
    else:
        ap.print_help()


if __name__ == '__main__':
    cli()
