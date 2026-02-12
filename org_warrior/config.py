import configparser
import os
import sys
import logging
import subprocess
from pathlib import Path

# Configuration: config file < env vars < CLI flags
CONFIG_DIR = os.path.expanduser("~/.org-warrior")
CONFIG_FILE = os.path.join(CONFIG_DIR, "config.toml")
CONTEXT_FILE = os.path.join(CONFIG_DIR, "context")


def _load_config():
    """Load INI config from ~/.org-warrior/config."""
    cfg = configparser.ConfigParser()
    if os.path.isfile(CONFIG_FILE):
        cfg.read(CONFIG_FILE)
    return cfg


_cfg = _load_config()


def _cfgval(section, key, default=""):
    try:
        return _cfg.get(section, key)
    except (configparser.NoSectionError, configparser.NoOptionError):
        return default


ORG_FILES = os.environ.get("ORG_WARRIOR_FILES", _cfgval("core", "files", "~/org"))
EMACS_CMD = os.environ.get("EMACS_CMD", _cfgval("core", "emacs_cmd", "emacsclient"))
EMACS_SERVER = os.environ.get("ORG_WARRIOR_SERVER", _cfgval("core", "server", "edit"))
EMACS_LOAD_PATH = os.environ.get(
    "ORG_WARRIOR_LOAD_PATH", _cfgval("core", "load_path", "")
)
EMACS_TIMEOUT = 15
DEFAULT_SORT = _cfgval("display", "default_sort", "")
DEFAULT_LIMIT = _cfgval("display", "default_limit", "")
CONFIG_COLOR = _cfgval("display", "color", "auto")
CONFIG_SHOW_HANDLES = _cfgval("display", "show_handles", "true").lower() != "false"
CONFIG_SHOW_IDS = _cfgval("display", "show_ids", "false").lower() == "true"
DEBUG = _cfgval("core", "verbose") == "true"


logging.basicConfig(
    level=logging.DEBUG if DEBUG else logging.INFO,
    format="\033[90m%(levelname)s: %(message)s\033[0m",
    stream=sys.stderr,
)


def git_commit_org(message: str) -> None:
    """
    Auto-commit changes in ~/org if it's a git repo and there are modifications.

    Args:
        message: Git commit message
    """
    org_root = os.path.expanduser("~/org")
    if not os.path.isdir(os.path.join(org_root, ".git")):
        return

    # Only commit when there are changes
    st = subprocess.run(
        ["git", "-C", org_root, "status", "--porcelain"],
        capture_output=True,
        text=True,
        timeout=5,
    )
    if st.returncode != 0:
        return
    if not st.stdout.strip():
        return

    # Stage all changes
    subprocess.run(
        ["git", "-C", org_root, "add", "-A"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        timeout=5,
    )

    # Commit
    subprocess.run(
        ["git", "-C", org_root, "commit", "-m", message],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        timeout=5,
    )
