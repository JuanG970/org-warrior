"""Helper utilities for executing elisp code from files."""

import os
import logging
from typing import Optional, Dict
from pathlib import Path


# Path to elisp directory
ELISP_DIR = Path(__file__).parent / "elisp"


def load_elisp_template(filename: str) -> str:
    """
    Load an elisp template file.

    Args:
        filename: Name of the elisp file (e.g., "add-task.el")

    Returns:
        The elisp code as a string

    Raises:
        FileNotFoundError: If the elisp file doesn't exist
    """
    filepath = ELISP_DIR / filename
    if not filepath.exists():
        raise FileNotFoundError(f"Elisp file not found: {filepath}")

    with open(filepath, "r", encoding="utf-8") as f:
        return f.read()


def format_elisp(template: str, **params: str) -> str:
    """
    Format an elisp template with parameters.

    Args:
        template: Elisp code template with {param} placeholders
        **params: Parameters to substitute in the template

    Returns:
        Formatted elisp code

    Example:
        >>> elisp = format_elisp(template, org_id="123", title="Test")
    """
    return template.format(**params)


def emacs_run_elisp_file(
    filename: str, params: Optional[Dict[str, str]] = None, daemon: Optional[str] = None
) -> Optional[str]:
    """
    Load and execute an elisp file with parameters.

    Args:
        filename: Name of the elisp file (e.g., "add-task.el")
        params: Dictionary of parameters to substitute in the template
        daemon: Emacs daemon name (uses config default if not provided)

    Returns:
        The result from emacs evaluation, or None on error

    Example:
        >>> result = emacs_run_elisp_file(
        ...     "add-task.el",
        ...     params={"title": "My Task", "file": "~/org/inbox.org", "heading": "Inbox"}
        ... )
    """
    from org_warrior import config
    from org_warrior.EmacsHandler import EmacsHandler

    try:
        # Load template
        template = load_elisp_template(filename)

        # Format with parameters
        elisp = format_elisp(template, **(params or {}))

        # Execute via tmpfile to avoid stdout corruption on large responses
        handler = EmacsHandler(daemon=daemon or config.EMACS_SERVER)
        result = handler.client.eval(elisp, tmpfile=True)

        return result
    except KeyError as e:
        raise Exception(f"Missing parameter for elisp template: {e}")


def emacs_run_elisp_string(elisp: str, daemon: Optional[str] = None) -> Optional[str]:
    """
    Execute an elisp string directly.

    Args:
        elisp: Elisp code to execute
        daemon: Emacs daemon name (uses config default if not provided)

    Returns:
        The result from emacs evaluation, or None on error
    """
    from org_warrior import config
    from org_warrior.EmacsHandler import EmacsHandler

    try:
        handler = EmacsHandler(daemon=daemon or config.EMACS_SERVER)
        result = handler.client.eval(elisp)
        return result
    except Exception as e:
        logging.error(f"Error executing elisp: {e}")
        return None
