from emacs import EmacsClient
from pathlib import Path
from dataclasses import dataclass
from typing import Any


@dataclass
class EmacsResponse:
    """Response from Emacs evaluation."""

    success: bool
    data: Any
    error: str = ""


class EmacsHandler:
    """
    EmacsHandler is responsible for safely loading and running elisp files and expressions in Emacs.
    """

    def __init__(self, daemon=None):
        self.daemon = daemon
        self.client = EmacsClient(server=daemon)

    def _load_elisp(self, filename):
        """Load elisp file from package."""
        elisp_dir = Path(__file__).parent / "elisp"
        try:
            file_content = (elisp_dir / filename).read_text()
            return file_content
        except Exception as e:
            raise RuntimeError(f"Error loading elisp file {filename}: {e}")

    def _parse_emacs_response(self, response, output_format="string"):
        """Parse the response from Emacs and return a structured result."""
        if output_format == "string":
            return str(response)
        elif output_format == "json":
            import json

            try:
                return json.loads(response)
            except json.JSONDecodeError as e:
                raise ValueError(f"Error parsing JSON response from Emacs: {e}")
        else:
            raise ValueError(f"Unsupported output format: {output_format}")

    def execute_file(self, filename, params=None, output_format="string"):
        """Run an elisp file in Emacs and return the output."""
        elisp_code = self._load_elisp(filename)
        if params is not None:
            elisp_code = elisp_code.replace("{{params}}", params)
        try:
            result = self.client.eval(elisp_code)
            parsed_result = self._parse_emacs_response(result, output_format)
            return EmacsResponse(success=True, data=parsed_result)
        except Exception as e:
            return EmacsResponse(success=False, data=None, error=str(e))

    @staticmethod
    def run_file(filename, params=None, output_format="string"):
        """Static method to run elisp file with default daemon."""
        from org_warrior import config

        handler = EmacsHandler(daemon=config.EMACS_SERVER)
        return handler.execute_file(filename, params, output_format)
