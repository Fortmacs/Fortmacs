2026-10-23 — CI: the Arch job installs python-filelock, which Arch's pipx imports but its package doesn't depend on (pipx failed with "No module named 'filelock'"). On `ci-arch-filelock`.
