"""Bounded macOS subprocess runner for disposable packaging fixtures."""
from pathlib import Path
import subprocess

PROFILE = '(version 1)\n(allow default)\n(deny network*)\n(deny signal)\n(deny file-write*)\n(deny file-link)\n(allow file-write* (subpath (param "ROOT")))\n(deny process-exec (literal "/usr/bin/true")\n (literal "/usr/bin/osascript") (literal "/usr/bin/open")\n (literal "/bin/launchctl") (literal "/usr/bin/defaults")\n (literal "/usr/bin/sudo") (literal "/bin/kill"))\n'

def run(root, payload):
    root = root.resolve(strict=True)
    sandbox = Path('/usr/bin/sandbox-exec')
    if not sandbox.is_file():
        raise RuntimeError('ERROR: sandbox-exec unavailable')
    for item in root.rglob('*'):
        if item.is_file() and not item.is_symlink() and item.stat().st_nlink != 1:
            raise RuntimeError('ERROR: fixture contains an existing hardlink')
    for name in ('home', 'tmp'):
        (root / name).mkdir(exist_ok=True)
    env = {'PATH': '/usr/bin:/bin', 'HOME': str(root / 'home'), 'TMPDIR': str(root / 'tmp'),
           'LANG': 'C', 'LC_ALL': 'C', 'PYTHONDONTWRITEBYTECODE': '1'}
    result = subprocess.run([str(sandbox), '-D', 'ROOT=' + str(root), '-p', PROFILE,
                             '/bin/bash', '--noprofile', '--norc', '-c',
                             'printf "READY\\n"\n' + payload], cwd=root, env=env,
                            stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=20)
    if 'READY\n' not in result.stdout:
        raise RuntimeError('ERROR: sandbox payload did not start: ' + result.stderr)
    return result
