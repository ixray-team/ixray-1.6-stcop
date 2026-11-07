#!/usr/bin/env python3
"""Exercise actual packaging functions with disposable data; never touch /Applications."""
from pathlib import Path
import os
import re
import shlex
import subprocess
import tempfile
import unittest
from PackageSandbox import run as sandbox_run

SCRIPT = Path(__file__).resolve().parents[3] / 'build-macos.command'

class PackagingTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='ixray-package-')
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.build = self.root / 'build'
        self.repo = self.root / 'repo'
        self.target = self.root / 'Applications/IX-Ray.app'
        (self.build / 'bin/Release').mkdir(parents=True)
        engine = self.build / 'bin/Release/xrEngine'
        engine.write_text('#!/bin/sh\nexit 0\n')
        engine.chmod(0o755)
        (self.repo / 'gamedata').mkdir(parents=True)
        (self.repo / 'fsgame.ltx').write_text('owned filesystem')
        source = SCRIPT.read_text()
        override = 'local target="${IXRAY_INSTALL_TARGET:-/Applications/IX-Ray.app}"'
        if source.count(override) != 1:
            raise RuntimeError('Production install target override missing or ambiguous')
        names = re.findall(r'^([a-z_]+)\(\)', source, re.M)
        functions = re.findall(r'^[a-z_]+\(\)[^\n]*\n.*?^}', source, re.M | re.S)
        extracted = [re.match(r'([a-z_]+)\(\)', function).group(1) for function in functions]
        expected = ['say', 'usage', 'require', 'make_icon', 'write_info_plist',
                    'prompt_for_game_dir', 'find_game_dir', 'preserve_mutable_data',
                    'replace_bundle', 'package_game', 'install_to_applications',
                    'move_to_trash', 'offer_cleanup']
        if names != expected or extracted != names:
            raise RuntimeError('Unexpected or incomplete packaging function extraction')
        self.functions = '\n'.join(functions)

    def data(self, path):
        for directory in ('levels', 'resources', '_appdata_ixray_', 'ixr_addons', 'mods'):
            (path / directory).mkdir(parents=True, exist_ok=True)
            (path / directory / 'keep').write_text('preserve ' + directory)
        return path

    def run_functions(self, command, data='', extra=''):
        variables = {'REPO_DIR': self.repo, 'BUILD_DIR': self.build, 'DATA_DIR': data,
                     'GAME_DIR': data, 'IXRAY_INSTALL_TARGET': self.target, 'GREEN': '', 'RESET': '', 'FIXTURE_ROOT': self.root}
        setup = '\n'.join(k + '=' + shlex.quote(str(v)) for k, v in variables.items())
        source = 'set -euo pipefail\n' + setup + '\n' + self.functions
        source += '\nsay() { printf "%s\\n" "$*"; }\ncodesign() { return 0; }\nosascript() { return 90; }\n'
        # Positive control executes under the same kernel boundary as the functions.
        positive = """set -e
cat > fixture-positive <<'EOF'
owned positive fixture
EOF
/bin/mv fixture-positive fixture-renamed
/usr/bin/ditto fixture-renamed fixture-copied
/usr/bin/cmp fixture-renamed fixture-copied
printf 'GUARD_POSITIVE_OK\\n'
"""
        result = sandbox_run(self.root, positive + source + extra + '\n' + command)
        self.assertIn('GUARD_POSITIVE_OK\n', result.stdout, result.stderr)
        return result

    def test_package_can_reuse_its_own_bundle_without_losing_data(self):
        data = self.data(self.build / 'IX-Ray.app/Contents/Resources')
        result = self.run_functions('package_game', data)
        self.assertEqual(result.returncode, 0, result.stderr)
        for name in ('levels', 'resources', '_appdata_ixray_', 'ixr_addons', 'mods'):
            self.assertEqual((data / name / 'keep').read_text(), 'preserve ' + name)

    def test_install_preserves_existing_mutable_data_and_backup(self):
        previous = self.data(self.target / 'Contents/Resources')
        new = self.data(self.build / 'IX-Ray.app/Contents/Resources')
        for name in ('_appdata_ixray_', 'ixr_addons', 'mods'):
            (new / name / 'keep').unlink()
        result = self.run_functions('install_to_applications')
        self.assertEqual(result.returncode, 0, result.stderr)
        for name in ('_appdata_ixray_', 'ixr_addons', 'mods'):
            self.assertEqual((previous / name / 'keep').read_text(), 'preserve ' + name)
        self.assertTrue(list(self.target.parent.glob('IX-Ray-previous.*/IX-Ray.app')))

    def test_partial_move_restores_exact_previous_bundle(self):
        self.data(self.target / 'Contents/Resources')
        self.data(self.build / 'IX-Ray.app/Contents/Resources')
        old_marker = self.target / 'Contents/old-version'
        old_marker.write_bytes(b'original bundle bytes')
        def snapshot(path):
            return {str(item.relative_to(path)): (item.read_bytes() if item.is_file() else None)
                    for item in path.rglob('*')}
        original = snapshot(self.target)
        extra = r"""
mv() {
    if [ "$1" = "${BUILD_DIR}/IX-Ray.app" ] && [ "$2" = "${IXRAY_INSTALL_TARGET}" ]; then
        /bin/mkdir -p "${IXRAY_INSTALL_TARGET}/Contents"
        printf 'incomplete replacement' > "${IXRAY_INSTALL_TARGET}/Contents/partial"
        echo FIXTURE_PARTIAL_MOVE_FAILURE >&2
        return 7
    fi
    /bin/mv "$@"
}
"""
        result = self.run_functions('install_to_applications', extra=extra)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn('FIXTURE_PARTIAL_MOVE_FAILURE', result.stderr)
        self.assertIn('Previous bundle restored:', result.stderr)
        self.assertNotIn('Установлено:', result.stdout)
        self.assertEqual(snapshot(self.target), original)
        incomplete = list(self.target.parent.glob('IX-Ray-incomplete.*/IX-Ray.app/Contents/partial'))
        self.assertEqual(len(incomplete), 1)
        self.assertEqual(incomplete[0].read_bytes(), b'incomplete replacement')
        self.assertFalse((self.target / 'IX-Ray.app').exists())

    def test_invalid_explicit_data_does_not_fall_back(self):
        self.data(self.repo.parent / 'cop-baseline')
        result = self.run_functions('find_game_dir', self.root / 'missing')
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn('Invalid --data directory:', result.stderr)

    def test_failed_copy_preserves_previous_bundle(self):
        previous = self.data(self.build / 'IX-Ray.app/Contents/Resources')
        source = self.data(self.root / 'source')
        result = self.run_functions('package_game', source, '\nditto() { echo FIXTURE_COPY_FAILURE >&2; return 7; }\n')
        self.assertEqual(result.returncode, 7, result.stderr)
        self.assertIn('FIXTURE_COPY_FAILURE', result.stderr)
        self.assertEqual((previous / '_appdata_ixray_/keep').read_text(), 'preserve _appdata_ixray_')

if __name__ == '__main__':
    unittest.main()
