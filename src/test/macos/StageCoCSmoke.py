#!/usr/bin/env python3
"""Check CoC staging ownership and preservation without proprietary archives."""

from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class StagingTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="ixray-stage-check-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.base = self.root / "base"
        self.patch = self.root / "patch"
        self.output = self.root / "runtime"
        for directory in ("config", "maps", "resource", "sound"):
            (self.base / "database" / directory).mkdir(parents=True)
        (self.patch / "database/config").mkdir(parents=True)
        (self.patch / "database/config/scripts.db").write_bytes(b"owned test placeholder")
        self.repository = Path(__file__).resolve().parents[3]

    def stage(self):
        return subprocess.run(
            [sys.executable, str(self.repository / "util/StageMacOSCoC.py"),
             "--base", str(self.base), "--patch", str(self.patch), "--output", str(self.output)],
            capture_output=True, text=True,
        )

    def mark_owned(self):
        self.output.mkdir(parents=True, exist_ok=True)
        (self.output / ".ixray-macos-stage").write_text("owned regression staging\n")

    def test_unrelated_output_is_preserved(self):
        self.output.mkdir()
        keep = self.output / "keep.txt"
        keep.write_text("keep")
        self.assertNotEqual(self.stage().returncode, 0)
        self.assertEqual(keep.read_text(), "keep")
        self.assertFalse((self.output / "database").exists())

    def test_output_inside_original_data_is_refused(self):
        self.output = self.base / "child"
        self.assertNotEqual(self.stage().returncode, 0)
        self.assertFalse(self.output.exists())

    def test_regular_archive_is_preserved(self):
        self.mark_owned()
        archive = self.output / "database/config/scripts.db"
        archive.parent.mkdir(parents=True)
        archive.write_bytes(b"keep regular archive")
        self.assertNotEqual(self.stage().returncode, 0)
        self.assertEqual(archive.read_bytes(), b"keep regular archive")

    def test_nested_config_symlink_is_refused_before_external_writes(self):
        self.mark_owned()
        configs = self.output / "gamedata/configs"
        configs.mkdir(parents=True)
        outside = self.root / "outside"
        outside.mkdir()
        (configs / "ui").symlink_to(outside, target_is_directory=True)
        self.assertNotEqual(self.stage().returncode, 0)
        self.assertEqual(list(outside.rglob("*")), [])

    def test_restage_preserves_mutable_settings(self):
        first = self.stage()
        self.assertEqual(first.returncode, 0, first.stderr)
        mutable = self.output / "gamedata/configs/axr_options.ltx"
        mutable.write_text("owned mutable settings\n")
        user = self.output / "_appdata_ixray_/user.ltx"
        user.write_text("owned user settings\n")
        second = self.stage()
        self.assertEqual(second.returncode, 0, second.stderr)
        self.assertEqual(mutable.read_text(), "owned mutable settings\n")
        self.assertEqual(user.read_text(), "owned user settings\n")
        self.assertEqual((self.patch / "database/config/scripts.db").read_bytes(), b"owned test placeholder")


if __name__ == "__main__":
    unittest.main()
