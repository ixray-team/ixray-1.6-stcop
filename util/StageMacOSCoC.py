#!/usr/bin/env python3
"""Stage an installed CoC base, its 1.4.22 patch and the IX-Ray overlay."""

import argparse
from pathlib import Path
import shutil


def main():
    repository = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", required=True, type=Path, help="Extracted CoC 1.4.12 installation")
    parser.add_argument("--patch", required=True, type=Path, help="Extracted CoC 1.4.22 patch")
    parser.add_argument("--output", type=Path, default=repository / "build/runtime/coc")
    args = parser.parse_args()
    if args.output.is_symlink():
        parser.error("Output must be a real runtime directory, not a symlink")
    base, patch, output = args.base.resolve(), args.patch.resolve(), args.output.resolve()
    for name in ("config", "maps", "resource", "sound"):
        if not (base / "database" / name).is_dir():
            parser.error(f"Base installation is missing database/{name}")
    if not (patch / "database/config/scripts.db").is_file():
        parser.error("Patch is missing database/config/scripts.db")
    if (output in (base, patch, repository) or base in output.parents or patch in output.parents
            or output in base.parents or output in patch.parents or output in repository.parents):
        parser.error("Output must be a separate runtime directory")
    marker = output / ".ixray-macos-stage"
    filesystem = output / "fsgame.ltx"
    appdata = output / "_appdata_ixray_"
    settings = appdata / "user.ltx"
    # These entries are written by staging or by the engine in the staged runtime.
    for destination in (marker, filesystem, appdata, settings):
        if destination.is_symlink():
            raise FileExistsError(f"Refusing to write through a staging symlink: {destination}")
    if output.exists() and any(output.iterdir()) and not marker.is_file():
        parser.error("Output is not an IX-Ray staging directory; choose an empty directory")
    output.mkdir(parents=True, exist_ok=True)
    marker.write_text("IX-Ray macOS runtime staging\n", encoding="utf-8")

    def link(source, destination):
        for parent in destination.parents:
            if parent == output:
                break
            if parent.is_symlink():
                raise FileExistsError(f"Refusing to write through a staging directory symlink: {parent}")
        destination.parent.mkdir(parents=True, exist_ok=True)
        if destination.is_symlink():
            if destination.resolve() == source:
                return
            destination.unlink()
        elif destination.exists():
            raise FileExistsError(f"Refusing to replace a regular file: {destination}")
        destination.symlink_to(source, target_is_directory=source.is_dir())

    for installation in (base, patch):
        for source in sorted((installation / "database").rglob("*")):
            if source.is_file():
                link(source, output / "database" / source.relative_to(installation / "database"))
    overlay_source = repository / "gamedata_coc"
    overlay = output / "gamedata"
    if overlay.is_symlink():
        raise FileExistsError(f"Overlay output must be a staging directory: {overlay}")
    overlay.mkdir(exist_ok=True)
    for source in overlay_source.iterdir():
        if source.name != "configs":
            link(source, overlay / source.name)

    # CoC scripts save these settings through $game_config$, outside appdata.
    mutable_configs = {"axr_options.ltx", "atmosfear_options.ltx", "atmosfear_default_settings.ltx"}
    configs = overlay / "configs"
    if configs.is_symlink():
        raise FileExistsError(f"Config output must be a staging directory: {configs}")
    if configs.exists():
        for existing in configs.rglob("*"):
            if existing.is_symlink():
                raise FileExistsError(f"Refusing to write through a config symlink: {existing}")

    def copy_config(source, destination):
        source, destination = Path(source), Path(destination)
        if destination.is_symlink():
            raise FileExistsError(f"Refusing to write through a config symlink: {destination}")
        if not (source.name in mutable_configs and destination.exists()):
            shutil.copy2(source, destination)
        return str(destination)

    shutil.copytree(overlay_source / "configs", configs, dirs_exist_ok=True, copy_function=copy_config)
    shutil.copyfile(repository / "fsgame_coc.ltx", filesystem)
    appdata.mkdir(exist_ok=True)
    if not settings.exists():
        settings.write_text("renderer renderer_r4\nrs_fullscreen off\nvid_mode 1280x720\nrs_v_sync on\nrs_main_menu_fps_limit 60\n", encoding="utf-8")
    print(f"Runtime: {output}")
    print("Original archives and repository files remain in place; mutable configs use the staging overlay.")
    print("User settings, saves, shader caches and logs use _appdata_ixray_.")


if __name__ == "__main__":
    main()
