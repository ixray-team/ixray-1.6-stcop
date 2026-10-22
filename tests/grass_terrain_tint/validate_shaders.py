#!/usr/bin/env python3
"""Compile the actual R4 HLSL sources, including the normal and shadow variants."""
import argparse
import pathlib
import re
import subprocess


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", required=True)
    parser.add_argument("--output", type=pathlib.Path, required=True)
    args = parser.parse_args()
    root = pathlib.Path(__file__).resolve().parents[2]
    shaders = root / "gamedata/shaders/d3d11"
    args.output.mkdir(parents=True, exist_ok=True)
    cases = []

    def add(name, file, stage, *defines):
        cases.append((name, file, stage, defines))

    add("capture_vs", "grass_terrain_tint_capture.vs.hlsl", "vert", "USE_LM_HEMI=1")
    add("capture_legacy", "grass_terrain_tint_capture.ps.hlsl", "frag", "USE_LEGACY_LIGHT=1")
    add("capture_pbr", "grass_terrain_tint_capture.ps.hlsl", "frag", "USE_PBR=1")
    add("map_preview", "grass_terrain_tint_debug.ps.hlsl", "frag")
    for motion in (True, False):
        for wind in (True, False):
            defines = ["USE_DETAIL_TERRAIN_TINT=1"]
            if not motion:
                defines.append("DISABLE_MOTION_VECTORS=1")
            if wind:
                defines.append("USE_TREEWAVE=1")
            add(f"grass_vs_motion_{motion}_wind_{wind}", "deffer_detail.vs.hlsl", "vert", *defines)
    for pbr in (True, False):
        for bump in (True, False):
            defines = ["USE_DETAIL_TERRAIN_TINT=1", "USE_AREF=1", "FIX_CULL_NORMAL=1"]
            defines.append("USE_PBR=1" if pbr else "USE_LEGACY_LIGHT=1")
            if bump:
                defines += ["USE_BUMP=1", "USE_TDETAIL=1", "USE_TDETAIL_BUMP=1"]
            add(f"grass_ps_pbr_{pbr}_bump_{bump}", "deffer_base.ps.hlsl", "frag", *defines)
    add("shadow_vs", "deffer_detail.vs.hlsl", "vert", "DETAIL_SHADOW_PASS=1", "DISABLE_MOTION_VECTORS=1", "USE_TREEWAVE=1")
    add("untinted_vs", "deffer_detail.vs.hlsl", "vert")
    add("untinted_ps", "deffer_base.ps.hlsl", "frag", "USE_AREF=1", "FIX_CULL_NORMAL=1", "USE_LEGACY_LIGHT=1")

    logs = []
    for name, file, stage, defines in cases:
        command = [args.compiler, "-D", "-V", "--target-env", "vulkan1.1", "-S", stage,
                   "-e", "main", "--auto-map-bindings", "--auto-map-locations", "-l", "-q",
                   f"-I{shaders}", *(f"-D{x}" for x in defines), str(shaders / file),
                   "-o", str(args.output / f"{name}.spv")]
        result = subprocess.run(command, capture_output=True, text=True)
        output = result.stdout + result.stderr
        logs.append(f"CASE {name}\n{' '.join(command)}\n{output}")
        if result.returncode:
            print(output)
            raise SystemExit(f"FAIL: {name}")
        if name.startswith("grass_vs_"):
            for resource, binding in (("s_grass_tint_color", 1), ("s_grass_tint_height", 2)):
                if not re.search(rf"{resource}:.*binding {binding},", output):
                    raise SystemExit(f"FAIL: {name}: unexpected VS slot for {resource}")
            if "topLevelArrayStride 48" not in output:
                raise SystemExit(f"FAIL: {name}: detail instance buffer layout changed")
        print(f"PASS: {name}")
    (args.output / "compilation.log").write_text("\n".join(logs), encoding="utf-8")
    print(f"PASS: {len(cases)} real HLSL variants; terrain maps use VS t1/t2; detail instance stride remains 48 bytes")


if __name__ == "__main__":
    main()
