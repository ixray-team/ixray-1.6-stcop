#!/usr/bin/env python3
"""Package the compiled renderer, matching shaders and an installer for both bin layouts."""
import argparse
import hashlib
import json
from pathlib import Path
import struct
import zipfile


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--renderer', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--commit', required=True)
    args = parser.parse_args()
    utility = Path(__file__).resolve().parent
    root = utility.parents[1]
    renderer = args.renderer.read_bytes()
    if renderer[:2] != b'MZ':
        raise SystemExit('Expected a compiled Windows renderer DLL')
    pe = struct.unpack_from('<I', renderer, 0x3C)[0]
    if renderer[pe:pe + 4] != b'PE\0\0' or struct.unpack_from('<H', renderer, pe + 4)[0] != 0x8664:
        raise SystemExit('Expected a Windows x64 PE renderer')
    if b'[v0.2, terrain color transfer]' not in renderer:
        raise SystemExit('Renderer does not contain the v0.2 implementation')
    files = {'bin/xrRender_R4.dll': renderer}
    for name in ('common_iostructs.hlsli', 'deffer_base.ps.hlsl', 'deffer_detail.vs.hlsl',
                 'grass_terrain_tint.hlsli', 'grass_terrain_tint_color.hlsli',
                 'grass_terrain_tint_capture.vs.hlsl', 'grass_terrain_tint_capture.ps.hlsl',
                 'grass_terrain_tint_debug.ps.hlsl'):
        relative = f'gamedata/shaders/d3d11/{name}'
        files[relative] = (root / relative).read_bytes()
    for path in sorted(utility.glob('grass_tint_*.ltx')):
        files[f'presets/{path.name}'] = path.read_bytes()
    for name in ('Run_GrassTint.cmd', 'Prepare_GameFolder.ps1'):
        files[name] = (utility / name).read_bytes()
    manifest = {'version': '0.2', 'commit': args.commit,
                'files': [{'path': name, 'sha256': hashlib.sha256(data).hexdigest()} for name, data in files.items()]}
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(args.output, 'w', compression=zipfile.ZIP_DEFLATED, compresslevel=9) as archive:
        for name in ('Install_GrassTint_Update.cmd', 'Install_GrassTint_Update.ps1'):
            archive.writestr(name, (utility / name).read_bytes())
        archive.writestr('README_GRASS_TINT_v0.2_RU.md', (utility / 'README_RU.md').read_bytes())
        archive.writestr('IXRay-LICENSE.md', (root / 'LICENSE.md').read_bytes())
        archive.writestr('_grass_tint_update_/manifest.json', json.dumps(manifest, indent=2).encode('utf-8'))
        for name, data in files.items():
            archive.writestr(f'_grass_tint_update_/{name}', data)
    with zipfile.ZipFile(args.output) as archive:
        if archive.testzip() is not None:
            raise SystemExit('ZIP integrity verification failed')
    print(f'Ready: {args.output} ({args.output.stat().st_size} bytes), commit {args.commit}')


if __name__ == '__main__':
    main()
