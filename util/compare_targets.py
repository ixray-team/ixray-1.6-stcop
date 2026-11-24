import glob, os, re, sys

import numpy as np

# Compares the targets r_dump_targets wrote: what the scene shaded itself against the G-buffer, over the
# pixels the scene covers. Prints the mean and the 99th percentile of the absolute difference per channel.
# Argument: the folder of the dump
Folder = sys.argv[1]


def Load(Name, Type):
    Files = glob.glob(os.path.join(Folder, Name + '_*.raw'))

    if not Files:
        return None

    Width, Height = map(int, re.search(r'_(\d+)x(\d+)\.raw$', Files[0]).groups())
    return np.fromfile(Files[0], dtype=Type).reshape(Height, Width, -1)


def Unpack(Packed):
    Packed = Packed[..., 0]
    return np.stack([(Packed >> Shift & Mask) / Mask for Shift, Mask in ((0, 1023), (10, 1023), (20, 1023), (30, 3))], axis=-1)


Ids = Load('scene_ids', np.uint32)[..., 0]
Covered = Ids != 0
print('covered %.1f%% of %dx%d' % (100.0 * Covered.mean(), Ids.shape[1], Ids.shape[0]))

Pairs = [
    ('colour', np.uint8, lambda Data: Data / 255.0),
    ('normal', np.uint32, Unpack),
    ('surface', np.uint8, lambda Data: Data / 255.0),
    ('velocity', np.int16, lambda Data: Data / 32767.0),
]

for Name, Type, Decode in Pairs:
    Scene, Old = Load('scene_' + Name, Type), Load('gbuffer_' + Name, Type)

    if Scene is None or Old is None or Scene.shape != Old.shape:
        print(Name.ljust(10) + 'not comparable')
        continue

    Difference = np.abs(Decode(Scene) - Decode(Old))[Covered]
    Mean = ' '.join('%.4f' % Value for Value in Difference.mean(axis=0))
    High = ' '.join('%.4f' % Value for Value in np.percentile(Difference, 99, axis=0))
    print(Name.ljust(10) + 'mean ' + Mean + '   p99 ' + High)

# Lit colour of the scene's own shading against the deferred frame, by material. Only where there is next to no fog:
# the deferred frame already has the sky blended in by the fog, the scene's target keeps that weight in its alpha
Lit, Old = Load('scene_lit', np.float16), Load('deferred_lit', np.float16)
Normal, OldNormal = Load('scene_normal', np.uint32), Load('gbuffer_normal', np.uint32)

if Lit is not None and Old is not None and Normal is not None and OldNormal is not None:
    Lit, Old = Lit.astype(np.float64), Old.astype(np.float64)
    Kind, OldKind = Normal[..., 0] >> 30, OldNormal[..., 0] >> 30
    Relative = np.abs(Lit[..., :3] - Old[..., :3]).sum(-1) / np.maximum(Old[..., :3].sum(-1), 1e-3)

    for Id, Name in ((0, 'base'), (2, 'terrain'), (3, 'foliage')):
        Chosen = Covered & (Kind == Id) & (OldKind == Id) & (Lit[..., 3] < 0.001)

        if Chosen.any():
            print(('lit ' + Name).ljust(12) + '%5.1f%% of screen, mean %.4f against %.4f, difference median %.1f%% mean %.1f%%' % (100.0 * Chosen.mean(), Lit[Chosen][:, :3].mean(), Old[Chosen][:, :3].mean(), 100.0 * np.median(Relative[Chosen]), 100.0 * Relative[Chosen].mean()))
