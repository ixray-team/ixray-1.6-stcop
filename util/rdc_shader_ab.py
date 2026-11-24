# A/B of shader edits inside one RenderDoc capture: no rebuild, no game launch, both sides see the same frame.
#
#   set RDC_CAPTURE=<a capture>.rdc           
#   set RDC_PATH=ScenePrepass/SceneDraw/lmap_bump     part of the marker path of the draws or dispatches to take
#   set RDC_STAGE=ps                                     vs, ps or cs
#   set RDC_EVENT=1                                      which of the taken events gives the shader to edit; the first by default
#   set RDC_VARIANTS=<folder>/variants.py                VARIANTS = {'name': [('old text', 'new text'), ...], ...}
#   set RDC_OUT=D:/somewhere/out                         directory for the report, sources and pictures
#   "C:/Program Files/RenderDoc/qrenderdoc.exe" --python util/rdc_shader_ab.py
#
# Without RDC_VARIANTS it only writes the shader's source (source.hlsl) and its command line, to write edits against.
# The source is the preprocessed text the engine embedded, so an edit is plain text replacement in it. Every variant,
# and the unedited source as 'base', is compiled by the dxc found here with the engine's own arguments; compare
# variants with 'base', not with 'captured' (another compiler build made that one).
#
# The report gives, per variant: the time of the taken events (median of RDC_RUNS replays), the time of the whole
# pass they are in, pixel shader runs, and how far the picture moved from base. Replay times rank variants; the
# number to report is still the engine's own GPU track.
import os, shlex, statistics, subprocess, traceback
import renderdoc as rd

OUT = os.environ.get('RDC_OUT', '.').rstrip('/\\') + '/'
WANT = os.environ.get('RDC_PATH', '')
STAGE = {'vs': rd.ShaderStage.Vertex, 'ps': rd.ShaderStage.Pixel, 'cs': rd.ShaderStage.Compute}[os.environ.get('RDC_STAGE', 'ps')]
RUNS = int(os.environ.get('RDC_RUNS', '9'))
DXC = os.environ.get('RDC_DXC', 'C:/Program Files (x86)/Windows Kits/10/bin/10.0.26100.0/x64/dxc.exe')

os.makedirs(OUT, exist_ok=True)
log = open(OUT + 'report.txt', 'w', encoding='utf-8')


def w(*parts):
    log.write(' '.join(str(p) for p in parts) + '\n')
    log.flush()


def compile_source(text, cmdline, tag):
    source = OUT + tag + '.hlsl'
    binary = OUT + tag + '.dxil'
    open(source, 'w', encoding='utf-8', newline='').write(text)
    args = [a for a in shlex.split(cmdline) if a not in ('-Zi', '-Qembed_debug')]
    done = subprocess.run([DXC] + args + ['-Fo', binary, source], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
    if done.returncode != 0:
        w('  ', tag, 'does not compile:\n' + done.stdout.decode('utf-8', 'replace')[-1500:])
        return None
    return open(binary, 'rb').read()


try:
    cap = rd.OpenCaptureFile()
    cap.OpenFile(os.environ['RDC_CAPTURE'], '', None)
    result, ctl = cap.OpenCapture(rd.ReplayOptions(), None)
    taken = []
    passes = {}

    def walk(action, here):
        name = action.customName or action.GetName(ctl.GetStructuredFile())
        if action.flags & (rd.ActionFlags.Drawcall | rd.ActionFlags.Dispatch):
            passes.setdefault(here, []).append(action.eventId)
            if WANT in here:
                taken.append((action.eventId, here))
        for child in action.children:
            walk(child, here + '/' + name if (action.flags & rd.ActionFlags.PushMarker) else here)

    for root in ctl.GetRootActions():
        walk(root, '')
    events = [e for e, here in taken]
    w('events taken', len(events), 'under', sorted(set(here for e, here in taken))[:4])

    # The pass is the marker the first part of RDC_PATH names
    first = WANT.split('/')[0]
    top = taken[0][1][:taken[0][1].find(first) + len(first)]
    whole = [e for here, ids in passes.items() if here.startswith(top) for e in ids]
    w('pass', top, 'events', len(whole))

    ctl.SetFrameEvent(events[int(os.environ.get('RDC_EVENT', '0'))], False)
    state = ctl.GetPipelineState()
    refl = state.GetShaderReflection(STAGE)
    original = refl.resourceId
    cmdline = [f.value for f in refl.debugInfo.compileFlags.flags if f.name == '@cmdline'][0]
    source = refl.debugInfo.files[0].contents
    open(OUT + 'source.hlsl', 'w', encoding='utf-8', newline='').write(source)
    w('shader', original, 'entry', refl.entryPoint, 'source', len(source), 'bytes\ncmdline', cmdline)

    def measure():
        rows = {e: [] for e in whole}
        runs = {}
        for run in range(RUNS):
            for r in ctl.FetchCounters([rd.GPUCounter.EventGPUDuration, rd.GPUCounter.PSInvocations]):
                if r.eventId in rows:
                    if r.counter == rd.GPUCounter.EventGPUDuration:
                        rows[r.eventId].append(r.value.d * 1000.0)
                    else:
                        runs[r.eventId] = min(runs.get(r.eventId, r.value.u64), r.value.u64)
        ms = lambda ids: statistics.median(sum(rows[e][run] for e in ids) for run in range(RUNS))
        return ms(events), ms(whole), sum(runs.get(e, 0) for e in events)

    textures = {t.resourceId: t for t in ctl.GetTextures()}

    def picture(tag):
        # Every target the taken events write, as bytes; the last one also as a picture
        ctl.SetFrameEvent(events[-1], True)
        targets = [t.resource for t in ctl.GetPipelineState().GetOutputTargets() if t.resource != rd.ResourceId.Null()]
        if not targets:
            return None
        if tag == 'base':
            w('   targets', [(str(textures[t].format.Name()), textures[t].width, textures[t].height) for t in targets])
        save = rd.TextureSave()
        save.resourceId = targets[-1]
        save.destType = rd.FileType.PNG
        save.alpha = rd.AlphaMapping.Discard
        ctl.SaveTexture(save, OUT + tag + '.png')
        return b''.join(bytes(ctl.GetTextureData(t, rd.Subresource(0, 0, 0))) for t in targets)

    def moved(a, b):
        if a is None or b is None or len(a) != len(b):
            return 'no picture'
        step = 37 if len(a) > 400000 else 1
        differ = sum(1 for i in range(0, len(a), step) if a[i] != b[i])
        return '%.2f %% of sampled bytes differ' % (100.0 * differ / len(range(0, len(a), step)))

    path = os.environ.get('RDC_VARIANTS')
    if path:
        space = {}
        exec(open(path, encoding='utf-8').read(), space)
        w('%-24s %10s %10s %12s  %s' % ('variant', 'events ms', 'pass ms', 'ps runs', 'picture against base'))
        w('%-24s %10.4f %10.4f %12d' % (('captured',) + measure()))
        base_picture = None
        for name, edits in [('base', [])] + list(space['VARIANTS'].items()):
            text = source
            for old, new in edits:
                if old not in text:
                    w('  ', name, 'edit not found:', old[:80])
                text = text.replace(old, new)
            code = compile_source(text, cmdline, name)
            if code is None:
                continue
            built, errors = ctl.BuildTargetShader(refl.entryPoint, rd.ShaderEncoding.DXIL, code, rd.ShaderCompileFlags(), STAGE)
            if built == rd.ResourceId.Null():
                w('  ', name, 'not accepted by the replay:', errors)
                continue
            ctl.ReplaceResource(original, built)
            row = measure()
            shot = picture(name)
            base_picture = shot if name == 'base' else base_picture
            w('%-24s %10.4f %10.4f %12d  %s' % (name, row[0], row[1], row[2], '' if name == 'base' else moved(base_picture, shot)))
            ctl.RemoveReplacement(original)
            ctl.FreeTargetResource(built)
except Exception:
    w(traceback.format_exc())

log.close()
os._exit(0)
