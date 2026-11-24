# One check of the picture: every save in its own engine launch, its shot compared with a reference, its load log read.
#
#   python util/render_check.py [--api d3d12|d3d11|both] [--saves render_perf,sun_shafts] [--presets minimal,low,ultra] [--profile [N]] [--update-reference]
#   python util/render_check.py --compare a.json b.json
#
# A launch runs -autotest -autotest_dt (fixed step, no real-time budgets, fixed seeds) on a config this script writes
# into the game's app data folder (-ltx render_check.ltx: user.ltx is neither read nor written) and a script of
# start <save>, the save's commands, wait, its 'late ' commands (they need the loaded level), settle, shot. The shot becomes <out>/<api>/<save>.png (the TGA is deleted)
# and is compared with <reference>/<api>/<save>.png by the mean absolute difference of its RGB, 0..255. The scene
# counters of the load log are kept with the reference in <reference>/<api>/scene.json and have to match exactly.
# A preset other than ultra keeps its pictures and numbers in <preset>/<api>/ of the output and the reference.
# --profile N launches every save N times (3 without a number); after the shot each launch profiles PROFILE counted
# frames, logs render_memory_stats and reads the frame times of autotest.csv. The medians of the N launches are kept in
# <reference>/<api>/profile.json and printed against it, beyond NOISE percent marked with '!'.
# --update-reference writes this run's pictures, counters and medians as the reference. Every run writes all it read to
# --json (<out>/render_check.json); --compare a.json b.json prints the medians of b against a. Needs Python 3 and Pillow.
import argparse, csv, json, os, re, shutil, statistics, subprocess, sys, tempfile, time

from PIL import Image, ImageChops, ImageStat

ENGINE = os.environ.get('IXRAY_ENGINE', '')
GAME = os.environ.get('IXRAY_GAME', '')
APPDATA = '_appdata_ixray_'
PRESETS = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'gamedata_cs', 'configs', 'ixray_settings')
PRESET = os.path.join(PRESETS, 'rspec_ultra.ltx')
SAVES = {'render_perf': [], 'sun_shafts': ['set_weather sun_shafts'], 'point_light': [], 'hud_light_test': [], 'test_rain': [], 'water_problem': [],
         '3d_scope': ['late run_string level.press_action(bind_to_dik(key_bindings["k"..("wpn_zoom"):upper()], -1))']}
STEP_MS = '16.667'
SETTLE = 256
WAIT = 30
THRESHOLD = 0.5
TIMEOUT = 300
PROFILE = 600
SPIKE_FRAMES = 600
MEMORY_WAIT = 10
NOISE = 7.0
FLOOR_MS = 0.01

PINNED = ['r_vsync off', 'r_fps_limit 0', 'r_bloom_amount 0.04', 'r_saturation 1', 'r_tonemap_compression 0.7', 'r_tonemap_white 4',
    'r_tonemap_desaturation 0.2', 'r_exposure_bias 0', 'r_exposure_speed 1', 'r_vignette off', 'r_sharpening 0', 'r_brightness 1',
    'r_contrast 1', 'r_gamma 1', 'r_resolution 1920x1080']

COUNTERS = {
    'materials': r'\[scene\] materials: (\d+) of (\d+) complete',
    'graph': r'level visuals the main view draws from the graph: (\d+) of (\d+)',
    'lod_cards': r'LOD cards: (\d+) of (\d+) drawn by the scene',
    'depth_batch': r'level visuals outside the depth batch: (\d+) of (\d+)',
}

PROBLEMS = [r'! NRI', r'! Dynamic geometry', r'is still held after its release', r'FATAL', r"Can't compile shader", r': error[ :]', r'error X\d{4}',
    r'\[error\]', r'! \[autotest\]', r'\[autotest\] timed out', r'RESULT: FAIL']

# Profiler capture rows (track, path or the end of a path) and the zones under FORWARD_LEVEL_FRAME that must be there
FORWARD = 'FORWARD_LEVEL_FRAME'
ZONES = {'on_idle ms': ('Primary', 'on_idle'), 'Present ms': ('Primary', 'Render: End/Present'), 'OnFrame ms': ('Primary', 'IGame_Level::OnFrame'),
    'scheduler ms': ('X-Ray Game Thread', 'Sheduler'), 'GPU frame ms': ('GPU', 'Frame')}
CPU_ZONES = ['ForwardLampShadows', 'CollectMain', 'RenderSunCascade', 'phase_post', 'ScenePrepass', 'OffscreenReflections', 'ForwardOcclusion', 'ForwardOpaque',
    'ForwardSorted', 'SurfaceLights', 'ForwardOit', 'ForwardCull']
GPU_ZONES = ['ForwardLampShadows', 'RenderSunCascade', 'phase_post', 'ScenePrepass', 'OffscreenReflections', 'ForwardOcclusion', 'ForwardOpaque', 'ForwardSorted', 'ForwardOit']

# Log numbers: key, pattern, how (last: the last line's number; sum: of every line; count: of the integers in the last line's
# text, the surfaces weighed apart left out), the only API that logs it
LEFT = r'left in the graph by reason: (.*)$'
NUMBERS = [
    ('old: DIP/DP a frame', r'\[autotest\] old path: DIP/DP (\d+) median', 'last', ''),
    ('old: graph packets a frame', r'\[autotest\] old path: .*graph packets (\d+) median', 'last', ''),
    ('graph: main view static packets', r'\[scene\] main view left by reason: the graph holds (\d+) static packets', 'last', ''),
    ('graph: moving mesh parts', r'\[scene\] forward cull: .*mesh parts left in the graph (\d+)$', 'last', ''),
    ('graph: HUD parts', r'\[scene\] HUD parts taken \d+, ' + LEFT, 'count', ''),
    ('graph: sorted list', r'\[scene\] sorted list in the frame before: .*?' + LEFT, 'count', ''),
    ('graph: WBOIT list', r'\[scene\] weighed surfaces \(WBOIT\) in the frame before: .*?' + LEFT, 'count', ''),
    ('graph: HUD sorted list', r"\[scene\] HUD's sorted list in the frame before: .*?" + LEFT, 'count', ''),
    ('graph: distortion', r'\[scene\] distortion in the frame before: .*?' + LEFT, 'count', ''),
    ("graph: HUD's distortion", r"\[scene\] HUD's distortion in the frame before: .*?" + LEFT, 'count', ''),
    ("graph: scope's mask", r"\[scene\] scope's mask in the frame before: .*?" + LEFT, 'count', ''),
    ('graph: second priority static, moving', r'\[scene\] graph passes in the frame before: (second priority static \d+, moving \d+)$', 'count', ''),
    ('graph: sun cascades', r"\[scene\] sun cascade casters taken .*the cascades' graphs drew (.*) packets themselves", 'count', ''),
    ('graph: lamp shadow views', r'\[scene\] lamp shadow views in the frame before: .*the graphs drew (.*) of the translucent pass', 'count', ''),
    ('graph: rain map', r'\[scene\] rain map in the frame before: .*the graphs drew (.*) of the translucent pass', 'count', ''),
    ('graph: spot shadow cache', r'\[scene\] spot shadow cache in the frame before: .*the graphs drew (.*) of the translucent pass', 'count', ''),
    ('3D icons: drawn', r'\[scene\] 3D icons in the frame before: (\d+) drawn', 'last', ''),
    ('3D icons: kept', r'\[scene\] 3D icons in the frame before: \d+ drawn, (\d+) kept', 'last', ''),
    ('3D icons: parts not drawn', r'\[scene\] 3D icons in the frame before: .*not drawn: (.*)$', 'count', ''),
    ('load ms', r'Game Loading Timer: Finished for (\d+) ms', 'last', ''),
    ('level shaders made', r"shaders at the (?:start of the level|level's shaders): (\d+) made", 'sum', ''),
    ('level shaders compiled', r"shaders at the (?:start of the level|level's shaders): \d+ made, (\d+) compiled", 'sum', ''),
    ('level shader compile ms', r"shaders at the (?:start of the level|level's shaders): .*, compile (\d+) ms", 'sum', ''),
    ('manifest stages', r'shader manifest: (\d+) stages made behind the start screen', 'last', ''),
    ('manifest pipelines', r'shader manifest: (\d+) of \d+ pipelines made behind the start screen', 'last', ''),
    ('forward pipelines', r'\[scene\] forward pipelines: (\d+) used of', 'last', ''),
    ('pipelines made ahead', r'forward pipelines made ahead of their first cluster: (\d+) in', 'sum', ''),
    ('pipelines made ahead ms', r'forward pipelines made ahead of their first cluster: \d+ in (\d+) ms', 'sum', ''),
    ('D3D12 pso misses', r'\* D3D12 desc-copy .* pso \d+/(\d+)', 'last', 'd3d12'),
    ('latency mean ms', r'\* Frame latency: ([\d.]+) ms mean', 'last', ''),
    ('latency worst ms', r'\* Frame latency: [\d.]+ ms mean, ([\d.]+) ms worst', 'last', ''),
    ('textures MB', r'\* video memory: \d+ of \d+ textures loaded, (\d+) MB on the device', 'last', ''),
    ('targets MB', r'\* video memory: .*render targets, (\d+) MB$', 'last', ''),
    ('process video memory MB', r'\[scene\] video memory of the process: (\d+) MB used', 'last', ''),
    ('D3D12 live surfaces', r'\* D3D12 live: (\d+) surfaces', 'last', 'd3d12'),
    ('D3D12 live buffers', r'\* D3D12 live: \d+ surfaces, (\d+) buffers', 'last', 'd3d12'),
    ('texture buckets MB', r'\[scene\] texture buckets: .*, (\d+) MB$', 'last', ''),
    ('head pools MB', r'\[scene\] texture heads: .*pools (\d+) MB of a budget', 'last', ''),
    ('material tails MB', r'\[scene\] materials: .*tails (\d+) MB', 'last', ''),
    ('material heads MB', r'\[scene\] materials: .*heads (\d+) MB$', 'last', ''),
    ('alpha array MB', r'\[scene\] alpha array: .*, (\d+) MB$', 'last', ''),
    ('working set MB', r'\[autotest\] process memory: working set (\d+) MB', 'last', ''),
    ('working set peak MB', r'\[autotest\] process memory: working set \d+ MB \(peak (\d+) MB\)', 'last', ''),
    ('commit MB', r'\[autotest\] process memory: .*commit (\d+) MB', 'last', ''),
    ('commit peak MB', r'\[autotest\] process memory: .*commit \d+ MB \(peak (\d+) MB\)', 'last', ''),
]


# The GPU's utilisation with no engine running, the median of a few samples: other programs' load stretches every pass
def BackgroundGpu():
    Samples = []
    for _ in range(5):
        try:
            Samples.append(int(subprocess.run(['nvidia-smi', '--query-gpu=utilization.gpu', '--format=csv,noheader,nounits'], capture_output=True, text=True, timeout=10).stdout.split()[0]))
        except (OSError, ValueError, IndexError, subprocess.TimeoutExpired):
            return 'unknown'
        time.sleep(0.5)
    return int(statistics.median(Samples))


def Running():
    if os.name == 'nt':
        Lines = subprocess.run(['tasklist', '/FO', 'CSV', '/NH'], capture_output=True, text=True, errors='replace').stdout.splitlines()
        Names = {Line.split('","')[0].strip('"').lower() for Line in Lines if Line}
    else:
        Names = {os.path.basename(Name).lower() for Name in subprocess.run(['ps', '-A', '-o', 'comm='], capture_output=True, text=True).stdout.split()}

    return sorted(Names & {'xrengine.exe', 'leveleditor.exe', 'xrengine', 'leveleditor'})


def Launch(Api, Save, Preset, Args, Logs, Out):
    Config = [f'r_api {Api}'] + [Line.strip() for Line in open(Preset, encoding='utf-8', errors='replace') if Line.strip() and not Line.startswith(';')] + PINNED

    with open(os.path.join(Args.game, APPDATA, 'render_check.ltx'), 'w', newline='\r\n') as File:
        File.write('\n'.join(Config) + '\n')

    Shot = 'render_check_' + Save
    Commands = SAVES.get(Save, [])
    Steps = [f'start {Save}'] + ['cmd ' + Command for Command in Commands if not Command.startswith('late ')] + [f'wait {Args.wait}']
    Steps += ['cmd ' + Command[5:] for Command in Commands if Command.startswith('late ')] + [f'settle {Args.settle}', f'shot {Shot}']
    Steps += [f'profile {PROFILE}', 'cmd render_memory_stats', f'wait {MEMORY_WAIT}'] if Args.profile else []
    Script = os.path.join(tempfile.mkdtemp(prefix='render_check_'), 'script.txt')

    if ' ' in Script:
        sys.exit(f'the script path has a space, the engine takes one token: {Script}')

    with open(Script, 'w', newline='\r\n') as File:
        File.write('\n'.join(Steps) + '\n')

    Line = [Args.engine, '-autotest', '-nointro', '-autotest_dt', STEP_MS, '-autotest_timeout', str(Args.timeout), '-ltx', 'render_check.ltx', '-autotest_script', Script]
    Start = time.time()

    Process = subprocess.Popen(Line, cwd=Args.game)

    try:
        Code = Process.wait(timeout=Args.timeout + 60)
    except subprocess.TimeoutExpired:
        Process.kill()
        Process.wait()
        Code = f'killed after {Args.timeout + 60} s'

    shutil.rmtree(os.path.dirname(Script), ignore_errors=True)
    Fresh = [os.path.join(Logs, Name) for Name in os.listdir(Logs) if Name.startswith('ixray-') and Name.endswith('.log') and os.path.getmtime(os.path.join(Logs, Name)) >= Start - 1]
    Log = open(max(Fresh, key=os.path.getmtime), encoding='utf-8', errors='replace').read() if Fresh else ''
    Tga = os.path.join(Logs, Shot + '.tga')
    Png = os.path.join(Out, Save + '.png')
    Frames = os.path.join(Logs, 'autotest.csv')

    if os.path.exists(Tga):
        Image.open(Tga).convert('RGB').save(Png)
        os.remove(Tga)
    elif os.path.exists(Png):
        os.remove(Png)

    return Code, Log, time.time() - Start, Frames if os.path.exists(Frames) and os.path.getmtime(Frames) >= Start - 1 else ''


def Compare(Png, Reference, Out, Save):
    if not os.path.exists(Png):
        return None, 'no shot'

    if not os.path.exists(Reference):
        return None, 'no reference'

    A, B = Image.open(Reference).convert('RGB'), Image.open(Png).convert('RGB')

    if A.size != B.size:
        return None, f'size {B.size} vs {A.size}'

    Diff = ImageChops.difference(A, B)
    Mean = sum(ImageStat.Stat(Diff).mean) / 3
    Diff.point(lambda Value: min(255, Value * 8)).save(os.path.join(Out, Save + '_diff.png'))
    return Mean, ''


# The averages of a profiler capture: the frame, the zones of ZONES, and every zone right under FORWARD_LEVEL_FRAME
def ReadProfile(Path):
    Rows = [(Track, int(Depth), Name, float(Avg), float(Peak)) for Track, Depth, Name, _, Avg, Peak in list(csv.reader(open(Path, newline='', encoding='utf-8', errors='replace')))[1:]]
    Values, Lost = {}, []

    def Find(Track, Key):
        Found = [Row for Row in Rows if Row[0] == Track and (Row[2] == Key or Row[2].endswith('/' + Key))]
        return min(Found, key=lambda Row: Row[1]) if Found else None

    if Frame := next((Row for Row in Rows if Row[0] == 'frame'), None):
        Values['profile frames'], Values['frame ms'], Values['profile worst frame ms'] = int(Frame[2].split()[0]), Frame[3], Frame[4]
    else:
        Lost.append('frame')

    for Key, (Track, Zone) in ZONES.items():
        if Row := Find(Track, Zone):
            Values[Key] = Row[3]
        else:
            Lost.append(f'{Track}/{Zone}')

    for Track, Prefix, Required in (('Primary', 'cpu', CPU_ZONES), ('GPU', 'gpu', GPU_ZONES)):
        if not (Root := Find(Track, FORWARD)):
            Lost.append(f'{Track}/{FORWARD}')
            continue

        Children = {Row[2][len(Root[2]) + 1:]: Row[3] for Row in Rows if Row[0] == Track and Row[1] == Root[1] + 1 and Row[2].startswith(Root[2] + '/')}
        Values.update({'render cpu ms': sum(Children.values())} if Prefix == 'cpu' else {})
        Values[f'{Prefix} {FORWARD} ms'] = Root[3]
        Values.update({f'{Prefix} {Name} ms': Ms for Name, Ms in Children.items()})
        Lost += [f'{Track}/{FORWARD}/{Name}' for Name in Required if Name not in Children]

    return Values, Lost


# Drawn level frames of autotest.csv from the end of the load up to the SPIKE_FRAMES-th counted one; frames that ran a step
# (the shot's readback) are left out
def ReadSpikes(Path):
    Rows = list(csv.DictReader(open(Path, newline='')))

    if not Rows or 'step' not in Rows[0]:
        return {}, ['autotest.csv has no frame_ms / counted / step columns: an engine older than this script']

    Window, Counted = [], 0

    for Row in Rows:
        if Counted >= SPIKE_FRAMES:
            break

        Counted += int(Row['counted'])
        Window += [float(Row['frame_ms'])] if float(Row['frame_ms']) > 0 and not int(Row['step']) else []

    if not Window:
        return {}, ['autotest.csv holds no drawn level frame']

    Median = statistics.median(Window)
    Values = {'first frames': len(Window), 'first frames median ms': Median, 'spikes (> 2x median)': sum(Ms > 2 * Median for Ms in Window), 'worst frame ms': max(Window)}
    return Values, [] if Counted >= SPIKE_FRAMES else [f'autotest.csv: {Counted} of {SPIKE_FRAMES} counted frames']


def Measure(Api, Log, Frames, Logs):
    Values, Issues, Notes = {}, [], []
    Capture = re.findall(r'\* Profiler: capture of \d+ frames.*written to (profiler_capture_\d+\.csv)', Log)

    if Capture and os.path.exists(os.path.join(Logs, Capture[-1])):
        Found, Lost = ReadProfile(os.path.join(Logs, Capture[-1]))
        Values.update(Found)
        Notes += ['zone not found: ' + Zone for Zone in Lost]
        Issues += [] if Found.get('profile frames') == PROFILE else [f'the profile holds {Found.get("profile frames")} frames, not {PROFILE}']
    else:
        Issues.append('no profiler capture in the log')

    if Frames:
        Found, Lost = ReadSpikes(Frames)
        Values.update(Found)
        Notes += Lost
    else:
        Issues.append('no autotest.csv of this launch')

    for Key, Pattern, How, Only in NUMBERS:
        if Only and Only != Api:
            continue

        if not (Found := re.findall(Pattern, Log, re.M)):
            Notes.append('line not found: ' + Key)
            continue

        Values[Key] = sum(map(float, Found)) if How == 'sum' else float(Found[-1]) if How == 'last' else float(sum(map(int, re.findall(r'\d+', re.sub(r'weighed apart \d+', '', Found[-1])))))

    return Values, Issues, Notes


def Medians(Runs):
    Keys = list(dict.fromkeys(Key for Run in Runs for Key in Run))
    Result = {Key: statistics.median([Run[Key] for Run in Runs if Key in Run]) for Key in Keys}

    if len(Runs) > 1 and all('frame ms' in Run for Run in Runs):
        Frames = [Run['frame ms'] for Run in Runs]
        Result['frame ms spread %'] = (max(Frames) - min(Frames)) / statistics.median(Frames) * 100

    return Result


def Cell(Key, Value, Old):
    if Value is None:
        return 'n/f'

    IsMs = Key.endswith(' ms')
    Text = f'{Value:.3f}' if IsMs else f'{Value:.1f}' if Key.endswith('%') else f'{Value:g}'

    if Old is None or Value == Old:
        return Text

    Delta = Value - Old
    Percent = Delta / Old * 100 if Old else float('inf')
    IsOut = abs(Percent) > NOISE and not Key.endswith('%') and (not IsMs or abs(Delta) >= FLOOR_MS)
    return Text + (f' {Delta:+.3f}' if IsMs else f' {Delta:+g}') + f' {Percent:+.1f}%' + ('!' if IsOut else '')


# One table of a preset and an API: a row a number, a column a save; with a reference, the difference in units and percent.
# Of the zones under FORWARD_LEVEL_FRAME only those of CPU_ZONES and GPU_ZONES are shown; the json holds every one
def PrintTable(Title, Saves, Now, Reference):
    Keys = [Key for Key in dict.fromkeys(Key for Table in (Now, Reference) for Save in Saves for Key in Table.get(Save, {}))
        if not Key.startswith(('cpu ', 'gpu ')) or Key[4:-3] in CPU_ZONES + GPU_ZONES + [FORWARD]]
    Rows = [[Key] + [Cell(Key, Now.get(Save, {}).get(Key), Reference.get(Save, {}).get(Key)) for Save in Saves] for Key in Keys]
    Head = [Title] + Saves
    Widths = [max(len(Row[Index]) for Row in Rows + [Head]) for Index in range(len(Head))]

    for Row in [Head] + Rows:
        print('  '.join(Text.ljust(Width) for Text, Width in zip(Row, Widths)))

    Out = sum(Text.endswith('!') for Row in Rows for Text in Row[1:])
    print(f'{Out} values outside the {NOISE:g} % band (!)' if Reference else '(no reference numbers)', end='\n\n')


def CompareFiles(Before, After):
    Tables = [{(Row['preset'], Row['api']): {} for Row in Data['rows']} for Data in (Before, After)]

    for Data, Table in zip((Before, After), Tables):
        for Row in Data['rows']:
            Table[(Row['preset'], Row['api'])][Row['save']] = Row['median']

    for Group in dict.fromkeys(list(Tables[1]) + list(Tables[0])):
        Saves = list(dict.fromkeys(list(Tables[1].get(Group, {})) + list(Tables[0].get(Group, {}))))
        PrintTable(' '.join(Group), Saves, Tables[1].get(Group, {}), Tables[0].get(Group, {}))

    return 0


def Main():
    # The engine's log is not all in the console's code page
    sys.stdout.reconfigure(errors='replace')
    Parser = argparse.ArgumentParser(description='Engine picture check against a reference')
    Parser.add_argument('--api', choices=['d3d12', 'd3d11', 'both'], default='d3d12')
    Parser.add_argument('--saves', default=','.join(SAVES))
    Parser.add_argument('--engine', default=ENGINE, help='xrEngine.exe of a RelWithDebInfo or Debug build; IXRAY_ENGINE by default')
    Parser.add_argument('--game', default=GAME, help='the game folder the engine runs in; IXRAY_GAME by default')
    Parser.add_argument('--preset', default=PRESET)
    Parser.add_argument('--presets', help='rspec_<name>.ltx of gamedata_cs, comma separated: minimal,low,medium,high,ultra; replaces --preset')
    Parser.add_argument('--reference', help='<game>/_appdata_ixray_/render_check/reference by default')
    Parser.add_argument('--out', help='<game>/_appdata_ixray_/render_check/last by default')
    Parser.add_argument('--threshold', type=float, default=THRESHOLD, help='largest mean absolute difference that passes, 0..255')
    Parser.add_argument('--settle', type=int, default=SETTLE)
    Parser.add_argument('--wait', type=int, default=WAIT)
    Parser.add_argument('--timeout', type=int, default=TIMEOUT)
    Parser.add_argument('--profile', type=int, nargs='?', const=3, default=0, help=f'launches a save, each profiling {PROFILE} counted frames; their medians are printed and kept')
    Parser.add_argument('--json', help='file of everything the run read, <out>/render_check.json by default')
    Parser.add_argument('--compare', nargs=2, metavar=('BEFORE', 'AFTER'), help='print the medians of two --json files, AFTER against BEFORE, and launch nothing')
    Parser.add_argument('--update-reference', action='store_true')
    Args = Parser.parse_args()

    if Args.compare:
        return CompareFiles(*(json.load(open(Path)) for Path in Args.compare))

    if not Args.engine or not Args.game:
        sys.exit('give --engine and --game, or set IXRAY_ENGINE and IXRAY_GAME')

    Args.reference = Args.reference or os.path.join(Args.game, APPDATA, 'render_check', 'reference')
    Args.out = Args.out or os.path.join(Args.game, APPDATA, 'render_check', 'last')

    if Busy := Running():
        sys.exit('close these first: ' + ', '.join(Busy))

    if Args.profile:
        print(f'background GPU load before the launches: {BackgroundGpu()} % (NVIDIA; above a few percent every GPU pass reads slower)')

    Presets = [(Name, os.path.join(PRESETS, f'rspec_{Name}.ltx')) for Name in Args.presets.split(',')] if Args.presets else [(os.path.splitext(os.path.basename(Args.preset))[0].removeprefix('rspec_'), Args.preset)]
    Logs = os.path.join(Args.game, APPDATA, 'logs')
    Rows, Report, Tables, Failed = [], [], [], False

    for Name, Preset in Presets:
        for Api in (['d3d12', 'd3d11'] if Args.api == 'both' else [Args.api]):
            Folder = Api if Name == 'ultra' else os.path.join(Name, Api)
            Out, Reference = os.path.join(Args.out, Folder), os.path.join(Args.reference, Folder)
            os.makedirs(Out, exist_ok=True)
            Stored, Timed = os.path.join(Reference, 'scene.json'), os.path.join(Reference, 'profile.json')
            Known = json.load(open(Stored)) if os.path.exists(Stored) else {}
            KnownTimes = json.load(open(Timed)) if os.path.exists(Timed) else {}
            Counted, Times = {}, {}

            for Save in Args.saves.split(','):
                Runs, Issues, Notes, Means, Seconds = [], [], [], [], 0.0

                for Run in range(max(1, Args.profile)):
                    Code, Log, Took, Frames = Launch(Api, Save, Preset, Args, Logs, Out)
                    Seconds += Took
                    Scene = {Key: list(map(int, Found.groups())) for Key, Pattern in COUNTERS.items() if (Found := re.search(Pattern, Log))}
                    Fresh = [f'{Pattern} x{len(Lines)}: {Lines[0].strip()[:120]}' for Pattern in PROBLEMS if (Lines := [Line for Line in Log.splitlines() if re.search(Pattern, Line)])]
                    Fresh += [] if Log else ['no log']
                    Fresh += [f'old draws a frame: {Old}' for Old in re.findall(r'\[autotest\] old path: DIP/DP (\d+) median', Log)[-1:] if int(Old)]
                    Fresh += [] if Code == 0 else [f'exit {Code}']
                    Fresh += [] if Save not in Counted or Counted[Save] == Scene else ['scene counters differ between launches']
                    Counted.setdefault(Save, Scene)
                    Mean, Why = Compare(os.path.join(Out, Save + '.png'), os.path.join(Reference, Save + '.png'), Out, Save)
                    Means.append(Mean)
                    Waited = re.search(r'textures loaded after the scene: (\d+) of (\d+)', Log)

                    if Args.profile:
                        Values, Wrong, Lost = Measure(Api, Log, Frames, Logs)
                        Runs.append(Values)
                        Fresh += Wrong
                        Notes += Lost

                    if Fresh or Mean is None or Mean > Args.threshold:
                        print(f'--- {Name} {Api} {Save} launch {Run + 1}: last lines of the log', *Log.splitlines()[-15:], sep='\n')

                    Issues += Fresh

                Issues = list(dict.fromkeys(Issues))
                Missing = [Key for Key in COUNTERS if Key not in Counted[Save]]
                Scene = 'missing ' + ','.join(Missing) if Missing else 'ok' if Args.update_reference or Known.get(Save) == Counted[Save] else f'{Counted[Save]} vs {Known.get(Save)}'
                Png = os.path.join(Out, Save + '.png')

                if Args.update_reference and os.path.exists(Png):
                    os.makedirs(Reference, exist_ok=True)
                    shutil.copyfile(Png, os.path.join(Reference, Save + '.png'))
                    Means = [Compare(Png, os.path.join(Reference, Save + '.png'), Out, Save)[0]]

                Worst = None if None in Means else max(Means)
                IsPass = Worst is not None and Worst <= Args.threshold and Scene == 'ok' and not Issues
                Failed |= not IsPass
                Times[Save] = Medians(Runs) if Runs else {}
                Notes = list(dict.fromkeys(Notes))
                Rows.append((Name, Api, Save, f'{Worst:.3f}' if Worst is not None else Why, Scene, '; '.join(Issues) or 'ok', f'{Seconds:.0f}s', '/'.join(Waited.groups()) if Waited else '', 'PASS' if IsPass else 'FAIL'))
                Report.append({'preset': Name, 'api': Api, 'save': Save, 'result': Rows[-1][-1], 'diff': Means, 'scene': Counted[Save], 'issues': Issues, 'notes': Notes, 'runs': Runs, 'median': Times[Save]})

            if Args.update_reference:
                os.makedirs(Reference, exist_ok=True)
                json.dump({**Known, **Counted}, open(Stored, 'w'), indent=1)

                if Args.profile:
                    json.dump({**KnownTimes, **Times}, open(Timed, 'w'), indent=1)

            Tables.append((f'{Name} {Api}', Times, {} if Args.update_reference else KnownTimes))

    Head = ('preset', 'api', 'save', 'diff', 'scene', 'log', 'time', 'late textures', 'result')
    Widths = [max(len(str(Row[Index])) for Row in Rows + [Head]) for Index in range(len(Head) - 1)]

    for Row in [Head] + Rows:
        print('  '.join(str(Text).ljust(Width) for Text, Width in zip(Row, Widths)) + '  ' + Row[-1])

    print()

    for Title, Times, KnownTimes in Tables if Args.profile else []:
        PrintTable(Title, list(Times), Times, KnownTimes)

    for Item in Report:
        if Item['notes']:
            print(f'--- {Item["preset"]} {Item["api"]} {Item["save"]}: ' + '; '.join(Item['notes']))

    Path = Args.json or os.path.join(Args.out, 'render_check.json')
    json.dump({'time': time.strftime('%Y-%m-%d %H:%M:%S'), 'engine': Args.engine, 'profile': Args.profile, 'rows': Report}, open(Path, 'w'), indent=1)
    print('numbers:', Path)
    print('RESULT:', 'FAIL' if Failed else 'PASS')
    return 1 if Failed else 0


if __name__ == '__main__':
    sys.exit(Main())
