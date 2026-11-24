import csv, os, sys

# Table of Nsight GPU Trace exports, made by nsight_passes.ps1. Per pass the run with the shortest time
# wins; its metrics are printed with it. A pass is a GPU event's name or the end of its path
# (DEFER_SUN/RenderSunCascade); events of that name are summed, the metrics are those of the longest.
# Arguments: the BASE folders of the traces joined by ';', the passes joined by ',', optionally "all"
Bases, Passes = sys.argv[1].split(';'), sys.argv[2].split(',')
IsDump = len(sys.argv) > 3 and sys.argv[3] == 'all'
Indent = 8
Runs = []


def Read(Path):
    return list(csv.reader(open(Path, encoding='utf-8', errors='replace'), delimiter='\t'))


for Base in Bases:
    if not os.path.exists(os.path.join(Base, 'D3DPERF_EVENTS.xls')):
        print('no trace in', Base)
        continue

    # Events are listed in order, nested by their indent
    Events = []
    Stack = []

    for Row in Read(os.path.join(Base, 'D3DPERF_EVENTS.xls'))[1:]:
        if len(Row) < 2:
            continue

        Depth = (len(Row[0]) - len(Row[0].lstrip(' '))) // Indent
        Stack = Stack[:Depth] + [Row[0].strip()]
        Events.append(('/'.join(Stack), float(Row[1])))

    Rows = Read(os.path.join(Base, 'GPUTRACE_REGIMES.xls'))
    Regimes = [Row for Row in Rows[1:] if Row]
    Runs.append((Events, Rows[0], Regimes))

if not Runs:
    sys.exit(1)

Columns = [
    ('sm%', 'sm__throughput.avg.pct'),
    ('l1%', 'l1tex__throughput.avg.pct'),
    ('dram%', 'dramc__throughput.avg.pct'),
    ('cs_warps%', 'tpc__warps_active_shader_cs_realtime.avg.pct'),
    ('ps_warps%', 'tpc__warps_active_shader_ps_realtime.avg.pct'),
    ('sm_idle%', 'tpc__warps_inactive_sm_idle_realtime.avg.pct'),
    ('cs_regs%', 'registers_allocated_shader_cs_queue_sync_realtime.avg.pct'),
    ('Minst', 'sm__inst_executed_realtime.sum'),
]


def IsNamed(Path, Name):
    return Path == Name or Path.endswith('/' + Name)


print('pass'.ljust(32) + 'ms'.rjust(8) + ' runs'.ljust(len(Runs) * 7 + 2) + ''.join(Name.rjust(11) for Name, _ in Columns))

for Name in Passes:
    Timed = []

    for Events, Header, Regimes in Runs:
        Found = [(Index, Time) for Index, (Path, Time) in enumerate(Events) if IsNamed(Path, Name)]
        Longest = max(Found, key=lambda Item: Item[1])[0] if Found else None
        # The regimes are listed in the order of the events
        Row = Regimes[Longest] if Longest is not None and Longest < len(Regimes) and IsNamed(Regimes[Longest][0], Name) else None
        Timed.append((sum(Time for _, Time in Found) if Found else float('inf'), Header, Row))

    Best, Header, Row = min(Timed, key=lambda Run: Run[0])
    Line = Name.ljust(32) + ('%8.3f' % Best) + ' ' + ' '.join('%6.3f' % Run[0] for Run in Timed) + '  '

    for Column, Key in Columns:
        Index = next((I for I, H in enumerate(Header) if Key in H), None)
        Value = float(Row[Index]) if Row is not None and Index is not None and Row[Index] else None
        Line += ' ' * 11 if Value is None else '%11.2f' % (Value / 1e6 if Column == 'Minst' else Value)

    print(Line)

    if IsDump and Row is not None:
        for H, V in zip(Header, Row):
            print(Name, H, V)
