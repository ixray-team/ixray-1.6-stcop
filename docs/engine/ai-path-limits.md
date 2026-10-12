# Online AI path limits

Add this section to the level's `levels/<level>/level.ltx` to bound online ground
navigation for stalkers and monsters:

```ini
[ai_path_limits]
npc_max_path_length = 200
monster_max_path_length = 100
max_iterations = 8192
max_visited_nodes = 8192
```

Lengths are meters along the AI graph, including slopes and detours. They limit
only the active movement segment, not A* exploration. A* may find a much longer
route, including a first step away from the goal to get around a wall. It uses
geometric distance as its heuristic and 3D edge lengths as costs.

A complete route is cached per creature. The manager takes consecutive segments
from it without repeating A* at every segment endpoint. The cache is discarded
when the destination changes, the creature leaves the route, restrictions change,
or the next segment has broken/blocked links. Segments never mark the final patrol
or game-graph goal complete before it is actually reached.

Iteration and visited-node budgets still bound each search, including unreachable
and distant targets. Search state is not resumed across frames. If the work budget
expires, an already-settled vertex closer to the goal can provide a partial segment.
Such a result does not cache a complete route. If the queue is exhausted without
reaching the goal, the search reports failure instead of offering a dead-end
partial route. A destination without four free links fails immediately.
If no useful segment fits the meter
limit, the search fails and clears the old movement path. Limited searches retry
with a delay instead of permanently caching the failed start/goal pair.
The detailed path uses the segment endpoint while retaining the final target position.

Missing keys within the section use the values shown above. Zero disables the
corresponding limit. Negative/non-finite lengths fall back to the default and
produce a log message. Without the section, the original `ILevelGraph::Search` A* is used unchanged,
including its cell costs, weighted Manhattan heuristic, and existing search budgets.
Only levels with the section use `SearchLimited` and geometric path limits. Settings are read when creatures
spawn; reload the level after editing.

The limit applies to each generated segment. Distant patrol, scripted, and
game-graph goals remain valid. This bounded local search cannot guarantee a route
through arbitrary large mazes: obstacles requiring exploration beyond the work budget
may still require closer waypoints or a larger work budget.
Offline ALife and custom flying-monster navigation are not affected. The precise
smoothed movement trajectory may differ from the AI graph's length.

For the Death or Die Pripyat addon, the section is supplied through DLTX by
`ixr_addons/04_death_or_die__mod_base/levels/pripyat_full/mod_level_ai_path_limits.ltx`.
The level's original `level.ltx` remains in `00_livning_zone__base`; the engine
automatically loads the matching `mod_level_*.ltx` file from the same virtual directory.

## Path drawing

`al_draw_paths 1` draws the bounded algorithm's online creature paths through
`HUD().world_prims`. The default is `al_draw_paths 0`.

Cyan lines show the AI graph route, purple marks the endpoint of a partial segment,
and yellow marks the final destination. A short yellow arrow points toward the
final destination; no kilometer-long connection line is generated. Labels contain only the creature ID (`#123`), at the
creature, the segment endpoint, and the final destination. Red marks a failed search. Drawing works independently of
the original AI debug flags and only includes creatures using `[ai_path_limits]`.

The render thread reads a locked snapshot, not the path search's working buffer.
With the command disabled, path snapshots and object enumeration are skipped.

## Ground-monster traffic recovery

With path limits enabled, a ground monster that wants to move but makes less than
0.4 m of progress for two seconds checks nearby monsters within 6 m. The monster
with the larger ID yields to a moving neighbour; either ID may yield to an idle
neighbour. Enemies and intentional idle/script/animation movement are excluded.
Up to four short side/back steps are checked against the AI grid, restrictions
and nearby living creatures. A detour uses the same bounded path search. The
original control target is preserved and resumed on arrival or after five seconds.
Neighbour scans are spaced at least two seconds apart, including failed attempts.
This resolves open-space traffic jams; it cannot guarantee passage in a narrow
corridor without a free side step. Levels without `[ai_path_limits]` retain the
original navigation behaviour.

## AI-grid boundary nodes

The limited search only enters accessible nodes with four valid, non-self links
to accessible neighbours, each with a return link.
This applies to intermediate nodes, partial-segment endpoints and the destination.
An existing start node may have fewer links, so a creature can leave a boundary
spawn point. Both graph topology and current access restrictions are checked.
The original unlimited search is unchanged.
A destination without four free links is rejected before exploration. The normal
failed-search retry applies. Stalker obstacle-bypass probes use the same limited
search and only accept a route that actually reaches its destination. A passage
consisting solely of nodes with fewer links is excluded by this rule, even if
physically wide enough. Four links do not by themselves verify collision geometry.

## One-time expensive recovery

When a regular search fails for a valid four-transition destination, the level
path manager may make one larger search for that destination. Its iteration and
node budgets are at least 65536 each (or the configured value if larger), so this
is deliberately more expensive but still bounded. Attempts are remembered for active/recent goals; revisiting a retained target
does not repeat recovery. Every 30 seconds, records unused for at least 60 seconds
are removed and their vector capacity is trimmed. Unused planned routes are freed
as well. The current active goal and route are protected; cleanup never races a
distributed computation. An expired destination can receive a new one-time attempt.
New destinations may trigger recovery at most once per 10 seconds per creature.
Recovery only accepts a complete route to the goal. It caches that route and walks
it in the same meter-limited segments. `al_draw_paths` draws recovery route segments
and the creature ID red, while the goal remains yellow. A red ID without a route
still means failure. Invalid/broken destination nodes do not trigger recovery.

In limited mode the detailed smoothed trajectory is sampled at quarter-cell
intervals and checked against the same four-transition rule. If smoothing cuts
through a forbidden node, a centre-to-centre trajectory along the A* nodes is tried
using an allowed forward velocity. If that trajectory fails validation, movement
is rejected. This protects the AI-grid route but is not a full collision-volume
sweep against every static or dynamic object.
