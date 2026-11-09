# IX-Ray LuaJIT sources

Upstream: https://github.com/ixray-team/luajit-ixray
Revision: `015e23580d9f14ec1960beeebeaf0c5319d9135d` (LuaJIT 2.1.0-beta3).
The upstream license is retained in `COPYRIGHT`.

`lua_replacefunction(L, from, to)` clones the replacement and rewrites ordinary
Lua references once: tables (including keys), stacks, closed/open upvalues,
C closure upvalues and registry references used by luabind. The replacement is
returned in the `to` slot. Executing/suspended frame function slots are preserved
so that an invocation already in progress finishes its original prototype.

Compiled traces are invalidated on replacement. There is no forwarding lookup
or additional branch on the normal function call path. Function hash identity
is retained so a replacement does not relocate table nodes or invalidate an
ongoing table iterator. Ordinary function keys keep the existing JIT hash path
with an identity guard; indexing by a replaced function uses the interpreter.
Normal calls, numeric/string indexing and table traversal remain JIT enabled.

DynASM GC64 configuration matches `lj_arch.h`. Generator commands depend on
`buildvm` and their inputs so incremental builds cannot reuse stale VM/fold
tables after an internal layout or IR field change.

These are private VM layout changes. Rebuild the bundled LuaJIT from these
sources and use it together with the rebuilt engine. Do not replace this build
with the old NuGet DLL. Public Lua 5.1 structures and the luabind ABI are unchanged.

This operation updates references owned by the Lua VM. Arbitrary raw pointers
returned to external native code by `lua_topointer` are not writable Lua
references and cannot be rewritten. Standard `luabind::functor` references live
in the Lua registry and are covered.

The first external write through `lua_setupvalue` to an immutable Lua upvalue
marks it mutable and invalidates compiled traces once. Later reads and writes
use the live upvalue without repeated trace flushes. This keeps injected methods
from reading a constant-folded value when updating a formerly read-only local.
