# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_table_utils: `\gamedata\scripts\ixr_framework\utils\ffx_table_utils.script`
Utilities for tables: searching, cloning, sorting, traversal, keys and values:
* `is_value_exists_by_key`
* `push_back_by_key`
* `clone`
* `get_size`
* `sort_assoc_tables`
* `deep_concat`
* `is_contains`
* `get_contains_index`
* `get_contains_element_number`
* `get_last_value`
* `get_max_key_index`
* `get_min_key_index`
* `get_array_keys`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_table_utils.script]
--// Check whether an array of tables contains an element whose given key matches the target value.
is_value_exists_by_key(tbl, key, value)
args:
  tbl (table)(required) - array of tables.
  key (any)(required) - key to check in each subtable.
  value (any)(required) - value to find.
retval: (boolean) - true if a match is found; otherwise false.

--// Add a value to the subtable at the given key, creating the subtable if missing.
push_back_by_key(tbl, key, value)
args:
  tbl (table)(required) - parent table.
  key (any)(required) - key whose value should be a table.
  value (any)(required) - value to add to the subtable.
retval: (none)

--// Create a deep copy of a table recursively, preserving metatables.
clone(tbl)
args:
  tbl (table)(required) - table to clone.
retval: (table) - deep copy of the table.

--// Count table entries, including associative keys.
get_size(tbl)
args:
  tbl (table)(required) - table to count.
retval: (number) - number of key-value pairs.

--// Sort an associative table by keys converted to strings and return an array of values in that order.
sort_assoc_tables(tbl)
args:
  tbl (table)(required) - associative table.
retval: (table) - array of values sorted by key.

--// Recursively traverse a table, apply a closure to each non-table value and concatenate the results. Sort keys for deterministic ordering.
deep_concat(tbl, closure_fn)
args:
  tbl (table)(required) - table to process.
  closure_fn (function)(required) - function that takes a value and returns a string.
retval: (string) - concatenated string of processed values.

--// Check whether a flat (non-nested) table contains the given value.
is_contains(t, item)
args:
  t (table)(required) - flat table.
  item (any)(required) - value to find.
retval: (boolean) - true if the value is found; otherwise false.

--// Return the key corresponding to a value in a flat table, or false if not found.
get_contains_index(t, item)
args:
  t (table)(required) - flat table.
  item (any)(required) - value to find.
retval: (any|false) - the key if found; otherwise false.

--// Return the numeric index (starting at 1) of a value in a flat array, or false if not found.
get_contains_element_number(t, value)
args:
  t (table)(required) - flat table (array).
  value (any)(required) - value to find.
retval: (number|false) - the index if found; otherwise false.

--// Get the root-level value with the highest numeric key. If there are no numeric keys, return default_value.
get_last_value(input_table, default_value)
args:
  input_table (table)(required) - table to check.
  default_value (any)(optional) - value returned when no numeric keys exist.
retval: (any) - last value found or default_value.

--// Find the highest numeric key in a table, optionally recursing to the given depth.
get_max_key_index(input_table, default_value, depth)
args:
  input_table (table)(required) - table to check.
  default_value (any)(optional) - value returned if no numeric keys are found.
  depth (number)(optional) - recursion depth: nil or 1 — root only; >1 — nested tables; -1 — unlimited (default 1).
retval: (number|any) - highest numeric key or default_value.

--// Find the lowest numeric key in a table, optionally recursing to the given depth.
get_min_key_index(input_table, default_value, depth)
args:
  input_table (table)(required) - table to check.
  default_value (any)(optional) - value returned if no numeric keys are found.
  depth (number)(optional) - recursion depth: nil or 1 — root only; >1 — nested tables (default 1).
retval: (number|any) - lowest numeric key or default_value.

--// Get an array of all table keys, including string and numeric keys.
get_array_keys(tbl)
args:
  tbl (table)(required) - table from which to extract keys.
retval: (table) - array of keys.
```
:::

### Usage examples:
```lua
--// Check for a matching key value in an array of tables.
local items = {{id=1, name="apple"}, {id=2, name="banana"}}
if ffx_table_utils.is_value_exists_by_key(items, "name", "banana") then
    SemiLog("Banana found")
end

--// Add to a subtable.
local data = {}
ffx_table_utils.push_back_by_key(data, "players", "John")
--// data = { players = {"John"} }

--// Deep cloning.
local original = {a=1, b={c=2}}
local copy = ffx_table_utils.clone(original)
copy.b.c = 3
--// original.b.c remains 2

--// Table size.
local sz = ffx_table_utils.get_size({x=10, y=20, z=30}) --// 3

--// Sort an associative table.
local assoc = {z=1, a=2, m=3}
local sorted_vals = ffx_table_utils.sort_assoc_tables(assoc)
--// sorted_vals = {2,3,1}  (by keys a, m, z)

--// Deep concatenation.
local tbl = {x=1, y={a="hello", b="world"}}
local result = ffx_table_utils.deep_concat(tbl, function(v) return tostring(v) end)
--// result: "1helloworld" (order depends on key sorting)

--// Check whether a value exists.
local flat = {10, 20, 30}
if ffx_table_utils.is_contains(flat, 20) then
    SemiLog("20 found")
end

--// Get an index.
local idx = ffx_table_utils.get_contains_index({a=1, b=2}, 2) --// "b"
local numIdx = ffx_table_utils.get_contains_element_number({10, 20, 30}, 20) --// 2

--// Get the last value.
local last = ffx_table_utils.get_last_value({10, 20, 30}, nil) --// 30

--// Highest numeric key.
local maxKey = ffx_table_utils.get_max_key_index({[1]=10, [5]=20, [2]=30}, nil, 1) --// 5

--// Lowest numeric key.
local minKey = ffx_table_utils.get_min_key_index({[1]=10, [5]=20, [2]=30}, nil, 1) --// 1

--// Get all keys.
local keys = ffx_table_utils.get_array_keys({name="John", age=30, city="NY"})
--// keys = {"name", "age", "city"} (order is not guaranteed)
```
