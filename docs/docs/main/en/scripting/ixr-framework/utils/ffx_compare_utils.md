# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0


### ffx_compare_utils: `\gamedata\scripts\ixr_framework\utils\ffx_compare_utils.script`
Utilities for checking types and comparing values:
* `is_table`
* `is_empty_flat_or_assoc_table`
* `is_function`
* `is_userdata`
* `is_not_empty_string`
* `is_empty_string`
* `is_number`
* `has_pattern`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_compare_utils.script]
--// Check whether an object is a table.
is_table(object)
args:
  object (any) - object to check
retval: (boolean) - true if the object is a table; otherwise false

--// Check whether a table is empty at the root level (not recursively). Returns true for non-table input.
is_empty_flat_or_assoc_table(input_table)
args:
  input_table (table) - table to check
retval: (boolean) - true if the table is empty or the input is not a table; otherwise false

--// Check whether an object is a function.
is_function(object)
args:
  object (any) - object to check
retval: (boolean) - true if the object is a function; otherwise false

--// Check whether an object is userdata.
is_userdata(object)
args:
  object (any) - object to check
retval: (boolean) - true if the object is userdata; otherwise false

--// Check whether a string is non-nil and non-empty after conversion to a string.
is_not_empty_string(str)
args:
  str (string) - string to check
retval: (boolean) - true if the string is non-empty and non-nil; otherwise false

--// Check whether a string is nil or empty after conversion to a string.
is_empty_string(str)
args:
  str (string) - string to check
retval: (boolean) - true if the string is empty or nil; otherwise false

--// Check whether an object is a number.
is_number(object)
args:
  object (any) - object to check
retval: (boolean) - true if the object is a number; otherwise false

--// Check whether a string matches a wildcard pattern (default wildcard: '*').
--// A wildcard matches any sequence of characters, including an empty sequence.
--// Consecutive wildcards are collapsed into one.
--// Special regex characters in the pattern are escaped automatically.
--// If the pattern has no wildcard, use exact comparison.
--// If the pattern consists only of wildcards, match any string, including an empty one.
has_pattern(str, pattern, wildcard)
args:
  str (string) - string to check
  pattern (string) - pattern containing literals and wildcards
  wildcard (string, optional) - wildcard character, default '*'
retval: (boolean) - true if the string matches the pattern; otherwise false
```
:::

### Usage examples:
```lua
--// Check the type.
if ffx_compare_utils.is_table({}) then
  print("This is a table")
end

--// Check whether the table is empty.
local t = {}
if ffx_compare_utils.is_empty_flat_or_assoc_table(t) then
  print("The table is empty")
end

--// Check strings.
local s = "hello"
if ffx_compare_utils.is_not_empty_string(s) then
  print("The string is not empty")
end

--// Check a number.
local n = 42
if ffx_compare_utils.is_number(n) then
  print("This is a number")
end

--// Match a wildcard pattern.
if ffx_compare_utils.has_pattern("se_test", "se_*") then
  print("The string starts with 'se_'")
end

if ffx_compare_utils.has_pattern("start_end", "start*end") then
  print("The string starts with 'start' and ends with 'end'")
end

--// Multiple wildcards and empty matches.
if ffx_compare_utils.has_pattern("ac", "a*b*c") then
  print("'ac' matches 'a*b*c' (wildcards match empty sequences)")
end

--// Exact comparison without a wildcard.
if ffx_compare_utils.has_pattern("hello", "hello") then
  print("Exact match")
end
```
