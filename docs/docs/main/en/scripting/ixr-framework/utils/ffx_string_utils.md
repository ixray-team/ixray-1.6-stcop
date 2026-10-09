# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_string_utils: `\gamedata\scripts\ixr_framework\utils\ffx_string_utils.script`
Utilities for strings: searching, replacement, splitting, trimming, case conversion and more:
* `get_length`
* `trim`
* `contains`
* `split`
* `start_with`
* `end_with`
* `collapse`
* `explode`
* `escape_regex_special_chars`
* `chunk`
* `ucfirst`
* `lcfirst`
* `normalize_spaces`
* `replace`
* `remove_substring`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_string_utils.script]
--// Return the string length, or 0 for nil input.
get_length(str)
args:
  str (string|nil)(optional) - string to measure.
retval: (number) - string length or 0.

--// Remove leading and trailing whitespace.
trim(str)
args:
  str (string)(required) - string to process.
retval: (string) - trimmed string, or an empty string for nil/empty input.

--// Check whether a string contains the given substring.
contains(str, substr)
args:
  str (string)(required) - string to search.
  substr (string)(required) - substring to find.
retval: (boolean) - true if the substring is found; otherwise false.

--// Split a string by a substring (wrapper for explode). Trim the parts and include the remainder.
split(text, substr)
args:
  text (string)(required) - string to split.
  substr (string)(required) - separator.
retval: (table) - array of trimmed segments.

--// Check whether a string starts with the given prefix.
start_with(text, prefix)
args:
  text (string)(required) - string to check.
  prefix (string)(required) - prefix to find.
retval: (boolean) - true if the string starts with the prefix; otherwise false.

--// Check whether a string ends with the given suffix.
end_with(text, suffix)
args:
  text (string)(required) - string to check.
  suffix (string)(required) - suffix to find.
retval: (boolean) - true if the string ends with the suffix; otherwise false.

--// Collapse repeated occurrences of a substring into one.
collapse(text, substr)
args:
  text (string)(required) - source string.
  substr (string)(required) - substring to collapse.
retval: (string) - string with repeated substrings replaced by one occurrence.

--// Split a string by a substring, trim the parts and include the remainder.
explode(str, substr)
args:
  str (string)(required) - string to split.
  substr (string)(required) - separator.
retval: (table) - array of trimmed segments.

--// Escape special regular expression characters for use as a literal pattern.
escape_regex_special_chars(str)
args:
  str (string)(required) - string to escape.
retval: (string) - escaped string, with special characters prefixed by %.

--// Split a string into chunks of the given size.
chunk(text, size)
args:
  text (string)(required) - string to split.
  size (number)(required) - size of each chunk (use 1 if <= 0).
retval: (table) - array of string chunks.

--// Convert the first character to uppercase.
ucfirst(text)
args:
  text (string)(required) - source string.
retval: (string) - string with an uppercase first character, or an empty string for nil/empty input.

--// Convert the first character to lowercase.
lcfirst(text)
args:
  text (string)(required) - source string.
retval: (string) - string with a lowercase first character, or an empty string for nil/empty input.

--// Replace multiple whitespace characters with a single space.
normalize_spaces(text)
args:
  text (string)(required) - source string.
retval: (string) - string with normalized spaces.

--// Replace all occurrences of a substring with another substring.
replace(text, search, replace)
args:
  text (string)(required) - source string.
  search (string)(required) - substring to find.
  replace (string)(optional) - replacement substring (default "").
retval: (string) - string with all replacements applied.

--// Remove all occurrences of a substring.
remove_substring(text, substring)
args:
  text (string)(required) - source string.
  substring (string)(required) - substring to remove.
retval: (string) - string with the specified occurrences removed.
```
:::

### Usage examples:
```lua
--// String length.
local len = ffx_string_utils.get_length("Hello") --// 5
SemiLog(string.format("Length: %d", len))

--// Trimming.
local trimmed = ffx_string_utils.trim("  text  ") --// "text"

--// Check for a substring.
local has = ffx_string_utils.contains("hello world", "world") --// true

--// Splitting.
local parts = ffx_string_utils.split("one,two,three", ",")
--// parts = {"one", "two", "three"}

--// Check prefix/suffix.
if ffx_string_utils.start_with("Hello", "He") then
    SemiLog("Starts with He")
end

if ffx_string_utils.end_with("Hello", "lo") then
    SemiLog("Ends with lo")
end

--// Collapse repeated substrings.
local collapsed = ffx_string_utils.collapse("a---b---c", "---") --// "a-b-c"

--// Escape special characters.
local escaped = ffx_string_utils.escape_regex_special_chars("(test)") --// "%(test%)"

--// Split into chunks.
local chunks = ffx_string_utils.chunk("abcdef", 2) --// {"ab", "cd", "ef"}

--// First character case.
local upper = ffx_string_utils.ucfirst("hello") --// "Hello"
local lower = ffx_string_utils.lcfirst("HELLO") --// "hELLO"

--// Normalize spaces.
local normalized = ffx_string_utils.normalize_spaces("a  b   c") --// "a b c"

--// Replacement.
local replaced = ffx_string_utils.replace("hello world", "world", "there") --// "hello there"

--// Remove a substring.
local removed = ffx_string_utils.remove_substring("abc123abc", "abc") --// "123"
```
