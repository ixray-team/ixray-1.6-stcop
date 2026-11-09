#pragma once

namespace ScriptInjection
{
// Kept in the engine binary: no modifiable bootstrap script or LuaJIT internals.
static constexpr char Runtime[] = R"IXR_LUA(
local class_table, replace_references = ...
local assert, error, type, pairs, ipairs, next = assert, error, type, pairs, ipairs, next
local getfenv, setfenv, setmetatable = getfenv, setfenv, setmetatable
local getupvalue, setupvalue, upvaluejoin = debug.getupvalue, debug.setupvalue, debug.upvaluejoin
local getinfo = debug.getinfo
local dump, loadstring, concat, sort = string.dump, loadstring, table.concat, table.sort
assert(type(upvaluejoin) == "function", "script injection requires debug.upvaluejoin")

local contexts, owners = {}, {}
local api = {}
local function track_replacement(previous, installed)
    if type(previous) == "function" and type(installed) == "function" and previous ~= installed then
        installed = replace_references(previous, installed)
    end
    return installed
end
local keywords = {}
for word in ("and break do else elseif end false for function goto if in local nil not or repeat return then true until while"):gmatch("%S+") do
    keywords[word] = true
end
local function identifier(name)
    return type(name) == "string" and name:match("^[%a_][%w_]*$") and not keywords[name]
end
local function check_name(name)
    assert(identifier(name), "expected a Lua identifier")
end
local function context(target)
    local methods = class_table(target)
    if methods then
        if not owners[target] then
            local found
            for _, method in pairs(methods) do
                if type(method) == "function" then
                    local candidate = contexts[getfenv(method)]
                    if candidate then
                        assert(not found or found == candidate, "ambiguous class owner")
                        found = candidate
                    end
                end
            end
            owners[target] = found
        end
        assert(owners[target], "class has no registered script context")
        return owners[target], target
    end
    assert(type(target) == "table" and contexts[target], "expected a loaded namespace or Lua class")
    return contexts[target], target
end
local function cell(value)
    return function() return value end
end
local function finish(ctx)
    for _, value in pairs(ctx.namespace) do
        if class_table(value) and not owners[value] then owners[value] = ctx end
    end
    for _, getter in pairs(ctx.locals) do
        local value = getter()
        if class_table(value) and not owners[value] then owners[value] = ctx end
    end
end
function api.Begin(namespace)
    local ctx = {namespace = namespace, locals = {}}
    contexts[namespace] = ctx
    return function(name, getter)
        if name == nil then
            finish(ctx)
        else
            ctx.locals[name] = getter
        end
    end
end
function api.override_or_create_local_variable(target, name, value)
    check_name(name)
    local ctx = context(target)
    local getter = ctx.locals[name]
    local previous = getter and getter()
    value = track_replacement(previous, value)
    if getter then setupvalue(getter, 1, value)
    else ctx.locals[name] = cell(value) end
end
local function get_locals(target, names, functions_only)
    local ctx = context(target)
    if type(names) == "string" then
        check_name(names)
        local getter = ctx.locals[names]
        if getter then
            local value = getter()
            if not functions_only or type(value) == "function" then return value end
        end
        return nil
    end
    local result = {}
    if names == nil then
        for name, getter in pairs(ctx.locals) do
            local value = getter()
            if not functions_only or type(value) == "function" then
                result[#result + 1] = {name = name, value = value}
            end
        end
        sort(result, function(a, b) return a.name < b.name end)
    else
        assert(type(names) == "table", "expected a local name or an array of names")
        local count = 0
        for index, name in pairs(names) do
            assert(type(index) == "number" and index >= 1 and index % 1 == 0, "local names must be an array")
            check_name(name)
            count = count + 1
        end
        for index = 1, count do
            local name = names[index]
            assert(name ~= nil, "local names must be contiguous")
            local getter = ctx.locals[name]
            if getter then
                local value = getter()
                if not functions_only or type(value) == "function" then
                    result[#result + 1] = {name = name, value = value}
                end
            end
        end
    end
    return result
end
function api.get_local_variables(target, names)
    return get_locals(target, names, false)
end
function api.get_local_functions(target, names)
    return get_locals(target, names, true)
end

function api.override_or_create_method(target, name, source, source_locals)
    check_name(name)
    local ctx, destination = context(target)
    assert(type(source) == "function" and getinfo(source, "S").what ~= "C", "expected a Lua function")
    assert(source_locals == nil or type(source_locals) == "table", "source_locals must be an array of names")
    local selected, count = {}, 0
    for index, variable in pairs(source_locals or {}) do
        assert(type(index) == "number" and index >= 1 and index % 1 == 0, "source_locals must be an array")
        check_name(variable)
        assert(not selected[variable], "duplicate source local: " .. variable)
        selected[variable], count = true, count + 1
    end
    for index = 1, count do assert(source_locals[index] ~= nil, "source_locals must be contiguous") end
    -- Clone bytecode, never compile caller-provided text or mutate its closure.
    local installed = assert(loadstring(dump(source)))
    local index = 1
    while true do
        local variable = getupvalue(source, index)
        if not variable then break end
        if selected[variable] then
            upvaluejoin(installed, index, source, index)
            selected[variable] = nil
        else
            local getter = ctx.locals[variable]
            assert(getter, "target has no private local: " .. variable)
            upvaluejoin(installed, index, getter, 1)
        end
        index = index + 1
    end
    assert(next(selected) == nil, "source_locals contains a name not captured by the function")
    setfenv(installed, setmetatable({}, {
        __index = function(_, key)
            local getter = ctx.locals[key]
            if getter then return getter() end
            return ctx.namespace[key]
        end,
        __newindex = function(_, key, value)
            local getter = ctx.locals[key]
            if getter then setupvalue(getter, 1, value)
            else ctx.namespace[key] = value end
        end
    }))
    -- Use luabind's normal assignment to update both class tables and caches.
    local previous = rawget(class_table(target) or destination, name)
    installed = track_replacement(previous, installed)
    destination[name] = installed
end

-- A Lua 5.1/LuaJIT token reader and recursive-descent syntax walker. It only
-- inserts captures after declarations in the outermost block. It neither
-- rewrites expressions nor guesses statement boundaries from newlines.
)IXR_LUA"
R"IXR_LUA(
local function tokens(source)
    local result, position, size = {}, 1, #source
    local function long_string(start)
        local equals = source:match("^%[(=*)%[", start)
        if not equals then return nil end
        local _, last = source:find("]" .. equals .. "]", start + #equals + 2, true)
        assert(last, "unfinished long string/comment")
        return last + 1
    end
    while position <= size do
        local start, char = position, source:sub(position, position)
        if char:match("%s") then
            position = position + 1
        elseif source:sub(position, position + 1) == "--" then
            position = long_string(position + 2) or ((source:find("[\r\n]", position + 2)) or (size + 1))
        else
            local kind
            if char == "'" or char == '"' then
                position = position + 1
                while position <= size and source:sub(position, position) ~= char do
                    if source:sub(position, position) == "\\" then position = position + 1 end
                    position = position + 1
                end
                assert(position <= size, "unfinished quoted string")
                position, kind = position + 1, "string"
            elseif char == "[" and long_string(position) then
                position, kind = long_string(position), "string"
            elseif char:match("[%a_]") then
                position = position + #source:match("^[%w_]+", position)
                kind = "name"
            elseif char:match("%d") or (char == "." and source:sub(position + 1, position + 1):match("%d")) then
                -- LuaJIT also accepts hexadecimal floats and integer suffixes.
                local number = source:match("^0[xX][%da-fA-F%.]+[pP][%+%-]?%d+[%w_]*", position)
                    or source:match("^0[xX][%da-fA-F]+[%w_]*", position)
                    or source:match("^%d*%.?%d+[eE][%+%-]?%d+[%w_]*", position)
                    or source:match("^%d+%.?%d*[%w_]*", position)
                    or source:match("^%.%d+[%w_]*", position)
                position, kind = position + #number, "number"
            else
                local triple, double = source:sub(position, position + 2), source:sub(position, position + 1)
                position = position + (triple == "..." and 3 or
                    ((double == ".." or double == "==" or double == "~=" or double == "<=" or double == ">=" or double == "::") and 2 or 1))
            end
            result[#result + 1] = {text = source:sub(start, position - 1), start = start, stop = position, kind = kind}
        end
    end
    result[#result + 1] = {text = "<eof>", start = size + 1, stop = size + 1}
    return result
end

function api.Prepare(header, source)
    local list, position, edits = tokens(source), 1, {}
    local function_depth, root_return = 0, false
    local helper = "__ixr_capture"
    while source:find(helper, 1, true) do helper = helper .. "_" end
    local function peek() return list[position].text end
    local function take(value)
        if peek() == value then position = position + 1; return true end
        return false
    end
    local function expect(value)
        if not take(value) then error("script injection parser: expected '" .. value .. "' near '" .. peek() .. "'") end
    end
    local function name()
        local value = peek()
        assert(list[position].kind == "name" and not keywords[value], "script injection parser: expected identifier near '" .. value .. "'")
        position = position + 1
        return value
    end
    local function insert(offset, text) edits[#edits + 1] = {offset, text} end
    local block, expression, body, prefix
    local precedence = { ["or"]=1, ["and"]=2, ["<"]=3, [">"]=3, ["<="]=3, [">="]=3, ["~="]=3, ["=="]=3,
        [".."]=4, ["+"]=5, ["-"]=5, ["*"]=6, ["/"]=6, ["%"]=6, ["^"]=8 }
    local function arguments()
        if take("(") then
            if not take(")") then
                expression(0)
                while take(",") do expression(0) end
                expect(")")
            end
        elseif peek() == "{" then expression(9)
        else assert(list[position].kind == "string", "script injection parser: expected arguments"); position = position + 1 end
    end
    function prefix()
        if take("(") then expression(0); expect(")") else name() end
        while true do
            if take("[") then expression(0); expect("]")
            elseif take(".") then name()
            elseif take(":") then name(); arguments()
            elseif peek() == "(" or peek() == "{" or list[position].kind == "string" then arguments()
            else break end
        end
    end
    function body()
        expect("(")
        if not take(")") then
            if not take("...") then
                name()
                while take(",") do if take("...") then break else name() end end
            end
            expect(")")
        end
        function_depth = function_depth + 1
        block(false); expect("end")
        function_depth = function_depth - 1
    end
    function expression(minimum)
        local token = peek()
        if token == "not" or token == "-" or token == "#" then position = position + 1; expression(7)
        elseif take("function") then body()
        elseif take("{") then
            while not take("}") do
                if take("[") then expression(0); expect("]"); expect("="); expression(0)
                elseif list[position].kind == "name" and list[position + 1].text == "=" then position = position + 2; expression(0)
                else expression(0) end
                if not take(",") and not take(";") then expect("}"); break end
            end
        elseif token == "nil" or token == "true" or token == "false" or token == "..." or list[position].kind == "number" or list[position].kind == "string" then position = position + 1
        else prefix() end
        while precedence[peek()] and precedence[peek()] > minimum do
            local operator = peek()
            position = position + 1
            local power = precedence[operator]
            expression((operator == "^" or operator == "..") and power - 1 or power)
        end
    end
    local function expressions()
        expression(0)
        while take(",") do expression(0) end
    end
    local terminator = {["end"]=true, ["else"]=true, ["elseif"]=true, ["until"]=true, ["<eof>"]=true}
    function block(root)
        while not terminator[peek()] do
            local start = list[position].start
            if take(";") then
            elseif take("local") then
                local names = {}
                if take("function") then names[1] = name(); body()
                else
                    names[1] = name()
                    while take(",") do names[#names + 1] = name() end
                    if take("=") then expressions() end
                end
                if root then
                    local captures = {take(";") and "" or ";"}
                    for _, variable in ipairs(names) do
                        captures[#captures + 1] = helper .. "('" .. variable .. "',function() return " .. variable .. " end);"
                    end
                    insert(list[position - 1].stop, concat(captures))
                end
            elseif take("function") then
                name(); while take(".") do name() end; if take(":") then name() end; body()
            elseif take("if") then
                expression(0); expect("then"); block(false)
                while take("elseif") do expression(0); expect("then"); block(false) end
                if take("else") then block(false) end
                expect("end")
            elseif take("while") then expression(0); expect("do"); block(false); expect("end")
            elseif take("do") then block(false); expect("end")
            elseif take("for") then
                name()
                if take("=") then expression(0); expect(","); expression(0); if take(",") then expression(0) end
                else while take(",") do name() end; expect("in"); expressions() end
                expect("do"); block(false); expect("end")
            elseif take("repeat") then block(false); expect("until"); expression(0)
            elseif take("return") then
                if function_depth == 0 then insert(start, helper .. "();") end
                if root then root_return = true end
                if not terminator[peek()] and peek() ~= ";" then expressions() end
                take(";")
                return
            elseif take("break") then
            elseif take("goto") then name()
            elseif take("::") then name(); expect("::")
            else
                prefix()
                if peek() == "," or peek() == "=" then
                    while take(",") do prefix() end
                    expect("="); expressions()
                end
            end
        end
    end
    block(true); expect("<eof>")
    local output, offset = {}, 1
    for _, edit in ipairs(edits) do
        output[#output + 1] = source:sub(offset, edit[1] - 1)
        output[#output + 1], offset = edit[2], edit[1]
    end
    output[#output + 1] = source:sub(offset)
    -- The factory parameter is outside the script's vararg scope. Captures add
    -- no newlines, so error/debug line numbers continue to refer to the file.
    return "local " .. helper .. "=...;return function(...) " .. header ..
        helper .. "=" .. helper .. "(this);" ..
        helper .. "('this',function() return this end);" ..
        helper .. "('script_name',function() return script_name end);" ..
        concat(output) .. (root_return and "" or "\n" .. helper .. "();") .. "\nend"
end
return api
)IXR_LUA";
} // namespace ScriptInjection
