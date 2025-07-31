local intercepts = 
{
	save = {},
	load = {},
	update = {},

	save_state = {},
	load_state = {}
}

function RegisterScriptCallback(name, func_or_userdata)
	if (func_or_userdata == nil) then
		SemiLog("! func_or_userdata == nil")
		callstack()
	end

	if (name == nil) then
		SemiLog("! name == nil")
		callstack()
	end
	if (intercepts == nil) then
		SemiLog("! intercepts == nil")
		callstack()
	end

	if (intercepts[name]) then
		intercepts[name][func_or_userdata] = true
	end
end

function UnregisterScriptCallback(name, func_or_userdata)
	if (intercepts[name]) then
		intercepts[name][func_or_userdata] = nil
	end
end

function SendScriptCallback(name,...)
	if (intercepts[name]) then
		for func_or_userdata,v in pairs(intercepts[name]) do 
			if (type(func_or_userdata) == "function") then 
				func_or_userdata(...)
			elseif (func_or_userdata[name]) then
				func_or_userdata[name](func_or_userdata,...)
			end
		end
	end
end