local intercepts = 
{
	save = {},
	load = {},
	update = {},

	save_state = {},
	load_state = {}
}



function AddIntercept(name, args_map)
	-- FFx0001 retranslate call to signals module is installed
	if ixr_framework and ixr_framework.is_module_loaded("ixr_signals") then
		ixr_framework.get_module("ixr_signals").add_intercept(name, args_map)
	else
		if not editor() then
			SemiLog("! error ixr_signals not initialized:: [SKIP] AddIntercept: (" .. tostring(name) .. ",...)")
		end
	end
end

function RemoveIntercept(name)
	-- FFx0001 retranslate call to signals module is installed
	if ixr_framework and ixr_framework.is_module_loaded("ixr_signals") then
		ixr_framework.get_module("ixr_signals").remove_intercept(name)
	else
		if not editor() then 
			SemiLog("! error ixr_signals not initialized:: [SKIP] RemoveIntercept: (" .. tostring(name) .. ",...)")
		end
	end
end


-- duplicate for dead air and anomaly
function callback_set(name, func_or_userdata)
	RegisterScriptCallback(name, func_or_userdata)
end

function RegisterScriptCallback(name, func_or_userdata)
	-- FFx0001 retranslate call to signals module is installed
	if ixr_framework and ixr_framework.is_module_loaded("ixr_signals") then
		ixr_framework.get_module("ixr_signals").subscribe_to_event(name, func_or_userdata)
		return
	else
		if not editor() then
			SemiLog("! error ixr_signals not initialized:: [SKIP] RegisterScriptCallback: (" .. tostring(name) .. ",...)")
		end
	end
	
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



-- duplicate for dead air and anomaly
function callback_unset(name, func_or_userdata)
	UnregisterScriptCallback(name, func_or_userdata)
end

function UnregisterScriptCallback(name, func_or_userdata)
	-- FFx0001 retranslate call to signals module is installed
	if ixr_framework and ixr_framework.is_module_loaded("ixr_signals") then
		ixr_framework.get_module("ixr_signals").un_subscribe_from_event(name, func_or_userdata)
		return
	else
		if not editor() then
			SemiLog("! error ixr_signals not initialized:: [SKIP] UnregisterScriptCallback: (" .. tostring(name) .. ",...)")
		end
	end
	
	if (intercepts[name]) then
		intercepts[name][func_or_userdata] = nil
	end
end



-- duplicate for dead air and anomaly
function make_callback(name, ...)
	SendScriptCallback(name, ...)
end

function SendScriptCallback(name, ...)
	-- FFx0001 retranslate call to signals module is installed
	if ixr_framework and ixr_framework.is_module_loaded("ixr_signals") then
		ixr_framework.get_module("ixr_signals").send_event(name, ...)
		return
	else
		if not editor() then
			SemiLog("! error ixr_signals not initialized:: [SKIP] SendScriptCallback: (" .. tostring(name) .. ",...)")
		end
	end

	
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