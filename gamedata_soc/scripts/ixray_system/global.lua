--// General 
jit.opt.start(2)

string.gfind = string.gmatch
math.mod = math.fmod

--// LuaPandas
DebuggerMode = false

function debug_jit_off()
	if DebuggerMode then
		if jit then jit.off() end
	end
end

function debug_jit_on()
	if DebuggerMode then
		if jit then jit.on() end
	end
end

function debugger_attach() 
	if DebuggerMode then
		debug_jit_off()
		LuaPanda.reConnect()
		debug_jit_on()
	else
		debug_jit_off()
		SemiLog('LuaPanda starting...')
		LuaPanda.start("127.0.0.1", 8818)
		DebuggerMode = true
		debug_jit_on()
	end
end