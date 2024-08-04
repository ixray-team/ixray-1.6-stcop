#pragma once
#include <mutex>

namespace StackTrace
{
	static std::mutex look_function;
	static bool symEngineInitialized = false;

	using CrashHandler = void(*)();
	using DialogHandler = void(*)(bool);
	using OutOfMemoryCallbackFunc = void(*)();

#ifdef IXR_WIN64
#	define MACHINE_TYPE IMAGE_FILE_MACHINE_AMD64
#else
#	define MACHINE_TYPE IMAGE_FILE_MACHINE_I386
#endif

	bool GetNextStackFrameString(LPSTACKFRAME stackFrame, PCONTEXT threadCtx, std::string& frameStr)
	{
		BOOL result = StackWalk(MACHINE_TYPE, GetCurrentProcess(), GetCurrentThread(), stackFrame, threadCtx, nullptr,
			SymFunctionTableAccess, SymGetModuleBase, nullptr);

		if (result == FALSE || stackFrame->AddrPC.Offset == 0)
		{
			return false;
		}

		frameStr.clear();

		// Module name:
		std::string moduleName = Platform::GetModuleNameForAddress(stackFrame->AddrPC.Offset);
		if (!moduleName.empty())
		{
			frameStr.append(moduleName);
		}

		//-' Address:
		string512 formatBuff;
		xr_sprintf(formatBuff, _countof(formatBuff), " at %p", stackFrame->AddrPC.Offset);
		frameStr.append(formatBuff);

		//-' Function info:
		u8 arrSymBuffer[512];
		ZeroMemory(arrSymBuffer, sizeof(arrSymBuffer));
		PIMAGEHLP_SYMBOL functionInfo = reinterpret_cast<PIMAGEHLP_SYMBOL>(arrSymBuffer);
		functionInfo->SizeOfStruct = sizeof(*functionInfo);
		functionInfo->MaxNameLength = sizeof(arrSymBuffer) - sizeof(*functionInfo) + 1;
		DWORD_PTR dwFunctionOffset;

		result = SymGetSymFromAddr(GetCurrentProcess(), stackFrame->AddrPC.Offset, &dwFunctionOffset, functionInfo);

		if (result)
		{
			if (dwFunctionOffset)
			{
				xr_sprintf(formatBuff, _countof(formatBuff), " %s() + %Iu byte(s)", functionInfo->Name, dwFunctionOffset);
			}
			else
			{
				xr_sprintf(formatBuff, _countof(formatBuff), " %s()", functionInfo->Name);
			}
			frameStr.append(formatBuff);
		}

		//-' Source info:
		DWORD dwLineOffset;
		IMAGEHLP_LINE sourceInfo = {};
		sourceInfo.SizeOfStruct = sizeof(sourceInfo);

		result = SymGetLineFromAddr(GetCurrentProcess(), stackFrame->AddrPC.Offset, &dwLineOffset, &sourceInfo);

		if (result)
		{
			if (dwLineOffset)
			{
				xr_sprintf(formatBuff, _countof(formatBuff), " in %s line %u + %u byte(s)", sourceInfo.FileName,
					sourceInfo.LineNumber, dwLineOffset);
			}
			else
			{
				xr_sprintf(formatBuff, _countof(formatBuff), " in %s line %u", sourceInfo.FileName, sourceInfo.LineNumber);
			}
			frameStr.append(formatBuff);
		}

		return true;
	}

	bool InitializeSymbolEngine()
	{
		if (!symEngineInitialized)
		{
			u32 dwOptions = SymGetOptions();
			SymSetOptions(dwOptions | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES | SYMOPT_UNDNAME);

			if (SymInitialize(GetCurrentProcess(), nullptr, TRUE))
			{
				symEngineInitialized = true;
			}
		}

		return symEngineInitialized;
	}

	void DeinitializeSymbolEngine(void)
	{
		if (symEngineInitialized)
		{
			SymCleanup(GetCurrentProcess());

			symEngineInitialized = false;
		}
	}

	std::vector<std::string> BuildStackTrace(PCONTEXT threadCtx, u16 maxFramesCount)
	{
		std::lock_guard<std::mutex> lock(look_function);

		//SStringVec traceResult;
		std::vector<std::string> traceResult;
		STACKFRAME stackFrame = {};
		std::string frameStr;

		if (!InitializeSymbolEngine())
		{
			Msg("[xrDebug::BuildStackTrace]InitializeSymbolEngine failed with error: %d", GetLastError());
			traceResult.push_back("NULL");
			return traceResult;
		}

		traceResult.reserve(maxFramesCount);

		stackFrame.AddrPC.Mode = AddrModeFlat;
		stackFrame.AddrStack.Mode = AddrModeFlat;
		stackFrame.AddrFrame.Mode = AddrModeFlat;

#if defined(IXR_WIN64) && !defined(IXR_ARM64)
		stackFrame.AddrPC.Offset = threadCtx->Rip;
		stackFrame.AddrStack.Offset = threadCtx->Rsp;
		stackFrame.AddrFrame.Offset = threadCtx->Rbp;
#elif defined(IXR_ARM64)
		stackFrame.AddrPC.Offset = threadCtx->Pc;
		stackFrame.AddrStack.Offset = threadCtx->Sp;
		stackFrame.AddrFrame.Offset = threadCtx->Fp;
#else
		stackFrame.AddrPC.Offset = threadCtx->Eip;
		stackFrame.AddrStack.Offset = threadCtx->Esp;
		stackFrame.AddrFrame.Offset = threadCtx->Ebp;
#endif

		while (GetNextStackFrameString(&stackFrame, threadCtx, frameStr) && traceResult.size() <= maxFramesCount)
		{
			traceResult.push_back(frameStr);
		}

		DeinitializeSymbolEngine();

		return traceResult;
	}
}