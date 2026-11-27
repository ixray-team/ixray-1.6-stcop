#pragma once
#include "../Kernel/xrSyncronize.h"

#ifdef IXR_WINDOWS
extern XRCORE_API xrCriticalSection g_ThreadStackSymbolLock;

namespace StackTrace
{
	XRCORE_API std::vector<std::string> BuildStackTrace(PCONTEXT threadCtx, u16 maxFramesCount);
}
#endif
