#include "stdafx.h"
#include "appinfo.h"

XRCORE_API CAppInfo g_AppInfo;

bool CAppInfo::IsSecondaryThread() const noexcept
{
	return SecondaryThread == Platform::GetCurrentThread();
}

void CAppInfo::SetMainThread()
{
	MainThread = Platform::GetCurrentThread();
	MainThreadId = Platform::GetThreadId(MainThread);
}

bool CAppInfo::IsPrimaryThread() const noexcept
{
	return MainThreadId != 0 && MainThreadId == Platform::GetThreadId(Platform::GetCurrentThread());
}
