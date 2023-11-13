#include "stdafx.h"
#include "appinfo.h"

XRCORE_API CAppInfo g_AppInfo;

bool CAppInfo::IsSecondaryThread() const noexcept
{
	return SecondaryThread == GetCurrentThread();
}

bool CAppInfo::IsPrimaryThread() const noexcept
{
	return MainThread == GetCurrentThread();
}
