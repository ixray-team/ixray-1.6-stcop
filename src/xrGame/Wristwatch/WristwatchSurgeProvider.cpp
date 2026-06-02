#include "StdAfx.h"
#include "WristwatchSurgeProvider.h"
#include "WristwatchSurgeLuaBridge.h"

#include "../Level.h"
#include "../../xrEngine/device.h"
#include "../../xrEngine/WristwatchSettings.h"

bool CWristwatchSurgeProvider::TryRefreshSurgeState()
{
	if (!g_pGameLevel || !Level().game)
	{
		return false;
	}

	const u32 now = Device.dwTimeContinual;
	if (now < _nextSurgeRefreshMs)
	{
		return true;
	}

	_nextSurgeRefreshMs = now + kRefreshIntervalMs;
	return WristwatchSurgeLuaBridge::RefreshSurgeState();
}

void CWristwatchSurgeProvider::TrySuppressVanillaNotifications()
{
	if (!_replaceSurgeNotifications)
	{
		return;
	}

	const u32 now = Device.dwTimeContinual;
	if (now < _nextSuppressMs)
	{
		return;
	}

	_nextSuppressMs = now + kRefreshIntervalMs;
	WristwatchSurgeLuaBridge::SuppressVanillaNotifications();
}

SWristwatchSurgeState CWristwatchSurgeProvider::QueryState()
{
	if (_watchSessionActive)
	{
		TryRefreshSurgeState();
		TrySuppressVanillaNotifications();
	}

	return GetWristwatchSurgeState();
}

void CWristwatchSurgeProvider::OnWatchesActive(const bool replaceSurgeNotifications)
{
	_replaceSurgeNotifications = replaceSurgeNotifications;

	if (_watchSessionActive)
	{
		return;
	}

	_watchSessionActive = true;
	_nextSurgeRefreshMs = 0;
	_nextSuppressMs = 0;

	SetWristwatchHudSessionActive(true);

	if (_replaceSurgeNotifications && !_notificationHooksInstalled)
	{
		WristwatchSurgeLuaBridge::TryInstallNotificationHooks();
		_notificationHooksInstalled = true;
	}

	TryRefreshSurgeState();
	if (_replaceSurgeNotifications)
	{
		_nextSuppressMs = 0;
		TrySuppressVanillaNotifications();
	}
}

void CWristwatchSurgeProvider::OnWatchesInactive()
{
	if (!_watchSessionActive)
	{
		return;
	}

	_watchSessionActive = false;
	_replaceSurgeNotifications = false;

	SetWristwatchHudSessionActive(false);
	SetWristwatchSurgeState(0, 0, 0);
}
