#pragma once

#include "WristwatchTypes.h"

class CWristwatchSurgeProvider
{
public:
	SWristwatchSurgeState QueryState();
	void OnWatchesActive(bool replaceSurgeNotifications);
	void OnWatchesInactive();

private:
	bool TryRefreshSurgeState();
	void TrySuppressVanillaNotifications();

	bool _watchSessionActive = false;
	bool _replaceSurgeNotifications = false;
	bool _notificationHooksInstalled = false;
	u32 _nextSurgeRefreshMs = 0;
	u32 _nextSuppressMs = 0;

	static constexpr u32 kRefreshIntervalMs = 1000;
};
