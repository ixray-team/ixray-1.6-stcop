#pragma once

namespace WristwatchSurgeLuaBridge
{
void EnsureSurgeManagerReady();
bool RefreshSurgeState();
void SuppressVanillaNotifications();
void TryInstallNotificationHooks();
}
