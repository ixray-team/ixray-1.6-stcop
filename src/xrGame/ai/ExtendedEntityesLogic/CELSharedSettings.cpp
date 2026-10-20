#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"
#include "ai/ExtendedEntityesLogic/Debug/DebugCommands.h"
#include <atomic>

namespace ExtendedEntityesSettings
{
xr_unique_ptr<CInifile> SystemSettings;
xr_map<shared_str, SELPerceptionProfile> Profiles;
std::atomic<int> OverrideValue{-1};
std::atomic<u32> ControlRevision{1};
xrCriticalSection SettingsLock;
bool IsInitialized = false;
bool IsConfiguredEnabled = false;
bool CanShareFriends = true;
bool CanDistribute = true;
bool CanCall = true;
SELSharedWorkSettings WorkSettings;
} // namespace ExtendedEntityesSettings

// Загружает настройки и подготавливает состояние перед использованием.
void CELSharedSettings::Initialize()
{
	using namespace ExtendedEntityesSettings;
	xrCriticalSectionGuard Guard(SettingsLock);
	if (IsInitialized)
	{
		return;
	}
	CELSharedDebugCommands::Synchronize();
	string_path FileName;
	FS.update_path(FileName, _game_config_, "new_monsters_logic.ltx");
	if (FS.exist(FileName))
	{
		SystemSettings = xr_make_unique<CInifile>(FileName, true);
		IsConfiguredEnabled = (EngineExternal().GetIniFile())->read_if_exists<bool>("gameplay", "UseDeathOrDieMonstersLogic", false);
		CanShareFriends = (SystemSettings.get())->read_if_exists<bool>("monsters_logic", "EnableMonsterFriendlyEnemySharing", true);
		CanDistribute = (SystemSettings.get())->read_if_exists<bool>("monsters_logic", "EnableMonsterDistributedTargeting", true);
		WorkSettings.PerceptionOperations = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedPerceptionOperationsPerFrame", 384u);
		WorkSettings.PerceptionRays = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "MonsterBehaviorRaysPerFrame", 16u);
		WorkSettings.SpatialQueries = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "MonsterBehaviorQueriesPerFrame", 2u);
		WorkSettings.SharingSessions = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedSharingSessionsPerFrame", 12u);
		WorkSettings.TargetAssignments = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedTargetAssignmentsPerFrame", 64u);
		WorkSettings.SharingTransfers = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedSharingTransfersPerFrame", 32u);
		WorkSettings.NavigationCaptureNodes = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "MonsterBehaviorNodesPerFrame", 72u);
		WorkSettings.NavigationSteps = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedNavigationStepsPerFrame", 256u);
		WorkSettings.NavigationSliceMicroseconds = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedNavigationSliceMicroseconds", 1500u);
		WorkSettings.FlightDynamicChecks = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedFlightDynamicChecksPerFrame", 4u);
		WorkSettings.FlightGeometryRays = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedFlightGeometryRaysPerFrame", 350u);
		WorkSettings.NavigationTaskLimit = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedNavigationTaskLimit", 256u);
		WorkSettings.NavigationGraphNodeLimit = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedNavigationGraphNodeLimit", 16384u);
		WorkSettings.GroundCaptureTaskLimit = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedGroundCaptureTaskLimit", 128u);
		WorkSettings.GroundCaptureNodeLimit = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedGroundCaptureNodeLimit", 4096u);
		WorkSettings.GroundCaptureTimeout = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedGroundCaptureTimeoutMs", 20000u);
		WorkSettings.GroundRouteReplanDistance = (SystemSettings.get())->read_if_exists<float>("monsters_logic", "ExtendedGroundRouteReplanDistance", 2.f);
		WorkSettings.GroundWaypointLookahead = (SystemSettings.get())->read_if_exists<float>("monsters_logic", "ExtendedGroundWaypointLookahead", 4.f);
		WorkSettings.GroundRouteRetryDelay = (SystemSettings.get())->read_if_exists<u32>("monsters_logic", "ExtendedGroundRouteRetryDelayMs", 250u);
		if (!_valid(WorkSettings.GroundRouteReplanDistance))
		{
			WorkSettings.GroundRouteReplanDistance = 2.f;
		}
		if (!_valid(WorkSettings.GroundWaypointLookahead))
		{
			WorkSettings.GroundWaypointLookahead = 4.f;
		}
		clamp(WorkSettings.GroundRouteReplanDistance, .5f, 20.f);
		clamp(WorkSettings.GroundWaypointLookahead, 1.f, 12.f);
		clamp(WorkSettings.GroundRouteRetryDelay, 50u, 2000u);
		clamp(WorkSettings.NavigationTaskLimit, 16u, 4096u);
		clamp(WorkSettings.NavigationGraphNodeLimit, 64u, 65536u);
		clamp(WorkSettings.GroundCaptureTaskLimit, 16u, WorkSettings.NavigationTaskLimit);
		clamp(WorkSettings.GroundCaptureNodeLimit, 16u, WorkSettings.NavigationGraphNodeLimit);
		clamp(WorkSettings.GroundCaptureTimeout, 1000u, 120000u);
		clamp(WorkSettings.PerceptionOperations, 16u, 4096u);
		clamp(WorkSettings.PerceptionRays, 1u, 64u);
		clamp(WorkSettings.SpatialQueries, 1u, 8u);
		clamp(WorkSettings.SharingSessions, 1u, 64u);
		clamp(WorkSettings.TargetAssignments, 1u, 1024u);
		clamp(WorkSettings.SharingTransfers, 1u, 256u);
		clamp(WorkSettings.NavigationCaptureNodes, 8u, 256u);
		clamp(WorkSettings.NavigationSteps, 16u, 2048u);
		clamp(WorkSettings.NavigationSliceMicroseconds, 100u, 5000u);
		clamp(WorkSettings.FlightGeometryRays, 16u, 512u);
		clamp(WorkSettings.FlightDynamicChecks, 1u, std::max(1u, WorkSettings.FlightGeometryRays / 18u));
		CanCall = (SystemSettings.get())->read_if_exists<bool>("monsters_logic", "EnableMonsterEnemySharingCallSounds", true);
	}
	IsInitialized = true;
}

// Возвращает общий кэш профиля, прочитанного из секции сущности.
const SELPerceptionProfile& CELSharedSettings::Profile(const char* Section)
{
	Initialize();
	xrCriticalSectionGuard Guard(ExtendedEntityesSettings::SettingsLock);
	xr_map<shared_str, SELPerceptionProfile>::iterator ProfileIterator = ExtendedEntityesSettings::Profiles.find(Section);
	if (ProfileIterator == ExtendedEntityesSettings::Profiles.end())
	{
		ProfileIterator = ExtendedEntityesSettings::Profiles.emplace(shared_str(Section), CELSharedPerceptionProfiles::Read(Section, ExtendedEntityesSettings::SystemSettings.get())).first;
	}
	return ProfileIterator->second;
}

// Возвращает общие ограничения порционных расчётов.
const SELSharedWorkSettings& CELSharedSettings::Work()
{
	return ExtendedEntityesSettings::WorkSettings;
}

// Сообщает, включена ли соответствующая подсистема.
bool CELSharedSettings::IsEnabled()
{
	const int OverrideValue = ExtendedEntityesSettings::OverrideValue.load(std::memory_order_relaxed);
	return OverrideValue < 0 ? ExtendedEntityesSettings::IsConfiguredEnabled : OverrideValue != 0;
}

// Запоминает консольный режим и меняет ревизию управления.
void CELSharedSettings::SetEnabled(int OverrideValue)
{
	clamp(OverrideValue, -1, 1);
	ExtendedEntityesSettings::OverrideValue.store(OverrideValue, std::memory_order_relaxed);
	ExtendedEntityesSettings::ControlRevision.fetch_add(1, std::memory_order_relaxed);
}

// Возвращает ревизию, по которой компоненты сбрасывают старое управление.
u32 CELSharedSettings::Revision()
{
	return ExtendedEntityesSettings::ControlRevision.load(std::memory_order_relaxed);
}

// Возвращает выбранный консольный режим: конфиг, выключено или включено.
int CELSharedSettings::GetOverride()
{
	return ExtendedEntityesSettings::OverrideValue.load(std::memory_order_relaxed);
}
// Возвращает разрешение обмениваться сведениями с дружественными видами.
bool CELSharedSettings::CanShareWithFriends()
{
	return ExtendedEntityesSettings::CanShareFriends;
}
// Возвращает разрешение распределять противников между союзниками.
bool CELSharedSettings::CanDistributeTargets()
{
	return ExtendedEntityesSettings::CanDistribute;
}
// Возвращает разрешение звуковых кличей при передаче сведений.
bool CELSharedSettings::CanPlayCalls()
{
	return ExtendedEntityesSettings::CanCall;
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedSettings::Clear()
{
	ExtendedEntityesSettings::Profiles.clear();
	ExtendedEntityesSettings::SystemSettings.reset();
	ExtendedEntityesSettings::WorkSettings = SELSharedWorkSettings();
	ExtendedEntityesSettings::IsInitialized = false;
	ExtendedEntityesSettings::IsConfiguredEnabled = false;
}
