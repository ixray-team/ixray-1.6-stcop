#include "FFxExtendedEntityesLogic/stdafx.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/Debug/DebugCommands.h"
#include <atomic>

namespace ExtendedEntityesSettings
{
const CInifile* SystemSettings = nullptr;
shared_str GlobalSectionName;
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
	const CInifile* External = EngineExternal().GetIniFile();
	const char* ConfiguredSection = External->read_if_exists<str_c>("gameplay", "DeathOrDieMonstersLogicSectionName", "monsters_logic");
	GlobalSectionName = ConfiguredSection && *ConfiguredSection ? ConfiguredSection : "";
	const bool HasSection = GlobalSectionName.size() && pSettings && pSettings->section_exist(GlobalSectionName.c_str());
	SystemSettings = HasSection ? pSettings : nullptr;
	IsConfiguredEnabled = External->read_if_exists<bool>("gameplay", "UseDeathOrDieMonstersLogic", false) && HasSection;
	if (!HasSection)
	{
		Msg("! [ExtendedEntityesLogic] Missing system.ltx section selected by DeathOrDieMonstersLogicSectionName: [%s]", GlobalSectionName.size() ? GlobalSectionName.c_str() : "<empty>");
	}
	if (SystemSettings)
	{
		CanShareFriends = (SystemSettings)->read_if_exists<bool>(SectionName(), "EnableMonsterFriendlyEnemySharing", true);
		CanDistribute = (SystemSettings)->read_if_exists<bool>(SectionName(), "EnableMonsterDistributedTargeting", true);
		WorkSettings.PerceptionOperations = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedPerceptionOperationsPerFrame", 384u);
		WorkSettings.PerceptionRays = (SystemSettings)->read_if_exists<u32>(SectionName(), "MonsterBehaviorRaysPerFrame", 16u);
		WorkSettings.SpatialQueries = (SystemSettings)->read_if_exists<u32>(SectionName(), "MonsterBehaviorQueriesPerFrame", 2u);
		WorkSettings.SharingSessions = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedSharingSessionsPerFrame", 12u);
		WorkSettings.TargetAssignments = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedTargetAssignmentsPerFrame", 64u);
		WorkSettings.SharingTransfers = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedSharingTransfersPerFrame", 32u);
		WorkSettings.NavigationCaptureNodes = (SystemSettings)->read_if_exists<u32>(SectionName(), "MonsterBehaviorNodesPerFrame", 72u);
		WorkSettings.NavigationSteps = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedNavigationStepsPerFrame", 256u);
		WorkSettings.NavigationSliceMicroseconds = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedNavigationSliceMicroseconds", 1500u);
		WorkSettings.FlightDynamicChecks = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedFlightDynamicChecksPerFrame", 4u);
		WorkSettings.FlightGeometryRays = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedFlightGeometryRaysPerFrame", 350u);
		WorkSettings.NavigationTaskLimit = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedNavigationTaskLimit", 256u);
		WorkSettings.NavigationGraphNodeLimit = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedNavigationGraphNodeLimit", 16384u);
		WorkSettings.GroundCaptureTaskLimit = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedGroundCaptureTaskLimit", 128u);
		WorkSettings.GroundCaptureNodeLimit = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedGroundCaptureNodeLimit", 4096u);
		WorkSettings.GroundCaptureTimeout = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedGroundCaptureTimeoutMs", 20000u);
		WorkSettings.GroundRouteReplanDistance = (SystemSettings)->read_if_exists<float>(SectionName(), "ExtendedGroundRouteReplanDistance", 2.f);
		WorkSettings.GroundWaypointLookahead = (SystemSettings)->read_if_exists<float>(SectionName(), "ExtendedGroundWaypointLookahead", 4.f);
		WorkSettings.GroundRouteRetryDelay = (SystemSettings)->read_if_exists<u32>(SectionName(), "ExtendedGroundRouteRetryDelayMs", 250u);
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
		CanCall = (SystemSettings)->read_if_exists<bool>(SectionName(), "EnableMonsterEnemySharingCallSounds", true);
	}
	IsInitialized = true;
}

// Accessor is intentionally non-initializing: profile reads already hold the settings lock.
const char* CELSharedSettings::SectionName()
{
	return ExtendedEntityesSettings::GlobalSectionName.size() ? ExtendedEntityesSettings::GlobalSectionName.c_str() : "monsters_logic";
}

// Возвращает общий кэш профиля, прочитанного из секции сущности.
const SELPerceptionProfile& CELSharedSettings::Profile(const char* Section)
{
	Initialize();
	xrCriticalSectionGuard Guard(ExtendedEntityesSettings::SettingsLock);
	xr_map<shared_str, SELPerceptionProfile>::iterator ProfileIterator = ExtendedEntityesSettings::Profiles.find(Section);
	if (ProfileIterator == ExtendedEntityesSettings::Profiles.end())
	{
		ProfileIterator = ExtendedEntityesSettings::Profiles.emplace(shared_str(Section), CELSharedPerceptionProfiles::Read(Section, ExtendedEntityesSettings::SystemSettings)).first;
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
	ExtendedEntityesSettings::SystemSettings = nullptr;
	ExtendedEntityesSettings::GlobalSectionName = nullptr;
	ExtendedEntityesSettings::WorkSettings = SELSharedWorkSettings();
	ExtendedEntityesSettings::CanShareFriends = true;
	ExtendedEntityesSettings::CanDistribute = true;
	ExtendedEntityesSettings::CanCall = true;
	ExtendedEntityesSettings::IsInitialized = false;
	ExtendedEntityesSettings::IsConfiguredEnabled = false;
}
