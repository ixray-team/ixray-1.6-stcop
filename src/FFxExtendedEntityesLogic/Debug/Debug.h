#pragma once
#include <atomic>
#include "xrCore/FTimer.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELBehavior.h"

class CEntityAlive;

struct SELDebugFrame
{
	u32 OwnerCount = 0;
	u32 FlyingOnline = 0;
	u32 OtherMonstersOnline = 0;
	u32 WorkerRequests = 0;
	u32 GraphCaptures = 0;
	u32 GeometryChecks = 0;
	u32 WorldGeneration = 0;
	u32 LastWorkerMicroseconds = 0;
	bool IsLogicEnabled = false;
	bool IgnoresActor = false;
};

struct SELDebugRecord
{
	u16 OwnerIdentifier = u16(-1);
	Fvector Position = {0.f, 0.f, 0.f};
	Fvector GoalPosition = {0.f, 0.f, 0.f};
	u32 ExpiresAt = 0;
	float Radius = .5f;
	float Speed = 0.f;
	float ScentRadius = 0.f;
	bool HasVitalBars = false;
	bool HasCharacterBars = false;
	Fvector BarsPosition = {0.f, 0.f, 0.f};
	float HealthPercent = 100.f;
	float CouragePercent = 0.f;
	float AggressionPercent = 0.f;
	float BaseCouragePercent = 0.f;
	float BaseAggressionPercent = 0.f;
	bool IsAfraidOfActor = false;
	bool IsCounterAttacking = false;
	bool IsBerserk = false;
	Fvector StateMarkerPosition = {0.f, 0.f, 0.f};
	u16 PsiGuideIdentifier = u16(-1);
	Fvector PsiGuidePosition = {0.f, 0.f, 0.f};
	u16 TargetIdentifier = u16(-1);
	u32 KnownTargets = 0;
	EELBehaviorState Validation = EELBehaviorState::Running;
	bool IsAttacking = false;
	bool IsEvading = false;
	Fobb Bounds = {};
	bool IgnoresFoliage = false;
	shared_str State;
	shared_str Mode;
	xr_vector<shared_str> Senses;
};

enum class EELDebugEvent : u8
{
	Sound,
	SoundEmission,
	SoundReception,
	Interest,
	Fear,
	Sharing,
	PsiSharing,
	Call,
	Trail,
	Search,
	Route,
	Hazard,
	Anomaly,
	Damage,
	Light
};
struct SELDebugEvent
{
	EELDebugEvent Kind = EELDebugEvent::Sound;
	u16 OwnerIdentifier = u16(-1);
	u16 TargetIdentifier = u16(-1);
	Fvector Start = {0.f, 0.f, 0.f};
	Fvector Finish = {0.f, 0.f, 0.f};
	xr_vector<Fvector> Points;
	xr_vector<u16> Attackers;
	float Radius = 0.f;
	u32 ExpiresAt = 0;
	shared_str Label;
	Fobb Bounds = {};
	u32 TargetGeneration = 0;
	u32 StimulusType = 0;
};
enum class EELDebugGroup : u8
{
	Ground,
	Flying,
	Shared,
	Count
};
enum class EELDebugStage : u8
{
	Perception,
	Behavior,
	Movement,
	Hearing,
	Sharing,
	World,
	Worker,
	Graph,
	Geometry,
	Count
};
struct SELDebugTiming
{
	float FrameMilliseconds = 0.f;
	float AverageMilliseconds = 0.f;
	float PeakMilliseconds = 0.f;
};
struct SELDebugPerformance
{
	SELDebugTiming Groups[u32(EELDebugGroup::Count)];
	SELDebugTiming Stages[u32(EELDebugGroup::Count)][u32(EELDebugStage::Count)];
	SELDebugTiming Main;
	SELDebugTiming Total;
	SELDebugTiming Worker;
};
class CELSharedDebugScope final
{
public:
	CELSharedDebugScope(EELDebugGroup Group, EELDebugStage Stage);
	~CELSharedDebugScope();
	CELSharedDebugScope(const CELSharedDebugScope&) = delete;
	CELSharedDebugScope& operator=(const CELSharedDebugScope&) = delete;

private:
	xr_optional<CTimer> Timer;
	CELSharedDebugScope* Parent = nullptr;
	u64 ChildrenMicroseconds = 0;
	u32 FrameIdentifier = 0;
	EELDebugGroup Group;
	EELDebugStage Stage;
	bool IsActive = false;
};

class CELSharedDebug final
{
public:
	static CELSharedDebug& Instance();
	void SetDrawEnabled(bool IsGeometryEnabled, bool IsTextEnabled);
	void SetStatsEnabled(bool IsEnabled);
	void SetCombatLog(bool Enabled);
	void LogCombat(u16 OwnerIdentifier, const char* Format, ...);
	bool CanLogCombat(const Fvector& Position) const;
	void SetDetails(u32 Mask) { Details = Mask; }
	bool Detail(u32 Index) const { return (Details.load() & (1u << Index)) != 0; }
	bool DrawDetail(const char* Label) const;
	bool DrawEvent(EELDebugEvent Kind) const;
	bool IsEnabled() const;
	bool IsDrawEnabled() const;
	bool IsStatsEnabled() const;
	void PublishEvent(SELDebugEvent Event, u32 Lifetime = 1500);
	void RecordHit(CEntityAlive& Target, u16 AttackerIdentifier, float PreviousHealth);
	void UpdateHits();
	void RecordTiming(u32 FrameIdentifier, EELDebugGroup Group, EELDebugStage Stage, u64 Microseconds);
	void Publish(SELDebugRecord Record);
	void PublishFrame(const SELDebugFrame& Frame);
	void Draw();
	void Clear();
	void ClearWorld();

private:
	xrCriticalSection QueueLock;
	struct SCombatTime
	{
		u32 Generation = 0;
		u32 Time = 0;
	};
	xr_map<u16, SCombatTime> CombatTimes;
	xr_vector<SELDebugRecord> Records;
	xr_vector<SELDebugEvent> Events;

	struct SFrameTiming
	{
		u32 FrameIdentifier = u32(-1);
		u64 Microseconds[u32(EELDebugGroup::Count)][u32(EELDebugStage::Count)] = {};
	};
	static constexpr u32 TimingFrameCount = 120;
	SFrameTiming Timings[TimingFrameCount];
	u32 TimingStartedAt = u32(-1);
	SELDebugPerformance Performance;
	void UpdatePerformance();
	SELDebugFrame Frame;
	bool HasGeometry = false;
	bool HasText = false;
	std::atomic<bool> HasDraw{false};
	std::atomic<bool> HasCombatLog{false};
	std::atomic<u32> Details{0};
	std::atomic<bool> HasStats{false};
};
