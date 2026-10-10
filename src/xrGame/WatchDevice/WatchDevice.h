#pragma once

#include "WatchTypes.h"
#include "WatchUI.h"
#include <luabind/functor.hpp>

class IKinematics;
class CBoneInstance;
class CAnomalyZone;

struct SWatchLedRuntime
{
	Fmatrix Offset = Fidentity;
	float BlinkPhase = 0.0f;
	float Value = 0.0f;
	u16 Bone = u16(-1);
	u16 HiddenBone = u16(-1);
	ref_light Light = nullptr;
};

struct SWatchDisplayRuntime
{
	float Timer = 0.0f;
	float Hz = 0.0f;
	float Delay = 0.0f;
	float Freeze = 0.0f;
	float FreezeChance = 0.0f;
	float GlitchPhase = 0.0f;
	float Intensity = 1.0f;
	u32 Tick = 0;
	xr_vector<SWatchFormatToken> TimeTokens;
	xr_vector<SWatchFormatToken> DateTokens;
};

struct SWatchSurgeRuntime
{
	float Timer = 0.0f;
	float Seconds = -1.0f;
	float Progress = -1.0f;
	float Lag = 0.0f;
	float AnomalyLag = 0.0f;
	bool Valid = false;
	bool BridgeMissing = false;
	bool BridgeResolved = false;
	luabind::functor<float> SecondsFunction;
	luabind::functor<float> ProgressFunction;
};

struct SWatchCompassRuntime
{
	float Angle = 0.0f;
	float Desired = 0.0f;
	float Velocity = 0.0f;
	float ChaosTime = 0.0f;
	bool SpringActive = false;
};

struct SWatchZoneRuntime
{
	float SampleTimer = 0.0f;
	float Gravity = 0.0f;
	float AnomalyTarget = 0.0f;
	xr_vector<ISpatialShared> Spatial;
	xr_vector<xr_string> AnomalyExclude;
	xr_hash_map<shared_str, bool> GravitySections;
};

struct SWatchLightRuntime
{
	float Timer = 0.0f;
	Fmatrix Offset = Fidentity;
	xr_vector<SWatchLightKey> Keys;
};

struct SWatchConditionRuntime
{
	Fmatrix IconOffset = Fidentity;
	Fmatrix BarOffset = Fidentity;
};

class CWatchDevice
{
public:
	CWatchDevice() = default;
	CWatchDevice(const CWatchDevice&) = delete;
	CWatchDevice& operator=(const CWatchDevice&) = delete;
	~CWatchDevice();

	void Load(const shared_str& RootSection);
	void BindModel(IKinematics* NewModel);
	void Unbind();
	void Update(float Dt);
	bool RenderUIQuery() const;
	void RenderUI(const Fmatrix& WatchesXform);
	void SyncHudLights(const Fmatrix& WatchesXform);
	void TurnOffHudLights();

	void DrawImGui();
	void HotSaveChanged();
	void HotSaveAll();
	void Revert();
	void Reload();

	bool IsEnabled() const { return Loaded && Config.Root.Enabled; }
	bool BonesOk() const;

private:
	void LoadConfig(const CInifile& Ini, const shared_str& Root, bool PersistentOnly);
	void ApplyIndicatorMasters();
	void ApplyConditionMasters();
	bool ResolveOverridePath(string_path& Out) const;
	void ApplyOverrideFromDisk();
	bool IsAnyDirty();
	void FormatDirtySections(string1024& Out);
	void WriteOverride(bool ChangedOnly, const char* Label);

	void DrawCompassImGui();
	void DrawDisplayImGui();
	void DrawBarometerImGui();
	void DrawLagImGui(const char* Id, const char* Title, SWatchLag& Lag, float LagValue);
	void DrawLightImGui();
	void DrawChannelImGui(EWatchLedChannel Channel);
	void DrawConditionImGui(EWatchCondition Condition);
	void DrawPreviewImGui();
	void DrawHotSaveImGui();

	void ResetRuntime();
	void CreateUI();
	void UpdateUILayout();
	void UpdateDisplayFormats();
	void UpdateDisplay(float Dt);
	void UpdateDisplayLag(float Dt);
	bool SampleSurge(float& SecondsToSurge, float& Progress);
	void UpdateSurge(float Dt);
	void UpdateBarometer();
	void SetBarometerText(const char* Text);
	void UpdateLightSchedule();
	void UpdateLight(float Dt);

	void DestroyHudLights();
	void CreateHudPointLight(ref_light& Light, float Range);
	void DestroyHudPointLight(ref_light& Light);
	void TurnOffHudPointLight(ref_light& Light);
	void SyncChannelLight(EWatchLedChannel Channel, const Fmatrix& WatchesXform);
	bool MakeLedXform(const SWatchLedRuntime& Led, const Fmatrix& WatchesXform, Fmatrix& Out) const;

	void UpdateAnomalyExclude();
	bool IsChannelActive(EWatchLedChannel Channel) const;
	float SampleChannel(EWatchLedChannel Channel) const;
	void UpdateChannel(EWatchLedChannel Channel, float Dt);
	bool IsConditionAvailable(EWatchCondition Condition) const;
	bool IsConditionActive(EWatchCondition Condition) const;
	float SampleCondition(EWatchCondition Condition) const;
	void UpdateCondition(EWatchCondition Condition, float Dt);
	u32 ConditionDrawColor(EWatchCondition Condition, u32 Fallback) const;
	float ConditionDrawGlow(EWatchCondition Condition) const;
	void SampleZones(float Dt);
	bool SectionLooksLikeGravity(const shared_str& Section);
	float EvaluateGravityZoneIntensity(CAnomalyZone* Zone, const Fvector& ActorPos);

	void RenderBoneGlow(EWatchGlow Id, u16 Bone, const Fmatrix& Offset, const Fvector2& Size, float Intensity, const Fmatrix& WatchesXform);
	void RenderChannel(EWatchLedChannel Channel, const Fmatrix& WatchesXform);
	void RenderCondition(EWatchCondition Condition, const Fmatrix& DisplayXform);

	void ResolveBones();
	void RestoreLedMesh(SWatchLedRuntime& Led);
	void UpdateLedMeshes();
	void ClearBones();
	void SetBoneCallbacks();
	void ResetBoneCallbacks();
	void AttachModel();

	void ResetCompassRuntime();
	void UpdateCompass(float Dt);
	void UpdateCompassTarget();
	void UpdateCompassNorth();
	void ApplyCompassNoTarget();
	void SetCompassVisible(bool Visible);
	bool HasCompassBone() const;
	bool WorldDirToCompassAngle(const Fvector& WorldDir, float& OutAngle) const;
	bool ApplyCompassWorldDir(const Fvector& WorldDir);
	void CommitCompassAngle(float Angle, bool ResetVelocity);
	void SyncCompassState();
	bool UpdateCompassGravityChaos(float Dt);
	void UpdateCompassSpring(float Dt);

	static void BoneCallbackCompass(CBoneInstance* Bone);

	u16 ResolveBoneId(const shared_str& Name) const;

	shared_str Section;
	SWatchConfig Config;
	SWatchConfig ConfigBaseline;
	SWatchState State;
	SWatchDebugPreview Preview;
	SWatchBoneIds BoneIds;
	IKinematics* Model = nullptr;
	xr_unique_ptr<CUIWatchWnd> Ui;
	Fmatrix UiOffset = Fidentity;
	SWatchDisplayRuntime Display;
	SWatchSurgeRuntime Surge;
	SWatchCompassRuntime Compass;
	SWatchZoneRuntime Zones;
	SWatchLightRuntime Light;
	SWatchLedRuntime Leds[WatchLedChannelCount];
	SWatchConditionRuntime ConditionRuntime[WatchConditionCount];
	bool Loaded = false;
	bool CallbacksBound = false;
	string_path OverridePath = {};
	string_path LastHotSaveStatus = {};
};
