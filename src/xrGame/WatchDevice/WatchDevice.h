#pragma once

#include "WatchTypes.h"
#include "WatchUI.h"
#include "WatchEnvironment.h"
#include "WatchRig.h"

class IKinematics;

struct SWatchLedRuntime
{
	Fmatrix Offset = Fidentity;
	Fmatrix Anchor = Fidentity;
	shared_str SourceName;
	SWatchSource Source;
	bool Enabled = false;
	bool AnchorValid = false;
	bool Preview = false;
	float PreviewValue = 0.0f;
	float Intensity = 0.0f;
	float BlinkPhase = 0.0f;
	float Value = 0.0f;
	ref_light Light = nullptr;
};

struct SWatchGaugeRuntime
{
	Fmatrix IconOffset = Fidentity;
	Fmatrix BarOffset = Fidentity;
	shared_str SourceName;
	SWatchSource Source;
	bool Enabled = false;
	bool Preview = false;
	float PreviewValue = 0.0f;
	float Value = 0.0f;
	float DisplayValue = 0.0f;
	float Badness = 0.0f;
	bool Visible = false;
	EWatchConditionSeverity Severity = EWatchConditionSeverity::None;
	u32 Colors[WatchSeverityCount] = {};
	bool HasColor[WatchSeverityCount] = {};
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
	xr_vector<xr_string> AnomalyExclude;
};

struct SWatchLightRuntime
{
	float Timer = 0.0f;
	Fmatrix Offset = Fidentity;
	xr_vector<SWatchLightKey> Keys;
};

struct SWatchAnchors
{
	u32 Frame = u32(-1);
	bool DisplayValid = false;
	bool CompassValid = false;
	Fmatrix Display = Fidentity;
	Fmatrix Compass = Fidentity;
};

class CWatchDevice
{
public:
	CWatchDevice();
	CWatchDevice(const CWatchDevice&) = delete;
	CWatchDevice& operator=(const CWatchDevice&) = delete;
	~CWatchDevice();

	void SetEnvironment(xr_unique_ptr<IWatchEnvironment> NewEnvironment);

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
	bool BonesOk() const { return Rig.BonesOk(); }

private:
	void LoadConfig(const CInifile& Ini, const shared_str& Root, bool PersistentOnly);
	void LoadElementLists(const CInifile& Ini, const shared_str& Root);
	void BuildElements();
	void ApplyConfigLayers();
	void CompileConfig(bool Report);
	SWatchSource ResolveSource(const shared_str& Name, bool Report);
	shared_str ResolveProfileSection(IKinematics* NewModel) const;
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
	void DrawLedImGui(u32 Index);
	void DrawGaugeImGui(u32 Index);
	void DrawPreviewImGui();
	void DrawHotSaveImGui();

	void ResetRuntime();
	void CreateUI();
	void UpdateUILayout();
	void UpdateDisplayFormats();
	void UpdateDisplay(float Dt);
	void UpdateDisplayLag(float Dt);
	void UpdateSurge(float Dt);
	void UpdateBarometer();
	void SetBarometerText(const char* Text);
	void UpdateLightSchedule(bool Report);
	void UpdateLight(float Dt);

	void DestroyHudLights();
	void CreateHudPointLight(ref_light& Light, float Range);
	void DestroyHudPointLight(ref_light& Light);
	void TurnOffHudPointLight(ref_light& Light);
	void SyncLedLight(u32 Index);
	void PrepareAnchors(const Fmatrix& WatchesXform);

	void UpdateAnomalyExclude();
	bool IsLedActive(u32 Index) const;
	bool IsGaugeActive(u32 Index) const;
	bool UsesSource(EWatchSourceKind Kind) const;
	float SampleSource(const SWatchSource& Source, float Normalize) const;
	void SampleZones(float Dt);
	void UpdateLeds(float Dt);
	void UpdateGauges(float Dt);
	void RenderGauge(u32 Index);
	u32 GaugeColor(const SWatchGaugeRuntime& Gauge, u32 Fallback) const;

	void UpdateLedMeshes();
	void AttachModel(IKinematics* NewModel);

	void RefreshCompassSettings(bool Report);
	u32 CompassColorForKind(EWatchCompassSourceKind Kind) const;
	void ResetCompassRuntime();
	void UpdateCompass(float Dt);
	void ApplyCompassSample(const SWatchCompassSample& Sample, bool HadTarget);
	void ApplyCompassNoTarget();
	void SetCompassVisible(bool Visible);
	void CommitCompassAngle(float Angle, bool ResetVelocity);
	void SyncCompassState();
	bool UpdateCompassGravityChaos(float Dt);
	void UpdateCompassSpring(float Dt);

	shared_str Section;
	shared_str Profile;
	SWatchConfig Config;
	SWatchConfig ConfigBaseline;
	SWatchState State;
	SWatchDebugPreview Preview;
	CWatchRig Rig;
	xr_unique_ptr<IWatchEnvironment> Env;
	xr_unique_ptr<CUIWatchWnd> Ui;
	Fmatrix UiOffset = Fidentity;
	SWatchDisplayRuntime Display;
	SWatchSurgeRuntime Surge;
	SWatchCompassRuntime Compass;
	SWatchCompassSettings CompassSettings;
	SWatchZoneRuntime Zones;
	SWatchLightRuntime Light;
	SWatchAnchors Anchors;
	xr_vector<SWatchLedRuntime> Leds;
	xr_vector<SWatchGaugeRuntime> Gauges;
	u32 LastVisibleFrame = 0;
	bool Hidden = true;
	bool Loaded = false;
	bool Editing = false;
	string_path OverridePath = {};
	string_path LastHotSaveStatus = {};
};
