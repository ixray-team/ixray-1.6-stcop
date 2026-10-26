#pragma once
#include "xrCore/xrCore.h"

class CDetailWind
{
public:
	enum GlobalParameter
	{
		Force,
		MaxAngle,
		Anchor,
		Profile,
		Compliance,
		Frequency,
		Damping,
		Variation,
		VerticalResponse,
		DirectionWander,
		DirectionTime,
		TransitionTime,
		TrampleResidual,
		GustRate,
		GustStrength,
		GustLength,
		GustWidth,
		GustHeight,
		GustSpeed,
		GustLife,
		GustAttack,
		GustRelease,
		GustWander,
		GustWarp,
		GustVertical,
		GustLocalFraction,
		GustLocalAngle,
		GustLocalScale,
		GustClustering,
		GustPause,
		FieldSpacing,
		GlobalCount
	};
	enum LayerParameter
	{
		Length,
		Width,
		Height,
		Speed,
		Evolution,
		Xz,
		Y,
		Curl,
		Warp,
		Contrast,
		Angle,
		Shape,
		LayerStride
	};
	static constexpr int LayerCount = 6;
	static constexpr int ParameterCount = GlobalCount + LayerCount * LayerStride;
	struct Definition
	{
		xr_string Key;
		xr_string Label;
		float Lower, Upper, AtZero, AtOne;
		int Group;
	};
	static constexpr int BehaviorCount = 8;
	struct BehaviorSettings
	{
		bool Enabled = true;
		float Influence[2] = {.5f, 1.0f};
		float Duration[2] = {6.0f, 10.0f};
		float Interval[2] = {20.0f, 12.0f};
	};
	static const char* BehaviorName(int Index);
	struct Settings
	{
		float Values[ParameterCount][2] = {};
		bool Enabled[LayerCount] = {true, true, true, true, true, true};
		u32 Seed = 12345;
		BehaviorSettings Behaviors[BehaviorCount];
		Settings();
	};
	struct Constants
	{
		Fvector4 global;
		Fvector4 xz1, xz2, xz3;
	};
	static const xr_vector<Definition>& Definitions();
	static Settings& Editable();
	static void BeginFrame(float Dt, const Fvector& Camera, const Settings& Snapshot, float Blend, float Direction, bool Enabled);
	static void PrefetchField(const xr_vector<Fbox>& Bounds);
	static void Update(const Fvector& Position, float Trample, float Height, Fvector4& Bend, Fvector4& Previous, Fvector4& Velocity, Fvector4& PreviousVelocity);
	static void Clear();
	static void ComputeConstants(Constants& Out);
	static u32 ActiveGustCount();
	static void OpenEditor();
	static void RequestGust();
	static void RequestBehavior(int Index);
	static u32 ActiveBehaviorCount(int Index);
	static void FinishFrame();
	static u32 SimulatedPlants();
	static u32 FieldSamples();
	static float StepMilliseconds();
};

// Owns editable preset data; the master switch and blend remain external controls.
class CDetailWindPresets
{
public:
	static CDetailWindPresets& Get();
	bool Reload();
	void Clear();
	bool Select(const char* Section);
	bool Save();
	bool Add(const char* Section);
	bool Remove();
	bool SetEnabled(bool Enabled);
	bool IsEnabled(const char* Section) const;
	bool IsListed(const char* Section) const;
	const xr_vector<shared_str>& GetSections() const { return Sections; }
	const char* GetSelected() const { return Selected.c_str() ? Selected.c_str() : ""; }
	const char* GetDefault() const { return DefaultSection.c_str() ? DefaultSection.c_str() : ""; }
	char* GetDisplayName() { return DisplayName; }
	xr_string GetName(const char* Section) const;
	const char* GetStatus() const { return Status.c_str(); }

private:
	bool ReadRegistry();
	bool SelectAvailable();
	bool Commit(CInifile& Data);
	void WriteParameters(CInifile& Data, const char* Section);
	xr_unique_ptr<CInifile> Config;
	xr_vector<shared_str> Sections;
	shared_str Selected;
	shared_str DefaultSection;
	shared_str ListSection;
	xr_string Status;
	char DisplayName[256] = {};
};
