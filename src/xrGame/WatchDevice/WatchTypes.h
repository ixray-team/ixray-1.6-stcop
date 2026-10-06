#pragma once

inline constexpr float WatchLedMaxBrightness = 16.0f;

enum class EWatchGlow : u8
{
	Compass,
	Anomaly,
	Motion,
	Noise,
	Count
};

enum class EWatchLedChannel : u8
{
	Anomaly,
	Motion,
	Noise,
	Count
};

inline constexpr u32 WatchLedChannelCount = u32(EWatchLedChannel::Count);

struct SWatchRoot
{
	bool Enabled = true;

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(Enabled, "enabled");
	}
};

struct SWatchBones
{
	shared_str Compass = "j_compas";
	shared_str CompassLight = "j_compas_light";
	shared_str Display = "j_watch_ui";
	shared_str RadIndicator = "j_rad_indicator";
	shared_str AnomIndicator = "j_anom_indicator";

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(Compass, "compass");
		Visitor.Field(CompassLight, "compass_light");
		Visitor.Field(Display, "display");
		Visitor.Field(RadIndicator, "rad_indicator");
		Visitor.Field(AnomIndicator, "anom_indicator");
	}
};

struct SWatchIndicatorMasters
{
	bool Enabled[WatchLedChannelCount] = {true, true, true};

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(Enabled[u32(EWatchLedChannel::Anomaly)], "anomaly_enabled");
		Visitor.Field(Enabled[u32(EWatchLedChannel::Motion)], "motion_enabled");
		Visitor.Field(Enabled[u32(EWatchLedChannel::Noise)], "noise_enabled");
	}
};

struct SWatchDisplay
{
	shared_str TimeFormat = "HH:MM";
	shared_str DateFormat = "DD:MM:YY";
	shared_str BarometerUnit = "hPa";
	float UpdateHz = 1.0f;
	bool BarometerEnabled = true;
	float BarometerLookaheadFar = 300.0f;
	float BarometerLookaheadNear = 30.0f;
	float BarometerNormal = 1013.0f;
	float BarometerDrop = 60.0f;
	Fvector UiPosition = {0.0f, 0.0f, 0.0f};
	Fvector UiRotation = {0.0f, 0.0f, 0.0f};
	float UiScale = 1.0f;
	Fvector4 UiTime = {-0.01f, -0.006f, 0.02f, 0.007f};
	Fvector4 UiDate = {-0.01f, 0.0015f, 0.02f, 0.0035f};
	Fvector4 UiBarometer = {-0.01f, -0.0095f, 0.0135f, 0.003f};
	Fvector4 UiBarometerUnit = {0.0045f, -0.009f, 0.0055f, 0.0025f};

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(TimeFormat, "time_format");
		Visitor.Field(DateFormat, "date_format");
		Visitor.Field(UpdateHz, "update_hz", 0.0f);
		Visitor.Field(UiPosition, "ui_p");
		Visitor.Field(UiRotation, "ui_r");
		Visitor.Field(UiScale, "ui_scale");
		Visitor.Field(UiTime, "ui_time");
		Visitor.Field(UiDate, "ui_date");
		Visitor.Field(UiBarometer, "ui_barometer");
		Visitor.Field(UiBarometerUnit, "ui_barometer_unit");
		Visitor.Field(BarometerUnit, "barometer_unit");
		Visitor.Field(BarometerEnabled, "barometer_enabled");
		Visitor.Field(BarometerLookaheadFar, "barometer_lookahead_far", 0.0f);
		Visitor.Field(BarometerLookaheadNear, "barometer_lookahead_near", 0.0f);
		Visitor.Field(BarometerNormal, "barometer_normal");
		Visitor.Field(BarometerDrop, "barometer_drop");
	}
};

struct SWatchFonts
{
	shared_str Time;
	shared_str Date;
	shared_str Barometer;
	shared_str BarometerUnit;

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(Time, "time");
		Visitor.Field(Date, "date");
		Visitor.Field(Barometer, "barometer");
		Visitor.Field(BarometerUnit, "barometer_unit");
	}
};

enum class EWatchFormatToken : u8
{
	Literal,
	Hour,
	Minute,
	Second,
	Day,
	Month,
	Year2,
	Year4
};

struct SWatchFormatToken
{
	EWatchFormatToken Type = EWatchFormatToken::Literal;
	char Literal = 0;
};

struct SWatchCompass
{
	shared_str Mode = "target";
	shared_str NoTarget = "hide";
	float Stiffness = 12.0f;
	float Damping = 5.0f;
	float GravityStrength = 35.0f;
	float AngleOffset = 0.0f;

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(Mode, "mode");
		Visitor.Field(NoTarget, "no_target");
		Visitor.Field(Stiffness, "stiffness", 0.0f);
		Visitor.Field(Damping, "damping", 0.0f);
		Visitor.Field(GravityStrength, "gravity_strength", 0.0f);
		Visitor.Field(AngleOffset, "angle_offset");
	}
};

struct SWatchLag
{
	float MaxDelay = 0.0f;
	float MinUpdateHz = 1.0f;
	float FreezeChance = 0.0f;
	float Flicker = 0.0f;

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(MaxDelay, "max_delay", 0.0f);
		Visitor.Field(MinUpdateHz, "min_update_hz", 0.0f);
		Visitor.Field(FreezeChance, "freeze_chance", 0.0f, 1.0f);
		Visitor.Field(Flicker, "flicker", 0.0f, 1.0f);
	}
};

struct SWatchChannelPresent
{
	bool Enabled = true;
	bool PresentShader = true;
	bool PresentUi = false;
	shared_str Bone;
	Fvector UiPosition = {0.0f, 0.0f, 0.0f};
	Fvector UiRotation = {0.0f, 0.0f, 0.0f};
	Fvector2 UiSize = {0.004f, 0.004f};
	bool HideMesh = false;
	Fvector2 GlassSize = {0.007f, 0.0017f};
	float Brightness = 4.0f;
	bool LightEnabled = true;
	float LightRange = 0.12f;
	float LightBrightness = 1.0f;

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(Enabled, "enabled");
		Visitor.Field(PresentShader, "present_shader");
		Visitor.Field(PresentUi, "present_ui");
		Visitor.Field(Bone, "bone");
		Visitor.Field(UiPosition, "ui_p");
		Visitor.Field(UiRotation, "ui_r");
		Visitor.Field(UiSize, "ui_size");
		Visitor.Field(HideMesh, "hide_mesh");
		Visitor.Field(GlassSize, "glass_size");
		Visitor.Field(Brightness, "brightness", 0.0f, WatchLedMaxBrightness);
		Visitor.Field(LightEnabled, "light_enabled");
		Visitor.Field(LightRange, "light_range", 0.0f);
		Visitor.Field(LightBrightness, "light_brightness", 0.0f);
	}
};

struct SWatchLedBlink
{
	float Threshold = 0.02f;
	float SolidThreshold = 0.8f;
	float BlinkMinHz = 0.5f;
	float BlinkMaxHz = 8.0f;
	float BlinkDuty = 0.35f;

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(Threshold, "threshold");
		Visitor.Field(SolidThreshold, "solid_threshold");
		Visitor.Field(BlinkMinHz, "blink_min_hz", 0.0f);
		Visitor.Field(BlinkMaxHz, "blink_max_hz", 0.0f);
		Visitor.Field(BlinkDuty, "blink_duty", 0.0f, 1.0f);
	}
};

struct SWatchAnomalySensor
{
	float Radius = 20.0f;
	bool ExcludeRadiation = true;
	shared_str Exclude;

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(Radius, "radius", 0.0f);
		Visitor.Field(ExcludeRadiation, "exclude_radiation");
		Visitor.Field(Exclude, "exclude");
	}
};

enum class EWatchLightLevel : u8
{
	Day,
	Dusk,
	Night,
	Dark
};

struct SWatchLightKey
{
	float Hour = 0.0f;
	EWatchLightLevel Level = EWatchLightLevel::Day;
};

struct SWatchLight
{
	float DayEmission = 0.0f;
	float DuskEmission = 0.25f;
	float NightEmission = 0.75f;
	float DarkEmission = 1.0f;
	shared_str Schedule = "3:dark, 5:night, 6:dusk, 7:day, 19:day, 20:dusk, 21:night, 23:dark";
	Fvector UiPosition = {0.0f, 0.0003f, 0.0f};
	Fvector UiRotation = {0.0f, 90.0f, 0.0f};
	Fvector2 UiSize = {0.004f, 0.004f};

	template <class V>
	void ForEach(V& Visitor)
	{
		Visitor.Field(DayEmission, "day_emission");
		Visitor.Field(DuskEmission, "dusk_emission");
		Visitor.Field(NightEmission, "night_emission");
		Visitor.Field(DarkEmission, "dark_emission");
		Visitor.Field(Schedule, "schedule");
		Visitor.Field(UiPosition, "ui_p");
		Visitor.Field(UiRotation, "ui_r");
		Visitor.Field(UiSize, "ui_size");
	}
};

struct SWatchConfig
{
	SWatchRoot Root;
	SWatchBones Bones;
	SWatchIndicatorMasters Masters;
	SWatchDisplay Display;
	SWatchFonts Fonts;
	SWatchCompass Compass;
	SWatchLag SurgeLag;
	SWatchLag AnomalyLag;
	SWatchLight Light;
	SWatchChannelPresent Present[WatchLedChannelCount];
	SWatchLedBlink Blink[WatchLedChannelCount];
	SWatchAnomalySensor Anomaly;

	SWatchConfig()
	{
		SWatchChannelPresent& Noise = Present[u32(EWatchLedChannel::Noise)];
		Noise.PresentShader = false;
		Noise.PresentUi = true;
		Noise.HideMesh = false;
		Noise.LightEnabled = false;
		Noise.UiPosition = {0.0075f, 0.0f, 0.0f};
		Noise.UiSize = {0.0025f, 0.0025f};
	}
};

struct SWatchBoneIds
{
	u16 Compass = u16(-1);
	u16 CompassLight = u16(-1);
	u16 Display = u16(-1);
	u16 RadIndicator = u16(-1);
	u16 AnomIndicator = u16(-1);
};

struct SWatchDebugPreview
{
	bool ForceAngle = false;
	float ForceAngleDeg = 0.0f;
	bool PreviewGravity = false;
	float PreviewGravityValue = 0.0f;
	bool PreviewSurge = false;
	float PreviewSurgeValue = 0.0f;
	bool PreviewChannel[WatchLedChannelCount] = {};
	float PreviewChannelValue[WatchLedChannelCount] = {};
	bool PreviewEmission = false;
	float PreviewEmissionValue = 0.0f;
};

struct SWatchState
{
	float CompassAngle = 0.0f;
	float CompassError = 0.0f;
	float Barometer = 0.0f;
	float SurgeFactor = 0.0f;
	float Intensity[WatchLedChannelCount] = {};
	float Emission = 0.0f;
	float DisplayQuality = 1.0f;
	xr_string TimeText;
	xr_string DateText;
	xr_string BarometerText;
	bool DisplayGlitched = false;
	bool TargetValid = false;
};
