#pragma once
#include "CinemaScene.h"

namespace Cinema
{
inline constexpr u16 WhereActor = 1, WhereCameraBlock = 2, WhereLight = 4, WhereParticle = 8, WhereShakeClip = 16, WherePose = 32, WherePoseBone = 64, WhereDecal = 128, WhereWeather = 256, WherePost = 512, WherePlaced = WhereActor | WhereLight | WhereParticle | WhereDecal;
inline constexpr u16 ObjectWhere[7] = { WhereActor, 0, WhereLight, WhereParticle, WhereDecal, WhereWeather, WherePost };

struct ParamInfo
{
	const char* Id;
	const char* Label;
	u8 Components;
	float Min;
	float Max;
	bool IsIntegral;
	u16 Where;
};

inline constexpr ParamInfo Params[static_cast<u32>(Param::Count)] =
{
	{ "position", "Position", 3, -100000.0f, 100000.0f, false, WherePlaced | WhereCameraBlock },
	{ "rotation", "Rotation", 3, -3600.0f, 3600.0f, false, WherePlaced | WhereCameraBlock },
	{ "scale", "Scale", 3, 0.1f, 5.0f, false, WhereActor },
	{ "time_scale", "Time scale", 1, 0.0f, 4.0f, true, WhereActor | WhereCameraBlock },
	{ "fov", "Vertical FOV", 1, 1.0f, 179.0f, false, WhereCameraBlock },
	{ "color", "Color", 3, 0.0f, 100000.0f, false, WhereLight },
	{ "intensity", "Intensity", 1, 0.0f, 100.0f, false, WhereLight },
	{ "range", "Range", 1, 0.1f, 1000.0f, false, WhereLight },
	{ "cone", "Cone", 1, 1.0f, 120.0f, false, WhereLight },
	{ "virtual_size", "Virtual size", 1, 0.0f, 100.0f, false, WhereLight },
	{ "volume_intensity", "Volume intensity", 1, 0.0f, 100.0f, false, WhereLight },
	{ "volume_distance", "Volume distance", 1, 0.01f, 1.0f, false, WhereLight },
	{ "volume_quality", "Volume quality", 1, 0.01f, 1.0f, false, WhereLight },
	{ "size", "Scale", 1, 0.01f, 25.0f, false, WhereParticle },
	{ "speed", "Speed", 1, 0.0f, 4.0f, true, WhereParticle },
	{ "layer_weight", "Weight", 1, 0.0f, 1.0f, false, WhereActor },
	{ "dof_focus", "Focus", 1, 0.05f, 10000.0f, false, WhereCameraBlock },
	{ "dof_offset", "Focus offset", 1, -1000.0f, 1000.0f, false, WhereCameraBlock },
	{ "dof_near", "Near range", 1, 0.01f, 1000.0f, false, WhereCameraBlock },
	{ "dof_far", "Far range", 1, 0.01f, 10000.0f, false, WhereCameraBlock },
	{ "dof_blur", "Blur", 1, 0.0f, 10.0f, false, WhereCameraBlock },
	{ "dof_aperture", "Aperture", 1, 0.7f, 64.0f, false, WhereCameraBlock },
	{ "dof_length", "Focal length", 1, 0.0f, 1000.0f, false, WhereCameraBlock },
	{ "camera_smooth", "Smoothing", 1, 0.0f, 2.0f, false, WhereCameraBlock },
	{ "pose_weight", "Weight", 1, 0.0f, 1.0f, false, WherePoseBone },
	{ "bone_rotation", "Rotation", 3, -3600.0f, 3600.0f, false, WherePoseBone },
	{ "bone_position", "Position", 3, -10.0f, 10.0f, false, WherePoseBone },
	{ "bone_scale", "Scale", 3, 0.01f, 100.0f, false, WherePoseBone },
	{ "attach_weight", "Attach", 1, 0.0f, 1.0f, false, WherePlaced | WhereCameraBlock },
	{ "look", "Look", 1, 0.0f, 15.0f, false, WhereActor },
	{ "ik_target", "IK target", 3, -100000.0f, 100000.0f, false, WherePoseBone },
	{ "ik_turn", "IK turn", 3, -3600.0f, 3600.0f, false, WherePoseBone },
	{ "ik_pole", "IK pole", 3, -100000.0f, 100000.0f, false, WherePoseBone },
	{ "ik_weight", "IK weight", 1, 0.0f, 1.0f, false, WherePoseBone },
	{ "constraint_weight", "Weight", 1, 0.0f, 1.0f, false, WherePoseBone },
	{ "decal_width", "Width", 1, 0.01f, 20.0f, false, WhereDecal },
	{ "decal_height", "Height", 1, 0.01f, 20.0f, false, WhereDecal },
	{ "weather_time", "Time of day", 1, 0.0f, 24.0f, false, WhereWeather },
	{ "sun_heading", "Sun heading", 1, -3600.0f, 3600.0f, false, WhereWeather },
	{ "sun_height", "Sun height", 1, 1.0f, 89.0f, false, WhereWeather },
	{ "sun_color", "Sun color", 3, 0.0f, 10.0f, false, WhereWeather },
	{ "sun_brightness", "Sun brightness", 1, 0.0f, 4.0f, false, WhereWeather },
	{ "sun_shafts", "Sun shafts", 1, 0.0f, 1.0f, false, WhereWeather },
	{ "ambient_color", "Ambient", 3, 0.0f, 10.0f, false, WhereWeather },
	{ "hemi_color", "Hemisphere", 3, 0.0f, 10.0f, false, WhereWeather },
	{ "fog_color", "Fog color", 3, 0.0f, 1.0f, false, WhereWeather },
	{ "fog_density", "Fog density", 1, 0.0f, 1.0f, false, WhereWeather },
	{ "fog_distance", "Fog distance", 1, 1.0f, 10000.0f, false, WhereWeather },
	{ "far_plane", "View distance", 1, 10.0f, 10000.0f, false, WhereWeather },
	{ "rain_density", "Rain", 1, 0.0f, 1.0f, false, WhereWeather },
	{ "wind_speed", "Wind speed", 1, 0.0f, 1000.0f, false, WhereWeather },
	{ "wind_direction", "Wind direction", 1, -3600.0f, 3600.0f, false, WhereWeather },
	{ "sky_rotation", "Sky rotation", 1, -3600.0f, 3600.0f, false, WhereWeather },
	{ "post_base", "Color base", 3, 0.0f, 2.0f, false, WherePost },
	{ "post_add", "Color add", 3, -1.0f, 1.0f, false, WherePost },
	{ "post_gray_color", "Gray tint", 3, 0.0f, 1.0f, false, WherePost },
	{ "post_gray", "Gray", 1, 0.0f, 1.0f, false, WherePost },
	{ "post_blur", "Blur", 1, 0.0f, 1.0f, false, WherePost },
	{ "post_duality_h", "Duality H", 1, 0.0f, 0.2f, false, WherePost },
	{ "post_duality_v", "Duality V", 1, 0.0f, 0.2f, false, WherePost },
	{ "post_noise", "Noise", 1, 0.0f, 1.0f, false, WherePost },
	{ "post_grain", "Noise grain", 1, 0.01f, 1.0f, false, WherePost },
	{ "post_noise_fps", "Noise rate", 1, 1.0f, 100.0f, false, WherePost },
	{ "post_fade", "Fade", 1, 0.0f, 1.0f, false, WherePost },
	{ "post_fade_color", "Fade color", 3, 0.0f, 1.0f, false, WherePost },
	{ "post_exposure", "Exposure", 1, -5.0f, 5.0f, false, WherePost },
	{ "post_bloom", "Bloom", 1, 0.0f, 3.0f, false, WherePost },
	{ "post_tonemap", "Tonemap", 1, 0.0f, 1.0f, false, WherePost },
	{ "post_middle_gray", "Middle gray", 1, 0.0f, 2.0f, false, WherePost },
	{ "post_motion_blur", "Motion blur", 1, 0.0f, 1.0f, false, WherePost },
	{ "shake_influence", "Influence", 1, 0.0f, 4.0f, false, WhereShakeClip },
	{ "shake_position_scale", "Position scale", 1, 0.0f, 100.0f, false, WhereShakeClip },
	{ "shake_speed", "Speed", 1, 0.0f, 100.0f, true, WhereShakeClip },
};

inline const ParamInfo& Info(Param Id)
{
	return Params[static_cast<u32>(Id)];
}
}

namespace Cinema::Curves
{
struct AreaTable
{
	Curve Source;
	float Rate = 0.0f;
	float Step = 0.0f;
	float Cover = 0.0f;
	xr_vector<double> Prefix;
};

inline constexpr double BezierValue(double A, double B, double C, double D, double S)
{
	const double Left = 1.0 - S;
	return Left * Left * Left * A + 3.0 * Left * Left * S * B + 3.0 * Left * S * S * C + S * S * S * D;
}

float Ease(Interp Mode, float U);
float EaseArea(Interp Mode, float U);
float WaveSignal(Wave Type, float Cycle);
float Value(const Curve& Source, float Time, float Fallback, float Tension = 1.0f);
float Result(const Curve& Source, float Time, float Fallback, const ParamInfo& Meta, float Tension = 1.0f);
void Refresh(AreaTable& Table, const Curve& Source, float Rate, float Step, float Cover, const ParamInfo& Meta);
double Area(const Curve& Source, const AreaTable* Table, float Time, float Rate, const ParamInfo& Meta);
float InverseArea(const Curve& Source, const AreaTable* Table, float Rate, const ParamInfo& Meta, double TargetArea, float Limit);
const Lane* FindLane(const xr_vector<Lane>& Lanes, Param Id, u8 Layer);
Lane& EnsureLane(xr_vector<Lane>& Lanes, Param Id, u8 Layer);
float LaneScalar(const xr_vector<Lane>& Lanes, Param Id, u8 Layer, float Time, float Fallback);
Fvector LaneVector(const xr_vector<Lane>& Lanes, Param Id, float Time, const Fvector& Fallback, u8 Layer = 0, float Tension = 1.0f);
double LaneArea(const xr_vector<Lane>& Lanes, const AreaTable* Table, Param Id, u8 Layer, float Time, float Rate);
float LaneInverseArea(const xr_vector<Lane>& Lanes, const AreaTable* Table, Param Id, u8 Layer, float Rate, double TargetArea, float Limit);
u32 KeyAt(const Curve& Source, float Time, float HalfFrame);
void SetKey(Curve& Target, float Time, float NewValue, float HalfFrame);
void PlaceKey(Curve& Target, const Key& Item, float HalfFrame);
void SetMode(Curve& Target, u32 Index, Interp Mode);
void EasyEase(Curve& Target, u32 Index);
u32 Fit(Curve& Target, float From, float To, float Tolerance);

inline u32 LookAt(const xr_vector<Lane>& Lanes, float Time)
{
	const Lane* Track = FindLane(Lanes, Param::Look, 0);
	if (!Track || Track->Curves[0].Keys.empty())
	{
		return 0;
	}
	const xr_vector<Key>& Keys = Track->Curves[0].Keys;
	const auto After = std::upper_bound(Keys.begin(), Keys.end(), Time, [](float Moment, const Key& Item) { return Moment < Item.Time; });
	return static_cast<u32>(std::max((After == Keys.begin() ? After : After - 1)->Value, 0.0f) + 0.5f);
}
}
