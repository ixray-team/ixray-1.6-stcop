#pragma once
#include "CinemaScene.h"

namespace Cinema
{
inline constexpr u8 WhereActor = 1, WhereCameraBlock = 2, WhereLight = 4, WhereParticle = 8, WhereShakeClip = 16, WherePose = 32, WherePlaced = WhereActor | WhereLight | WhereParticle;
inline constexpr u8 ObjectWhere[4] = { WhereActor, 0, WhereLight, WhereParticle };

struct ParamInfo
{
	const char* Id;
	const char* Label;
	u8 Components;
	float Min;
	float Max;
	bool IsIntegral;
	u8 Where;
};

inline constexpr ParamInfo Params[static_cast<u32>(Param::Count)] =
{
	{ "position", "Position", 3, -100000.0f, 100000.0f, false, WherePlaced | WherePose },
	{ "rotation", "Rotation", 3, -3600.0f, 3600.0f, false, WherePlaced | WherePose },
	{ "scale", "Scale", 3, 0.1f, 5.0f, false, WhereActor },
	{ "time_scale", "Time scale", 1, 0.0f, 4.0f, true, WhereActor | WhereCameraBlock },
	{ "fov", "FOV", 1, 1.0f, 179.0f, false, WhereCameraBlock },
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
float Value(const Curve& Source, float Time, float Fallback);
float Result(const Curve& Source, float Time, float Fallback, const ParamInfo& Meta);
void Refresh(AreaTable& Table, const Curve& Source, float Rate, float Step, float Cover, const ParamInfo& Meta);
double Area(const Curve& Source, const AreaTable* Table, float Time, float Rate, const ParamInfo& Meta);
float InverseArea(const Curve& Source, const AreaTable* Table, float Rate, const ParamInfo& Meta, double TargetArea, float Limit);
const Lane* FindLane(const xr_vector<Lane>& Lanes, Param Id, u8 Layer);
Lane& EnsureLane(xr_vector<Lane>& Lanes, Param Id, u8 Layer);
float LaneScalar(const xr_vector<Lane>& Lanes, Param Id, u8 Layer, float Time, float Fallback);
Fvector LaneVector(const xr_vector<Lane>& Lanes, Param Id, float Time, const Fvector& Fallback);
double LaneArea(const xr_vector<Lane>& Lanes, const AreaTable* Table, Param Id, u8 Layer, float Time, float Rate);
float LaneInverseArea(const xr_vector<Lane>& Lanes, const AreaTable* Table, Param Id, u8 Layer, float Rate, double TargetArea, float Limit);
u32 KeyAt(const Curve& Source, float Time, float HalfFrame);
void SetKey(Curve& Target, float Time, float NewValue, float HalfFrame);
void PlaceKey(Curve& Target, const Key& Item, float HalfFrame);
void SetMode(Curve& Target, u32 Index, Interp Mode);
void EasyEase(Curve& Target, u32 Index);
}
