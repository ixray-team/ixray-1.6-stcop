#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELNavigation.h"
class CObject;
struct SELFlyingCorridor;

enum class EELGeometryResult : u8
{
	Pending,
	Clear,
	Blocked
};

class CELFlyingGeometry final
{
public:
	static Fbox AirBounds(float HorizontalMargin, float Radius, float CenterHeight);
	static bool Submit(SELNavigationResult Route, float Radius, float CenterHeight, bool Stream = false);
	static EELGeometryResult Take(const SELNavigationIdentity& Identity, SELNavigationResult& Route);
	static void Cancel(const SELNavigationIdentity& Identity);
	static void BeginFrame();
	static bool HasFrameBudget();
	static EELGeometryResult CheckEmergency(const SELNavigationResult& Route, float Radius, float CenterHeight, const SELFlyingCorridor& Corridor);
	static void Clear();
	static u32 OutstandingRequests();
	static EELGeometryResult QuerySupport(const Fvector& From, float Distance, Fvector& Contact, Fvector& Normal, CObject* Ignore = nullptr, bool HasReservedRay = false);
	static EELGeometryResult QuerySegment(const Fvector& Start, const Fvector& End, CObject* Ignore = nullptr, bool HasReservedRay = false);
	static EELGeometryResult QueryDynamicCorridor(const SELFlyingCorridor& Corridor, SELNavigationObstacle& Obstacle, float& ClearDistance, bool RaysReserved = false);
	static EELGeometryResult QueryBody(const Fvector& Position, float Radius, float CenterHeight, CObject* Ignore, bool HasReservedRays = false);
	static EELGeometryResult QueryLanding(const Fvector& From, float Distance, float Radius, float CenterHeight, float GroundOffset, float MinimumNormalY, float HeightTolerance, Fvector& RootPosition, CObject* Ignore);
	static bool Support(const Fvector& From, float Distance, Fvector& Contact, Fvector& Normal, CObject* Ignore = nullptr, bool HasReservedRay = false);
	static EELGeometryResult QueryHeightBand(const Fvector& Reference, const Fvector& Column, float Radius, float CenterHeight, float Margin, float& Minimum, float& Maximum, bool& HasCeiling);
	static bool HasCover(const Fvector& Point, float Height);
	static bool ClearSegment(const Fvector& Start, const Fvector& End, CObject* Ignore = nullptr);
	static bool Surface(const Fvector& Position, float& Height, bool HasReservedRay = false);

private:
	struct SState;
	static SState& GetState();
	CELFlyingGeometry() = delete;
	static EELGeometryResult CheckFloor(const Fvector& Position, const Fvector& Previous, float Radius, float CenterHeight, bool CanLeaveOverlap, bool ExtraRaysReserved = false);
};
