#pragma once

struct SELNavigationIdentity
{
	u32 WorldGeneration = 0;
	u32 OwnerGeneration = 0;
	u32 PlanGeneration = 0;
	u16 OwnerIdentifier = u16(-1);
	bool operator==(const SELNavigationIdentity& Other) const = default;
};

struct SELNavigationNode
{
	Fvector Position = {0.f, 0.f, 0.f};
	u32 VertexIdentifier = u32(-1);
	u32 Neighbors[7] = {u32(-1), u32(-1), u32(-1), u32(-1), u32(-1), u32(-1), u32(-1)};
};

struct SELNavigationObstacle
{
	Fvector Position = {0.f, 0.f, 0.f};
	float Radius = 0.f;
};

struct SELNavigationRequest
{
	SELNavigationIdentity Identity;
	Fvector StartPosition = {0.f, 0.f, 0.f};
	Fvector StartDirection = {0.f, 0.f, 1.f};
	Fvector GoalPosition = {0.f, 0.f, 0.f};
	Fvector GoalDirection = {0.f, 0.f, 1.f};
	xr_vector<SELNavigationNode> Graph;
	xr_vector<SELNavigationObstacle> Obstacles;
	xr_vector<Fvector> GuidePoints;
	xr_vector<Fvector> GuideDirections;
	float InvertedBodyRadius = 0.f;
	bool CanInvert = false;
	u32 StartNode = 0;
	u32 GoalNode = 0;
	bool IsFlying = false;
	bool CanConnectStartToGoal = false;
	float CurveDistance = 5.f;
};

struct SELNavigationResult
{
	SELNavigationIdentity Identity;
	xr_vector<Fvector> Points;
	xr_vector<u32> Vertices;
	bool HasPath = false;
	bool IsPartial = false;
	bool IsContinuation = false;
	float InvertedBodyRadius = 0.f;
	bool CanInvert = false;
};

class IELNavigation
{
public:
	virtual ~IELNavigation() = default;
	virtual bool Submit(SELNavigationRequest Request) = 0;
	virtual bool Poll(SELNavigationResult& Result) = 0;
	virtual void Cancel() = 0;
	virtual bool IsPending() const = 0;
};
