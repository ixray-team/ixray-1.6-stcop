#pragma once
#include "../Interfaces/IELNavigation.h"
#include "CELSharedNavigationBudget.h"

class CELSharedNavigationScheduler final
{
public:
	bool Submit(SELNavigationRequest Request);
	void Cancel(const SELNavigationIdentity& Identity);
	bool Take(const SELNavigationIdentity& Identity, SELNavigationResult& Result);
	void Process();
	void DiscardAbandoned();
	void Clear();
	u32 GetGeneration() const;
	u32 OutstandingRequests() const;
	u32 LastSliceMicroseconds() const;
	bool TryCaptureNode();
	bool TryGeometryRays(u32 RayCount);
	bool TryEmergencyGeometryRays(u32 RayCount);
	u32 GeometryRaysRemaining() const;
	u32 EmergencyRaysRemaining() const;

private:
	struct SOpenNode
	{
		u32 Index = 0;
		float Cost = 0.f;
		float Score = 0.f;
		bool operator<(const SOpenNode& Other) const { return Score > Other.Score; }
	};
	struct STask
	{
		SELNavigationRequest Request;
		SELNavigationResult Result;
		xr_vector<Fvector> CurvePath;
		xr_vector<float> Costs;
		xr_vector<u32> Parents;
		xr_vector<u8> States;
		xr_vector<SOpenNode> OpenNodes;
		u32 CurveStep = 0;
		bool IsComplete = false;
	};
	static void Expand(STask& Task);
	static void ExpandCurve(STask& Task);
	static void ExpandGraph(STask& Task);
	static void FinishGraph(STask& Task, u32 NodeIndex);

	xr_vector<STask> Tasks;
	mutable xrCriticalSection QueueLock;
	CELSharedNavigationBudget Budget;
	u32 WorldGeneration = 1;
	u32 NextTask = 0;
	u32 SliceMicroseconds = 0;
};
