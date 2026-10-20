#pragma once
#include "../../../Navigation/CELNavigationBase.h"

class CELFlyingNavigation final : public CELNavigationBase
{
public:
	~CELFlyingNavigation() override;
	bool Submit(SELNavigationRequest Request) override;
	bool Poll(SELNavigationResult& Result) override;
	void Cancel() override;
	bool IsPending() const override;
	void SetClearance(float CollisionRadius, float CenterHeight);
	void SetSearch(float CellSize, u32 NodeLimit, u32 Timeout);
	const char* Status() const;
	void SetStreaming(bool Enabled) { StreamsRoute = Enabled; }
	const SELNavigationIdentity& Identity() const { return OriginalRequest.Identity; }
	void Reject() { HasBlocked = true; }
	bool WasBlocked() const { return HasBlocked; }

private:
	bool RetryShorterCurve();
	bool StartDetour();
	bool SubmitGuidedLeg();
	bool IsGuidedDetour = false;
	u32 DetourLeg = 0;
	u32 GuidedNodes = 0;
	u32 GuidedDeadline = 0;
	xr_vector<Fvector> GuidedPath;
	SELNavigationRequest OriginalRequest;
	float SearchCellSize = 4.f;
	u32 SearchNodeLimit = 128;
	u32 SearchTimeout = 8000;
	bool HasCapture = false;
	bool HasTriedDetour = false;
	bool HasTriedShorterCurve = false;
	float Radius = .35f;
	float CenterHeight = .4f;
	SELNavigationIdentity GeometryIdentity;
	bool HasGeometry = false;
	bool StreamsRoute = false;
	bool HasDeliveredPrefix = false;
	bool HasBlocked = false;
};
