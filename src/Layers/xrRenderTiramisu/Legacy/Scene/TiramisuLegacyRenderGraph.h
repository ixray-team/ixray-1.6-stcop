#pragma once

#include "TiramisuRenderTypes.h"
#include "Legacy/Visual/XRayRenderVisual.h"

// Адаптер старой геометрии уровня к порядку проходов Tiramisu; не является новым render graph.
class TiramisuLegacyRenderGraph
{
public:
	using TRenderList = xr_vector<FLegacyVisualRenderItem>;

	void Clear();
	void ClearViews();
	void PushView(CFrustum* View);
	void SetSsaThresholds(float DiscardThreshold, float GlodStartThreshold, float GlodEndThreshold);

	void AddStatic(CDS0_RenderVisual* Visual);
	void AddStaticLeafs(CDS0_RenderVisual* Visual);


	TRenderList RenderList;

private:
	bool IsValuableToRender(CDS0_RenderVisual* Visual) const;
	bool IsVisible(CDS0_RenderVisual* Visual, bool& OutPartial) const;
	bool ShouldAddLeaf(CDS0_RenderVisual* Visual) const;
	void InsertStatic(CDS0_RenderVisual* Visual);
	float CalculateSsa(CDS0_RenderVisual* Visual, float& OutDistanceSq) const;
	float CalculateLod(float Ssa) const;

	xr_vector<CFrustum*> Views;
	xr_vector<u32> ViewMasks;
	float SsaDiscardThreshold = 0.f;
	float GlodSsaStartThreshold = 1.f;
	float GlodSsaEndThreshold = 0.f;
};
