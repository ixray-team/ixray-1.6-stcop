#include "TLegacyRenderGraph.h"

#include "Visual/XRayFHierrarhyVisual.h"

void TLegacyRenderGraph::Clear()
{
	RenderList.clear();
}

void TLegacyRenderGraph::ClearViews()
{
	Views.clear();
	ViewMasks.clear();
}

void TLegacyRenderGraph::PushView(CFrustum* View)
{
	Views.push_back(View);
	ViewMasks.push_back(View->getMask());
}

void TLegacyRenderGraph::SetSsaThresholds(float DiscardThreshold, float GlodStartThreshold, float GlodEndThreshold)
{
	SsaDiscardThreshold = DiscardThreshold;
	GlodSsaStartThreshold = GlodStartThreshold;
	GlodSsaEndThreshold = GlodEndThreshold;
}

bool TLegacyRenderGraph::IsValuableToRender(CDS0_RenderVisual* Visual) const
{
	const float SphereVolume = Visual->getVisData().sphere.volume();
	const float Distance = DevicePtr->vCameraPosition.distance_to(Visual->Vis.sphere.P);
	const float FovFactor = 70.f / DevicePtr->fFOV;
	const float AdjustedDistance = Distance / FovFactor;

	if (SphereVolume < 60.f && AdjustedDistance > 45.f)
	{
		return false;
	}
	if (SphereVolume < 400.f && AdjustedDistance > 100.f)
	{
		return false;
	}
	if (SphereVolume < 2000.f && AdjustedDistance > 160.f)
	{
		return false;
	}
	if (SphereVolume < 3800.f && AdjustedDistance > 200.f)
	{
		return false;
	}
	if (SphereVolume < 13000.f && AdjustedDistance > 300.f)
	{
		return false;
	}

	return true;
}

bool TLegacyRenderGraph::IsVisible(CDS0_RenderVisual* Visual, bool& OutPartial) const
{
	if (Views.empty())
	{
		OutPartial = false;
		return true;
	}

	vis_data& Vis = Visual->Vis;
	bool Visible = false;
	OutPartial = false;

	for (u32 i = 0; i < Views.size(); i++)
	{
		u32 Mask = ViewMasks[i];
		const EFC_Visible Result = Views[i]->testSAABB(Vis.sphere.P, Vis.sphere.R, Vis.box.data(), Mask);
		if (Result == fcvNone)
		{
			continue;
		}

		Visible = true;
		OutPartial = OutPartial || (Result == fcvPartial);
	}

	return Visible;
}

bool TLegacyRenderGraph::ShouldAddLeaf(CDS0_RenderVisual* Visual) const
{
	float DistanceSq = 0.f;
	const float Ssa = CalculateSsa(Visual, DistanceSq);
	return Ssa > SsaDiscardThreshold;
}

void TLegacyRenderGraph::AddStatic(CDS0_RenderVisual* Visual)
{
	if (!Visual)
	{
		return;
	}

	if (!IsValuableToRender(Visual))
	{
		return;
	}

	bool Partial = false;
	if (!IsVisible(Visual, Partial))
	{
		return;
	}
	
	CDS0_FHierrarhyVisual* HierarchyVisual = nullptr;
	if (Visual->Type == MT_HIERRARHY)
	{
		HierarchyVisual = static_cast<CDS0_FHierrarhyVisual*>(Visual);
	}
	if (!HierarchyVisual)
	{
		InsertStatic(Visual);
		return;
	}

	for (CDS0_RenderVisual* Child : HierarchyVisual->children)
	{
		if (Partial)
		{
			AddStatic(Child);
		}
		else
		{
			AddStaticLeafs(Child);
		}
	}
}

void TLegacyRenderGraph::AddStaticLeafs(CDS0_RenderVisual* Visual)
{
	if (!Visual)
	{
		return;
	}

	if (!IsValuableToRender(Visual))
	{
		return;
	}
	
	CDS0_FHierrarhyVisual* HierarchyVisual = nullptr;
	if (Visual->Type == MT_HIERRARHY)
	{
		HierarchyVisual = static_cast<CDS0_FHierrarhyVisual*>(Visual);
	}
	
	if (!HierarchyVisual)
	{
		InsertStatic(Visual);
		return;
	}

	for (CDS0_RenderVisual* Child : HierarchyVisual->children)
	{
		AddStaticLeafs(Child);
	}
}

void TLegacyRenderGraph::InsertStatic(CDS0_RenderVisual* Visual)
{
	float DistanceSq = 0.f;
	const float Ssa = CalculateSsa(Visual, DistanceSq);
	if (Ssa <= SsaDiscardThreshold)
	{
		return;
	}

	const auto FoundVisual = std::find_if(RenderList.begin(), RenderList.end(), [Visual](const FLegacyVisualRenderItem& Item)
	{
		return Item.Owner == Visual;
	});

	if (FoundVisual != RenderList.end())
	{
		return;
	}
	FLegacyVisualRenderItem RenderItem;
	if (Visual->MakeRenderItem(CalculateLod(Ssa),RenderItem))
	{
		RenderList.emplace_back(RenderItem);
	}
}

float TLegacyRenderGraph::CalculateSsa(CDS0_RenderVisual* Visual, float& OutDistanceSq) const
{
	OutDistanceSq = DevicePtr->vCameraPosition.distance_to_sqr(Visual->Vis.sphere.P) + EPS;
	return Visual->Vis.sphere.R / OutDistanceSq;
}

float TLegacyRenderGraph::CalculateLod(float Ssa) const
{
	const float Range = GlodSsaStartThreshold - GlodSsaEndThreshold;
	if (Range <= EPS)
	{
		return 1.f;
	}

	return _sqrt(clampr((Ssa - GlodSsaEndThreshold) / Range, 0.f, 1.f));
}
