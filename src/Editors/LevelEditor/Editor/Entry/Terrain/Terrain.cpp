#include "stdafx.h"
#include "Terrain.h"

CTerrain::CTerrain(LPVOID data, LPCSTR name):
	inherited(data,name), TerrainObject("terrain")
{
	Construct(data);
	FScale.set(1, 1, 1);
	m_RT_Flags.set(flRT_Visible, true);
}

void CTerrain::Construct(LPVOID data)
{
	FClassID = OBJCLASS_TERRAIN;
}

CTerrain::~CTerrain()
{
}

void CTerrain::OnUpdateTransform()
{
	inherited::OnUpdateTransform();
}

bool CTerrain::LoadStream(IReader& F)
{
	HMap.LoadSteam(&F);
	XRay::Editor::HeightmapUtils::GenerateMeshByHeightmap(HMap, &TerrainObject);

	return true;
}

void CTerrain::SaveStream(IWriter& F)
{
	inherited::SaveStream(F);
}

void CTerrain::OnFrame()
{
	CCustomObject::OnFrame();

	auto fequal = [](float a, float b, float eps = EPS)
	{
		return fabs(a - b) < eps;
	};

	if (!fequal(HMap.Pos.x, FPosition.x) || !fequal(HMap.Pos.y, FPosition.y) || !fequal(HMap.Pos.z, FPosition.z))
	{
		HMap.Pos = FPosition;
		HMap.MarkDirty();
	}

	if (!fequal(HMap.Size.x, FScale.x) || !fequal(HMap.Size.y, FScale.y) || !fequal(HMap.Size.z, FScale.z))
	{
		HMap.Size = FScale;
		HMap.MarkDirty();
	}
}

bool CTerrain::RayPick(float& dist, const Fvector& S, const Fvector& D, SRayPickInfo* pinf)
{
	if (!IsLoaded && !pinf->IsForcePickup)
		return false;

	if (LTools->GetTarget() == OBJCLASS_TERRAIN)
	{
		if (HMap.RayPick(dist, S, D, _ITransform(), pinf))
		{
			if (pinf) pinf->s_obj = this;
			return true;
		}
	}
	else
	{
		if (TerrainObject.RayPick(dist, S, D, _ITransform(), pinf))
		{
			if (pinf) pinf->s_obj = this;
			return true;
		}
	}

	return false;
}

void CTerrain::Render(int priority, bool strictB2F)
{
	if (LTools->GetTarget() == OBJCLASS_TERRAIN)
	{
		if (priority == 1)
		{
			HMap.Draw(100, 1.f, 0xffffff);
		}
	}
	else
	{
		TerrainObject.Render(_Transform(), priority, strictB2F);
	}
}

void CTerrain::Move(Fvector& amount)
{
	inherited::Move(amount);
	HMap.MarkDirty();
	HMap.Pos = FPosition;
}

void CTerrain::Scale(Fvector& amount)
{
	inherited::Scale(amount);
	HMap.MarkDirty();
	HMap.Size = FScale;
}
