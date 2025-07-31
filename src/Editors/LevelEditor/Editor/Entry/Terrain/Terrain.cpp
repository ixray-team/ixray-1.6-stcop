#include "stdafx.h"
#include "Terrain.h"

CTerrain::CTerrain(LPVOID data, LPCSTR name):
	inherited(data,name)
{
	Construct(data);
	FScale.set(1, 0.3f, 1);
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
	return HMap.LoadSteam(&F);
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

void CTerrain::Render(int priority, bool strictB2F)
{
	if (priority == 1)
	{
		HMap.Draw(100, 1, 0xffffff);
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
