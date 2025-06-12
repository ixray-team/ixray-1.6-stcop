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
}

void CTerrain::Render(int priority, bool strictB2F)
{
	if (priority == 1)
	{
		HMap.Draw();
	}
}