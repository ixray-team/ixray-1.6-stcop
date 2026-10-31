#include "stdafx.h"
#include "dxStatsRender.h"
#include "../../xrEngine/GameFont.h"
#include "dxRenderDeviceRender.h"

void dxStatsRender::Copy(IStatsRender&_in)
{
	*this = *((dxStatsRender*)&_in);
}

void dxStatsRender::OutData1 (CGameFont &F)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	F.OutNext	("POLY:        %d/%d",		Cmd.Stats.Polys, Cmd.Stats.Calls ? Cmd.Stats.Polys / Cmd.Stats.Calls : 0);
	F.OutNext	("DIP/DP:      %d",			Cmd.Stats.Calls);
}

void dxStatsRender::OutData2 (CGameFont &F)
{
}

void dxStatsRender::OutData3 (CGameFont &F)
{
}

void dxStatsRender::OutData4 (CGameFont &F)
{
}

void dxStatsRender::GuardVerts (CGameFont &F)
{
}

void dxStatsRender::GuardDrawCalls (CGameFont &F)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	if (Cmd.Stats.Calls > 1000)		F.OutNext	("DIP/DP    > 1k:   %d",	Cmd.Stats.Calls);
}

void dxStatsRender::SetDrawParams (IRenderDeviceRender *pRender)
{
	dxRenderDeviceRender *pR = (dxRenderDeviceRender*) pRender;
	GfxCmdList& Cmd = RImplementation.Imm();

	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetPass		(pR->m_SelectionShader);
	Cmd.Constants.Write<CBMaterial>().tfactor.set(1, 1, 1, 1);
}