#include "stdafx.h"
#include "dxStatsRender.h"
#include "../../xrEngine/GameFont.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include "../../xrEngine/Rain.h"
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
	const NriStats& Nri = RImplementation.NriPasses.Shown;
	F.OutNext	("DYN:         %u nri, %u dropped",	Nri.Dynamic, Nri.Dropped);
	const auto& Graph = R_dsgraph_structure::PacketsDrawn;
	F.OutNext	("GRAPH:       %u main, %u shadow, %u faces",	Graph[0].load(), Graph[1].load(), Graph[2].load());
	F.OutNext	("NRI D3D11:   %u runs, %u submits",	Nri.Runs, Nri.Submits);
	if (g_pGamePersistent && g_pGamePersistent->pEnvironment && g_pGamePersistent->pEnvironment->eff_Rain)
	{
		g_pGamePersistent->pEnvironment->eff_Rain->Statistics(F);
	}
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
