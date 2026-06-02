#pragma once

class CDS0_StatsRender:public IStatsRender
{
public:
	CDS0_StatsRender();
	virtual void Copy(IStatsRender &_in) ;
	virtual void OutData1(CGameFont &F) ;
	virtual void OutData2(CGameFont &F) ;
	virtual void OutData3(CGameFont &F) ;
	virtual void OutData4(CGameFont &F) ;
	virtual void GuardVerts(CGameFont &F) ;
	virtual void GuardDrawCalls(CGameFont &F) ;
	virtual void SetDrawParams(IRenderDeviceRender *pRender) ;
};
