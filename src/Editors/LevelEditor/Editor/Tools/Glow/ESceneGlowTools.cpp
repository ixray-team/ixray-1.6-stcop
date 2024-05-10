#include "stdafx.h"

class TUI_ControlGlowSelect : public TUI_CustomControl
{
public:
	TUI_ControlGlowSelect(int st, int act, ESceneToolBase* parent) :TUI_CustomControl(st, act, parent) {}
	virtual bool IsSupportRotate() { return false; }
};

void ESceneGlowTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
	AddControl(xr_new<TUI_ControlGlowSelect>(estDefault, etaSelect, this));
    m_Flags.zero	();
}


void ESceneGlowTool::RemoveControls()
{
	inherited::RemoveControls();
}


void ESceneGlowTool::FillProp(LPCSTR pref, PropItemVec& items)
{
    PHelper().CreateFlag32(items, PrepareKey(pref,"Common\\Test Visibility"),	&m_Flags,	flTestVisibility);
    PHelper().CreateFlag32(items, PrepareKey(pref,"Common\\Draw Cross"),		&m_Flags,	flDrawCross);
	inherited::FillProp(pref, items);
}

CCustomObject* ESceneGlowTool::CreateObject(LPVOID data, LPCSTR name)
{
	CCustomObject* O	= xr_new<CGlow>(data,name);
    O->FParentTools		= this;
    return O;
}


