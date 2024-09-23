#include "stdafx.h"

void ESceneGlowTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
	// AddControl(new TUI_CustomControl(estDefault, etaSelect, this));
    m_Flags.zero	();
}


void ESceneGlowTool::RemoveControls()
{
	inherited::RemoveControls();
}


void ESceneGlowTool::FillProp(LPCSTR pref, PropItemVec& items)
{
    PHelper().CreateFlag32(items, PrepareKey(pref,g_pStringTable->translate("ed_st_test_vis").c_str()),	&m_Flags,	flTestVisibility);
    PHelper().CreateFlag32(items, PrepareKey(pref,g_pStringTable->translate("ed_st_draw_cross").c_str()),		&m_Flags,	flDrawCross);
	inherited::FillProp(pref, items);
}

CCustomObject* ESceneGlowTool::CreateObject(LPVOID data, LPCSTR name)
{
	CCustomObject* O	= new CGlow(data,name);
    O->FParentTools		= this;
    return O;
}


