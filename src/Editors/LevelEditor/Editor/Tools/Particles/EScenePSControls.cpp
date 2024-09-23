#include "stdafx.h"

 TUI_ControlPSAdd::TUI_ControlPSAdd(int st, int act, ESceneToolBase* parent):TUI_CustomControl(st,act,parent)
 {
}

bool  TUI_ControlPSAdd::AfterAppendCallback(TShiftState Shift, CCustomObject* obj)
{
	EParticlesObject* pg= dynamic_cast<EParticlesObject*>(obj); R_ASSERT(pg);
    LPCSTR ref_name		= ((UIParticlesTool*)parent_tool->pForm)->Current();
    if (!ref_name){
    	ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_nothing_selected").c_str());
    	return false;
    }
	if (!pg->Compile(ref_name)){
    	ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_cant_compile_ps").c_str(), ref_name);
        return false;
    }
    return true;
}

bool  TUI_ControlPSAdd::Start(TShiftState Shift)
{
    DefaultAddObject(Shift,0, TAfterAppendCallback(this,&TUI_ControlPSAdd::AfterAppendCallback));
    return false;
}

void  TUI_ControlPSAdd::Move(TShiftState _Shift)
{
}
bool  TUI_ControlPSAdd::End(TShiftState _Shift)
{
    return true;
}


