#include "stdafx.h"

// Node Add
TUI_ControlWallmarkAdd::TUI_ControlWallmarkAdd(int st, int act, ESceneToolBase* parent):TUI_CustomControl(st,act,parent) {}

bool  TUI_ControlWallmarkAdd::Start(TShiftState Shift)
{
    ESceneWallmarkTool* S = (ESceneWallmarkTool*)parent_tool;
    
    S->SelectObjects(false);
    wm_cnt = 0;
	if (S->AddWallmark(EContext.UI->m_CurrentRStart, EContext.UI->m_CurrentRDir))
    {
        wm_cnt++;
        if (!(Shift&ssAlt))
        { 
            Scene->UndoSave();
            ResetActionToSelect();
            return false;
        }else return true;
    }
    return false;
}
void TUI_ControlWallmarkAdd::Move(TShiftState _Shift) {}
bool TUI_ControlWallmarkAdd::End(TShiftState _Shift)
{
    if (!(_Shift&ssAlt))ResetActionToSelect();
    if (wm_cnt)
        Scene->UndoSave();
    return true;
}

// WM Move
TUI_ControlWallmarkMove::TUI_ControlWallmarkMove(int st, int act, ESceneToolBase* parent) : TUI_CustomControl(st, act, parent) {}

bool TUI_ControlWallmarkMove::Start(TShiftState Shift)
{
	// Plain drag is the gizmo; Ctrl drags the wallmark over the surface under the cursor
	wm_cnt = 0;
	if (!(Shift & ssCtrl))
	{
		return false;
	}

	ESceneWallmarkTool* S = (ESceneWallmarkTool*)parent_tool;
	if (S->MoveSelectedWallmarkTo(EContext.UI->m_CurrentRStart, EContext.UI->m_CurrentRDir))
	{
		wm_cnt++;
	}
	return true;
}

void TUI_ControlWallmarkMove::Move(TShiftState _Shift)
{
	ESceneWallmarkTool* S = (ESceneWallmarkTool*)parent_tool;
	if (S->MoveSelectedWallmarkTo(EContext.UI->m_CurrentRStart, EContext.UI->m_CurrentRDir))
	{
		wm_cnt++;
		EContext.UI->RedrawScene();
	}
}

bool TUI_ControlWallmarkMove::End(TShiftState _Shift)
{
	if (wm_cnt)
	{
		Scene->UndoSave();
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
	}
	return true;
}
