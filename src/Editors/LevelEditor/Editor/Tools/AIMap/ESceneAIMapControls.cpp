#include "stdafx.h"

// Node Add
TUI_ControlAIMapNodeAdd::TUI_ControlAIMapNodeAdd(int st, int act, ESceneToolBase* parent):TUI_CustomControl(st,act,parent)
{
}

bool  TUI_ControlAIMapNodeAdd::Start(TShiftState Shift)
{
    append_nodes = 0;                           
	Fvector p;
    ESceneAIMapTool* S 		= (ESceneAIMapTool*)parent_tool;
    if (S->PickObjects(p,UI->m_CurrentRStart,UI->m_CurrentRDir,UI->ZFar())){
    	S->SelectObjects	(false);
	    append_nodes		= S->AddNode(p,((UIAIMapTool*)S->pForm)->IsIgnoreConstraints(),((UIAIMapTool*)S->pForm)->IsAutoLink(),S->m_BrushSize);
		if (Shift&ssAlt){ 
		    if (append_nodes) Scene->UndoSave();
        	ResetActionToSelect();
            return false;
        }else return true;
    }
    return false;
}
void TUI_ControlAIMapNodeAdd::Move(TShiftState _Shift)
{
    Fvector p;
    ESceneAIMapTool* S 	= (ESceneAIMapTool*)parent_tool;
    if (S->PickObjects(p,UI->m_CurrentRStart,UI->m_CurrentRDir,UI->ZFar())){
	    append_nodes+=S->AddNode(p,((UIAIMapTool*)S->pForm)->IsIgnoreConstraints(),((UIAIMapTool*)S->pForm)->IsAutoLink(),S->m_BrushSize);
    }
}
bool TUI_ControlAIMapNodeAdd::End(TShiftState _Shift)
{
	if (!(_Shift&ssAlt)) ResetActionToSelect();
    if (append_nodes) Scene->UndoSave();
	return true;
}


TUI_ControlAIMapNodeSelect::TUI_ControlAIMapNodeSelect(int st, int act, ESceneToolBase* parent) :TUI_CustomControl(st, act, parent)
{
}

void TUI_ControlAIMapNodeSelect::MoveStart()
{
	AINodeVec& lst = ((ESceneAIMapTool*)parent_tool)->Nodes();
	for (AINodeIt _F = lst.begin(); _F != lst.end(); _F++)
	{
		if ((*_F)->flags.is(SAINode::flSelected))
		{
			(*_F)->SavePos = (*_F)->Pos;
		}
	}
	
}


void TUI_ControlAIMapNodeSelect::MoveProcess(Fvector Delta, Fvector Vector)
{
	AINodeVec& lst = ((ESceneAIMapTool*)parent_tool)->Nodes();
	for (AINodeIt _F = lst.begin(); _F != lst.end(); _F++)
		if ((*_F)->flags.is(SAINode::flSelected)) {
            (*_F)->Pos = (*_F)->SavePos;
            (*_F)->Pos.mad(Delta, Vector);
			if (LTools->GetGimzo()->IsStepEnable(Gizmo::EType::Move) && abs(Vector.y) > EPS)
			{
				(*_F)->Pos.y = snapto((*_F)->Pos.y, LTools->GetGimzo()->GetStep(Gizmo::EType::Move));
			}
			(*_F)->Plane.build((*_F)->Pos, (*_F)->Plane.n);
		}
}

void TUI_ControlAIMapNodeSelect::RotateProcess(float Delta)
{
	Fmatrix R;
	if (fis_zero(m_RotateVector.x)) 	R.rotateZ(Delta);
	else								R.rotateX(-Delta);

	AINodeVec& lst = ((ESceneAIMapTool*)parent_tool)->Nodes();
	for (AINodeIt _F = lst.begin(); _F != lst.end(); _F++)
		if ((*_F)->flags.is(SAINode::flSelected)) {
			Fvector 	new_n;
			R.transform_dir(new_n, (*_F)->Plane.n);
			if (Fvector().set(0, 1, 0).dotproduct(new_n) > 0.02f) {
				(*_F)->Plane.build((*_F)->Pos, new_n);
			}
		}
}