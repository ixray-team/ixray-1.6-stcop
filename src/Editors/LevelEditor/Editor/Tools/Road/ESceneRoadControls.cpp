#include "stdafx.h"
#include "ESceneRoadControls.h"
#include "ESceneRoadTools.h"

// Add action in road point mode (Select / Move are the standard controls, see CRoad):
//   LMB on ground         - append a point to the end of the selected road (creates a road if none)
//   Ctrl + LMB on ground  - insert a point into the nearest segment
//   Alt + LMB on a point  - delete the point
// A freshly placed point can be dragged while the button is held.

TUI_ControlRoadEdit::TUI_ControlRoadEdit(int st, int act, ESceneToolBase* parent)
	: TUI_CustomControl(st, act, parent)
{
}

CRoad* TUI_ControlRoadEdit::PickAnyPoint(int& OutIndex) const
{
	ESceneRoadTool* Tool = (ESceneRoadTool*)parent_tool;
	const Fvector& Start = EContext.UI->m_CurrentRStart;
	const Fvector& Dir = EContext.UI->m_CurrentRDir;

	CRoad* Best = nullptr;
	float BestT = flt_max;
	OutIndex = -1;

	for (CCustomObject* Obj : Tool->GetObjects())
	{
		CRoad* Road = (CRoad*)Obj;
		if (!Road->Visible() || Road->Locked())
		{
			continue;
		}

		float T = flt_max;
		const int Hit = Road->PickPointRay(Start, Dir, &T);
		if (Hit >= 0 && T < BestT)
		{
			BestT = T;
			Best = Road;
			OutIndex = Hit;
		}
	}
	return Best;
}

bool TUI_ControlRoadEdit::Start(TShiftState Shift)
{
	if (Shift == ssRBOnly)
	{
		ExecCommand(COMMAND_SHOWCONTEXTMENU, parent_tool->FClassID);
		return false;
	}

	ESceneRoadTool* Tool = (ESceneRoadTool*)parent_tool;

	if (Shift & ssAlt)
	{
		int Hit = -1;
		if (CRoad* HitRoad = PickAnyPoint(Hit))
		{
			Tool->DeletePoint(HitRoad, Hit);
		}
		return false;
	}

	Fvector P, N;
	if (!LUI->PickGround(P, EContext.UI->m_CurrentRStart, EContext.UI->m_CurrentRDir, 1, &N))
	{
		return false;
	}

	CRoad* Road = Tool->GetActiveRoad();
	if (!Road)
	{
		Road = Tool->CreateRoad("road");
	}

	u32 Index = u32(Road->Spline.Points.size());
	if ((Shift & ssCtrl) && Road->Spline.Points.size() >= 2)
	{
		Fvector Local;
		Road->_ITransform().transform_tiny(Local, P);
		Index = Road->Spline.FindInsertIndex(Local);
	}

	const float Width = Road->Spline.Points.empty() ? Tool->m_DefaultWidth : Road->Spline.Points[std::min<size_t>(Index, Road->Spline.Points.size() - 1)].Width;
	Index = Road->InsertPointWorld(Index, P, Width);
	Road->SelectPoint(Index, false);
	Road->RebuildMesh();

	m_DragRoad = Road;
	m_DragPoint = int(Index);
	m_Moved = true;
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	return true;
}

void TUI_ControlRoadEdit::Move(TShiftState Shift)
{
	if (!m_DragRoad || m_DragPoint < 0 || !(Shift & ssLeft) || m_DragPoint >= int(m_DragRoad->Spline.Points.size()))
	{
		return;
	}

	Fvector P, N;
	if (LUI->PickGround(P, EContext.UI->m_CurrentRStart, EContext.UI->m_CurrentRDir, 1, &N))
	{
		m_DragRoad->SetPointWorld(u32(m_DragPoint), P);
		m_DragRoad->UpdateTransform(true);
	}
}

bool TUI_ControlRoadEdit::End(TShiftState Shift)
{
	if (m_DragRoad && m_Moved)
	{
		m_DragRoad->RebuildMesh();
		Scene->UndoSave();
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
	}

	m_DragRoad = nullptr;
	m_DragPoint = -1;
	m_Moved = false;
	return true;
}
