#include "stdafx.h"
#include "ESceneTerrainControls.h"
#include "ESceneTerrainTools.h"

TUI_ControlTerrainSculpt::TUI_ControlTerrainSculpt(int st, int act, ESceneToolBase* parent)
	: TUI_CustomControl(st, act, parent)
{
	m_Painting = false;
}

bool TUI_ControlTerrainSculpt::Start(TShiftState Shift)
{
	if (Shift == ssRBOnly)
	{
		ExecCommand(COMMAND_SHOWCONTEXTMENU, parent_tool->FClassID);
		return false;
	}

	ESceneTerrainTool* S = (ESceneTerrainTool*)parent_tool;

	Fvector p;
	float dist = 0.f;
	CTerrain* t = S->PickTerrain(dist, p);
	if (!t)
		return false;

	S->BeginSculpt(t, p);
	m_Painting = true;
	S->SculptTerrain(t, p);

	return true;
}

void TUI_ControlTerrainSculpt::Move(TShiftState Shift)
{
	ESceneTerrainTool* S = (ESceneTerrainTool*)parent_tool;

	Fvector p;
	float dist = 0.f;
	CTerrain* t = S->PickTerrain(dist, p);
	if (!t)
		return;

	S->m_BrushPos = p;

	if (m_Painting && (Shift & ssLeft) && t == S->m_EditedTerrain)
		S->SculptTerrain(t, p);
}

bool TUI_ControlTerrainSculpt::End(TShiftState Shift)
{
	ESceneTerrainTool* S = (ESceneTerrainTool*)parent_tool;

	if (m_Painting && S->m_EditedTerrain)
	{
		S->m_EditedTerrain = nullptr;
		m_Painting = false;
		Scene->UndoSave();
	}

	return true;
}

bool TUI_ControlTerrainSculpt::Wheel(int direction, TShiftState Shift)
{
	ESceneTerrainTool* S = (ESceneTerrainTool*)parent_tool;

	if (Shift & ssCtrl)
	{
		// ctrl + колесо — чувствительность кисти
		float step = (direction > 0) ? 0.005f : -0.005f;
		S->m_BrushStrength = clampr(S->m_BrushStrength + step, 0.001f, 0.2f);
	}
	else
	{
		// колесо — ширина (радиус) кисти
		int step = (direction > 0) ? 2 : -2;
		S->m_BrushSize = clampr(S->m_BrushSize + step, 1, 200);
	}

	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	return true;
}
