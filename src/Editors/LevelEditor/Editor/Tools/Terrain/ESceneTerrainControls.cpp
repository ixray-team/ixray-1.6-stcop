#include "stdafx.h"
#include "ESceneTerrainControls.h"
#include "ESceneTerrainTools.h"

// Controls in sculpt mode:
//   LMB            - paint with the current brush
//   Ctrl + LMB     - inverted brush (Raise <-> Lower, negative noise, restore holes)
//   Alt + LMB      - pick the Flatten target height under the cursor
//   Wheel          - brush radius
//   Ctrl + Wheel   - brush strength
//   Alt + Wheel    - brush hardness
//   Shift          - camera (handled by the viewport)

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
	{
		return false;
	}

	S->m_HoverTerrain = t;
	S->m_BrushPos = p;
	S->m_BrushActive = true;

	if (Shift & ssAlt)
	{
		S->PickFlattenTarget(t, p);
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		return false;
	}

	m_Painting = true;
	S->BeginSculpt(t, p, IsInvert(Shift));
	return true;
}

void TUI_ControlTerrainSculpt::Move(TShiftState Shift)
{
	ESceneTerrainTool* S = (ESceneTerrainTool*)parent_tool;

	// Hole strokes: the surface under the cursor disappears while painting, so a surface pick
	// would fall through the new hole onto terrain further away. Keep the stroke on a plane.
	if (m_Painting && (Shift & ssLeft) && S->m_EditedTerrain && S->m_BrushMode == ESceneTerrainTool::bmHole)
	{
		const Fvector& RS = EContext.UI->m_CurrentRStart;
		const Fvector& RD = EContext.UI->m_CurrentRDir;
		if (!fis_zero(RD.y))
		{
			const float T = (S->m_StrokePlaneY - RS.y) / RD.y;
			if (T > 0.f)
			{
				Fvector p;
				p.mad(RS, RD, T);
				S->m_BrushPos = p;
				S->m_BrushActive = true;
				S->StrokeTo(S->m_EditedTerrain, p, IsInvert(Shift));
			}
		}
		return;
	}

	Fvector p;
	float dist = 0.f;
	CTerrain* t = S->PickTerrain(dist, p);
	S->m_HoverTerrain = t;
	S->m_BrushActive = (t != nullptr);
	if (!t)
	{
		return;
	}

	S->m_BrushPos = p;

	if (m_Painting && (Shift & ssLeft) && t == S->m_EditedTerrain)
	{
		S->StrokeTo(t, p, IsInvert(Shift));
	}
}

bool TUI_ControlTerrainSculpt::End(TShiftState Shift)
{
	ESceneTerrainTool* S = (ESceneTerrainTool*)parent_tool;

	if (m_Painting)
	{
		m_Painting = false;
		S->EndSculpt();
	}

	return true;
}

bool TUI_ControlTerrainSculpt::Wheel(int direction, TShiftState Shift)
{
	ESceneTerrainTool* S = (ESceneTerrainTool*)parent_tool;
	const float Dir = (direction > 0) ? 1.f : -1.f;

	if (Shift & ssCtrl)
	{
		S->m_BrushStrength = clampr(S->m_BrushStrength * (Dir > 0 ? 1.15f : 1.f / 1.15f), 0.0005f, 0.2f);
	}
	else if (Shift & ssAlt)
	{
		S->m_BrushHardness = clampr(S->m_BrushHardness + Dir * 0.05f, 0.f, 1.f);
	}
	else
	{
		// Multiplicative step: fine control for small brushes, fast for big ones
		S->m_BrushSize = clampr(S->m_BrushSize * (Dir > 0 ? 1.1f : 1.f / 1.1f), 0.5f, 500.f);
	}

	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	return true;
}

bool TUI_ControlTerrainSculpt::IsInvert(TShiftState Shift) const
{
	const ESceneTerrainTool* S = (const ESceneTerrainTool*)parent_tool;
	const bool Ctrl = (Shift & ssCtrl) != 0;
	return S->m_BrushInvert != Ctrl;
}
