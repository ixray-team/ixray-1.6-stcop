#include "stdafx.h"
#include "ESceneDOTools_Paint.h"
#include "ESceneDOTools.h"

TUI_ControlDOPaint::TUI_ControlDOPaint(int st, int act, ESceneToolBase* parent)
	: TUI_CustomControl(st, act, parent)
{
	Painting = false;
}

bool TUI_ControlDOPaint::Start(TShiftState Shift)
{
	if (Shift == ssRBOnly)
	{
		ExecCommand(COMMAND_SHOWCONTEXTMENU, parent_tool->FClassID);
		return false;
	}

	EDetailManager* Manager = (EDetailManager*)parent_tool;

	Manager->EnsureBaseTexture();

	Fvector Point;
	if (!Manager->PickPaintPoint(Point))
	{
		return false;
	}

	Manager->BrushPos = Point;
	Manager->BrushActive = true;
	Painting = true;
	Manager->PaintAt(Point);

	return true;
}

void TUI_ControlDOPaint::Move(TShiftState Shift)
{
	EDetailManager* Manager = (EDetailManager*)parent_tool;

	Manager->EnsureBaseTexture();

	Fvector Point;
	if (!Manager->PickPaintPoint(Point))
	{
		Manager->BrushActive = false;
		return;
	}

	Manager->BrushPos = Point;
	Manager->BrushActive = true;

	if (Painting && (Shift & ssLeft))
	{
		Manager->PaintAt(Point);
	}
}

bool TUI_ControlDOPaint::End(TShiftState Shift)
{
	EDetailManager* Manager = (EDetailManager*)parent_tool;

	if (Painting)
	{
		Painting = false;
		Scene->UndoSave();
	}

	return true;
}

bool TUI_ControlDOPaint::Wheel(int Direction, TShiftState Shift)
{
	EDetailManager* Manager = (EDetailManager*)parent_tool;

	if (Shift & ssCtrl)
	{
		float Step = (Direction > 0) ? 0.02f : -0.02f;
		Manager->BrushStrength = clampr(Manager->BrushStrength + Step, 0.01f, 1.f);
	}
	else
	{
		int Step = (Direction > 0) ? 2 : -2;
		Manager->BrushSize = clampr(Manager->BrushSize + Step, 1, 200);
	}

	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	return true;
}
