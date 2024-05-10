#pragma once

// refs
class ESceneAIMapTool;

#define estAIMapNode 	0

class TUI_ControlAIMapNodeAdd: public TUI_CustomControl{
	int 			append_nodes;
public:
    TUI_ControlAIMapNodeAdd(int st, int act, ESceneToolBase* parent);
	virtual bool Start  (TShiftState _Shift);
	virtual bool End    (TShiftState _Shift);
	virtual void Move   (TShiftState _Shift);
};

class TUI_ControlAIMapNodeSelect : public TUI_CustomControl
{
public:
	TUI_ControlAIMapNodeSelect(int st, int act, ESceneToolBase* parent);
	virtual void MoveStart();

	virtual void MoveProcess(Fvector Delta, Fvector Vector);
	virtual void RotateProcess(float Delta);
	virtual bool IsSupportScale() { return false; }
};
