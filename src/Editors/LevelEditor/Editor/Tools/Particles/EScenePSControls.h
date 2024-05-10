#pragma once
class TUI_ControlPSToolsSelect : public TUI_CustomControl
{
public:
	TUI_ControlPSToolsSelect(int st, int act, ESceneToolBase* parent) :TUI_CustomControl(st, act, parent) {}
	virtual bool IsSupportRotate() { return false; }
	virtual bool IsSupportScale() { return false; }
};
class TUI_ControlPSAdd: public TUI_CustomControl
{
	bool 	AfterAppendCallback(TShiftState Shift, CCustomObject* obj);
public:
    TUI_ControlPSAdd(int st, int act, ESceneToolBase* parent);
    virtual ~TUI_ControlPSAdd(){;}
	virtual bool Start  (TShiftState _Shift);
	virtual bool End    (TShiftState _Shift);
	virtual void Move   (TShiftState _Shift);
};