#pragma once
// refs
class ESceneWallmarkTools;


class TUI_ControlWallmarkAdd: public TUI_CustomControl{
	u32 wm_cnt;
public:
    TUI_ControlWallmarkAdd(int st, int act, ESceneToolBase* parent);
	virtual bool Start  (TShiftState _Shift);
	virtual bool End    (TShiftState _Shift);
	virtual void Move   (TShiftState _Shift);
};


class TUI_ControlWallmarkSelect: public TUI_CustomControl
{
public:
	TUI_ControlWallmarkSelect(int st, int act, ESceneToolBase* parent);	
	virtual bool IsSupportMove() { return false; }
	virtual bool IsSupportRotate() { return false; }
	virtual bool IsSupportScale() { return false; }
};