#pragma once

class EDetailManager;
class TUI_ControlDOPaint : public TUI_CustomControl
{
public:
	TUI_ControlDOPaint(int st, int act, ESceneToolBase* parent);
	virtual bool Start(TShiftState Shift) override;
	virtual void Move(TShiftState Shift) override;
	virtual bool End(TShiftState Shift) override;
	virtual bool Wheel(int Direction, TShiftState Shift) override;
private:
	bool Painting;
};
