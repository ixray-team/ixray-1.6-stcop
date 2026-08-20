#pragma once

class ESceneTerrainTool;
class TUI_ControlTerrainSculpt : public TUI_CustomControl
{
public:
	TUI_ControlTerrainSculpt(int st, int act, ESceneToolBase* parent);
	virtual bool Start(TShiftState Shift) override;
	virtual void Move(TShiftState Shift) override;
	virtual bool End(TShiftState Shift) override;
	virtual bool Wheel(int direction, TShiftState Shift) override;
private:
	bool m_Painting;
};
