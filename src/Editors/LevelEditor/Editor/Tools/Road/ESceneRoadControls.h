#pragma once

class ESceneRoadTool;
class CRoad;

class TUI_ControlRoadEdit : public TUI_CustomControl
{
public:
	TUI_ControlRoadEdit(int st, int act, ESceneToolBase* parent);
	virtual bool Start(TShiftState Shift) override;
	virtual void Move(TShiftState Shift) override;
	virtual bool End(TShiftState Shift) override;

private:
	// Closest control point under the cursor over all visible roads
	CRoad* PickAnyPoint(int& OutIndex) const;

	CRoad* m_DragRoad = nullptr;
	int m_DragPoint = -1;
	bool m_Moved = false;
};
