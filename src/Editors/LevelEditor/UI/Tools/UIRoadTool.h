#pragma once
class ESceneRoadTool;
class UIRoadTool : public UIToolCustom
{
public:
	UIRoadTool() = default;
	virtual ~UIRoadTool() = default;
	virtual void Draw() override;
	virtual void OnDrawUI() override {}
	ESceneRoadTool* tool = nullptr;
};
