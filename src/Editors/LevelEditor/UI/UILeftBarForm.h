#pragma once
class UILeftBarForm :
	public IEditorWnd
{
public:
	UILeftBarForm();
	virtual ~UILeftBarForm();
	virtual void Draw();
	virtual void Serialize(nlohmann::json& Data, bool IsSaving) override;
	void DrawObjectTool(ImVec2& WindowPadding, float PannelPadding, ImVec2& ItemSpacing);
	IC bool IsSnapListMode()const { return m_SnapListMode; }

	IC bool IsUseSnapList()const { return bDrawSnapListObjects; }
	IC bool IsShowSnapList()const { return bUseSnapList; }
	IC void ShowSnapList(bool v) { bUseSnapList = v; }

private:
	bool bDrawSnapListObjects;
	bool bUseSnapList;
	bool m_SnapListMode;
	int m_SnapItem_Current;
};

