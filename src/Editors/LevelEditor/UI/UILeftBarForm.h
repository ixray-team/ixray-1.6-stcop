#pragma once
class UILeftBarForm :
	public IEditorWnd
{
public:
	UILeftBarForm();
	virtual ~UILeftBarForm();
	virtual void Draw();
	IC bool IsSnapListMode()const { return m_SnapListMode; }

	IC bool IsUseSnapList()const { return bDrawSnapListObjects; }
	IC void ShowSnapList(bool v) { bUseSnapList = v; }

	IC bool IsUseObjectsTool()const { return bUseObjectsTool; }
	IC void ShowObjectsTool(bool v) { bUseObjectsTool = v; }

private:
	bool bDrawSnapListObjects;
	bool bUseSnapList;
	bool bUseObjectsTool;
	bool m_SnapListMode;
	int m_SnapItem_Current;
};

