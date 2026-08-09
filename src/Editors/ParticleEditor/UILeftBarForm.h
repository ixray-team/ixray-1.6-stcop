#pragma once
class UILeftBarForm :
	public IEditorWnd
{
public:
	UILeftBarForm();
	virtual ~UILeftBarForm();
	virtual void Draw();
private:
	char m_SearchFilter[256];
};

