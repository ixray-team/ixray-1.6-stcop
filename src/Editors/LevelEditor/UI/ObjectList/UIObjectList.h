#pragma once
class CCustomObject;
class UIObjectList:public IEditorWnd
{
	friend class UIObjectListItem;
public:
	UIObjectList();
	virtual ~UIObjectList();
	virtual void Draw();
	static void Update();
	static void Show();
	static void Close();
	static IC bool IsOpen()  { return Form; }
	static void Refresh();
private:
	static UIObjectList* Form;
private:
	void DrawObjects();
	void DrawObject(CCustomObject* obj,const char*name);
private:
	ObjClassID m_cur_cls;
	enum EMode
	{
		M_All,
		M_Visible,
		M_Inbvisible
	};
	EMode m_Mode;
	string_path m_Filter;
	UIObjectListItem m_Root;
	UIObjectListItem* m_LastSelected;
};