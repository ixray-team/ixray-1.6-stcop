#pragma once
class ESceneObjectTool;

class UIObjectTool :
	public UIToolCustom
{
public:
	UIObjectTool();
	virtual ~UIObjectTool();
	virtual void Draw();

	void DrawRandomAppend();

	void DrawObjectsList();

	void RefreshList();
	IC const char* Current() { return m_Current; }
	virtual void OnDrawUI();
	ESceneObjectTool* ParentTools;

private:
	void RefreshListInternal();
	void OnItemFocused(ListItem*item);
	void SelByRefObject(bool flag);
	void MultiSelByRefObject(bool flag);
	void ClearSurface(bool selected);
	void HandleDragDrop();
	void LoadFromFile(xr_string& Outfile);

private:
	UIItemListForm* m_ObjectList;
	UIPropertiesForm* m_Props;

	bool m_MultiAppend;
	bool m_PropRandom;
	bool m_RandomAppend;
	bool m_Selection;
	bool bDrawList = true;

	float m_selPercent;

	const char* m_Current;

	xr_string RAIFile;

	ref_texture m_TextureNull;

	ImTextureID m_RealTexture;
	ImTextureID m_RemoveTexture;
};