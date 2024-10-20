#pragma once

class CLAItem;

class ECORE_API UIEditLightAnim :
	public IEditorWnd
{
public:
	UIEditLightAnim();
	virtual ~UIEditLightAnim();
	virtual void Draw();

public:
	static void Update();
	static void Show();
	static bool IsOpen();
private:
	static UIEditLightAnim* Form;

private:
	UIItemListForm*		m_Items;
	UIPropertiesForm*	m_Props;
	ref_texture			m_TextureNull;
	ImTextureID			m_Texture;

	ID3DTexture2D* m_ItemTexture;
	CLAItem* m_CurrentItem;
	bool m_Modife;

	float m_PointerWeight;
	bool m_PointerResize;
	ID3DTexture2D* m_PointerTexture;
	u32* m_PointerRawImage;
	int m_PointerValue;
	bool m_RenderAlpha;

private:
	void RenderItem();
	void InitializeItems();
	void UpdateProperties();
	void OnCreateKeyClick();
	void OnModified();
	void OnItemFocused(ListItem*);
	bool OnFrameCountAfterEdit(PropValue* v, s32& val);
	void OnCloneItem(LPCSTR parent_path, LPCSTR new_full_name);
	void OnCreateItem(LPCSTR path);
	void OnRemoveItem(LPCSTR name, EItemType type);
	void OnRenameItem(LPCSTR old_full_name, LPCSTR new_full_name, EItemType type);

	void RenderPointer();
	void FillRectPointer(const ImVec4& rect, u32 color, bool plus_one = false);
	void FrameRectPointer(const ImVec4& rect, u32 color);
};