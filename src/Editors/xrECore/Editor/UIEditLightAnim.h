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
	CLAItem* m_CurrentItem;
	bool m_Modife;

	int m_PointerValue;
	bool m_RenderAlpha;

	// Preview playback
	bool Playing = false;
	bool Loop = true;
	float PlayTime = 0.f;

	// Timeline interaction
	int DraggedKey = -1;
	bool Scrubbing = false;
	float TimelineZoom = 1.f;
	u32 CopiedColor = 0xFFFFFFFF;
	bool HasCopiedColor = false;
	string256 SearchFilter = {};

private:
	void InitializeItems();
	void UpdateProperties();
	void OnCreateKeyClick();
	void OnModified();
	void OnItemFocused(ListItem*);
	bool OnFrameCountAfterEdit(PropValue* v, s32& val);
	void OnCloneItem(const char* parent_path, const char* new_full_name);
	void OnCreateItem(const char* path);
	void OnRemoveItem(UIItemListForm::Node& node);
	void OnRenameItem(UIItemListForm::Node& node, const char* old_full_name, const char* new_full_name, EItemType type);

	void DrawItemList();
	void DrawTransport();
	void DrawTimeline();
	void DrawPreview();
	void HandleShortcuts();

	void SetFrame(int Frame);
	void StopPlayback();
	void TogglePlayback();
	void DeleteCurrentKey();
	void CopyColor();
	void PasteColor();
	void MoveCurrentKey(int Direction);
	int GetPreviewFrame() const;
};
