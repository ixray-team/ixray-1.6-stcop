#pragma once

class UIEditLibrary :
	public IEditorWnd
{
public:
	UIEditLibrary();
	virtual ~UIEditLibrary();

	static void Update();
	static UIEditLibrary* Init();
	static void Show();
	static void Close();
	static void OnRender();

	ref_texture RealTexture = nullptr;

	void OnItemFocused(ListItem* item);
	void OnItemUnfocused(ListItem* item);

private:
	static UIEditLibrary* Form;
	ListItemsVec FocusedItems;

	virtual void Draw() override;
	void ImportClick();
	void DrawObjects();

	void DrawRightBar();
	void RenderSaveButton();
	void InitObjects();
	void OnPropertiesClick();
	void OnMakeThmClick();
	void OnPreviewClick();

	void MakeLOD(bool highQuality);
	void GenerateLOD(const RStringVec& props, bool bHighQuality);

	void RefreshSelected();
	void PickSurface();
	void PickCallback();
	void ChangeReference(const RStringVec& items);
	bool SelectionToReference(ListItemsVec* props);
	void ExportOneOBJ(CEditableObject* EO);
	void ExportObj();

	static void OnModified();
	UIItemListForm& ActualItemList() { if (SearchQuery.empty()) return *ObjectList; return SearchList; }
	UIItemListForm* ObjectList;
	UIItemListForm SearchList;
	const char* CurrentKey;
	bool IsPreview;
	bool m_Dropper;

	xr_string PrevClick;

	UIRenderForm View;

	bool SelectLods;
	bool m_HighQualityLod;

	bool IsModify = false;

	xr_vector<CSceneObject*> m_pEditObjects;
	UIPropertiesForm* PreviewProps = nullptr;
	UIPropertiesForm* InternalProps;
	xr_string SearchQuery;
};