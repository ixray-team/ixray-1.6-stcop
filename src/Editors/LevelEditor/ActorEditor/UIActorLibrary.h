#pragma once

// Object library of the Actor Editor workspace: browse, preview, thumbnails, LODs, import and OBJ export of the library objects
class UIActorLibrary
{
public:
	UIActorLibrary(UIRenderForm* RenderView);
	~UIActorLibrary();

	void Draw();
	// Renders the previewed library objects in the workspace viewport
	static void OnRender();
	// Selects the object loaded in the Actor Editor when the library has it
	static void SyncWithActor(const char* FileName);
	// The viewport shows the library objects instead of the edited object (LOD capture)
	static bool IsPreviewForced() { return Form != nullptr && Form->ForcePreview; }

	void OnItemFocused(ListItem* item);
	void OnItemUnfocused(ListItem* item);

	// The selected library objects are edited in the Properties window of the workspace
	bool HasProperties() const { return OwnsProperties; }
	void ReleaseProperties();
	void DrawProperties();
	// Library actions of the Object Tool window
	void DrawActions();

private:
	static UIActorLibrary* Form;
	ListItemsVec FocusedItems;

	void ImportClick();
	void DrawObjects();
	void OpenSelected();

	void UpdateLodChoose();
	void InitObjects();
	void OnMakeThmClick();
	void OnPreviewClick();

	void MakeLOD(bool highQuality);
	void GenerateLOD(const RStringVec& props, bool bHighQuality);

	void RefreshSelected();
	void ChangeReference(const RStringVec& items);
	bool SelectionToReference(ListItemsVec* props);
	void ExportOneOBJ(CEditableObject* EO);
	void ExportObj();

	void FillProperties();
	void SaveModified();
	void PickSurface();
	void PickCallback();
	static void OnModified();

	UIItemListForm& ActualItemList() { if (SearchQuery.empty()) return *ObjectList; return SearchList; }
	UIItemListForm* ObjectList;
	UIItemListForm SearchList;
	const char* CurrentKey;
	bool IsPreview;


	// Viewport of the Actor Editor workspace
	UIRenderForm* View;

	bool SelectLods;
	bool m_HighQualityLod;


	xr_vector<CSceneObject*> m_pEditObjects;
	UIPropertiesForm* Props = nullptr;
	bool OwnsProperties = false;
	bool IsModify = false;
	bool Dropper = false;
	// Selection set from the Actor Editor: does not load the object back
	bool Syncing = false;
	bool ForcePreview = false;
	xr_string PrevClick;
	xr_string SearchQuery;
};
