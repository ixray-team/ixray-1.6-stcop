//---------------------------------------------------------------------------
#pragma once

class UIImageEditorForm :public IEditorWnd
{
public:
	UIImageEditorForm();
	virtual ~UIImageEditorForm();
	virtual void Draw();
public:
	static void Update();
	static void Show(bool bImport);
	static void ImportTextures();
	static void FindInEditor(xr_string, bool = false);
private:
	using THMVec = xr_vector<ETextureThumbnail*>;
	using THMIt = THMVec::iterator;

	using THMMap = xr_map<shared_str, ETextureThumbnail*>;
	using THMMapIt = THMMap::iterator;

	THMMap m_THM_Used;
	THMVec m_THM_Current;
	UIItemListForm* m_ItemList;
	UIPropertiesForm* m_ItemProps;
	FS_FileSet	texture_map;
	FS_FileSet	modif_map;
	bool bImportMode;
	bool bReadonlyMode;
	static UIImageEditorForm* Form;
	ImTextureID m_Texture;
	ImTextureID m_TextureRemove;
private:
	ETextureThumbnail* FindUsedTHM(const shared_str& name);
	void RegisterModifiedTHM();
	void OnCubeMapBtnClick(ButtonValue* value, bool& bModif, bool& bSafe);
	void OnTypeChange(PropValue* prop);
	void UpdateProperties();
	void InitItemList();
	void HideLib();
	void UpdateLib();
	void UpdateSelected();
	void OnItemsFocused(ListItem* item);
	void SaveUsedTHM();
private:
	bool m_bFilterImage;
	bool m_bFilterTerrain;
	bool m_bUpdateProperties;
	bool m_bFilterBump;
	bool m_bFilterNormal;
	bool m_bFilterCube;
	void FilterUpdate();
};