#pragma once

class UIEditLibrary :
    public IEditorWnd
{
public:
    UIEditLibrary();
    virtual ~UIEditLibrary();

    static void Update();
    static void Show();
    static void Close();
    static void OnRender();

    ImTextureID m_RealTexture;

    void OnItemFocused(ListItem* item);
    void OnItemUnfocused(ListItem* item);

private:
    static UIEditLibrary* Form;
    ListItemsVec FocusedItems;

    virtual void Draw();
    void ImportClick();
    void DrawObjects();

    void DrawRightBar();
    void RenderSaveButton();
    void InitObjects();
    void OnPropertiesClick();
    void OnMakeThmClick();
    void OnPreviewClick();

    void MakeLOD(bool highQuality);
    void GenerateLOD(RStringVec& props, bool bHighQuality);

    void RefreshSelected();
    void PickSurface();
    void PickCallback();
    void ChangeReference(const RStringVec& items);
    bool SelectionToReference(ListItemsVec* props);
    void ExportOneOBJ(CEditableObject* EO);
    void ExportObj();

    UIPropertiesForm* GetPropertyWnd();

    static void OnModified();

    UIItemListForm* m_ObjectList;
    UIItemListForm SearchList;
    LPCSTR m_Current;
    bool m_Preview;
    bool m_Dropper;

    xr_string PrevClick;

    UIRenderForm View;

    bool m_SelectLods;
    bool m_HighQualityLod;
    bool bShowProps = false;

    bool IsModify = false;
    bool UseWorldPropWnd = false;

    xr_vector<CSceneObject*> m_pEditObjects;
    UIPropertiesForm* PreviewProps = nullptr;
    UIPropertiesForm* InternalProps;
    xr_string SearchQuery;
};