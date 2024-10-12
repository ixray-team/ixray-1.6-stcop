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

private:
    static UIEditLibrary* Form;

    virtual void Draw();
    void ImportClick();
    void DrawObjects();

    void DrawRightBar();
    void RenderSaveButton();
    void DrawObject(CCustomObject* obj, const char* name);
    void InitObjects();
    void OnPropertiesClick();
    void OnMakeThmClick();
    void OnPreviewClick();

    void MakeLOD(bool highQuality);
    void GenerateLOD(RStringVec& props, bool bHighQuality);

    void RefreshSelected();
    void ChangeReference(const RStringVec& items);
    bool SelectionToReference(ListItemsVec* props);
    void ShowProperty();
    void ExportOneOBJ(CEditableObject* EO);
    void ExportObj();

    static void OnModified();

    UIItemListForm* m_ObjectList;
    UIPropertiesForm* m_Props;
    UIPropertiesForm* m_PropsObjects;
    LPCSTR m_Current;
    bool m_Preview;

    UIRenderForm View;

    bool m_SelectLods;
    bool m_HighQualityLod;
    bool bShowProps = false;

    bool IsModify = false;

    xr_vector<CSceneObject*> m_pEditObjects;
};