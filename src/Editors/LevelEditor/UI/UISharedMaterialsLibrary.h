#pragma once

class UISharedMaterialsLibrary :
    public IEditorWnd
{
public:
    UISharedMaterialsLibrary();
    virtual ~UISharedMaterialsLibrary();

    static void Update();
    static void Show();
    static void Close();

    void OnItemFocused(ListItem* item);
    void OnItemUnfocused(ListItem* item);

private:
    static UISharedMaterialsLibrary* Form;
    xr_string SearchQuery;
    IC UIItemListForm& ActualItemList()
    {
        if (SearchQuery.empty())
        {
            return *m_ObjectList;
        }
        return SearchList;
    }
    UIItemListForm* m_ObjectList;
    UIItemListForm SearchList;

	ref_texture m_RealTexture;
    
    UIPropertiesForm* PreviewProps = nullptr;
	LPCSTR m_Current;

	virtual void Draw() override;
    
	void DrawObjects();
	void InitObjects();
	void DrawRightBar();
    
};