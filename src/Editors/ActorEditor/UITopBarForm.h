#pragma once
class UITopBarForm :
    public IEditorWnd
{
public:
    UITopBarForm();
    virtual ~UITopBarForm();
    virtual void Draw();

private:
    void InitIcons();
    xr_hash_map<xr_string, ref_texture> Icons;

    u32 m_timeUndo;
    u32 m_timeRedo;

    bool m_Simulate;

    void ClickUndo();
    void ClickRedo();

    void ClickNew();
    void ClickOpen();
    void ClickSave();

    void ClickOpenGameData();

    void ClickPreferences();
};
