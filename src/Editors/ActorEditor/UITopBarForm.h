#pragma once
class UITopBarForm: public IEditorWnd
{
public:
    UITopBarForm();
    virtual ~UITopBarForm();
    virtual void Draw();

private:
    void        ClickUndo();
    ref_texture m_tUndo;
    u32         m_timeUndo;
    void        ClickRedo();
    ref_texture m_tRedo;
    u32         m_timeRedo;

    void        ClickNew();
    ref_texture m_tNew;
    void        ClickOpen();
    ref_texture m_tOpen;
    void        ClickSave();
    ref_texture m_tSave;

    void        ClickOpenGameData();
    ref_texture m_tOpenGameData;
};
