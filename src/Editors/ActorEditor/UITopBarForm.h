#pragma once
class UITopBarForm :
    public IEditorWnd
{
public:
    UITopBarForm();
    virtual ~UITopBarForm();
    virtual void Draw();

private:
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
