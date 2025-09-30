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

    ref_texture m_tReload;

    void ClickUndo();
    void ClickRedo();

    void ClickSaveParticles();
    void ClickReloadParticles();

    void ClickOpen();
    void ClickSaveXr();

    void ClickOpenGameData();

    void ClickValidate();

    void ClickPreferences();
};