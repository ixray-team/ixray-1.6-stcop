#pragma once
class UITopBarForm :public IEditorWnd
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

    void        ClickSaveParticles();
    ref_texture m_tSaveParticles;
    void        ClickReloadParticles();
    ref_texture m_tReloadParticles;

    void        ClickOpen();
    ref_texture m_tOpen;
    void        ClickSaveXr();
    ref_texture m_tSaveXr;

    void        ClickOpenGameData();
    ref_texture m_tOpenGameData;

    void        ClickValidate();
    ref_texture m_tValidate;
};