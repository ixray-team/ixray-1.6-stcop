#pragma once

class CUIWidgetsTest : public IEditorWnd
{
private:
    CUIWidgetsTest();

    void DemoWindowWidgetsButtons();
    void Draw() override;

public:
    void InitIcons();

    xr_hash_map<xr_string, ref_texture> Icons;

    static CUIWidgetsTest& Instance();
    void Show(bool value);
};