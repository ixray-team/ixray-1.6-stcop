#pragma once

class CUIWidgetsTest : public IEditorWnd
{
private:
    CUIWidgetsTest();

    void DemoWindowWidgetsButtons();
    void Draw() override;

public:
    void InitIcons();

    xr_hash_map<xr_string, xr_string> Icons;

    static CUIWidgetsTest& Instance();
    void Show(bool value);
};
