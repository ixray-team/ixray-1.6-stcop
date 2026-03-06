#pragma once

class CUIWidgetsTest : public IEditorWnd
{
private:
    CUIWidgetsTest();
    void Draw() override;

public:
    static CUIWidgetsTest& Instance();
    void Show(bool value);
};