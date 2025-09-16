#pragma once
#include "EditorWnd.h"

class UIPACEditorForm :
    public IEditorWnd
{
public:
    UIPACEditorForm();
    virtual ~UIPACEditorForm();
    virtual void Draw();

    static void Open(PS::CPACDef* EditedPAC);
    static void Update();
private:
    static UIPACEditorForm* Form;
    PS::CPACDef* EditedPAC = nullptr;
};
