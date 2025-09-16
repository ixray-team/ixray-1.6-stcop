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
    
    xr_vector<float> R_keys_y, G_keys_y, B_keys_y, A_keys_y, keys_x, keys_y_dummy;
    double LinkXMin = 0.0f, LinkXMax = 1.0f;

    void DrawCurves();
};
