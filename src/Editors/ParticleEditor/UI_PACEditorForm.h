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

    struct DoubleKey
    {
        double Value = 0.0;
        size_t Index = -1;
        bool clicked = false;
        bool hovered = false;
        bool hold = false;
        bool held = false;

        //DoubleKey(float v): Value(v), clicked(false), hovered(false), hold(false) {}
    };
    
    xr_vector<float> R_keys_y, G_keys_y, B_keys_y, A_keys_y, keys_x; //, keys_y_fdummy;
    xr_vector<double> keys_y_ddummy;
    xr_vector<DoubleKey> dkeys_x;
    double LinkXMin = 0.0f, LinkXMax = 1.0f;

    size_t SelectedKeyframeIndex = -1;

    void DrawCurves();
};
