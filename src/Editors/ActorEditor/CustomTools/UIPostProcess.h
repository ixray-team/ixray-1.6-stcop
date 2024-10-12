#pragma once
#include "../../xrGame/PostprocessAnimator.h"

class CMainPPE:
    public IEditorWnd
{
public:
    struct PointItem
    {
        xr_string Name;
        float Value;
        bool IsActive;
    };

protected:
	CPostprocessAnimator mAnimator;
    xr_vector<PointItem> ListData;
    xr_string FileName;

	int ModeIter = 0;
	int ListIter = 0;
    
    Fvector2 Duality;
    Fvector Noise;

    float Itensity = 0.f;
    float Influence = 0.f;
    string_path Texture;
    bool UsePicker = false;
    ImVec4 Color;

    enum DialogType
    {
        Load,
        Save, 
        None
    };

    DialogType DrawDialogType = DialogType::None;
    bool NewClick = false;
    bool LoadClick = false;
    bool SaveClick = false;

    float HeaderSize = 0.f;

private:
    void AddKey(float Value, bool OnlyValue = false);
    CPostProcessParam* GetCurrentParam();
    size_t GetSelectedItemID() const;
    
    static void Apply(xr_string);
    void ClickHandle();

    void ApplyData();
    void LoadData();
    void UpdateData();

private:
	void DrawChart();
	void DrawTool();
    void DrawPicker();

public:
    CMainPPE();

	virtual void Draw();
    static CMainPPE& Instance();
    bool& OpenState() { return bOpen; }
};