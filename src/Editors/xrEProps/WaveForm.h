#pragma once
#include "../Public/WaveForm.h"

class XREPROPS_API CWaveForm:
    public IEditorWnd
{
public:
    static CWaveForm* form;
    WaveForm m_CurFunc;
    WaveForm m_SaveFunc;
    bool bLoadMode = false;
    bool IsOpen = false;

    // GUI state
    int selectedFunction = 0;
    float arg[4] = { 0,1,0,1 };
    float scale = 1.0f;

    // Labels
    char lbMax[32], lbMin[32], lbCenter[32], lbEnd[32];

    // Functions list
    struct FunctionToken { const char* name; int id; };
    xr_vector<FunctionToken> function_token = {
        {"Constant", WaveForm::fCONSTANT},
        {"Sin", WaveForm::fSIN},
        {"Triangle", WaveForm::fTRIANGLE},
        {"Square", WaveForm::fSQUARE},
        {"Saw-Tooth", WaveForm::fSAWTOOTH},
        {"Inv Saw-Tooth", WaveForm::fINVSAWTOOTH}
    };

    mutable bool ResultStatus = false;
public:
    CWaveForm()
    {
        form = this;
        IsOpen = false;
    }

    ~CWaveForm()
    {
        form = nullptr;
    }

    int Run(WaveForm* func)
    {
        m_CurFunc = *func;
        m_SaveFunc = *func;
        GetFuncData();

        IsOpen = true;
        return 0;
    }

    WaveForm* GetResult()
    {
        if (IsOpen)
        {
            return nullptr;
        }

        if (ResultStatus)
        {
            ResultStatus = false;
            return &m_CurFunc;
        }
        return nullptr;
    }

    void GetFuncData()
    {
        bLoadMode = true;
        selectedFunction = m_CurFunc.F;
        for (int i = 0; i < 4; i++) arg[i] = m_CurFunc.arg[i];
        scale = 1.0f;

        bLoadMode = false;
    }

    void UpdateFuncData();

    void DrawGraph(ImVec2 size);
    virtual void Draw() override;
};
