#pragma once

class CBlender_OutputScale : public IBlender
{
public:
    virtual LPCSTR getComment() { return "CBlender_OutputScale"; }
    virtual BOOL canBeDetailed() { return FALSE; }
    virtual BOOL canBeLMAPped() { return FALSE; }

    virtual void Compile(CBlender_Compile& C);

    CBlender_OutputScale();
    virtual ~CBlender_OutputScale();
};