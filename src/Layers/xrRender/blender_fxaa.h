#pragma once

class CBlender_FXAA : public IBlender
{
public:
    virtual LPCSTR getComment() { return "CBlender_FXAA"; }
    virtual BOOL canBeDetailed() { return FALSE; }
    virtual BOOL canBeLMAPped() { return FALSE; }

    virtual void Compile(CBlender_Compile& C);

    CBlender_FXAA();
    virtual ~CBlender_FXAA();
};