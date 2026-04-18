#pragma once

class CBlender_FXAA : public IBlender
{
public:
    virtual const char* getComment() { return "CBlender_FXAA"; }
    virtual bool canBeDetailed() { return FALSE; }
    virtual bool canBeLMAPped() { return FALSE; }

    virtual void Compile(CBlender_Compile& C);

    CBlender_FXAA();
    virtual ~CBlender_FXAA();
};