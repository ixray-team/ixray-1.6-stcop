#pragma once

class CBlender_SMAA : public IBlender
{
public:
    virtual const char* getComment() { return "CBlender_SMAA"; }
    virtual bool canBeDetailed() { return false; }
    virtual bool canBeLMAPped() { return false; }

    virtual void Compile(CBlender_Compile& C);

    CBlender_SMAA();
    virtual ~CBlender_SMAA();
};