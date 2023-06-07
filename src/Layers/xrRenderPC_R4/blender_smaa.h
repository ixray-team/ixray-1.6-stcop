#pragma once

class CBlender_SMAA : public IBlender
{
public:
    virtual LPCSTR getComment() { return "CBlender_SMAA"; }
    virtual BOOL canBeDetailed() { return FALSE; }
    virtual BOOL canBeLMAPped() { return FALSE; }

    virtual void Compile(CBlender_Compile& C);

    CBlender_SMAA();
    virtual ~CBlender_SMAA();
};