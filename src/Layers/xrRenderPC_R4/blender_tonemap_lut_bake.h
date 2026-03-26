#pragma once

class CBlender_tonemap_lut_bake : public IBlender
{
public:
    virtual LPCSTR getComment()     { return "INTERNAL: bake tonemap 3d lut"; }
    virtual BOOL   canBeDetailed()  { return FALSE; }
    virtual BOOL   canBeLMAPped()   { return FALSE; }

    virtual void   Compile(CBlender_Compile& C);

    CBlender_tonemap_lut_bake();
    virtual ~CBlender_tonemap_lut_bake();
};