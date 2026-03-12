#pragma once

class CBlender_new_dof : public IBlender
{
public:
    virtual LPCSTR  getComment()     { return "INTERNAL: new depth of field"; }
    virtual BOOL    canBeDetailed()  { return FALSE; }
    virtual BOOL    canBeLMAPped()   { return FALSE; }

    virtual void    Compile(CBlender_Compile& C);

    CBlender_new_dof();
    virtual ~CBlender_new_dof();
};