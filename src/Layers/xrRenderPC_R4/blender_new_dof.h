#pragma once

class CBlender_new_dof : public IBlender
{
public:
    virtual const char*  getComment()     { return "INTERNAL: new depth of field"; }
    virtual bool    canBeDetailed()  { return false; }
    virtual bool    canBeLMAPped()   { return false; }

    virtual void    Compile(CBlender_Compile& C);

    CBlender_new_dof();
    virtual ~CBlender_new_dof();
};