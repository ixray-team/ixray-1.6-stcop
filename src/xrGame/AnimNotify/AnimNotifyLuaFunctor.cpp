#include "stdafx.h"
#include "AnimNotifyLuaFunctor.h"

#include "ai_space.h"

void CAnimNotifyLuaFunctor::Construct(const CInifile& ini, LPCSTR sect)
{
    Func = ini.r_string(sect, "Func");
}

void CAnimNotifyLuaFunctor::Execute(IRenderVisual* visual, u16 bone_id)
{
    try
    {
        luabind::functor<void> funct;
        if (ai().script_engine().functor(Func.c_str(), funct))
        {
            funct();
        }
    } catch (...)
    {
        R_ASSERT3(false, "Unable to process AnimNotify functor", Func.c_str());
    }
}
