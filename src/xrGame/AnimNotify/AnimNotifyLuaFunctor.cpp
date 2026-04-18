#include "StdAfx.h"
#include "AnimNotifyLuaFunctor.h"
#include "script_game_object.h"

#include "ai_space.h"
#include "Actor.h"


void CAnimNotifyLuaFunctor::Construct(const CInifile& ini, const char* _sect)
{
    sect = _sect;
    Func = ini.r_string(_sect, "Func");
}

void CAnimNotifyLuaFunctor::Execute(IRenderVisual* visual, u16 bone_id)
{
    if (!visual)
    {
        g_pScriptEngine->print_stack();
        Msg("! Error CAnimNotifyLuaFunctor Cant found visual for section: %s", sect);
        return;
    }

    try
    {
        luabind::functor<void> funct;
        if (ai().script_engine().functor(Func.c_str(), funct))
        {
            ALife::_OBJECT_ID id = visual->dcast_PKinematics()->GetParentObjectId();
            if (CObject* obj = Level().Objects.net_Find(id))
            {
                if (CGameObject* gObj = obj->cast_game_object())
                {
                    funct(gObj->lua_game_object());
                }
            }
        }
    } catch (...)
    {
        R_ASSERT3(false, "Unable to process AnimNotify functor", Func.c_str());
    }
}
