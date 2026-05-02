#pragma once

#include "../ai_space.h"
#include "../../xrScripts/script_engine.h"

class PdaScriptBridge final
{
private:
    PdaScriptBridge() = delete;

public:
    template <typename TValue>
    static bool TryCall(const char* functorName, TValue& value)
    {
        luabind::functor<TValue> functor;
        if (!ai().script_engine().functor(functorName, functor))
        {
            return false;
        }

        value = functor();
        return true;
    }

    template <typename TValue, typename TArg0>
    static bool TryCall(const char* functorName, TArg0 arg0, TValue& value)
    {
        luabind::functor<TValue> functor;
        if (!ai().script_engine().functor(functorName, functor))
        {
            return false;
        }

        value = functor(arg0);
        return true;
    }
};
