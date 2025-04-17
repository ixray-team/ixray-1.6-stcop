#include "stdafx.h"
#include "AnimNotify.h"

IAnimNotifyHandler* IAnimNotifyHandler::Handler = nullptr;

void IAnimNotifyHandler::SetHandler(IAnimNotifyHandler* pHandler)
{
    if (Handler)
    {
        xr_delete(Handler);
    }
    Handler = pHandler;
}

IAnimNotifyHandler& IAnimNotifyHandler::Get()
{
    VERIFY(Handler);
    return *Handler;
}
