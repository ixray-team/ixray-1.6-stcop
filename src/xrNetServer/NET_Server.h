#pragma once
#include "NET_Shared.h"

#ifdef XR_MP_BUILD
#include "SteamNetServer.h"
#define IPureServer SteamNetServer
#else
#include "BaseServer.h"
#define IPureServer BaseServer
#endif