#pragma once
#include "NET_Shared.h"

#ifdef XR_MP_BUILD
#include "SteamNetClient.h"
#define IPureClient SteamNetClient
#else
#include "BaseClient.h"
#define IPureClient BaseClient
#endif