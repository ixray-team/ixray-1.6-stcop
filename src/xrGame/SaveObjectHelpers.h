#pragma once
#include "../xrCore/Save/SaveObject.h"

struct SaveObjectNetPacketHelper
{
    static void PrepareLocalSpawnPacket(NET_Packet& P, CSE_Abstract& ServerObj);
    static void PrepareLocalSpawnPacketPossibleFull(NET_Packet& P, CSE_Abstract& ServerObj);
    static void PrepareLocalSpawnPacketFull(NET_Packet& P, CSE_Abstract& ServerObj);
    static CSaveObjectLoad* GetLoadObjectFromPacket(NET_Packet& P);
};