#include "StdAfx.h"
#include "SaveObjectHelpers.h"

#include "xrMessages.h"
#include "xrServer_Object_Base.h"

void SaveObjectNetPacketHelper::PrepareLocalSpawnPacket(NET_Packet& P, CSE_Abstract& ServerObj)
{
    CSaveObjectSave* ObjSave = new CSaveObjectSave();
    ServerObj.Spawn_Serialize(*ObjSave, true, true);
    P.w_begin(M_SPAWN_LOCAL);
    P.w_stringZ(ServerObj.s_name);
    P.w(&ObjSave, sizeof(ObjSave)); // save ptr to transfer it for process
}

void SaveObjectNetPacketHelper::PrepareLocalSpawnPacketPossibleFull(NET_Packet& P, CSE_Abstract& ServerObj)
{
    CSaveObjectSave* ObjSave = new CSaveObjectSave();
    ServerObj.Spawn_Serialize(*ObjSave, true, true);
    if (ServerObj.s_flags.is(M_SPAWN_UPDATE))
    {
        ServerObj.UPDATE_Serialize(*ObjSave);
    }
    P.w_begin(M_SPAWN_LOCAL);
    P.w_stringZ(ServerObj.s_name);
    P.w(&ObjSave, sizeof(ObjSave)); // save ptr to transfer it for process
}

void SaveObjectNetPacketHelper::PrepareLocalSpawnPacketFull(NET_Packet& P, CSE_Abstract& ServerObj)
{
    CSaveObjectSave* ObjSave = new CSaveObjectSave();
    ServerObj.Spawn_Serialize(*ObjSave, true, true);
    ServerObj.UPDATE_Serialize(*ObjSave);
    P.w_begin(M_SPAWN_LOCAL);
    P.w_stringZ(ServerObj.s_name);
    P.w(&ObjSave, sizeof(ObjSave)); // save ptr to transfer it for process
}

CSaveObjectLoad* SaveObjectNetPacketHelper::GetLoadObjectFromPacket(NET_Packet& P)
{
    u16 dummy16;
    P.r_begin(dummy16);
    VERIFY(dummy16 == M_SPAWN_LOCAL);
    shared_str s_name;
    P.r_stringZ(s_name);
	
    CSaveObjectSave* ObjSave = nullptr; // object created elsewhere, we need to free it after use!
    P.r(&ObjSave, sizeof(ObjSave)); // we want to read pointer to object, not object itself!
    CSaveObjectLoad* ObjLoad = new CSaveObjectLoad();
    ObjLoad->TransferSaveData(*ObjSave);
    xr_delete(ObjSave);
    return ObjLoad;
}