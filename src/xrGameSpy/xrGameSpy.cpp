// xrGameSpy.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "xrGameSpy.h"
BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
    return true;
}

void	FillSecretKey(char* SecretKey)
{
	SecretKey[0] = 'L';
	SecretKey[1] = 'T';
	SecretKey[2] = 'U';
	SecretKey[3] = '2';
	SecretKey[4] = 'z';
	SecretKey[5] = '2';
	SecretKey[6] = '\0';
}

const char* GetGameVersion	(int PlatformID)
{
	switch (PlatformID)
	{
	case 0:	
		return GAME_VERSION_SOC;
	case 1:
		return GAME_VERSION_CS;
	case 2:
		return GAME_VERSION;
	default:
		return "Unknown";
	}
}

XRGAMESPY_API const char* xrGS_GetGameVersion	(int PlatformID)
{
	return GetGameVersion(PlatformID);
}

XRGAMESPY_API void xrGS_GetGameID	(int* GameID, int verID)
{
	*GameID = int(GAMESPY_GAMEID);

#ifdef DEMO_BUILD
	switch (verID)
	{
	case 1: *GameID = int(1067); break;
	case 2: *GameID = int(1576); break;
	case 3: *GameID = int(1620); break;
	default: *GameID = int(GAMESPY_GAMEID); break;
	}	
#endif
}