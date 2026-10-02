#include "SteamOverlay.h"

#include <steam/steam_api.h>
#include "../xrEngine/stdafx.h"

CSteamOverlay::CSteamOverlay() = default;

void CSteamOverlay::BeginPlay()
{
	bool HasEntryFile = std::filesystem::exists("steam_appid.txt");
	if (!HasEntryFile)
	{
		// FX: Если нет `steam_appid.txt` - не запускаем стим
		return;
	}

	Created = SteamAPI_Init();

	if (Created)
	{
		Msg("Started steamworks mode!");
		auto AppID = SteamUtils()->GetAppID();

		CurrentAppID = (ESteamAppID)AppID;
	}
}

void CSteamOverlay::EndPlay()
{
	if (Created)
	{
		SteamAPI_Shutdown();
		Created = false;
	}
}

CSteamOverlay::~CSteamOverlay()
{
	EndPlay();
}
