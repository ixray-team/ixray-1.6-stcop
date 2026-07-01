#pragma once

enum class ESteamAppID
{
	Unknown = 0,

	ShadowOfChernobyl = 4500,
	ClearSky = 20510,
	CallOfPripyat = 41700,

	ShadowOfChornobylEnhanced = 2427410,
	ClearSkyEnhanced = 2427420,
	CallOfPripyatEnhanced = 2427430,
};

class CSteamOverlay
{
public:
	CSteamOverlay();
	~CSteamOverlay();

public:
	ESteamAppID CurrentAppID = ESteamAppID::Unknown;

private:
	bool Created = false;
};