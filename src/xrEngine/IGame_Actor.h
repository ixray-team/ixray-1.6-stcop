#pragma once

class IGame_Actor
{
public:
	virtual xr_vector<xr_string> GetKnowedPortions() const = 0;
};

extern ENGINE_API IGame_Actor* g_pIGameActor;