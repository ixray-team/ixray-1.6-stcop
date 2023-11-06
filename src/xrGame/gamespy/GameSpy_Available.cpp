#include "StdAfx.h"
#include "GameSpy_Available.h"
#include "GameSpy_Base_Defs.h"

CGameSpy_Available::CGameSpy_Available()
{
	LoadGameSpy();
};

CGameSpy_Available::~CGameSpy_Available()
{
};

void CGameSpy_Available::LoadGameSpy()
{
	GAMESPY_LOAD_FN(xrGS_GSIStartAvailableCheckA);
	GAMESPY_LOAD_FN(xrGS_GSIAvailableCheckThink);
	GAMESPY_LOAD_FN(xrGS_msleep);
	GAMESPY_LOAD_FN(xrGS_GetQueryVersion);
}

bool CGameSpy_Available::CheckAvailableServices(shared_str& resultstr)
{
	GSIACResult result;
	xrGS_GSIStartAvailableCheckA();

	while ((result = xrGS_GSIAvailableCheckThink()) == GSIACWaiting)
		xrGS_msleep(5);

	if (result != GSIACAvailable)
	{
		switch (result)
		{
		case GSIACUnavailable:
		{
			resultstr = "! Online Services for STALKER are no longer available.";
		}break;
		case GSIACTemporarilyUnavailable:
		{
			resultstr = "! Online Services for STALKER are temporarily down for maintenance.";
		}break;
		}
		return false;
	}
	else
	{
		resultstr = "Success";
	};
	return true;
}