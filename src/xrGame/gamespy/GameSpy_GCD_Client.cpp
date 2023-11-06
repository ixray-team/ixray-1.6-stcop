#include "StdAfx.h"
#include "GameSpy_GCD_Client.h"
#include "GameSpy_Base_Defs.h"

#define	REGISTRY_CDKEY_STR	"Software\\S.T.A.L.K.E.R\\CDKey"

CGameSpy_GCD_Client::CGameSpy_GCD_Client()
{
	LoadGameSpy();
};

CGameSpy_GCD_Client::~CGameSpy_GCD_Client()
{
}

void	CGameSpy_GCD_Client::LoadGameSpy()
{
	GAMESPY_LOAD_FN(xrGS_gcd_compute_response);	
}

string64	gsCDKey = "";

extern	void	GetCDKey_FromRegistry(char* CDKeyStr);

void CGameSpy_GCD_Client::CreateRespond	(char* RespondStr, char* ChallengeStr, u8 Reauth)
{
	string512	CDKey = "";
	GetCDKey_FromRegistry(CDKey);
	xrGS_gcd_compute_response(_strupr(CDKey), ChallengeStr, RespondStr, (Reauth == 1));
}