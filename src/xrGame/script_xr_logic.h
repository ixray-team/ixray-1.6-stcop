#pragma once


#include "script_utility.h"


class CLevel;

class CScriptXRLogicManager
{
public:
	CScriptXRLogicManager();
	~CScriptXRLogicManager();

	void initialize(CLevel* pLevelManager);
	void destroy();

private:
	CLevel* m_pLevel;
};
