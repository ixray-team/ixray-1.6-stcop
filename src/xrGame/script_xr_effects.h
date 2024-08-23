#pragma once

#include "script_utility.h"

class CLevel;


class CScriptXREffectsStorage
{
public:
	CScriptXREffectsStorage();
	~CScriptXREffectsStorage();

	void initialize(CLevel* pLevelManager);
	void destroy();

	REGISTER_STORAGE_FOR_RETURN_TYPE(void);

private:
	CLevel* m_pLevel;
};
