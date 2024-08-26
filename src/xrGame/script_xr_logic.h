#pragma once


#include "script_utility.h"


class CLevel;
class CScriptXRConditionsStorage;
class CScriptXREffectsStorage;

class CScriptXRLogicManager
{
public:
	CScriptXRLogicManager();
	~CScriptXRLogicManager();

	void initialize(CLevel* pLevelManager, CScriptXRConditionsStorage* pStorageXRConditions, CScriptXREffectsStorage* pStorageXREffects);
	void destroy();

	// it means if symbol is in this mask: "%{}., qwertyuioplkjhgfdsamnbvcxz1234567890@-+=~!_():"
	bool isSymbolValidForParsing(char nSymbol);

private:
	CLevel* m_pLevel;
	CScriptXRConditionsStorage* m_pXRConditions;
	CScriptXREffectsStorage* m_pXREffects;
};
