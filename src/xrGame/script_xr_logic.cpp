#include "StdAfx.h"
#include "script_xr_logic.h"
#include "script_xr_conditions.h"
#include "script_xr_effects.h"

CScriptXRParser::CScriptXRParser() :
	m_pLevel{}, m_pXRConditions{}, m_pXREffects{}
{
}

CScriptXRParser::~CScriptXRParser() {}

void CScriptXRParser::initialize(CLevel* pLevelManager, CScriptXRConditionsStorage* pStorageXRConditions, CScriptXREffectsStorage* pStorageXREffects)
{
	m_pLevel = pLevelManager;
	m_pXRConditions = pStorageXRConditions;
	m_pXREffects = pStorageXREffects;
}

void CScriptXRParser::destroy() 
{
	m_pLevel = nullptr;
	m_pXRConditions = nullptr;
	m_pXREffects = nullptr;
}

bool CScriptXRParser::isSymbolValidForParsing(char nSymbol)
{
	constexpr char a = 'a';
	constexpr char z = 'z';
	constexpr char A = 'A';
	constexpr char Z = 'Z';
	constexpr char number_0 = '0';
	constexpr char number_9 = '9';

	bool result{};

	if (nSymbol >= a && nSymbol <= z)
	{
		result = true;
	}
	else if (nSymbol >= A && nSymbol <= Z)
	{
		result = true;
	}
	else if (nSymbol >= number_0 && nSymbol <= number_9)
	{
		result = true;
	}
	else if (nSymbol == ' ' || nSymbol == '.' || nSymbol == ',' ||
		nSymbol == '{' || nSymbol == '}' || nSymbol == '%' || nSymbol == '@' ||
		nSymbol == '-' || nSymbol == '+' || nSymbol == '=' || nSymbol == '~' ||
		nSymbol == '!' || nSymbol == '_' || nSymbol == '(' || nSymbol == ')' ||
		nSymbol == ':')
	{
		result = true;
	}
	

	return result;
}
