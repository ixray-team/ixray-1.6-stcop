#pragma once

#include "script_utility.h"

class CLevel;
class CScriptXRConditionsStorage;
class CScriptXREffectsStorage;

class CScriptXRParser
{
	// second is a real length of .first
	using xr_embedded_params_t =
		char[ixray::kXRParserParamsBufferSize][ixray::kXRParserParamBufferSize];

public:
	CScriptXRParser();
	~CScriptXRParser();

	void initialize(CLevel* pLevelManager,
		CScriptXRConditionsStorage* pStorageXRConditions,
		CScriptXREffectsStorage* pStorageXREffects);
	void destroy();

	// it means if symbol is in this mask: "%{}.,
	// qwertyuioplkjhgfdsamnbvcxz1234567890@-+=~!_():"
	bool isSymbolValidForParsing(char nSymbol) const;
	bool isFunctionArgumentSymbolValidForParsing(char nSymbol) const;
	bool isSymbolEvent(char nSymbol) const;

	// on stack implementation, no allocations
	const char* lua_pickSectionFromCondlist(CScriptGameObject* pClientPlayer,
		CScriptGameObject* pClientObject, const char* pSectionName,
		const char* pFieldName, const char* pSourceName);
	const char* lua_pickSectionFromCondlist(CScriptGameObject* pClientPlayer,
		CSE_ALifeDynamicObject* pServerObject, const char* pSectionName,
		const char* pFieldName, const char* pSourceName);
	const char* lua_pickSectionFromCondlist(
		CSE_ALifeDynamicObject* pServerPlayer,
		CSE_ALifeDynamicObject* pServerObject, const char* pSectionName,
		const char* pFieldName, const char* pSourceName);

	static void script_register(lua_State* L);

private:
	const char* pickSectionFromCondlist(CCondlistEmbedded& condlist,
		void* pClientActor, void* pClientObject, int nCallingVersion,
		bool& bNeedToBreak);

	size_t parseParams(const char* pParamsBuffer, xr_embedded_params_t& result);

	void parseCondlistInfo(CCondlistInfo& info, CCondlistEmbedded& result);
	// returns size of array
	u32 parseInfoportions(
		const char* pBuffer, CCondlistEmbedded::xr_condlistdata& result);

private:
	u32 m_nCurrentIndex;
	CLevel* m_pLevel;
	CScriptXRConditionsStorage* m_pXRConditions;
	CScriptXREffectsStorage* m_pXREffects;
};
