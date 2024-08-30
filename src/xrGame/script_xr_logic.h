#pragma once

#include "script_utility.h"

class CLevel;
class CScriptXRConditionsStorage;
class CScriptXREffectsStorage;

class CScriptXRParser
{
	// second is a real length of .first
	using xr_infos = std::pair<xr_array<CCondlistInfo, 10>, size_t>;

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
	bool isSymbolEvent(char nSymbol) const;

	xr_hash_map<u32, CCondlist> parseCondlist(const char* pSectionName,
		const char* pFieldName, const char* pSourceName);

	void eraseCondlist(u32 nHandle);

	u32 lua_parseCondlist(const char* pSectionName, const char* pFieldName,
		const char* pSourceName);
	void lua_deleteCondlist(u32 nHandle);

	static void script_register(lua_State* L);

private:
	void parseCondlistInfos(
		xr_infos& infos, xr_hash_map<u32, CCondlist>& result);
	void parseInfoportions(
		const char* pBuffer, xr_hash_map<u32, CCondlistData>& result);
	u32 generateHandle(void);

private:
	u32 m_nCurrentIndex;
	CLevel* m_pLevel;
	CScriptXRConditionsStorage* m_pXRConditions;
	CScriptXREffectsStorage* m_pXREffects;
	// on lua user receive the handle and when he calls other method he operates
	// with handle!!! in our case handle = id of parse condlist todo: think
	// about reducing size of CCondlist and think about storing hashes instead
	// of strings but for first iteration of development let's keep 'as is'
	xr_hash_map<u32, xr_hash_map<u32, CCondlist>> m_mStorage;
};
