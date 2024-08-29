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

	void initialize(CLevel* pLevelManager, CScriptXRConditionsStorage* pStorageXRConditions, CScriptXREffectsStorage* pStorageXREffects);
	void destroy();

	// it means if symbol is in this mask: "%{}., qwertyuioplkjhgfdsamnbvcxz1234567890@-+=~!_():"
	bool isSymbolValidForParsing(char nSymbol) const;
	bool isSymbolEvent(char nSymbol) const;

	xr_hash_map<u32, CCondlist> parseCondlist(const char* pSectionName, const char* pFieldName,
		const char* pSourceName);

private:
	void parseCondlistInfos(xr_infos& infos, xr_hash_map<u32, CCondlist>& result);
	void parseInfoportions(const char* pBuffer, xr_hash_map<u32, CCondlistData>& result);

private:
	CLevel* m_pLevel;
	CScriptXRConditionsStorage* m_pXRConditions;
	CScriptXREffectsStorage* m_pXREffects;
};
