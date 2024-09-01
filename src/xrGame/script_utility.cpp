#include "StdAfx.h"
#include "script_utility.h"

#include "script_game_object.h"

#include "object_factory.h"
#include "alife_simulator.h"
#include "InfoPortionDefs.h"
#include "alife_registry_container.h"
#include "alife_registry_container_composition.h"

bool ixray::is_weapon(CScriptGameObject* pObject)
{
	bool result{};

	if (!pObject)
		return result;

	// TODO: think about rational way of using CLSIDs check
	// class_registrator.script, filling array at runtime probably sus
	// P.S.: i did it temporarily because i don't have db implementation on C++
	// side, tbh even a such implementation will be good anyway
	int clsids_of_weapons[20]{};
	if (pObject)
	{
		clsids_of_weapons[0] =
			object_factory().script_clsid(TEXT2CLSID("WP_VINT"));
		clsids_of_weapons[1] =
			object_factory().script_clsid(TEXT2CLSID("WP_AK74"));
		clsids_of_weapons[2] =
			object_factory().script_clsid(TEXT2CLSID("WP_LR300"));
		clsids_of_weapons[3] =
			object_factory().script_clsid(TEXT2CLSID("WP_HPSA"));
		clsids_of_weapons[4] =
			object_factory().script_clsid(TEXT2CLSID("WP_PM"));
		clsids_of_weapons[5] =
			object_factory().script_clsid(TEXT2CLSID("WP_SHOTG"));
		clsids_of_weapons[6] =
			object_factory().script_clsid(TEXT2CLSID("WP_ASHTG"));
		clsids_of_weapons[7] =
			object_factory().script_clsid(TEXT2CLSID("WP_BM16"));
		clsids_of_weapons[8] =
			object_factory().script_clsid(TEXT2CLSID("WP_SVD"));
		clsids_of_weapons[9] =
			object_factory().script_clsid(TEXT2CLSID("WP_SVU"));
		clsids_of_weapons[10] =
			object_factory().script_clsid(TEXT2CLSID("WP_RG6"));
		clsids_of_weapons[11] =
			object_factory().script_clsid(TEXT2CLSID("WP_RPG7"));
		clsids_of_weapons[12] =
			object_factory().script_clsid(TEXT2CLSID("WP_VAL"));
		clsids_of_weapons[13] =
			object_factory().script_clsid(TEXT2CLSID("WP_WALTH"));
		clsids_of_weapons[14] =
			object_factory().script_clsid(TEXT2CLSID("WP_USP45"));
		clsids_of_weapons[15] =
			object_factory().script_clsid(TEXT2CLSID("WP_GROZA"));
		clsids_of_weapons[16] =
			object_factory().script_clsid(TEXT2CLSID("WP_KNIFE"));
		clsids_of_weapons[17] =
			object_factory().script_clsid(TEXT2CLSID("G_F1_S"));
		clsids_of_weapons[18] =
			object_factory().script_clsid(TEXT2CLSID("G_RGD5_S"));
		clsids_of_weapons[19] =
			object_factory().script_clsid(TEXT2CLSID("WP_GLAUN"));

		constexpr auto length_of_array =
			sizeof(clsids_of_weapons) / sizeof(decltype(clsids_of_weapons[0]));

		int clsid = pObject->clsid();

		for (int i = 0; i < length_of_array; ++i)
		{
			if (clsid == clsids_of_weapons[i])
			{
				result = true;
				break;
			}
		}
	}

#ifdef DEBUG
	if (pObject)
	{
		std::string_view section_name = pObject->Section();

		if (section_name.empty() == false)
		{
			bool isProbablyWeapon =
				section_name.find("wpn_") != xr_string::npos;

			if (isProbablyWeapon)
			{
				// check if user registered the clsid in this array
				constexpr auto length_of_array =
					sizeof(clsids_of_weapons) / sizeof(int);
				int clsid = pObject->clsid();

				bool found{};
				for (int i = 0; i < length_of_array; ++i)
				{
					if (clsid == clsids_of_weapons[i])
					{
						found = true;
						break;
					}
				}

				R_ASSERT2(found,
					"you forgot to register CLSID of some weapon in "
					"clsids_of_weapons, can't find!");
			}
		}
	}
#endif

	return result;
}

bool ixray::has_alife_info(LPCSTR info_id)
{
	bool result{};
	if (!info_id)
		return result;

	const KNOWN_INFO_VECTOR* known_info =
		ai().alife().registry(info_portions).object(0, true);

	if (!known_info)
		return result;

	if (std::find_if(known_info->begin(), known_info->end(),
			CFindByIDPred(info_id)) == known_info->end())
		return result;

	result = true;

	return result;
}

int ixray::get_script_clsid(LPCSTR str)
{
	R_ASSERT(str && "provide a valid string alwasys!");
	return object_factory().script_clsid(TEXT2CLSID(str));
}

CCondlistData::CCondlistData() :
	m_bRequired{}, m_bExpected{}, m_probability{}, m_functionname{},
	m_infoportionname{}, m_params{}
{
}

CCondlistData::~CCondlistData() {}

bool CCondlistData::getRequired(void) const
{
	return m_bRequired;
}

void CCondlistData::setRequired(bool bValue)
{
	m_bRequired = bValue;
}

bool CCondlistData::getExpected(void) const
{
	return m_bExpected;
}

void CCondlistData::setExpected(bool bValue)
{
	m_bExpected = bValue;
}

const char* CCondlistData::getProbability(void) const
{
	return m_probability;
}

void CCondlistData::setProbability(const char* pString)
{
	R_ASSERT2(
		strlen(pString) <= (sizeof(m_probability) / sizeof(char)), "overflow");

	std::memset(m_probability, 0, sizeof(m_probability));
	std::memcpy(m_probability, pString, strlen(pString) * sizeof(char));
}

const char* CCondlistData::getFunctionName(void) const
{
	return m_functionname;
}

void CCondlistData::setFunctionName(const char* pString)
{
	R_ASSERT2(
		strlen(pString) <= (sizeof(m_functionname) / sizeof(char)), "overflow");

	std::memset(m_functionname, 0, sizeof(m_functionname));
	std::memcpy(m_functionname, pString, strlen(pString) * sizeof(char));
}

const char* CCondlistData::getInfoPortionName(void) const
{
	return m_infoportionname;
}

void CCondlistData::setInfoPortionName(const char* pString)
{
	R_ASSERT2(strlen(pString) <= (sizeof(m_infoportionname) / sizeof(char)),
		"overflow");

	std::memset(m_infoportionname, 0, sizeof(m_infoportionname));
	std::memcpy(m_infoportionname, pString, strlen(pString) * sizeof(char));
}

const char* CCondlistData::getParams(void) const
{
	return m_params;
}

void CCondlistData::setParams(const char* pString)
{
	R_ASSERT2(
		strlen(pString) <= (sizeof(m_params) / sizeof(char)), "overflow!");
	std::memset(m_params, 0, sizeof(m_params));
	std::memcpy(m_params, pString, strlen(pString) * sizeof(char));
}

CCondlist::CCondlist() : m_sectionname{} {}

CCondlist::~CCondlist() {}

const xr_hash_map<u32, CCondlistData>& CCondlist::getInfoPortionCheck(
	void) const
{
	return m_mInfoPortionCheck;
}

xr_hash_map<u32, CCondlistData>& CCondlist::getInfoPortionCheck(void)
{
	return m_mInfoPortionCheck;
}

const xr_hash_map<u32, CCondlistData>& CCondlist::getInfoPortionSet(void) const
{
	return m_mInfoPortionSet;
}

xr_hash_map<u32, CCondlistData>& CCondlist::getInfoPortionSet(void)
{
	return m_mInfoPortionSet;
}

void CCondlist::addInfoPortionSet(u32 nID, const CCondlistData& data)
{
	R_ASSERT2(m_mInfoPortionSet.find(nID) == m_mInfoPortionSet.end(),
		"something is wrong?");
	m_mInfoPortionSet[nID] = data;
}

void CCondlist::addInfoPortionSet(const std::pair<u32, CCondlistData>& pair)
{
	addInfoPortionSet(pair.first, pair.second);
}

void CCondlist::addInfoPortionCheck(u32 nID, const CCondlistData& data)
{
	R_ASSERT2(m_mInfoPortionCheck.find(nID) == m_mInfoPortionCheck.end(),
		"something is bruh");

	m_mInfoPortionCheck[nID] = data;
}

void CCondlist::addInfoPortionCheck(const std::pair<u32, CCondlistData>& pair)
{
	addInfoPortionCheck(pair.first, pair.second);
}

const char* CCondlist::getSectionName(void) const
{
	return m_sectionname;
}

void CCondlist::setSectionName(const char* pFSStringField)
{
	R_ASSERT2(strlen(pFSStringField) <= (sizeof(m_sectionname) / sizeof(char)),
		"overflow, shrink buffer!");

	std::memset(m_sectionname, 0, sizeof(m_sectionname));
	std::memcpy(
		m_sectionname, pFSStringField, strlen(pFSStringField) * sizeof(char));
}

CCondlistInfo::CCondlistInfo() :
	m_infocheck_name{}, m_infoset_name{}, m_text_name{}
{
}

CCondlistInfo::~CCondlistInfo() {}

void CCondlistInfo::setInfoCheck(const char* pBuffer, size_t nStringLength)
{
	R_ASSERT2(nStringLength <= (sizeof(m_infocheck_name) / sizeof(char)),
		"overflow, shrink buffer!");

	std::memset(m_infocheck_name, 0, sizeof(m_infocheck_name));
	std::memcpy(m_infocheck_name, pBuffer, nStringLength);
}

void CCondlistInfo::setInfoSet(const char* pBuffer, size_t nStringLength)
{
	R_ASSERT2(nStringLength <= (sizeof(m_infoset_name) / sizeof(char)),
		"overflow, shrink buffer!");

	std::memset(m_infoset_name, 0, sizeof(m_infoset_name));
	std::memcpy(m_infoset_name, pBuffer, nStringLength);
}

void CCondlistInfo::setText(const char* pBuffer, size_t nStringLength)
{
	R_ASSERT2(nStringLength <= (sizeof(m_text_name) / sizeof(char)),
		"overflow, shrink  buffer!");

	std::memset(m_text_name, 0, sizeof(m_text_name));
	std::memcpy(m_text_name, pBuffer, nStringLength);
}

void CCondlistInfo::clearInfoCheck()
{
	std::memset(m_infocheck_name, 0, sizeof(m_infocheck_name));
}

void CCondlistInfo::clearInfoSet()
{
	std::memset(m_infoset_name, 0, sizeof(m_infoset_name));
}

void CCondlistInfo::clearText()
{
	std::memset(m_text_name, 0, sizeof(m_text_name));
}

void CCondlistInfo::clear()
{
	clearInfoCheck();
	clearInfoSet();
	clearText();
}

const char* CCondlistInfo::getInfoCheckName(void)
{
	return m_infocheck_name;
}

const char* CCondlistInfo::getTextName(void)
{
	return m_text_name;
}

const char* CCondlistInfo::getInfoSetName(void)
{
	return m_infoset_name;
}

CCondlistEmbedded::CCondlistEmbedded() :
	m_nArrayCheckSize{}, m_nArraySetSize{}, m_sectionname{}
{
}

CCondlistEmbedded::~CCondlistEmbedded() {}

const CCondlistEmbedded::xr_condlistdata& CCondlistEmbedded::getInfoPortionSet(
	void) const
{
	return this->m_aInfoPortionSet;
}

CCondlistEmbedded::xr_condlistdata& CCondlistEmbedded::getInfoPortionSet(void)
{
	return this->m_aInfoPortionSet;
}

const CCondlistEmbedded::xr_condlistdata&
CCondlistEmbedded::getInfoPortionCheck(void) const
{
	return this->m_aInfoPortionCheck;
}

CCondlistEmbedded::xr_condlistdata& CCondlistEmbedded::getInfoPortionCheck(void)
{
	return this->m_aInfoPortionCheck;
}

void CCondlistEmbedded::addInfoPortionSet(u32 nID, const CCondlistData& data)
{
	R_ASSERT2(nID <= this->m_aInfoPortionSet.size(), "overflow!");

	this->m_aInfoPortionSet[nID] = data;
}

void CCondlistEmbedded::addInfoPortionCheck(u32 nID, const CCondlistData& data)
{
	R_ASSERT2(nID <= this->m_aInfoPortionCheck.size(), "overflow!");

	this->m_aInfoPortionCheck[nID] = data;
}

const char* CCondlistEmbedded::getSectionName(void) const
{
	return this->m_sectionname;
}

void CCondlistEmbedded::setSectionName(const char* pString)
{
	R_ASSERT2(strlen(pString) <= (sizeof(this->m_sectionname) / sizeof(char)),
		"overflow! too big string");

	std::memset(this->m_sectionname, 0, sizeof(this->m_sectionname));
	std::memcpy(this->m_sectionname, pString, strlen(pString) * sizeof(char));
}

int CCondlistEmbedded::getArrayCheckSize(void) const
{
	return m_nArrayCheckSize;
}

void CCondlistEmbedded::setArrayCheckSize(int nSize) 
{
	m_nArrayCheckSize = nSize;
}

int CCondlistEmbedded::getArraySetSize(void) const
{
	return m_nArraySetSize;
}

void CCondlistEmbedded::setArraySetSize(int nSize) 
{
	m_nArraySetSize = nSize;
}
