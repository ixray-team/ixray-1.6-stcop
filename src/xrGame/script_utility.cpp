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
	m_bRequired{}, m_bExpected{}, m_nID{}, m_pProbabilityNumberAsString{},
	m_pFunctionName{}, m_pInfoPortionName{}, m_pParams{}
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
	return m_pProbabilityNumberAsString;
}

void CCondlistData::setProbability(const char* pFSStringField)
{
	m_pProbabilityNumberAsString = pFSStringField;
}

const char* CCondlistData::getFunctionName(void) const
{
	return m_pFunctionName;
}

void CCondlistData::setFunctionName(const char* pFSStringField)
{
	m_pFunctionName = pFSStringField;
}

const char* CCondlistData::getInfoPortionName(void) const
{
	return m_pInfoPortionName;
}

void CCondlistData::setInfoPortionName(const char* pFSStringField)
{
	m_pInfoPortionName = pFSStringField;
}

const char* CCondlistData::getParams(void) const
{
	return m_pParams;
}

void CCondlistData::setParams(const char* pFSStringField)
{
	m_pParams = pFSStringField;
}