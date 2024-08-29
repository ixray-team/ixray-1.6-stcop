#include "StdAfx.h"
#include "script_xr_logic.h"
#include "script_xr_conditions.h"
#include "script_xr_effects.h"

CScriptXRParser::CScriptXRParser() :
	m_pLevel{}, m_pXRConditions{}, m_pXREffects{}
{
}

CScriptXRParser::~CScriptXRParser() {}

void CScriptXRParser::initialize(CLevel* pLevelManager,
	CScriptXRConditionsStorage* pStorageXRConditions,
	CScriptXREffectsStorage* pStorageXREffects)
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

bool CScriptXRParser::isSymbolValidForParsing(char nSymbol) const
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

bool CScriptXRParser::isSymbolEvent(char nSymbol) const
{
	bool result{};

	if (nSymbol == '+' || nSymbol == '-' || nSymbol == '~' || nSymbol == '!' ||
		nSymbol == '=' || nSymbol == '}' || nSymbol == '%')
		result = true;

	return result;
}

xr_hash_map<u32, CCondlist> CScriptXRParser::parseCondlist(
	const char* pSectionName, const char* pFieldName, const char* pSourceName)
{
	bool was_found_check{};
	bool was_found_set{};
	bool was_found_section{};
	u8 counter_percent_symbol{};

	char current_section_name[ixray::kCondlistInfoStringSize]{};
	u32 current_section_size{};

	char current_check_name[ixray::kCondlistInfoStringSize]{};
	u32 current_check_size{};

	char current_set_name[ixray::kCondlistInfoStringSize]{};
	u32 current_set_size{};

	xr_infos buffer{};

	CCondlistInfo current_info;

	xr_hash_map<u32, CCondlist> result;

	if (pSourceName)
	{
		auto string_length = strlen(pSourceName);

		for (size_t i = 0; i < string_length; ++i)
		{
			char it = pSourceName[i];

			if (isSymbolValidForParsing(it) == false)
			{
				R_ASSERT2(
					false, "bruh, use symbol from mask only! unhandled symbol");
				break;
			}

			if (isspace(it))
			{
				continue;
			}

			if (it == ',')
			{
				if (!was_found_section)
				{
					current_info.clearText();
				}
				else
				{
					if (strlen(current_section_name))
					{
						current_info.setText(
							current_section_name, current_section_size);
					}
					else
					{
						current_info.clearText();
					}
				}

				was_found_section = false;
				buffer.first[buffer.second] = current_info;
				++buffer.second;
				current_info.clear();

				std::memset(
					current_section_name, 0, ixray::kCondlistInfoStringSize);
				current_section_size = 0;

				continue;
			}

			if (it == '{')
			{
				if (current_section_size)
				{
					current_info.setText(
						current_section_name, current_section_size);

					std::memset(current_section_name, 0,
						ixray::kCondlistInfoStringSize);
					current_section_size = 0;

					was_found_section = false;
				}

				if (was_found_set)
				{
					R_ASSERT2(false,
						"can't be! you forgot to add second percent in set "
						"infoportion statement! Check your ltx file -_-'");
					break;
				}

				if (was_found_check)
				{
					R_ASSERT2(false, "Found duplicate!");
					break;
				}

				was_found_check = true;
				current_check_name[current_check_size] = it;
				++current_check_size;

				continue;
			}

			if (it == '}')
			{
				if (!was_found_check)
				{
					R_ASSERT2(false,
						"Found right bracket without left bracket. Check your "
						"ltx file");
					break;
				}

				was_found_check = true;

				current_check_name[current_check_size] = it;
				++current_check_size;

				current_info.setInfoCheck(
					current_check_name, current_check_size);
				std::memset(
					current_check_name, 0, ixray::kCondlistInfoStringSize);
				current_check_size = 0;

				continue;
			}

			if (was_found_check)
			{
				current_check_name[current_check_size] = it;
				++current_check_size;
				continue;
			}

			if (it == '%')
			{
				if (current_section_size)
				{
					current_info.setText(
						current_section_name, current_section_size);
					std::memset(current_section_name, 0,
						ixray::kCondlistInfoStringSize);
					current_section_size = 0;
					was_found_section = false;
				}

				if (was_found_check)
				{
					R_ASSERT2(false,
						"Incorrect sentence of set infoportion. It has "
						"symbol bracket in!");
					break;
				}

				if (counter_percent_symbol > 2)
				{
					R_ASSERT2(false,
						"Incorrect sentence of infoportion. Bigger than two!");
					break;
				}

				++counter_percent_symbol;

				if (counter_percent_symbol < 2)
				{
					was_found_set = true;
					current_set_name[current_set_size] = it;
					++current_set_size;
					continue;
				}
				else if (counter_percent_symbol == 2)
				{
					was_found_set = false;
					counter_percent_symbol = 0;
					current_set_name[current_set_size] = it;
					++current_set_size;
					current_info.setInfoSet(current_set_name, current_set_size);

					std::memset(
						current_set_name, 0, ixray::kCondlistInfoStringSize);
					current_set_size = 0;

					continue;
				}
			}

			if (was_found_set)
			{
				if (!was_found_section)
					was_found_section = true;

				current_set_name[current_set_size] = it;
				++current_set_size;

				continue;
			}

			if (!was_found_set && !was_found_check)
			{
				switch (it)
				{
				case '+':
				{
					R_ASSERT2(false, "You forgot bracket or percent symbol!");
					break;
				}
				case '-':
				{
					R_ASSERT2(false, "You forgot bracket or percent symbol!");
					break;
				}
				case '=':
				{
					R_ASSERT2(false, "You forgot bracket or percent symbol!");
					break;
				}
				case '~':
				{
					R_ASSERT2(false, "You forgot bracket or percent symbol!");
					break;
				}
				case '!':
				{
					R_ASSERT2(false, "You forgot bracket or percent symbol!");
					break;
				}
				}

				was_found_section = true;
				current_section_name[current_section_size] = it;
				++current_section_size;
				continue;
			}
			else
			{
				R_ASSERT2(false,
					"Incorrect parsing. Can't parse section betwen two "
					"infoportion's sentences. Check your ltx");
				break;
			}
		}

		if (current_section_size)
		{
			current_info.setText(current_section_name, current_section_size);
		}

		buffer.first[buffer.second] = current_info;
		++buffer.second;

		current_info.clear();

		parseCondlistInfos(buffer, result);
	}

	// compiler must do std::move without obvious markering
	return result;
}

void CScriptXRParser::parseCondlistInfos(
	xr_infos& infos, xr_hash_map<u32, CCondlist>& result)
{
	if (!infos.second)
	{
		R_ASSERT2(false, "supposed to be a not empty buffer at all!");
		return;
	}

	for (size_t i = 0; i < infos.second; ++i)
	{
		result[i].setSectionName(infos.first[i].getTextName());
		parseInfoportions(
			infos.first[i].getInfoCheckName(), result[i].getInfoPortionCheck());
		parseInfoportions(
			infos.first[i].getInfoSetName(), result[i].getInfoPortionSet());
	}
}

void CScriptXRParser::parseInfoportions(
	const char* pBuffer, xr_hash_map<u32, CCondlistData>& result)
{
	R_ASSERT2(pBuffer, "string must be valid!");
}
