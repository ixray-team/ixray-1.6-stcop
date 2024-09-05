#include "StdAfx.h"
#include "script_xr_logic.h"
#include "script_xr_conditions.h"
#include "script_xr_effects.h"
#include "Level.h"
#include "ai_space.h"
#include "xrServer_Objects_ALife_All.h"
#include "script_game_object.h"

CScriptXRParser::CScriptXRParser() :
	m_nCurrentIndex{}, m_pLevel{}, m_pXRConditions{}, m_pXREffects{}
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
	m_nCurrentIndex = 0;
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
		nSymbol == ':' || nSymbol == '/' || nSymbol == '\\')
	{
		result = true;
	}

	return result;
}

bool CScriptXRParser::isFunctionArgumentSymbolValidForParsing(
	char nSymbol) const
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
	else if (nSymbol == '.' || nSymbol == ',' || nSymbol == '{' ||
		nSymbol == '}' || nSymbol == '%' || nSymbol == '@' || nSymbol == '-' ||
		nSymbol == '+' || nSymbol == '=' || nSymbol == '~' || nSymbol == '!' ||
		nSymbol == '_' || nSymbol == '/' || nSymbol == '\\')
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

const char* CScriptXRParser::lua_pickSectionFromCondlist(
	CScriptGameObject* pClientPlayer, CScriptGameObject* pClientObject,
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

	CCondlistInfo current_info;

	CCondlistEmbedded condlist;

	const char* pResult{};

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
						R_ASSERT2(current_section_size <=
								ixray::kCondlistInfoStringSize,
							"[xr_parser] too big section");
						current_info.setText(
							current_section_name, current_section_size);
					}
				}

				was_found_section = false;
				bool bNeedToBreak{};
				parseCondlistInfo(current_info, condlist);
				pResult = pickSectionFromCondlist(
					condlist, pClientPlayer, pClientObject, 0, bNeedToBreak);

				if (bNeedToBreak)
					return pResult;

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
					R_ASSERT2(
						current_section_size <= ixray::kCondlistInfoStringSize,
						"[xr_parser] too big string of section");
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

				was_found_check = false;

				current_check_name[current_check_size] = it;
				++current_check_size;

				R_ASSERT2(current_check_size <= ixray::kCondlistInfoStringSize,
					"[xr_parser] too big string between brackets {...}");
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
					R_ASSERT2(
						current_section_size <= ixray::kCondlistInfoStringSize,
						"[xr_parser] too big string of section");
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
					R_ASSERT2(
						current_set_size <= ixray::kCondlistInfoStringSize,
						"[xr_parser] too big string between percents %...%");
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

		if (string_length == 0)
			return pResult;

		if (current_section_size)
		{
			R_ASSERT2(current_section_size <= ixray::kCondlistInfoStringSize,
				"[xr_parser] too big string of section");
			current_info.setText(current_section_name, current_section_size);
		}

		bool bNeedToBreak{};
		parseCondlistInfo(current_info, condlist);
		pResult = pickSectionFromCondlist(
			condlist, pClientPlayer, pClientObject, 0, bNeedToBreak);

		if (bNeedToBreak)
			return pResult;

		current_info.clear();
	}

	return pResult;
}

const char* CScriptXRParser::lua_pickSectionFromCondlist(
	CScriptGameObject* pClientPlayer, CSE_ALifeDynamicObject* pServerObject,
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

	CCondlistInfo current_info;

	CCondlistEmbedded condlist;

	const char* pResult{};

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
						R_ASSERT2(current_section_size <=
								ixray::kCondlistInfoStringSize,
							"[xr_parser] too big string of section name");

						current_info.setText(
							current_section_name, current_section_size);
					}
				}

				was_found_section = false;
				bool bNeedToBreak{};
				parseCondlistInfo(current_info, condlist);
				pResult = pickSectionFromCondlist(
					condlist, pClientPlayer, pServerObject, 1, bNeedToBreak);

				if (bNeedToBreak)
					return pResult;

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
					R_ASSERT2(
						current_section_size <= ixray::kCondlistInfoStringSize,
						"[xr_parser] too big string of section name");

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

				was_found_check = false;

				current_check_name[current_check_size] = it;
				++current_check_size;

				R_ASSERT2(current_check_size <= ixray::kCondlistInfoStringSize,
					"[xr_parser] too big information string between brackets {...}");

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
					R_ASSERT2(
						current_section_size <= ixray::kCondlistInfoStringSize,
						"too big section name!");

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

					R_ASSERT2(
						current_set_size <= ixray::kCondlistInfoStringSize,
						"too big %...% string");
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

		if (string_length == 0)
			return pResult;

		if (current_section_size)
		{
			R_ASSERT2(current_section_size <= ixray::kCondlistInfoStringSize,
				"too big section length");
			current_info.setText(current_section_name, current_section_size);
		}

		bool bNeedToBreak{};
		parseCondlistInfo(current_info, condlist);
		pResult = pickSectionFromCondlist(
			condlist, pClientPlayer, pServerObject, 1, bNeedToBreak);

		if (bNeedToBreak)
			return pResult;

		current_info.clear();
	}

	return pResult;
}

const char* CScriptXRParser::lua_pickSectionFromCondlist(
	CSE_ALifeDynamicObject* pServerPlayer,
	CSE_ALifeDynamicObject* pServerObject, const char* pSectionName,
	const char* pFieldName, const char* pSourceName)
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

	CCondlistInfo current_info;

	CCondlistEmbedded condlist;

	const char* pResult{};

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
						R_ASSERT2(current_section_size <=
								ixray::kCondlistInfoStringSize,
							"[xr_parser] too big section name");

						current_info.setText(
							current_section_name, current_section_size);
					}
				}

				was_found_section = false;

				bool bNeedToBreak{};
				parseCondlistInfo(current_info, condlist);
				pResult = pickSectionFromCondlist(
					condlist, pServerPlayer, pServerObject, 2, bNeedToBreak);

				if (bNeedToBreak)
					return pResult;

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
					R_ASSERT2(
						current_section_size <= ixray::kCondlistInfoStringSize,
						"[xr_parser] too big section name");

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

				was_found_check = false;

				current_check_name[current_check_size] = it;
				++current_check_size;

				R_ASSERT2(current_check_size <= ixray::kCondlistInfoStringSize,
					"[xr_parser] too big string between brackets {...}");
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
					R_ASSERT2(
						current_section_size <= ixray::kCondlistInfoStringSize,
						"[xr_parser] too big string of section");
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
					R_ASSERT2(
						current_set_size <= ixray::kCondlistInfoStringSize,
						"too big string between percents %...%");
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

		if (string_length == 0)
			return pResult;

		if (current_section_size)
		{
			R_ASSERT2(current_section_size <= ixray::kCondlistInfoStringSize,
				"too big string of section");
			current_info.setText(current_section_name, current_section_size);
		}

		bool bNeedToBreak{};
		parseCondlistInfo(current_info, condlist);
		pResult = pickSectionFromCondlist(
			condlist, pServerPlayer, pServerObject, 2, bNeedToBreak);

		if (bNeedToBreak)
			return pResult;

		current_info.clear();
	}

	return pResult;
}

void CScriptXRParser::parseCondlistInfo(
	CCondlistInfo& info, CCondlistEmbedded& result)
{
	result.setSectionName(info.getTextName());
	u32 nCheckSize = parseInfoportions(
		info.getInfoCheckName(), result.getInfoPortionCheck());
	u32 nSetSize =
		parseInfoportions(info.getInfoSetName(), result.getInfoPortionSet());

	result.setArrayCheckSize(nCheckSize);
	result.setArraySetSize(nSetSize);
}

u32 CScriptXRParser::parseInfoportions(
	const char* pBuffer, CCondlistEmbedded::xr_condlistdata& result)
{
	R_ASSERT2(pBuffer, "string must be valid!");

	char buffer[ixray::kCondlistDataStringSize]{};
	u32 buffer_size{};
	u32 index{};
	auto string_length = strlen(pBuffer);

	if (string_length)
	{
		for (auto i = 0; i < string_length; ++i)
		{
			CCondlistData value;

			if (pBuffer[i] == '{' || pBuffer[i] == '%')
			{
				continue;
			}

			R_ASSERT2(index <= ixray::kCondlistEmbeddedDataSize, "overflow");

			switch (pBuffer[i])
			{
			case '+':
			{
				u32 z = i;
				z += 1;

				while (pBuffer[z] && (!isSymbolEvent(pBuffer[z])))
				{
					buffer[buffer_size] = pBuffer[z];
					++z;
					++buffer_size;
				}

				R_ASSERT2(buffer_size <= ixray::kCondlistDataStringSize,
					"too big string! reduce it in your config please");

				value.setInfoPortionName(buffer);
				value.setRequired(true);
				result[index] = value;
				++index;
				--z;
				i = z;

				std::memset(buffer, 0, sizeof(buffer));
				buffer_size = 0;

				break;
			}
			case '-':
			{
				u32 z = i;
				z += 1;

				while (pBuffer[z] && (!isSymbolEvent(pBuffer[z])))
				{
					buffer[buffer_size] = pBuffer[z];
					++z;
					++buffer_size;
				}

				R_ASSERT2(buffer_size <= ixray::kCondlistDataStringSize,
					"too big string! reduce it in your config please");

				value.setInfoPortionName(buffer);
				value.setRequired(false);
				result[index] = value;
				++index;
				--z;
				i = z;

				std::memset(buffer, 0, sizeof(buffer));
				buffer_size = 0;

				break;
			}
			case '=':
			{
				u32 z = i;
				char params[ixray::kCondlistDataStringSize]{};
				u32 params_size{};
				z += 1;

				while (pBuffer[z] && (!isSymbolEvent(pBuffer[z])))
				{
					if (pBuffer[z] == '(')
					{
						u32 z1 = z;
						++z1;

						while (pBuffer[z1] != ')')
						{
							params[params_size] = pBuffer[z1];
							++z1;
							++params_size;
						}

						R_ASSERT2(params_size <= ixray::kCondlistDataStringSize,
							"too big string!");

						z = z1;
						break;
					}

					buffer[buffer_size] = pBuffer[z];
					++z;
					++buffer_size;
				}

				R_ASSERT2(buffer_size <= ixray::kCondlistDataStringSize,
					"too big string! reduce it in your config please");

				value.setFunctionName(buffer);
				value.setParams(params);
				value.setExpected(true);

				result[index] = value;
				++index;
				--z;
				i = z;

				std::memset(buffer, 0, sizeof(buffer));
				buffer_size = 0;

				break;
			}
			case '~':
			{
				u32 z = i;
				z += 1;
				while (pBuffer[z] && (!isSymbolEvent(pBuffer[z])))
				{
					buffer[buffer_size] = pBuffer[z];
					++z;
				}

				R_ASSERT2(buffer_size <= ixray::kCondlistProbabilityStringSize,
					"too big number lol????? string is too big!");

				value.setProbability(buffer);
				result[index] = value;
				++index;
				--z;
				i = z;

				std::memset(buffer, 0, sizeof(buffer));
				buffer_size = 0;

				break;
			}
			case '!':
			{
				u32 z = i;
				char params[ixray::kCondlistDataStringSize]{};
				u32 params_size{};
				z += 1;

				while (pBuffer[z] && (!isSymbolEvent(pBuffer[z])))
				{
					if (pBuffer[z] == '(')
					{
						u32 z1 = z;
						++z1;

						while (pBuffer[z1] != ')')
						{
							params[params_size] = pBuffer[z1];
							++z1;
							++params_size;
						}

						z = z1;
						break;
					}

					buffer[buffer_size] = pBuffer[z];
					++z;
					++buffer_size;
				}

				R_ASSERT2(buffer_size <= ixray::kCondlistDataStringSize,
					"too big string!");

				value.setFunctionName(buffer);
				value.setParams(params);
				value.setExpected(false);
				result[index] = value;
				++index;
				--z;
				i = z;

				std::memset(buffer, 0, sizeof(buffer));
				buffer_size = 0;

				break;
			}
			}
		}
	}

	return index;
}

CScriptXRParser* get_xr_parser()
{
	R_ASSERT2(Level().getScriptXRParser(),
		"something is wrong! early calling or late calling");

	return Level().getScriptXRParser();
}

void CScriptXRParser::script_register(lua_State* pState)
{
	if (pState)
	{
		luabind::module(pState)
			[luabind::class_<CScriptXRParser>("CScriptXRParser")
					.def("pick_section_from_condlist",
						(const char* (CScriptXRParser::*)(CScriptGameObject*,
							CScriptGameObject*, const char*, const char*,
							const char*)) &
							CScriptXRParser::lua_pickSectionFromCondlist)
					.def("pick_section_from_condlist",
						(const char* (CScriptXRParser::*)(CScriptGameObject*,
							CSE_ALifeDynamicObject*, const char*, const char*,
							const char*)) &
							CScriptXRParser::lua_pickSectionFromCondlist)
					.def("pick_section_from_condlist",
						(const char* (
							CScriptXRParser::*)(CSE_ALifeDynamicObject*,
							CSE_ALifeDynamicObject*, const char*, const char*,
							const char*)) &
							CScriptXRParser::lua_pickSectionFromCondlist),

				luabind::def("get_xr_parser_manager", get_xr_parser)];
	}
}

// 0 - client actor | client object
// 1 - client actor | server object
// 2 - server actor | server object
const char* CScriptXRParser::pickSectionFromCondlist(
	CCondlistEmbedded& condlist, void* pActor, void* pObject,
	int nCallingVersion, bool& bNeedToBreak)
{
	CScriptGameObject* pClientActor{};
	CSE_ALifeDynamicObject* pServerActor{};

	CScriptGameObject* pClientObject{};
	CSE_ALifeDynamicObject* pServerObject{};

	R_ASSERT2(nCallingVersion <= 2,
		"we support only three types of overloadings (because original GSC "
		"built a such architecture), first variant is when we have actor as "
		"client and object as client, second variant is when client actor but "
		"server object and third the last variant is when server actor and "
		"object is server too");

	switch (nCallingVersion)
	{
	case 0:
	{
		pClientActor = static_cast<CScriptGameObject*>(pActor);
		pClientObject = static_cast<CScriptGameObject*>(pObject);
		break;
	}
	case 1:
	{
		pClientActor = static_cast<CScriptGameObject*>(pActor);
		pServerObject = static_cast<CSE_ALifeDynamicObject*>(pObject);
		break;
	}
	case 2:
	{
		pServerActor = static_cast<CSE_ALifeDynamicObject*>(pActor);
		pServerObject = static_cast<CSE_ALifeDynamicObject*>(pObject);
		break;
	}
	default:
	{
		R_ASSERT2(false, "can't be!");
		break;
	}
	}

	bool is_infoportion_conditions_met = true;

	u32 probability = Random.randI(0, 100);

	for (int i = 0; i < condlist.getArrayCheckSize(); ++i)
	{
		CCondlistData& data = condlist.getInfoPortionCheck()[i];

		if (data.getProbability() && strlen(data.getProbability()) > 0)
		{
			int nCastedProbability = atoi(data.getProbability());

			R_ASSERT2(nCastedProbability >= 0 && nCastedProbability <= 100,
				"expected probability to be from 0 to 100");

			// we need to be sure that atoi gave us a '0', because otherwise the
			// input is invalid and need to notify user
			R_ASSERT2(nCastedProbability > 0 ||
					(nCastedProbability == 0 &&
						data.getProbability()[0] == '0'),
				"invalid input of probability string! bad cast!");

			if (nCastedProbability < probability)
			{
				is_infoportion_conditions_met = false;
				break;
			}
		}
		else if (data.getFunctionName() && strlen(data.getFunctionName()) > 0)
		{
#ifdef IXRAY_XR_PARSER_USE_LUA_BACKEND
	// only for debug build, we should catch the invalid calling because of non
	// existing function or we need to understand why the calling of function
	// crash the system (because at some point, function expected array of
	// numbers but got string and thus function calling is ruined)
	#ifdef DEBUG
			try
			{
	#endif

				xr_embedded_params_t parsed_params{};

				size_t nParsedBufferSize =
					parseParams(data.getParams(), parsed_params);

				static_assert(ixray::kXRParserParamsBufferSize >= 1 &&
					"can't be negative or zero!");
				static_assert(ixray::kXRParserParamBufferSize >= 1 &&
					"can't be negative");

				luabind::object params_to_lua =
					luabind::newtable(ai().script_engine().lua());

				for (int i = 0; i < nParsedBufferSize; ++i)
				{
					// because in lua indexing starts from 1 not from 0!
					params_to_lua[i + 1] =
						static_cast<const char*>(&parsed_params[i][0]);
				}

				luabind::functor<bool> function_from_xr_conditions;

				char function_name[ixray::kXRParserFunctionNameBufferSize]{
					"xr_conditions."};

				constexpr auto sizeOfXRConditionsString =
					sizeof("xr_conditions.");

				R_ASSERT2(
					sizeOfXRConditionsString + strlen(data.getFunctionName()) <=
						ixray::kXRParserFunctionNameBufferSize,
					"overflow!");

				std::memcpy(function_name + (sizeOfXRConditionsString - 1),
					data.getFunctionName(), strlen(data.getFunctionName()));

				ai().script_engine().functor(
					function_name, function_from_xr_conditions);

				if (data.getParams() && strlen(data.getParams()))
				{
					bool bResultFromCalling{};

					switch (nCallingVersion)
					{
					case 0:
					{
						bResultFromCalling = function_from_xr_conditions(
							pClientActor, pClientObject, params_to_lua);

						break;
					}
					case 1:
					{
						bResultFromCalling = function_from_xr_conditions(
							pClientActor, pServerObject, params_to_lua);

						break;
					}
					case 2:
					{
						bResultFromCalling = function_from_xr_conditions(
							pServerActor, pServerObject, params_to_lua);

						break;
					}
					}

					if (bResultFromCalling)
					{
						if (data.getExpected() == false)
						{
							is_infoportion_conditions_met = false;
							break;
						}
					}
					else
					{
						if (data.getExpected())
						{
							is_infoportion_conditions_met = false;
							break;
						}
					}
				}
				else
				{
					bool bResultFromCalling{};
					switch (nCallingVersion)
					{
					case 0:
					{
						bResultFromCalling = function_from_xr_conditions(
							pClientActor, pClientObject);

						break;
					}
					case 1:
					{
						bResultFromCalling = function_from_xr_conditions(
							pClientActor, pServerObject);

						break;
					}
					case 2:
					{
						bResultFromCalling = function_from_xr_conditions(
							pServerActor, pServerObject);

						break;
					}
					default:
					{
						R_ASSERT(false);
						break;
					}
					}

					if (bResultFromCalling)
					{
						if (data.getExpected() == false)
						{
							is_infoportion_conditions_met = false;
							break;
						}
					}
					else
					{
						if (data.getExpected())
						{
							is_infoportion_conditions_met = false;
							break;
						}
					}
				}

	#ifdef DEBUG
			}
			catch (...)
			{
				R_ASSERT2(false, "failed to issue calling");
			}
	#endif

#elif defined(IXRAY_XR_PARSER_USE_CPP_BACKEND)
#endif
		}
		else if (ixray::has_alife_info(data.getInfoPortionName()))
		{
			if (!data.getRequired())
			{
				is_infoportion_conditions_met = false;
				break;
			}
		}
		else
		{
			if (data.getRequired())
			{
				is_infoportion_conditions_met = false;
				break;
			}
		}
	}

	if (is_infoportion_conditions_met)
	{
		for (int i = 0; i < condlist.getArraySetSize(); ++i)
		{
			CCondlistData& data = condlist.getInfoPortionSet()[i];
			if (data.getFunctionName() && strlen(data.getFunctionName()))
			{
#ifdef IXRAY_XR_PARSER_USE_LUA_BACKEND
	// only for debug build, we should catch the invalid calling because of non
	// existing function or we need to understand why the calling of function
	// crash the system (because at some point, function expected array of
	// numbers but got string and thus function calling is ruined)
	#ifdef DEBUG
				try
				{
	#endif

					xr_embedded_params_t parsed_params{};

					size_t nParsedBufferSize =
						parseParams(data.getParams(), parsed_params);

					static_assert(ixray::kXRParserParamsBufferSize >= 1 &&
						"can't be negative or zero!");
					static_assert(ixray::kXRParserParamBufferSize >= 1 &&
						"can't be negative or 0");

					luabind::object params_to_lua =
						luabind::newtable(ai().script_engine().lua());

					for (int i = 0; i < nParsedBufferSize; ++i)
					{
						// because in lua indexing starts from 1 not from 0!
						params_to_lua[i + 1] =
							static_cast<const char*>(&parsed_params[i][0]);
					}

					luabind::functor<void> function_from_xr_effects;

					char function_name[ixray::kXRParserFunctionNameBufferSize]{
						"xr_effects."};

					constexpr auto sizeOfXREffectsString =
						sizeof("xr_effects.");

					R_ASSERT2(sizeOfXREffectsString +
								strlen(data.getFunctionName()) <=
							ixray::kXRParserFunctionNameBufferSize,
						"overflow!");

					std::memcpy(function_name + (sizeOfXREffectsString - 1),
						data.getFunctionName(), strlen(data.getFunctionName()));

					ai().script_engine().functor(
						function_name, function_from_xr_effects);

					if (data.getParams() && strlen(data.getParams()))
					{
						switch (nCallingVersion)
						{
						case 0:
						{
							function_from_xr_effects(
								pClientActor, pClientObject, params_to_lua);

							break;
						}
						case 1:
						{
							function_from_xr_effects(
								pClientActor, pServerObject, params_to_lua);

							break;
						}
						case 2:
						{
							function_from_xr_effects(
								pServerActor, pServerObject, params_to_lua);

							break;
						}
						}
					}
					else
					{
						switch (nCallingVersion)
						{
						case 0:
						{
							function_from_xr_effects(
								pClientActor, pClientObject);

							break;
						}
						case 1:
						{
							function_from_xr_effects(
								pClientActor, pServerObject);

							break;
						}
						case 2:
						{
							function_from_xr_effects(
								pServerActor, pServerObject);

							break;
						}
						default:
						{
							R_ASSERT(false);
							break;
						}
						}
					}

	#ifdef DEBUG
				}
				catch (...)
				{
					R_ASSERT2(false, "failed to issue calling");
				}
	#endif

#elif defined(IXRAY_XR_PARSER_USE_CPP_BACKEND)
#endif
			}
			else if (data.getRequired())
			{
				if (!ixray::has_alife_info(data.getInfoPortionName()))
				{
					switch (nCallingVersion)
					{
					case 0:
					case 1:
					{
						R_ASSERT(pClientActor && "must be valid!");

						pClientActor->GiveInfoPortion(
							data.getInfoPortionName());

						break;
					}
					case 2:
					{
						R_ASSERT(false &&
							"suppose not to call this section, because "
							"implementation defined only for client!");

						break;
					}
					}
				}
			}
			else
			{
				if (ixray::has_alife_info(data.getInfoPortionName()))
				{
					switch (nCallingVersion)
					{
					case 0:
					case 1:
					{
						R_ASSERT(pClientActor &&
							"if it is invalid you must pass an argument not "
							"nil on lua side (db.actor)");

						pClientActor->DisableInfoPortion(
							data.getInfoPortionName());

						break;
					}
					case 2:
					{
						R_ASSERT(false &&
							"suppose not to call this section, because "
							"implementation defined only for client!");
						break;
					}
					}
				}
			}
		}

		std::string_view section_name = condlist.getSectionName();

		if (section_name == ixray::kReservedWordNever)
		{
			bNeedToBreak = true;
			return nullptr;
		}
		else
		{
			bNeedToBreak = true;
			return condlist.getSectionName();
		}
	}

	return nullptr;
}

size_t CScriptXRParser::parseParams(
	const char* pParamsBuffer, xr_embedded_params_t& result)
{
	size_t nIndex{};

	if (pParamsBuffer)
	{
		size_t nLength = strlen(pParamsBuffer);
		if (nLength)
		{
			char argument[ixray::kXRParserParamBufferSize]{};
			size_t argument_size{};
			for (size_t i = 0; i < nLength; ++i)
			{
				R_ASSERT2(nIndex <= ixray::kXRParserParamsBufferSize,
					"overflow, too many arguments for function!");

				char nCurrentSymbol = pParamsBuffer[i];

				if (isFunctionArgumentSymbolValidForParsing(nCurrentSymbol))
				{
					R_ASSERT2(argument_size <= ixray::kXRParserParamBufferSize,
						"too big string! reduce it vasyan");

					argument[argument_size] = nCurrentSymbol;
					++argument_size;
				}
				else if (nCurrentSymbol == ':')
				{
					if (argument_size > 0)
					{
						std::memcpy(&result[nIndex][0], argument,
							argument_size * sizeof(char));
						memset(argument, 0, sizeof(argument));
						argument_size = 0;
						++nIndex;
					}
				}
				else if (nCurrentSymbol == ' ' || nCurrentSymbol == '\n')
				{
					// ignore
				}
			}

			if (argument_size)
			{
				std::memcpy(
					&result[nIndex][0], argument, argument_size * sizeof(char));
				++nIndex;
			}

			R_ASSERT2(nIndex <= ixray::kXRParserParamsBufferSize,
				"overflow, reduce string");
		}
	}

	return nIndex;
}
