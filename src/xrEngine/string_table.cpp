#include "stdafx.h"
#include "string_table.h"
#include "xr_level_controller.h"
#include "../xrCore/FormatParsers/XML/xrXMLParser.h"
#include "XR_IOConsole.h"
#include "IGame_UICustom.h"
#include "IGame_Menu.h"
#include "IGame_Persistent.h"
#include "xr_input.h"

ENGINE_API CStringTable* g_pStringTable = nullptr;

STRING_TABLE_DATA* CStringTable::pData = nullptr;
xr_vector<xr_token> CStringTable::languages_token;
bool CStringTable::m_bWriteErrorsToLog = false;

CStringTable::CStringTable	()
{
	Init();
}

void CStringTable::Destroy	()
{
	xr_delete(pData);
}

void CStringTable::rescan()
{
	if(pData != nullptr)
		return;
	Destroy				();
	Init				();
}

inline static int CountFiles(const char* path, const char* mask)
{
	FS_FileSet folder = {};
	FS.file_list(folder, path, FS_ListFiles, mask);

	return folder.size();
}

// Если у нас не указаны доступные языки, то будем
// брать всё из папки text
inline static shared_str FillLanguages()
{
	FS_FileSet langSet = {};
	string_path path = {};
	FS.update_path(path, _game_config_, "");
	xr_sprintf(path, "%s%s", path, "text\\");
	FS.file_list(langSet, path, FS_ListFolders | FS_RootOnly);

	std::ostringstream result_stream = {};
	for (auto it = langSet.begin(); it != langSet.end(); ++it)
	{
		xr_string folderName = it->name;

		string_path current_path = {};
		xr_sprintf(current_path, "%s%s", path, it->name.c_str());
		if (CountFiles(current_path, "*.xml") <= 0)
		{
			continue;
		}

		if (!folderName.empty() && folderName.back() == '\\')
		{
			folderName.pop_back();
		}

		result_stream << folderName;
		if (std::next(it) != langSet.end())
		{
			result_stream << ",";
		}
	}

	return result_stream.str().c_str();
}

void CStringTable::Init		()
{
	if (pData != nullptr) {
		return;
	}

	if (languages_token.empty())
	{
		static auto languages = READ_IF_EXISTS(pSettings, r_string_wb, "string_table", "languages", nullptr);

		if (languages == nullptr)
		{
			languages = FillLanguages();
		}

		static int count = _GetItemCount(languages.c_str());

		for (u32 i = 0; i < count; i++)
		{
			string128 tmp = {};
			languages_token.push_back(xr_token());

			languages_token[i].id = i;
			languages_token[i].name = xr_strdup(_GetItem(languages.c_str(), i, tmp));
#ifdef DEBUG
			Msg("%d [%s]", languages_token[i].id, languages_token[i].name);
#endif // DEBUG
		}

		languages_token.push_back(xr_token());
		languages_token.back().id = -1;
		languages_token.back().name = nullptr;
	}
    
	pData = new STRING_TABLE_DATA();
	
	//имя языка, если не задано (nullptr), то первый <text> в <string> в XML
	pData->m_sLanguage = pSettings->r_string("string_table", "language");

	// Get preferred fallback language from EngineExternal
	pData->m_sFallbackLanguage = EngineExternal().GetPreferredFallbackLanguage();

	auto it = std::find_if(languages_token.begin(), languages_token.end(), [](const xr_token& item) {
		return item.name == pData->m_sLanguage;
	});
	if (it == languages_token.end())
	{
		pData->m_sLanguage = languages_token[0].name;
	}

	FS_FileSet fset;
	FS_FileSet efset;

	string_path files_mask = {};
	string_path exclude_files_mask = {};

	xr_sprintf(files_mask, "text\\%s\\*.xml", pData->m_sLanguage.c_str());
	FS.file_list(fset, _game_config_, FS_ListFiles, files_mask);

	xr_sprintf(exclude_files_mask, "text\\%s\\mod_*.xml", pData->m_sLanguage.c_str());
	FS.file_list(efset, _game_config_, FS_ListFiles, exclude_files_mask);

	for (const FS_File& File : fset)
	{
		if (efset.contains(File))
			continue;

		string_path fn, ext;
		_splitpath(File.name.c_str(), 0, 0, fn, ext);
		xr_strcat(fn, ext);

		Load(fn);
	}

	// Load fallback language files if fallback language is different from main language
	if (pData->m_sFallbackLanguage.size() > 0 && 
	    xr_strcmp(pData->m_sLanguage.c_str(), pData->m_sFallbackLanguage.c_str()) != 0)
	{
		FS_FileSet fallback_fset;
		FS_FileSet fallback_efset;

		xr_sprintf(files_mask, "text\\%s\\*.xml", pData->m_sFallbackLanguage.c_str());
		FS.file_list(fallback_fset, _game_config_, FS_ListFiles, files_mask);

		xr_sprintf(exclude_files_mask, "text\\%s\\mod_*.xml", pData->m_sFallbackLanguage.c_str());
		FS.file_list(fallback_efset, _game_config_, FS_ListFiles, exclude_files_mask);

		for (const FS_File& File : fallback_fset)
		{
			if (fallback_efset.contains(File))
				continue;

			string_path fn, ext;
			_splitpath(File.name.c_str(), 0, 0, fn, ext);
			xr_strcat(fn, ext);

			LoadFallback(fn);
		}
	}

	ReparseKeyBindings();
}

void CStringTable::Load	(const char* xml_file_full)
{
	CXml						uiXml;
	string_path					_s;
	xr_strconcat(_s, "text\\", pData->m_sLanguage.c_str() );

	uiXml.Load					(CONFIG_PATH, _s, xml_file_full);

	//общий список всех записей таблицы в файле
	int string_num = uiXml.GetNodesNum		(uiXml.GetRoot(), "string");

	for(int i=0; i<string_num; ++i)
	{
		const char* string_name = uiXml.ReadAttrib(uiXml.GetRoot(), "string", i, "id", nullptr);

		bool isDublicate = pData->m_StringTable.find(string_name) != pData->m_StringTable.end();
		if (isDublicate)
		{
			//VERIFY3(!isDublicate, "duplicate string table id", string_name);
			Msg("! duplicate string table id: %s", string_name);
		}

		const char* string_text		= uiXml.Read(uiXml.GetRoot(), "string:text", i,  nullptr);

		if(m_bWriteErrorsToLog && string_text)
			Msg("[string table] '%s' no translation in '%s'", string_name, pData->m_sLanguage.c_str() );
		
		VERIFY3						(string_text, "string table entry does not have a text", string_name);
		
		STRING_VALUE str_val		= ParseLine(string_text, false, &pData->m_string_key_binding, string_name);
		
		pData->m_StringTable[string_name] = str_val;
		const char* gamepad_text = uiXml.Read(uiXml.GetRoot(), "string:text_gamepad", i, nullptr);
		if (gamepad_text)
		{
			STRING_VALUE str_val = ParseLine(gamepad_text, false, &pData->m_string_key_binding_gamepad, string_name);
			pData->m_StringTableGamepad[string_name] = str_val;
		}
	}
}

void CStringTable::LoadFallback	(const char* xml_file_full)
{
	CXml						uiXml;
	string_path					_s;
	xr_strconcat(_s, "text\\", pData->m_sFallbackLanguage.c_str() );

	uiXml.Load					(CONFIG_PATH, _s, xml_file_full);

	//общий список всех записей таблицы в файле
	int string_num = uiXml.GetNodesNum		(uiXml.GetRoot(), "string");

	for(int i=0; i<string_num; ++i)
	{
		const char* string_name = uiXml.ReadAttrib(uiXml.GetRoot(), "string", i, "id", nullptr);

		bool isDublicate = pData->m_FallbackStringTable.find(string_name) != pData->m_FallbackStringTable.end();
		if (isDublicate)
		{
			//VERIFY3(!isDublicate, "duplicate string table id", string_name);
			Msg("! duplicate fallback string table id: %s", string_name);
			pData->FallbackBindingTemplates.erase(string_name);
		}

		const char* string_text		= uiXml.Read(uiXml.GetRoot(), "string:text", i,  nullptr);

		if(m_bWriteErrorsToLog && string_text)
			Msg("[fallback string table] '%s' no translation in '%s'", string_name, pData->m_sFallbackLanguage.c_str() );
		
		if (string_text != nullptr)
		{
			STRING_VALUE str_val		= ParseLine(string_text, false, &pData->FallbackBindingTemplates, string_name);
			
			pData->m_FallbackStringTable[string_name] = str_val;
		}
	}
}

void CStringTable::ReparseKeyBindings()
{
	if(pData == nullptr)
		return;
	
	const bool KeyboardMode = pInput && !pInput->GetControllerMode();
	for (const auto& [Id, Template] : pData->m_string_key_binding)
	{
		pData->m_StringTable[Id] = ParseLine(*Template);
		if (KeyboardMode)
		{
			pData->ObjectHintsWithShortKeys[Id] = ParseLine(*Template, true);
		}
	}
	if (KeyboardMode)
	{
		for (const auto& [Id, Template] : pData->FallbackBindingTemplates)
		{
			if (!pData->m_StringTable.contains(Id))
			{
				pData->ObjectHintsWithShortKeys[Id] = ParseLine(*Template, true);
			}
		}
	}
	for (auto& key : pData->m_string_key_binding_gamepad)
	{
		pData->m_StringTableGamepad[key.first] = ParseLine(*key.second);
	}
	if (g_pGameCustom)
	{
		g_pGameCustom->ReloadGamepadLegends();
	}
	if (g_pGamePersistent && g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive())
	{
		g_pGamePersistent->m_pMainMenu->ReloadLegend();
	}
}

void CStringTable::ReloadLanguage(const char* lang)
{
	// reload language
	Destroy();

	pData = new STRING_TABLE_DATA();
	pData->m_sLanguage = lang;

	// Get preferred fallback language from EngineExternal
	pData->m_sFallbackLanguage = EngineExternal().GetPreferredFallbackLanguage();

	FS_FileSet fset;
	FS_FileSet efset;

	string_path files_mask = {};
	string_path exclude_files_mask = {};

	xr_sprintf(files_mask, "text\\%s\\*.xml", pData->m_sLanguage.c_str());
	FS.file_list(fset, _game_config_, FS_ListFiles, files_mask);

	xr_sprintf(exclude_files_mask, "text\\%s\\mod_*.xml", pData->m_sLanguage.c_str());
	FS.file_list(efset, _game_config_, FS_ListFiles, exclude_files_mask);

	for (const FS_File& File : fset)
	{
		if (efset.contains(File))
			continue;

		string_path fn = {}, ext = {};
		_splitpath(File.name.c_str(), 0, 0, fn, ext);
		xr_strcat(fn, ext);

		Load(fn);
	}

	// Load fallback language files if fallback language is different from main language
	if (pData->m_sFallbackLanguage.size() > 0 && 
	    xr_strcmp(pData->m_sLanguage.c_str(), pData->m_sFallbackLanguage.c_str()) != 0)
	{
		FS_FileSet fallback_fset;
		FS_FileSet fallback_efset;

		xr_sprintf(files_mask, "text\\%s\\*.xml", pData->m_sFallbackLanguage.c_str());
		FS.file_list(fallback_fset, _game_config_, FS_ListFiles, files_mask);

		xr_sprintf(exclude_files_mask, "text\\%s\\mod_*.xml", pData->m_sFallbackLanguage.c_str());
		FS.file_list(fallback_efset, _game_config_, FS_ListFiles, exclude_files_mask);

		for (const FS_File& File : fallback_fset)
		{
			if (fallback_efset.contains(File))
				continue;

			string_path fn, ext;
			_splitpath(File.name.c_str(), 0, 0, fn, ext);
			xr_strcat(fn, ext);

			LoadFallback(fn);
		}
	}

	ReparseKeyBindings();
}

xr_string CStringTable::LangName()
{
	if (pData == nullptr || pData->m_sLanguage == nullptr)
	{
		return languages_token[0].name;
	}
	return pData->m_sLanguage.c_str();
}

STRING_VALUE CStringTable::ParseLine(const char* str, bool ShortKeyNames, STRING_TABLE_MAP* BindingTemplates, const char* Key)
{
	constexpr char ActionPrefix[] = "$$ACTION_";
	if (!strstr(str, ActionPrefix))
	{
		return str;
	}

	// The template and the key labels can come from different encodings.
	xr_string ConvertedTemplate;
	const char* Text = str;
	if (!IsUTF8(Text))
	{
		ConvertedTemplate = Platform::ANSI_TO_UTF8(Text);
		Text = ConvertedTemplate.c_str();
	}

	xr_string Result;
	const char* Cursor = Text;
	while (const char* Token = strstr(Cursor, ActionPrefix))
	{
		const char* Action = Token + sizeof(ActionPrefix) - 1;
		const char* End = strstr(Action, "$$");
		if (!End)
		{
			break;
		}

		Result.append(Cursor, Token);
		const xr_string ActionName(Action, static_cast<u32>(End - Action));
		string256 Binding;
		GetActionAllBinding(ActionName.c_str(), Binding, sizeof(Binding), ShortKeyNames);
		Result += Binding;
		Cursor = End + 2;
	}
	Result += Cursor;

	if (Cursor != Text && BindingTemplates)
	{
		(*BindingTemplates)[Key] = str;
	}

	return Result.c_str();
}

STRING_VALUE CStringTable::translate(const STRING_ID& str_id) const
{
	return Translate(str_id, false);
}

STRING_VALUE CStringTable::Translate(const STRING_ID& Id, bool ShortKeyNames) const
{
	if (!pData)
	{
		return Id;
	}

	const bool ControllerMode = pInput && pInput->GetControllerMode();
	if (ShortKeyNames && !ControllerMode)
	{
		if (const auto Hint = pData->ObjectHintsWithShortKeys.find(Id); Hint != pData->ObjectHintsWithShortKeys.end())
		{
			return Hint->second;
		}
	}

	if (const auto Main = pData->m_StringTable.find(Id); Main != pData->m_StringTable.end())
	{
		if (ControllerMode)
		{
			if (const auto Gamepad = pData->m_StringTableGamepad.find(Id); Gamepad != pData->m_StringTableGamepad.end())
			{
				return Gamepad->second;
			}
		}
		return Main->second;
	}
	if (const auto Fallback = pData->m_FallbackStringTable.find(Id); Fallback != pData->m_FallbackStringTable.end())
	{
		return Fallback->second;
	}
	return Id;
}

STRING_VALUE CStringTable::FormatObjectHint(const STRING_ID& Text) const
{
	if (Text.size() && !strpbrk(Text.c_str(), "#\n"))
	{
		return Translate(Text, true);
	}
	return ParseScriptText(Text, true);
}

// St4lker0k765: Enhanced Edition-style parsing for tasks, etc
STRING_VALUE CStringTable::ParseStringFromScript(STRING_ID input)
{
	return ParseScriptText(input, false);
}

STRING_VALUE CStringTable::ParseScriptText(const STRING_ID& input, bool ShortKeyNames) const
{
	xr_string out = "";
	if (!input.size())
	{
		return STRING_VALUE(out.c_str());
	}
	std::stringstream stringStream(input.c_str());
	xr_string line;
	while (std::getline(stringStream, line))
	{
		size_t prev = 0, pos;
		while ((pos = line.find_first_of("#", prev)) != xr_string::npos)
		{
			if (pos > prev)
				out += Translate(line.substr(prev, pos - prev).c_str(), ShortKeyNames).c_str();
			prev = pos + 1;
		}
		if (prev < line.length())
			out += Translate(line.substr(prev, xr_string::npos).c_str(), ShortKeyNames).c_str();
	}
	return STRING_VALUE(out.c_str());
}
