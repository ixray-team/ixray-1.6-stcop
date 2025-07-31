#include "stdafx.h"
#include "string_table.h"
#include "xr_level_controller.h"
#include "../xrCore/FormatParsers/XML/xrXMLParser.h"
#include "../xrEngine/XR_IOConsole.h"

ENGINE_API CStringTable* g_pStringTable = nullptr;

STRING_TABLE_DATA* CStringTable::pData = nullptr;
xr_vector<xr_token> CStringTable::languages_token;
BOOL CStringTable::m_bWriteErrorsToLog = FALSE;

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
	FS.update_path(path, "$game_config$", "");
	sprintf_s(path, "%s%s", path, "text\\");
	FS.file_list(langSet, path, FS_ListFolders | FS_RootOnly);

	std::ostringstream result_stream = {};
	for (auto it = langSet.begin(); it != langSet.end(); ++it)
	{
		xr_string folderName = it->name;

		string_path current_path = {};
		sprintf_s(current_path, "%s%s", path, it->name.c_str());
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
	FS.file_list(fset, "$game_config$", FS_ListFiles, files_mask);

	xr_sprintf(exclude_files_mask, "text\\%s\\mod_*.xml", pData->m_sLanguage.c_str());
	FS.file_list(efset, "$game_config$", FS_ListFiles, exclude_files_mask);

	for (const FS_File& File : fset)
	{
		if (efset.contains(File))
			continue;

		string_path fn, ext;
		_splitpath(File.name.c_str(), 0, 0, fn, ext);
		xr_strcat(fn, ext);

		Load(fn);
	}

	ReparseKeyBindings();
}

void CStringTable::Load	(LPCSTR xml_file_full)
{
	CXml						uiXml;
	string_path					_s;
	xr_strconcat(_s, "text\\", pData->m_sLanguage.c_str() );

	uiXml.Load					(CONFIG_PATH, _s, xml_file_full);

	//общий список всех записей таблицы в файле
	int string_num = uiXml.GetNodesNum		(uiXml.GetRoot(), "string");

	for(int i=0; i<string_num; ++i)
	{
		LPCSTR string_name = uiXml.ReadAttrib(uiXml.GetRoot(), "string", i, "id", nullptr);

		bool isDublicate = pData->m_StringTable.find(string_name) != pData->m_StringTable.end();
		if (isDublicate)
		{
			//VERIFY3(!isDublicate, "duplicate string table id", string_name);
			Msg("! duplicate string table id: %s", string_name);
		}

		LPCSTR string_text		= uiXml.Read(uiXml.GetRoot(), "string:text", i,  nullptr);

		if(m_bWriteErrorsToLog && string_text)
			Msg("[string table] '%s' no translation in '%s'", string_name, pData->m_sLanguage.c_str() );
		
		VERIFY3						(string_text, "string table entry does not have a text", string_name);
		
		STRING_VALUE str_val		= ParseLine(string_text, string_name, true);
		
		pData->m_StringTable[string_name] = str_val;
	}
}

void CStringTable::ReparseKeyBindings()
{
	if(pData == nullptr)
		return;
	
	for (auto& key : pData->m_string_key_binding)
	{
		pData->m_StringTable[key.first] = ParseLine(*key.second, *key.first, false);
	}
}

void CStringTable::ReloadLanguage(const char* lang)
{
	// reload language
	Destroy();

	pData = new STRING_TABLE_DATA();
	pData->m_sLanguage = lang;

	FS_FileSet fset;
	FS_FileSet efset;

	string_path files_mask = {};
	string_path exclude_files_mask = {};

	xr_sprintf(files_mask, "text\\%s\\*.xml", pData->m_sLanguage.c_str());
	FS.file_list(fset, "$game_config$", FS_ListFiles, files_mask);

	xr_sprintf(exclude_files_mask, "text\\%s\\mod_*.xml", pData->m_sLanguage.c_str());
	FS.file_list(efset, "$game_config$", FS_ListFiles, exclude_files_mask);

	for (const FS_File& File : fset)
	{
		if (efset.contains(File))
			continue;

		string_path fn = {}, ext = {};
		_splitpath(File.name.c_str(), 0, 0, fn, ext);
		xr_strcat(fn, ext);

		Load(fn);
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

STRING_VALUE CStringTable::ParseLine(LPCSTR str, LPCSTR skey, bool bFirst)
{
//	LPCSTR str = "1 $$action_left$$ 2 $$action_right$$ 3 $$action_left$$ 4";
	xr_string			res;
	int k = 0;
	const char*			b;
	#define ACTION_STR "$$ACTION_"

//.	int LEN				= (int)xr_strlen(ACTION_STR);
	#define LEN			9

	string256				buff;
	string256				srcbuff;
	bool	b_hit			= false;

	while( (b = strstr( str+k,ACTION_STR)) !=0 )
	{
		buff[0]				= 0;
		srcbuff[0]			= 0;
		res.append			(str+k, b-str-k);
		const char* e		= strstr( b+LEN,"$$" );

		int len				= (int)(e-b-LEN);

		strncpy_s				(srcbuff,b+LEN, len);
		srcbuff[len]		= 0;
		GetActionAllBinding	(srcbuff, buff, sizeof(buff) );
		res.append			(buff, xr_strlen(buff) );

		k					= (int)(b-str);
		k					+= len;
		k					+= LEN;
		k					+= 2;
		b_hit				= true;
	};

	if(k<(int)xr_strlen(str)){
		res.append(str+k);
	}

	if(b_hit&&bFirst) pData->m_string_key_binding[skey] = str;

	return STRING_VALUE(res.c_str());
}

STRING_VALUE CStringTable::translate (const STRING_ID& str_id) const
{
	if(pData != nullptr && pData->m_StringTable.find(str_id)!=pData->m_StringTable.end())
		return  pData->m_StringTable[str_id];
	else
		return str_id;
}
