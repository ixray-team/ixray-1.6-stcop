#include "stdafx.h"
#include "string_table.h"
#include "xr_level_controller.h"

ENGINE_API CStringTable* g_pStringTable = nullptr;

STRING_TABLE_DATA* CStringTable::pData = nullptr;
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
	if(nullptr != pData)	return;
	Destroy				();
	Init				();
}

void CStringTable::Init		()
{
	if(nullptr != pData) return;
    
	pData = new STRING_TABLE_DATA();
	
	//имя языка, если не задано (nullptr), то первый <text> в <string> в XML
	pData->m_sLanguage	= pSettings->r_string("string_table", "language");

	FS_FileSet fset;
	FS_FileSet efset;

	string_path files_mask;
	string_path exclude_files_mask;

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

#include "../xrCore/XmlParser/xrXMLParser.h"
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
			VERIFY3(!isDublicate, "duplicate string table id", string_name);
			Msg("! duplicate string table id: %s", string_name);
		}

		LPCSTR string_text		= uiXml.Read(uiXml.GetRoot(), "string:text", i,  nullptr);

		if(m_bWriteErrorsToLog && string_text)
			Msg("[string table] '%s' no translation in '%s'", string_name, pData->m_sLanguage.c_str() );
		
		VERIFY3						(string_text, "string table entry does not has a text", string_name);
		
		STRING_VALUE str_val		= ParseLine(string_text, string_name, true);
		
		pData->m_StringTable[string_name] = str_val;
	}
}

void CStringTable::ReparseKeyBindings()
{
	if(!pData)					return;
	STRING_TABLE_MAP_IT it		= pData->m_string_key_binding.begin();
	STRING_TABLE_MAP_IT it_e	= pData->m_string_key_binding.end();

	for(;it!=it_e;++it)
	{
		pData->m_StringTable[it->first]			= ParseLine(*it->second, *it->first, false);
	}
}

xr_string CStringTable::LangName()
{
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
