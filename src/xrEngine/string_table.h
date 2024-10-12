//////////////////////////////////////////////////////////////////////////
// string_table.h:		таблица строк используемых в игре
//////////////////////////////////////////////////////////////////////////

#pragma once
using STRING_ID   = shared_str;
using STRING_VALUE = shared_str;
using STRING_TABLE_MAP = xr_map<STRING_ID, STRING_VALUE>;
using STRING_TABLE_MAP_IT = STRING_TABLE_MAP::iterator;

struct STRING_TABLE_DATA
{
	STRING_VALUE			m_sLanguage;
	STRING_TABLE_MAP		m_StringTable;
	STRING_TABLE_MAP		m_string_key_binding;
};

class ENGINE_API CStringTable 
{
public:
								CStringTable			();

	static void					Destroy					();
	
	STRING_VALUE				translate				(const STRING_ID& str_id) const;
			void				rescan					();

	static	BOOL				m_bWriteErrorsToLog;
	static	void				ReparseKeyBindings		();
	static	xr_string			LangName				();
			void				ReloadLanguage			();
private:
			void				Init					();
			void				Load					(LPCSTR xml_file);
	static STRING_VALUE			ParseLine				(LPCSTR str, LPCSTR key, bool bFirst);
	static STRING_TABLE_DATA*	pData;
};

extern ENGINE_API CStringTable* g_pStringTable;