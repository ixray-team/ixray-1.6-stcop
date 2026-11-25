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
	STRING_VALUE			m_sFallbackLanguage;
	STRING_TABLE_MAP		m_StringTable;
	STRING_TABLE_MAP		m_StringTableGamepad;
	STRING_TABLE_MAP		m_FallbackStringTable;
	STRING_TABLE_MAP		m_string_key_binding;
	STRING_TABLE_MAP		m_string_key_binding_gamepad;
	STRING_TABLE_MAP FallbackBindingTemplates;
	STRING_TABLE_MAP ObjectHintsWithShortKeys;
};

class ENGINE_API CStringTable 
{
public:
	static xr_vector<xr_token> languages_token;

public:
								CStringTable			();

	static void					Destroy					();
	
	STRING_VALUE				translate				(const STRING_ID& str_id) const;
			void				rescan					();

	static	bool				m_bWriteErrorsToLog;
	static	void				ReparseKeyBindings		();
	static	xr_string			LangName				();
			void				ReloadLanguage(const char* lang);
			STRING_VALUE		ParseStringFromScript	(STRING_ID input);
	STRING_VALUE FormatObjectHint(const STRING_ID& Text) const;
private:
			void				Init					();
			void				Load					(const char* xml_file);
			void				LoadFallback			(const char* xml_file);
	static STRING_VALUE ParseLine(const char* Text, bool ShortKeyNames = false, STRING_TABLE_MAP* BindingTemplates = nullptr, const char* Key = nullptr);
	STRING_VALUE Translate(const STRING_ID& Id, bool ShortKeyNames) const;
	STRING_VALUE ParseScriptText(const STRING_ID& Text, bool ShortKeyNames) const;
	static STRING_TABLE_DATA*	pData;
};

extern ENGINE_API CStringTable* g_pStringTable;