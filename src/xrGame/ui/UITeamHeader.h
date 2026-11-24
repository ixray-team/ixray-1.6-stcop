#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../xrEngine/string_table.h"
#include "../xrCore/Containers/associative_vector.h"

class CUIStatic;

class UITeamState;
class CUIXml;

namespace tinyxml2
{
	class XMLNode;
}

class UITeamHeader final : public CUIWindow
{
private:
	typedef	CUIWindow									inherited;
	
	typedef associative_vector<shared_str, CUIStatic*>		FieldsStatics;
	typedef associative_vector<shared_str, CUIStatic*>		ColumnsStatics;
	typedef associative_vector<shared_str, STRING_VALUE>	TranslatedStrings;
	
	FieldsStatics				m_field_fillers;
	ColumnsStatics				m_columns_statics;
	TranslatedStrings			m_translated_strings;

	UITeamState const * const	m_parent;
	tinyxml2::XMLNode*					m_team_header_root;

	void			InitColumnsStatics	(CUIXml& uiXml);
	void			InitFieldsStatics	(CUIXml& uiXml);
public:
					UITeamHeader	(UITeamState const * const parent);
	virtual ~UITeamHeader	() override;
			void	Init			(CUIXml& uiXml, const char* path);
	virtual void	Update			() override;

	virtual CUIWindow* ui_cast_window() override { return this; }
};