#pragma once
#include "../../xrUI/Widgets/UIWindow.h"

class CUIXml;
class CUIStatic;
class UIHint;
class CUIScrollView;

class CUIRankingsCoC final : public CUIWindow
{
	typedef CUIWindow inherited;
private:
	CUIScrollView*				m_parent;
	CUIStatic*					m_name;
	CUIStatic*					m_descr;
	CUIStatic*					m_icon;
	//CUIStatic*					m_border;
	UIHint*						m_hint;
	u8							m_index;

public:
						CUIRankingsCoC		(CUIScrollView* parent);
	virtual				~CUIRankingsCoC	();

			void		init_from_xml		(CUIXml& xml,u8 index,bool bUnique);
			void		Update				();

			void		SetName				(const char* name);
			void		SetDescription		(const char* desc);
			void		SetHint				(const char* hint);
			void		SetIcon				(const char* icon);
			void		SetFunctor			(const char* func);

	virtual void		DrawHint			();
	virtual void		Reset				();
	virtual CUIWindow* ui_cast_window() { return this; }

protected:
			bool		ParentHasMe			();
};
