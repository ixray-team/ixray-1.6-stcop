#pragma once
#include "../../xrUI/Widgets/UIWindow.h"

class CUIXml;
class CUIStatic;
class UIHint;
class CUIScrollView;

class CUIAchievements final : 
	public CUIWindow
{
	typedef CUIWindow inherited;

private:
	CUIScrollView*				m_parent;
	CUIStatic*					m_name;
	CUIStatic*					m_descr;
	CUIStatic*					m_icon;
	UIHint*						m_hint;
	string128					m_functor_str;
	bool						m_repeat;

public:
						CUIAchievements		(CUIScrollView* parent);
	virtual				~CUIAchievements	();

			void		init_from_xml		(CUIXml& xml);
			void		Update				();

			void		SetName				(const char* name);
			void		SetDescription		(const char* desc);
			void		SetHint				(const char* hint);
			void		SetIcon				(const char* icon);
			void		SetFunctor			(const char* func);
			void		SetRepeatable		(bool repeat);

	virtual void		DrawHint			();
	virtual void		Reset				();

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
			bool		ParentHasMe			();
};
