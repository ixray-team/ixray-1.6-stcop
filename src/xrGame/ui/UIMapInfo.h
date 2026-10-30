#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrScripts/script_export_space.h"

class CUIScrollView;

XREFLECT(LuaExport, Bases="CUIWindow", Constructible)
class CUIMapInfo final : public CUIWindow
{
public:
				CUIMapInfo			();
	virtual ~CUIMapInfo			() override;
	XPROPERTY(LuaCallable, Name="Init")
			void InitMapInfo		(Fvector2 pos,Fvector2 size);
	XPROPERTY(LuaCallable)
			void InitMap			(const char* map_name, const char* map_ver);
		const char*	 GetLargeDesc		();

	virtual CUIWindow* ui_cast_window() override { return this; }

protected:
	CUIScrollView*	m_view;
	shared_str		m_large_desc;
};
