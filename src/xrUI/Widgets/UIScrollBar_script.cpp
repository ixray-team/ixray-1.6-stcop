#include "stdafx.h"
#include "UIScrollBar.h"
#include <luabind/luabind.hpp>

using namespace luabind;

namespace
{
bool InitScrollBarStretch_script(CUIScrollBar* self, float x, float y, float length, bool isHorizontal, const char* profile)
{
	const Fvector2 pos = Fvector2().set(x, y);
	return self->InitScrollBar(pos, length, isHorizontal, profile);
}

bool InitScrollBarFixed_script(CUIScrollBar* self, float x, float y, bool isHorizontal, const char* profile)
{
	const Fvector2 pos = Fvector2().set(x, y);
	return self->InitScrollBar(pos, isHorizontal, profile);
}
} // namespace

int QueryScrollBarProfileLayout_script(const char* profile, bool isHorizontal)
{
	ScrollLayoutMode layoutMode = ScrollLayoutMode::Stretch;
	if (!CUIScrollBar::QueryProfileLayout(profile, isHorizontal, layoutMode))
	{
		return -1;
	}
	return layoutMode == ScrollLayoutMode::Fixed ? 1 : 0;
}

#pragma optimize("s", on)
void CUIScrollBar::script_register(lua_State* L)
{
	module(L)
	[
		class_<CUIScrollBar, CUIWindow>("CUIScrollBar")
			.def(constructor<>())
			.def("InitScrollBarStretch", &InitScrollBarStretch_script)
			.def("InitScrollBarFixed", &InitScrollBarFixed_script)
			.def("SetRange", &CUIScrollBar::SetRange)
			.def("GetMinRange", &CUIScrollBar::GetMinRange)
			.def("GetMaxRange", &CUIScrollBar::GetMaxRange)
			.def("SetScrollPos", &CUIScrollBar::SetScrollPos)
			.def("GetScrollPos", &CUIScrollBar::GetScrollPos)
			.def("SetPageSize", &CUIScrollBar::SetPageSize)
			.def("GetPageSize", &CUIScrollBar::GetPageSize)
			.def("SetStepSize", &CUIScrollBar::SetStepSize)
			.def("GetStepSize", &CUIScrollBar::GetStepSize)
			.def("TryScrollInc", &CUIScrollBar::TryScrollInc)
			.def("TryScrollDec", &CUIScrollBar::TryScrollDec)
			.def("Refresh", &CUIScrollBar::Refresh)
			.def("IsFixedLayout", &CUIScrollBar::IsFixedLayout)
			.def("IsInitialized", &CUIScrollBar::IsInitialized)
			.def("SetEnabled", &CUIScrollBar::SetEnabled)
			.def("GetEnabled", &CUIScrollBar::GetEnabled)
			.enum_("layout_mode")
			[
				value("stretch", 0),
				value("fixed", 1)
			]
	];
}
