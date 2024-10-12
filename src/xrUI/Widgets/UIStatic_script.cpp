#include "stdafx.h"
#include "UIStatic.h"
#include "UIAnimatedStatic.h"

#include <luabind/luabind.hpp>

using namespace luabind;

#pragma optimize("s",on)

void CUIStatic::script_register(lua_State *L)
{
	module(L)
	[
		class_<CUILines>("CUILines")
		.def("SetFont",				&CUILines::SetFont)
		.def("SetText",				&CUILines::SetText)
		.def("SetTextST",			&CUILines::SetTextST)
		.def("GetText",				&CUILines::GetText)
		.def("SetElipsis",			&CUILines::SetEllipsis)
		.def("SetTextColor",		&CUILines::SetTextColor),


		class_<CUIStatic, CUIWindow>("CUIStatic")
		.def(						constructor<>())
		.def("SetTextureColor", 	&CUIStatic::SetTextureColor)
		.def("GetTextureColor", 	&CUIStatic::GetTextureColor)
		.def("AdjustHeightToText", 	&CUIStatic::AdjustHeightToText)
		.def("AdjustWidthToText", 	&CUIStatic::AdjustWidthToText)
		.def("GetStretchTexture", 	&CUIStatic::GetStretchTexture)														  
		.def("TextControl",			&CUIStatic::TextItemControl)
		.def("InitTexture",			&CUIStatic::InitTexture )
		.def("InitTextureEx",		&CUIStatic::InitTextureEx )													
		.def("SetTextureRect",		&CUIStatic::SetTextureRect_script)
		.def("SetStretchTexture",	&CUIStatic::SetStretchTexture)
		.def("GetTextureRect",		&CUIStatic::GetTextureRect_script),

		class_<CUITextWnd, CUIWindow>("CUITextWnd")
		.def(						constructor<>())
		.def("AdjustHeightToText",	&CUITextWnd::AdjustHeightToText)
		.def("AdjustWidthToText",	&CUITextWnd::AdjustWidthToText)
		.def("SetText",				&CUITextWnd::SetText)
		.def("SetTextST",			&CUITextWnd::SetTextST)
		.def("GetText",				&CUITextWnd::GetText)
		.def("SetFont",				&CUITextWnd::SetFont)
		.def("GetFont",				&CUITextWnd::GetFont)
		.def("SetTextColor",		&CUITextWnd::SetTextColor)
		.def("GetTextColor",		&CUITextWnd::GetTextColor)
		.def("SetTextComplexMode",	&CUITextWnd::SetTextComplexMode)
		.def("SetTextAlignment",	&CUITextWnd::SetTextAlignment)
		.def("SetVTextAlignment",	&CUITextWnd::SetVTextAlignment)
		.def("SetEllipsis",			&CUITextWnd::SetEllipsis)
		.def("SetTextOffset",		&CUITextWnd::SetTextOffset),
//		.def("",					&CUITextWnd::)

		class_<CUISleepStatic, CUIStatic>("CUISleepStatic")
		.def(						constructor<>())
	];
}