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
			
       .def("SetText",				(void (CUIStatic::*)(LPCSTR)) (&CUIStatic::SetText))
       .def("SetTextST",			(void (CUIStatic::*)(LPCSTR)) (&CUIStatic::SetTextST))

       .def("GetText",				&CUIStatic::GetText)

		.def("SetFont",				&CUIStatic::SetFont)

       .def("SetTextX",				&CUIStatic::SetTextX)
       .def("SetTextY",				&CUIStatic::SetTextY)
	   .def("GetTextX",				&CUIStatic::GetTextX)
       .def("GetTextY",				&CUIStatic::GetTextY)

       .def("SetColor",				&CUIStatic::SetTextureColor)
       .def("GetColor",				&CUIStatic::GetTextureColor)

        .def("SetTextColor",		&CUIStatic::SetTextColor_script)
		.def("SetTextColor",		&CUIStatic::SetTextColor)
		.def("GetTextColor",		&CUIStatic::GetTextColor)

        .def("InitTexture",			(bool (CUIStatic::*)(LPCSTR, bool)) &CUIStatic::InitTexture)
        .def("InitTexture",			+[](CUIStatic* self, pcstr texture) { self->InitTexture(texture); })
        .def("InitTextureEx",		&CUIStatic::InitTextureEx)
        .def("InitTextureEx",		+[](CUIStatic* self, pcstr texture, pcstr shader) { self->InitTextureEx(texture, shader); })
        .def("ResetOriginalRect", &CUIStatic::ResetOriginalRect)

		.def("SetTextureOffset",	&CUIStatic::SetTextureOffset)

		.def("SetTextureRect",		&CUIStatic::SetTextureRect_script)
		.def("GetTextureRect",		&CUIStatic::GetTextureRect_script)

        .def("SetOriginalRect",		&CUIStatic::SetTextureRect_script)
        .def("GetOriginalRect",		&CUIStatic::GetTextureRect_script)

		.def("SetStretchTexture",	&CUIStatic::SetStretchTexture)
        .def("GetStretchTexture",	&CUIStatic::GetStretchTexture)

        .def("SetTextAlign",		&CUIStatic::SetTextAlign_script)
        .def("GetTextAlign",		&CUIStatic::GetTextAlign_script)

        .def("SetHeading",			&CUIStatic::SetHeading)
        .def("GetHeading",			&CUIStatic::GetHeading)

        .def("SetElipsis",			&CUIStatic::SetEllipsis)
        .def("SetEllipsis",			&CUIStatic::SetEllipsis_script)
		.def("SetTextAlignment",	&CUIStatic::SetTextAlignment)
		.def("SetVTextAlignment",	&CUIStatic::SetVTextAlignment)
		.def("SetTextComplexMode",	&CUIStatic::SetTextComplexMode)
		.def("GetFont",				&CUIStatic::GetFont)
		.def("SetTextOffset",		&CUIStatic::SetTextOffset),

		class_<CUISleepStatic, CUIStatic>("CUISleepStatic")
		.def(						constructor<>())
	];
}