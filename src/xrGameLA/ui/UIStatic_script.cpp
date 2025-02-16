#include "pch_script.h"
#include "UIStatic.h"
#include "UIAnimatedStatic.h"

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
		.enum_("lanim")
		[
			value("LA_CYCLIC", int(LA_CYCLIC)),
			value("LA_ONLYALPHA", int(LA_ONLYALPHA)),
			value("LA_TEXTCOLOR", int(LA_TEXTCOLOR)),
			value("LA_TEXTURECOLOR", int(LA_TEXTURECOLOR))
		]
		.def(						constructor<>())
		.def("TextControl",			&CUIStatic::TextItemControl)
		.def("SetText",				&CUIStatic::SetText)
		.def("SetTextST",			&CUIStatic::SetTextST)
		.def("GetText",				&CUIStatic::GetText)
		.def("SetFont",				&CUIStatic::SetFont)
		.def("GetFont",				&CUIStatic::GetFont)
		.def("SetColor",			&CUIStatic::SetColor)
		.def("GetColor",			&CUIStatic::GetColor)
		.def("SetTextColor",		(void(CUIStatic::*)(u32))&CUIStatic::SetTextColor)
		.def("SetTextColor",		(void(CUIStatic::*)(u32,u32,u32,u32))&CUIStatic::SetTextColor)
		.def("GetTextColor",		&CUIStatic::GetTextColor)
		.def("SetTextAlignment",	&CUIStatic::SetTextAlignment)
		.def("SetVTextAlignment",	&CUIStatic::SetVTextAlignment)
		.def("SetEllipsis",			&CUIStatic::SetEllipsis)
		.def("SetTextOffset",		&CUIStatic::SetTextOffset)
		.def("SetTextureColor",			&CUIStatic::SetTextureColor)
		.def("GetTextureColor",			&CUIStatic::GetTextureColor)
		.def("Init",					(void(CUIStatic::*)(LPCSTR, float,float,float,float))&CUIStatic::Init )
		.def("InitEx",					(void(CUIStatic::*)(LPCSTR, LPCSTR,float,float,float,float))&CUIStatic::InitEx )
		.def("InitTexture",			&CUIStatic::InitTexture )
		.def("InitTextureOldRect",			&CUIStatic::InitTexture_script )
		.def("SetTextureOffset",	&CUIStatic::SetTextureOffset )
		.def("SetTextureRect",		&CUIStatic::SetTextureRect_script)
		.def("SetOriginalRect",		(void(CUIStatic::*)(float,float,float,float))&CUIStatic::SetTextureRect)
		.def("SetStretchTexture",	&CUIStatic::SetStretchTexture)
		.def("GetStretchTexture",	&CUIStatic::GetStretchTexture)
		.def("SetTextAlign",		&CUIStatic::SetTextAlign_script)
		.def("GetTextAlign",		&CUIStatic::GetTextAlign_script)
		.def("GetTextureRect",		&CUIStatic::GetTextureRect_script)
		.def("SetComplexMode",	&CUIStatic::SetTextComplexMode)
		.def("IsComplexMode",	&CUIStatic::IsTextComplexMode)
		.def("SetTextX",			&CUIStatic::SetTextPosX)
		.def("SetTextY",			&CUIStatic::SetTextPosY)
		.def("GetTextX",			&CUIStatic::GetTextPosX)
		.def("GetTextY",			&CUIStatic::GetTextPosY)
		.def("SetHeading",			&CUIStatic::SetHeading)
		.def("GetHeading",			&CUIStatic::GetHeading)
		.def("Heading",			&CUIStatic::Heading)
		.def("EnableHeading",			&CUIStatic::EnableHeading)
		.def("SetConstHeading",			&CUIStatic::SetConstHeading)
		.def("GetConstHeading",			&CUIStatic::GetConstHeading)
		.def("SetXformLightAnim",			&CUIStatic::SetXformLightAnim)
		.def("ResetXformAnimation",			&CUIStatic::ResetXformAnimation)
		.def("SetColorAnimation",			&CUIStatic::SetColorAnimation)
		.def("ResetColorAnimation",			&CUIStatic::ResetColorAnimation)
		.def("CreateShader",			&CUIStatic::CreateShader)
		.def("GetTextureOffset",			&CUIStatic::GetTextureOffeset)
		.def("TextureOn",			&CUIStatic::TextureOn)
		.def("IsTextureOn",			&CUIStatic::IsTextureOn)
		.def("TextureOff",			&CUIStatic::TextureOff)
		.def("AdjustHeightToText",			&CUIStatic::AdjustHeightToText)
		.def("AdjustWidthToText",			&CUIStatic::AdjustWidthToText)
		.def("ColorAnimationSetTextureColor",			&CUIStatic::ColorAnimationSetTextureColor)
		.def("ColorAnimationSetTextColor",			&CUIStatic::ColorAnimationSetTextColor)
		.def("SetHighlightColor",			&CUIStatic::SetHighlightColor)
		.def("EnableTextHighlighting",			&CUIStatic::EnableTextHighlighting)
		.def("IsHighlightText",			&CUIStatic::IsHighlightText)
	];
}