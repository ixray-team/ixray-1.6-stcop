// file:		UITextureMaster.h
// description:	holds info about shared textures. able to initialize external
//				through IUITextureControl interface
// created:		11.05.2005
// author:		Serge Vynnychenko
// mail:		narrator@gsc-game.kiev.ua
//
// copyright 2005 GSC Game World

#pragma once

class CUIStaticItem;
#include "ui_defs.h"
#include "../../Include/xrRender/SVGTypes.h"

struct TEX_INFO{
	shared_str	file;
	Frect		rect;
	const char*		get_file_name	() const	{return *file;}
	Frect		get_rect		() const	{return rect;}
};

struct sh_pair{
	shared_str	texture_name;
	shared_str	shader_name;
	bool operator < (const sh_pair& other) const
	{
		if (texture_name < other.texture_name)
			return true;
		else
			return shader_name < other.shader_name;
	}
};

class UI_API CUITextureMaster{
public:

	static void ParseShTexInfo			(const char* xml_file);
	static void ParseShTexInfoLegacy	(const char* xml_file); // for SoC
	static void FreeTexInfo				();
	static void FreeCachedShaders		();

	static bool		InitTexture			(const shared_str& texture_name, CUIStaticItem* tc, const shared_str& shader_name ="hud\\default", bool warn_about_missing_tex = true);
	static bool InitTexture(const shared_str& svg_texture_name, CUIStaticItem* tc, float fWidgetWidth, float fWidgetHeight, SVGTintRGBA svgTint = {});
	static bool		InitTexture			(const shared_str& texture_name, const shared_str& shader_name, ui_shader& out_shader, Frect& out_rect, bool warn_about_missing_tex = true);
	static float	GetTextureHeight	(const shared_str&  texture_name);
	static float	GetTextureWidth		(const shared_str&  texture_name);
	static Frect	GetTextureRect		(const shared_str&  texture_name);
	static const char*	GetTextureFileName	(const char* texture_name);
	static void		GetTextureShader	(const shared_str&  texture_name, ui_shader& sh);
	static TEX_INFO	FindItem			(const shared_str&  texture_name);
	static bool		ItemExist			(const shared_str&	texture_name);
	static ui_shader CreateTextureShader	(const shared_str& texture_name, const shared_str& shader_name);

protected:
	IC	static bool IsSh				(const shared_str& texture_name);

	static xr_map<shared_str, TEX_INFO>	m_textures;

	static xr_map<sh_pair, ui_shader>	m_shaders;
};