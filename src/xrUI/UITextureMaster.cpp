// file:		UITextureMaster.h
// description:	holds info about shared textures. able to initialize external controls
//				through IUITextureControl interface
// created:		11.05.2005
// author:		Serge Vynnychenko
// mail:		narrator@gsc-game.kiev.ua
//
// copyright 2005 GSC Game World


#include "stdafx.h"
#include "UITextureMaster.h"
#include "UIVectorBinding.h"
#include "Widgets/UIStaticItem.h"
#include "uiabstract.h"
#include "xrUIXmlParser.h"

xr_map<shared_str, TEX_INFO>	CUITextureMaster::m_textures;
xr_map<sh_pair, ui_shader>		CUITextureMaster::m_shaders;

void CUITextureMaster::FreeTexInfo()
{
	m_textures.clear	();
	FreeCachedShaders	();
}

void CUITextureMaster::FreeCachedShaders()
{
	m_shaders.clear();
}

void CUITextureMaster::ParseShTexInfoLegacy(const char* xml_file)
{
	CUIXml xml;
	xml.Load(CONFIG_PATH, UI_PATH, xml_file);
	shared_str file = xml.Read("file_name", 0, "");
	XML_NODE* RootNode = xml.GetLocalRoot() ? xml.GetLocalRoot() : xml.GetRoot();
	for (XML_NODE* TextureNode = RootNode ? RootNode->FirstChildElement("texture") : nullptr; TextureNode;
		 TextureNode = TextureNode->NextSiblingElement("texture"))
	{
		TEX_INFO info;
		info.file = file;
		info.rect.x1 = xml.ReadAttribFlt(TextureNode, "x", 0.f);
		info.rect.x2 = xml.ReadAttribFlt(TextureNode, "width", 0.f) + info.rect.x1;
		info.rect.y1 = xml.ReadAttribFlt(TextureNode, "y", 0.f);
		info.rect.y2 = xml.ReadAttribFlt(TextureNode, "height", 0.f) + info.rect.y1;
		shared_str id = xml.ReadAttrib(TextureNode, "id", "");
		m_textures.insert(std::make_pair(id, info));
	}
}

void CUITextureMaster::ParseShTexInfo(const char* xml_file)
{
	CUIXml xml;
	xml.Load(CONFIG_PATH, "ui\\textures_descr", xml_file);
	const int FilesNum = xml.GetNodesNum("", 0, "file");
	XML_NODE* RootNode = xml.GetLocalRoot() ? xml.GetLocalRoot() : xml.GetRoot();
	for (XML_NODE* FileNode = FilesNum ? RootNode->FirstChildElement("file") : nullptr; FileNode;
		 FileNode = FileNode->NextSiblingElement("file"))
	{
		shared_str file = xml.ReadAttrib(FileNode, "name", "");
		const float fileScale = xml.ReadAttribFlt(FileNode, "scale", 1.f);
		for (XML_NODE* TextureNode = FileNode->FirstChildElement("texture"); TextureNode;
			 TextureNode = TextureNode->NextSiblingElement("texture"))
		{
			TEX_INFO info;
			info.file = file;
			info.rect.x1 = xml.ReadAttribFlt(TextureNode, "x", 0.f) * fileScale;
			info.rect.x2 = xml.ReadAttribFlt(TextureNode, "width", 0.f) * fileScale + info.rect.x1;
			info.rect.y1 = xml.ReadAttribFlt(TextureNode, "y", 0.f) * fileScale;
			info.rect.y2 = xml.ReadAttribFlt(TextureNode, "height", 0.f) * fileScale + info.rect.y1;
			shared_str id = xml.ReadAttrib(TextureNode, "id", "");
			m_textures.insert(std::make_pair(id, info));
		}
	}
}

bool CUITextureMaster::IsSh(const shared_str& texture_name){
	return strstr(texture_name.c_str(),"\\") ? false : true;
}

bool CUITextureMaster::InitTexture(const shared_str& texture_name, const shared_str& shader_name, ui_shader& out_shader, Frect& out_rect, bool warn_about_missing_tex)
{
	// Step 1 - try to read texture from XML
	xr_map<shared_str, TEX_INFO>::iterator it	= m_textures.find(texture_name);
	if (it != m_textures.end())
	{
		out_shader			= CreateTextureShader(it->second.file, shader_name);
		out_rect			= (*it).second.rect;
		return true;
	}
	// Step 2 - if texture is not in XML, try to load existing file
	string_path tmp;
	bool texExist = FS.exist(tmp, _game_textures_, texture_name.c_str());
	if (texExist || warn_about_missing_tex)
	{
		out_shader = CreateTextureShader(texture_name, shader_name);
	}
	else
	{
		out_shader = CreateTextureShader("ed\\ed_not_existing_texture", shader_name);
	}
	return false;
}

bool CUITextureMaster::InitTexture(const shared_str& texture_name, CUIStaticItem* tc, const shared_str& shader_name, bool warn_about_missing_tex)
{
	// Step 1 - try to read texture from XML
	xr_map<shared_str, TEX_INFO>::iterator it = m_textures.find(texture_name);
	if (it != m_textures.end())
	{
		tc->SetShader(CreateTextureShader(it->second.file, shader_name));
		tc->SetTextureRect((*it).second.rect);
		tc->SetSize(Fvector2().set(it->second.rect.width(), it->second.rect.height()));
		return true;
	}
	// Step 2 - if texture is not in XML, try to load existing file
	string_path tmp;
	bool texExist = FS.exist(tmp, _game_textures_, texture_name.c_str());
	if (texExist || warn_about_missing_tex)
	{
		tc->SetShader(CreateTextureShader(texture_name, shader_name));
	}
	else
	{
		tc->SetShader(CreateTextureShader("ed\\ed_not_existing_texture", shader_name));
	}

	return false;
}

bool CUITextureMaster::InitTexture(const shared_str& svg_texture_name, CUIStaticItem* tc, float fWidgetWidth, float fWidgetHeight, SVGTintRGBA svgTint)
{
	if (tc == nullptr)
		return false;
	return CUIVectorBinding::ApplyVectorFileToStaticItem(svg_texture_name, svgTint, *tc, fWidgetWidth, fWidgetHeight);
}

Frect CUITextureMaster::GetTextureRect(const shared_str&  texture_name){
	TEX_INFO info = FindItem(texture_name);
	return info.rect;
}

const char* CUITextureMaster::GetTextureFileName(const char* texture_name) {
	xr_map<shared_str, TEX_INFO>::iterator	it;
	it = m_textures.find(texture_name);

	if (it != m_textures.end())
		return *((*it).second.file);
	R_ASSERT3(false, "CUITextureMaster::GetTextureFileName Can't find texture", texture_name);
	return 0;
}

float CUITextureMaster::GetTextureHeight(const shared_str&  texture_name){
	TEX_INFO info = FindItem(texture_name);
	return info.rect.height();
}

float CUITextureMaster::GetTextureWidth(const shared_str&  texture_name)
{
	TEX_INFO info = FindItem(texture_name);
	return info.rect.width();
}

TEX_INFO CUITextureMaster::FindItem(const shared_str&  texture_name)
{
	xr_map<shared_str, TEX_INFO>::iterator	it;
	it = m_textures.find(texture_name);

	if (it != m_textures.end())
		return (it->second);
	else{
		return TEX_INFO();
	}
}

bool CUITextureMaster::ItemExist(const shared_str& texture_name)
{
	const auto it = m_textures.find(texture_name);
	if (it != m_textures.end())
	{
		return true;
	};
	string_path tmp;
	return FS.exist(tmp, _game_textures_, texture_name.c_str());
}

void CUITextureMaster::GetTextureShader(const shared_str&  texture_name, ui_shader& sh){
	xr_map<shared_str, TEX_INFO>::iterator	it;
	it = m_textures.find(texture_name);

	R_ASSERT3(it != m_textures.end(), "can't find texture", texture_name.c_str());

	sh->create("hud\\default", *((*it).second.file));
}

ui_shader CUITextureMaster::CreateTextureShader(const shared_str& texture_name, const shared_str& shader_name)
{
	sh_pair p = {texture_name, shader_name};
	xr_map<sh_pair, ui_shader>::iterator sh_it = m_shaders.find(p);
	if (sh_it == m_shaders.end())
	{
		m_shaders[p]->create(shader_name.c_str(), texture_name.c_str());
	}
	return m_shaders[p];
}
