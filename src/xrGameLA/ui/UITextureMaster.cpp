// file:		UITextureMaster.h
// description:	holds info about shared textures. able to initialize external controls
//				through IUITextureControl interface
// created:		11.05.2005
// author:		Serge Vynnychenko
// mail:		narrator@gsc-game.kiev.ua
//
// copyright 2005 GSC Game World


#include "StdAfx.h"
#include "UITextureMaster.h"
#include "../UIStaticItem.h"
#include "uiabstract.h"
#include "xrUIXmlParser.h"
#include "../../Include/xrRender/UIShader.h"
#include "../MainMenu.h"

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


void CUITextureMaster::ParseShTexInfo(LPCSTR xml_file)
{
	CUIXml			xml;
	xml.Load		(CONFIG_PATH, UI_PATH, xml_file);
	shared_str file = xml.Read("file_name",0,""); 

	//skyloader: shoc reading
	int num = xml.GetNodesNum("",0,"texture");
	for (int i = 0; i<num; i++)
	{
		TEX_INFO info;

		info.file = file;

		info.rect.x1 = xml.ReadAttribFlt("texture",i,"x");
		info.rect.x2 = xml.ReadAttribFlt("texture",i,"width") + info.rect.x1;
		info.rect.y1 = xml.ReadAttribFlt("texture",i,"y");
		info.rect.y2 = xml.ReadAttribFlt("texture",i,"height") + info.rect.y1;
		shared_str id = xml.ReadAttrib("texture",i,"id");

		m_textures.insert(std::make_pair(id,info));
	}

	//skyloader: cs\cop reading
	int files_num				= xml.GetNodesNum("",0,"file");

	for(int fi=0; fi<files_num; ++fi)
	{
		XML_NODE* root_node			= xml.GetLocalRoot();
		shared_str file				= xml.ReadAttrib("file", fi, "name"); 

		XML_NODE* node				= xml.NavigateToNode("file", fi);

		int num						= xml.GetNodesNum(node, "texture");
		for (int i = 0; i<num; i++)
		{
			TEX_INFO info;

			info.file = file;

			info.rect.x1 = xml.ReadAttribFlt(node, "texture",i,"x");
			info.rect.x2 = xml.ReadAttribFlt(node, "texture",i,"width") + info.rect.x1;
			info.rect.y1 = xml.ReadAttribFlt(node, "texture",i,"y");
			info.rect.y2 = xml.ReadAttribFlt(node, "texture",i,"height") + info.rect.y1;
			shared_str id = xml.ReadAttrib	(node, "texture",i,"id");

			m_textures.insert(std::make_pair(id,info));
		}

		xml.SetLocalRoot		(root_node);
	}
}

bool CUITextureMaster::IsSh(const shared_str& texture_name){
	return strstr(texture_name.c_str(),"\\") ? false : true;
}

void CUITextureMaster::InitTexture(const shared_str& texture_name, const shared_str& shader_name, ui_shader& out_shader, Frect& out_rect)
{
	xr_map<shared_str, TEX_INFO>::iterator it	= m_textures.find(texture_name);
	if (it != m_textures.end())
	{
		sh_pair p={it->second.file, shader_name};
		xr_map<sh_pair, ui_shader>::iterator sh_it = m_shaders.find(p);
		if(sh_it==m_shaders.end())
			m_shaders[p]->create(shader_name.c_str(), it->second.file.c_str());

		out_shader			= m_shaders[p];
		out_rect			= (*it).second.rect;
	}else
		out_shader->create	(shader_name.c_str(), texture_name.c_str());
}

void CUITextureMaster::InitTexture(const shared_str& texture_name, CUIStaticItem* tc, const shared_str& shader_name)
{
	CTimer time; time.Start();
	xr_map<shared_str, TEX_INFO>::iterator it	= m_textures.find(texture_name);
	if (it != m_textures.end())
	{
		sh_pair p={it->second.file, shader_name};
		xr_map<sh_pair, ui_shader>::iterator sh_it = m_shaders.find(p);
		if(sh_it==m_shaders.end())
			m_shaders[p]->create(shader_name.c_str(), it->second.file.c_str());

		tc->SetShader		(m_shaders[p]);
		tc->SetTextureRect	((*it).second.rect);
		tc->SetSize			(Fvector2().set(it->second.rect.width(),it->second.rect.height()));
	}else{
		tc->CreateShader		(texture_name.c_str(), shader_name.c_str());
	}

	if (time.GetElapsed_sec()*1000.f > 10.0)//Determine if texture loading caused stutter and send its name to "SuggestPrefetching" pool
	{ 
		string256 str;
		xr_sprintf(str, "%s, %s", texture_name.c_str(), shader_name.c_str());

/*		if (g_uCommonFlags.test(CF_Prefetch_UI)) {
			Msg("UI:Initing texture = %s, stutter time = %fms", str, time.GetElapsed_sec()*1000.f);
		}*/

		shared_str str2 = str;
		MainMenu()->SuggestedForPrefetching.push_back(str2);
	}
	
}

Frect CUITextureMaster::GetTextureRect(const shared_str&  texture_name){
	TEX_INFO info = FindItem(texture_name);
	return info.rect;
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

TEX_INFO CUITextureMaster::FindItem(const shared_str& texture_name, const shared_str& def_texture_name)
{
	xr_map<shared_str, TEX_INFO>::iterator	it;
	it = m_textures.find(texture_name);

	if (it != m_textures.end())
		return (it->second);
	else
		return FindItem(def_texture_name);
}

void CUITextureMaster::GetTextureShader(const shared_str&  texture_name, ui_shader& sh){
	xr_map<shared_str, TEX_INFO>::iterator	it;
	it = m_textures.find(texture_name);

	R_ASSERT3(it != m_textures.end(), "can't find texture", texture_name.c_str());

	sh->create("hud\\default", *((*it).second.file));	
}

LPCSTR CUITextureMaster::GetTextureFileName(const shared_str& texture_name)
{
	TEX_INFO info = FindItem(texture_name);
	return *info.file;	
}