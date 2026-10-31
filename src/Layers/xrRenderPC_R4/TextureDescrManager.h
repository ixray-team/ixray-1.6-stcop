#ifndef _TextureDescrManager_included_
#define _TextureDescrManager_included_

#pragma once
#include "../../xrEngine/ETextureParams.h"

class CTextureDescrMngr
{
	struct texture_assoc
	{
		shared_str			detail_name;
		u8					usage = 0;
	};
	struct texture_spec
	{
		shared_str			m_bump_name;
		float				m_material;
		bool				m_use_steep_parallax;
		bool				m_use_pbr;
		ShaderExternalMap	ShadersOptions;
	};
	struct texture_desc{
		texture_assoc*		m_assoc;
		texture_spec*		m_spec;
		STextureParams::ETType	m_type;
        texture_desc            ():m_assoc(nullptr),m_spec(nullptr){}
	};

	using map_TD = xr_map<shared_str, texture_desc>;
	using map_TDIt = map_TD::iterator;

	using map_CS = xr_map<shared_str, float>;
	using map_CSIt = map_CS::iterator;

	map_TD									m_texture_details;
	map_CS									m_detail_scalers;

	void		LoadTHM		(const char* initial);
	void		LoadLTX		();

public:
				~CTextureDescrMngr();
	void		Load		();
	void		UnLoad		();
public:
	shared_str	GetBumpName		(const shared_str& tex_name) const;
	float		GetMaterial		(const shared_str& tex_name) const;
	void		GetTextureUsage	(const shared_str& tex_name, bool& bDiffuse, bool& bBump) const;
	bool		GetDetailTexture(const shared_str& tex_name, const char*& res, const float*& Scale) const;
	bool		UseSteepParallax(const shared_str& tex_name) const;
	bool		UsePBRTexures	(const shared_str& tex_name) const;

	ShaderExternalMap* GetShaderExternal(const shared_str& tex_name) const;
};
#endif