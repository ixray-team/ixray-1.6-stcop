#ifndef _TextureDescrManager_included_
#define _TextureDescrManager_included_

#pragma once
#include "ETextureParams.h"

class cl_dt_scaler;

class cl_gloss_coef_and_offset : public R_constant_setup {
public:
	float				coef;
	float				offset;

	cl_gloss_coef_and_offset(float s, float x) : coef(s), offset(x) {};
	virtual void setup(R_constant* C)
	{
		RCache.set_c(C, coef, offset, 0, 0);
	}
};

class CTextureDescrMngr
{
	struct texture_assoc
	{
		shared_str			detail_name;
		//R_constant_setup*	cs;
		u8					usage;
		u8					m_tesselation_method;
		texture_assoc       () : /*cs(NULL),*/ usage(0) { m_tesselation_method = 32; }
		~texture_assoc		() { /*xr_delete(cs);*/ }

	};
	struct texture_spec
	{
		shared_str			m_bump_name;
		float				m_material;
		bool				m_use_steep_parallax;
		bool				m_use_pbr;
		cl_gloss_coef_and_offset* textureglossparams;
		texture_spec() { textureglossparams = nullptr; }
		~texture_spec() { xr_delete(textureglossparams); }
	};
	struct texture_desc{
		texture_assoc*		m_assoc;
		texture_spec*		m_spec;
        texture_desc            ():m_assoc(nullptr),m_spec(nullptr){}
#ifdef _EDITOR
		STextureParams::ETType	m_type;
#endif
	};

	using map_TD = xr_map<shared_str, texture_desc>;
	using map_TDIt = map_TD::iterator;

	using map_CS = xr_map<shared_str, cl_dt_scaler*>;
	using map_CSIt = map_CS::iterator;

	map_TD									m_texture_details;
	map_CS									m_detail_scalers;

	void		LoadTHM		(LPCSTR initial);
	void		LoadMiniLTX ();
	void		LoadLTX		();
	
	void		CheckAndCreate_Assoc		(texture_desc*& desc);
	void		CheckAndCreate_Spec			(texture_desc*& desc);

public:
				~CTextureDescrMngr();
	void		Load		();
	void		UnLoad		();
public:
	shared_str	GetBumpName		(const shared_str& tex_name) const;
	float		GetMaterial		(const shared_str& tex_name) const;
	void		GetTextureUsage	(const shared_str& tex_name, BOOL& bDiffuse, BOOL& bBump) const;
	BOOL		GetDetailTexture(const shared_str& tex_name, LPCSTR& res, R_constant_setup* &CS) const;
	BOOL		UseSteepParallax(const shared_str& tex_name) const;
	BOOL		UsePBRTexures	(const shared_str& tex_name) const;
};
#endif