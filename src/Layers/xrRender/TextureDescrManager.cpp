#include "stdafx.h"
#pragma hdrstop
#include "TextureDescrManager.h"
#include "ETextureParams.h"

// eye-params
float					r__dtex_range	= 50;
class cl_dt_scaler		: public R_constant_setup {
public:
	float				scale;

	cl_dt_scaler		(float s) : scale(s)	{};
	virtual void setup	(R_constant* C)
	{
		RCache.set_c	(C,scale,scale,scale,1/r__dtex_range);
	}
};

void fix_texture_thm_name(LPSTR fn)
{
	LPSTR _ext = strext(fn);
	if(  _ext					&&
	  (0==_stricmp(_ext,".tga")	||
		0==_stricmp(_ext,".thm")	||
		0==_stricmp(_ext,".dds")	||
		0==_stricmp(_ext,".bmp")	||
		0==_stricmp(_ext,".ogm")	) )
		*_ext = 0;
}

void CTextureDescrMngr::LoadTHM(LPCSTR initial)
{
	FS_FileSet				flist;
	FS.file_list			(flist, initial, FS_ListFiles, "*.thm");
#ifdef DEBUG
	Msg						("count of .thm files=%d", flist.size());
#endif // #ifdef DEBUG
	FS_FileSetIt It			= flist.begin();
	FS_FileSetIt It_e		= flist.end();
	STextureParams			tp;
	string_path				fn;
	for(;It!=It_e;++It)
	{
		FS.update_path(fn, initial, (*It).name.c_str());
		IReader* F = FS.r_open(fn);

		shared_str InitialPath = fn;

		xr_strcpy(fn, (*It).name.c_str());
		fix_texture_thm_name(fn);

		bool FoundedChunk = !!F->find_chunk(THM_CHUNK_TYPE);
		R_ASSERT2(FoundedChunk, "Not found chunk THM_CHUNK_TYPE");

		[[maybe_unused]] u32 ThmType = F->r_u32();
		tp.Clear();
		[[maybe_unused]] bool NeedSave = tp.Load(*F);
		FS.r_close(F);

#ifdef _EDITOR
		if (NeedSave)
		{
			IWriter* W = FS.w_open(InitialPath.c_str());

			W->open_chunk(THM_CHUNK_VERSION);
			W->w_u16(0x0012);
			W->close_chunk();

			W->open_chunk(THM_CHUNK_TYPE);
			W->w_u32(ThmType);
			W->close_chunk();

			tp.Save(*W);
			FS.w_close(W);
		}
#endif

		if (STextureParams::ttImage		== tp.type ||
			STextureParams::ttTerrain	== tp.type ||
			STextureParams::ttNormalMap	== tp.type	)
		{
			texture_desc&	desc	= m_texture_details[fn];
			cl_dt_scaler*&	dts		= m_detail_scalers[fn];

			if( tp.detail_name.size() &&
				tp.flags.is_any(STextureParams::flDiffuseDetail|STextureParams::flBumpDetail) )
			{
				if(desc.m_assoc)
					xr_delete				(desc.m_assoc);

				desc.m_assoc				= new texture_assoc();
				desc.m_assoc->detail_name	= tp.detail_name;
				if (dts)
					dts->scale = tp.detail_scale;
				else
					/*desc.m_assoc->cs*/dts	= new cl_dt_scaler(tp.detail_scale);

				desc.m_assoc->usage			= 0;
				
				if( tp.flags.is(STextureParams::flDiffuseDetail) )
					desc.m_assoc->usage		|= (1<<0);
				
				if( tp.flags.is(STextureParams::flBumpDetail) )
					desc.m_assoc->usage		|= (1<<1);

			}
			if(desc.m_spec)
				xr_delete				(desc.m_spec);

			desc.m_spec					= new texture_spec();
			desc.m_spec->m_material		= (float)tp.material+tp.material_weight;
			desc.m_spec->m_use_steep_parallax = false;
			desc.m_spec->m_use_pbr = (tp.material == STextureParams::tmPBR_Material);
			
			if(tp.bump_mode==STextureParams::tbmUse)
			{
				desc.m_spec->m_bump_name	= tp.bump_name;
			}
			else if (tp.bump_mode==STextureParams::tbmUseParallax)
			{
				desc.m_spec->m_bump_name	= tp.bump_name;
				desc.m_spec->m_use_steep_parallax = true;
			}

		}
	}
}

void CTextureDescrMngr::LoadLTX()
{
	string_path				fname;
	FS.update_path(fname, "$game_textures$", "textures_associations.ltx");

	Msg("  ");
	Msg("* Reading Textures:LTX");

	if (FS.exist(fname))
	{
		CInifile			ini(fname);
		if (ini.section_exist("association"))
		{
			CInifile::Sect& data = ini.r_section("association");
			CInifile::SectCIt I = data.Data.begin();
			CInifile::SectCIt E = data.Data.end();
			for (; I != E; ++I)
			{
				const CInifile::Item& item = *I;

				texture_desc& desc = m_texture_details[item.first];
				cl_dt_scaler*& dts = m_detail_scalers[item.first];

				desc.m_assoc = new texture_assoc();

				string_path				T;
				float					s;

				int res = sscanf(*item.second, "%[^,],%f", T, &s);
				R_ASSERT(res == 2);
				desc.m_assoc->detail_name = T;

				if (dts)
					dts->scale = s;
				else
					dts = new cl_dt_scaler(s);

				desc.m_assoc->usage = 0;
				if (strstr(item.second.c_str(), "usage[diffuse_or_bump]"))
					desc.m_assoc->usage = (1 << 0) | (1 << 1);
				else
					if (strstr(item.second.c_str(), "usage[bump]"))
						desc.m_assoc->usage = (1 << 1);
					else

						if (strstr(item.second.c_str(), "usage[diffuse]"))
							desc.m_assoc->usage = (1 << 0);

				desc.m_assoc->m_tesselation_method = 32;

				if (strstr(item.second.c_str(), "tessellation_method[0]"))
					desc.m_assoc->m_tesselation_method = 0;
				if (strstr(item.second.c_str(), "tessellation_method[1]"))
					desc.m_assoc->m_tesselation_method = 1;
				if (strstr(item.second.c_str(), "tessellation_method[2]"))
					desc.m_assoc->m_tesselation_method = 2;
				if (strstr(item.second.c_str(), "tessellation_method[3]"))
					desc.m_assoc->m_tesselation_method = 3;
				//Msg("Tesselation Method for %s = %u", item.first.c_str(), desc.m_assoc->m_tesselation_method);
			}
		}//"association"
	}//file-exist

	FS.update_path(fname, "$game_textures$", "textures_specifications.ltx");
	if (FS.exist(fname))
	{
		CInifile			ini(fname);
		if (ini.section_exist("specification"))
		{
			CInifile::Sect& sect = ini.r_section("specification");
			for (CInifile::SectCIt I2 = sect.Data.begin(); I2 != sect.Data.end(); ++I2)
			{
				const CInifile::Item& item = *I2;

				float gcoef = 1.0;	// поумолчанию коэфициент, чтобы глосс не пропал вообще
				float goffset = 0.0;

				texture_desc& desc = m_texture_details[item.first];
				desc.m_spec = new texture_spec();

				cl_gloss_coef_and_offset*& glossparams = desc.m_spec->textureglossparams;

				string_path				bmode;
				int res = sscanf(item.second.c_str(), "bump_mode[%[^]]], material[%f], gloss_coef[%f], gloss_offset[%f]", bmode, &desc.m_spec->m_material, &gcoef, &goffset);
				R_ASSERT(res >= 2);
				if ((bmode[0] == 'u') && (bmode[1] == 's') && (bmode[2] == 'e') && (bmode[3] == ':'))
				{
					// bump-map specified
					desc.m_spec->m_bump_name = bmode + 4;
				}
				if ((bmode[0] == 'u') && (bmode[1] == 's') && (bmode[2] == 'e') && (bmode[3] == '_') && (bmode[4] == 'p') && (bmode[5] == ':'))
				{
					// bump-map specified with parallax
					desc.m_spec->m_bump_name = bmode + 6;
					desc.m_spec->m_use_steep_parallax = true;
				}

				//Msg("%s,Gloss params are %f, %f", item.first.c_str(), gcoef, goffset);

				if (glossparams) {
					glossparams->coef = gcoef;
					glossparams->offset = goffset;
				}
				else
					glossparams = new cl_gloss_coef_and_offset(gcoef, goffset);
			}
		}//"specification"
	}//file-exist

#ifdef _EDITOR
	FS.update_path(fname, "$game_textures$", "textures_types.ltx");

	if (FS.exist(fname))
	{
		CInifile			ini(fname);
		if (ini.section_exist("types"))
		{
			CInifile::Sect& data = ini.r_section("types");
			for (CInifile::SectCIt I = data.Data.begin(); I != data.Data.end(); I++)
			{
				CInifile::Item& item = (CInifile::Item&)*I;

				texture_desc& desc = m_texture_details[item.first];
				desc.m_type = (STextureParams::ETType)atoi(item.second.c_str());
			}
		}//"types"
	}//file-exist
#endif
}

void CTextureDescrMngr::CheckAndCreate_Assoc(texture_desc*& desc)
{
	// Create description, if needed
	if (!desc->m_assoc)
		desc->m_assoc = new texture_assoc();
}

void CTextureDescrMngr::CheckAndCreate_Spec(texture_desc*& desc)
{
	// Create specificator, if needed
	if (!desc->m_spec)
		desc->m_spec = new texture_spec();
}

void CTextureDescrMngr::LoadMiniLTX()
{
	FS_FileSet				flist;
	FS.file_list(flist, "$game_textures$", FS_ListFiles, "*.ltx");

	Msg("  ");
	Msg("* Reading Textures:MINI LTX: Count of .ltx files in Textures: = %u", flist.size());

	FS_FileSetIt It = flist.begin();
	FS_FileSetIt It_e = flist.end();

	STextureParams			texture_params;
	string_path				fn;
	string_path				stored_ltx_path;

	for (; It != It_e; ++It)
	{
		FS.update_path(fn, "$game_textures$", (*It).name.c_str());

		xr_strcpy(stored_ltx_path, fn);

		// check if there is a corresponding dds file
		LPSTR _ext = strext(fn);
		if (_ext)
			*_ext = 0;

#ifndef _EDITOR		
		xr_sprintf(fn, "%s.dds", fn);
#else
		sprintf(fn, "%s.dds", fn);
#endif
		
		if (FS.exist(fn))
		{
			// remove the extension
			xr_strcpy(fn, (*It).name.c_str());
			_ext = strext(fn);

			if (_ext)
				*_ext = 0;

			texture_desc* texture_desc = &m_texture_details[fn];

			R_ASSERT(texture_desc);

			// Load the min LTX cfg
			R_ASSERT2(FS.exist(stored_ltx_path), stored_ltx_path);
			CInifile texture_ltx(stored_ltx_path);

			string128 config_section = "config";

			if (texture_ltx.section_exist(config_section)) // Check the section existance
			{

				//THE PARAMETRES

				if (texture_ltx.line_exist(config_section, "detail_texture"))
				{
					CheckAndCreate_Assoc(texture_desc);

					texture_desc->m_assoc->detail_name = texture_ltx.r_string(config_section, "detail_texture");
				}


				if (texture_ltx.line_exist(config_section, "detail_texture_scaler"))
				{
					cl_dt_scaler*& detais_scaler = m_detail_scalers[fn];

					if (!detais_scaler)
						detais_scaler = new cl_dt_scaler(1.f);

					R_ASSERT(m_detail_scalers[fn]);

					detais_scaler->scale = texture_ltx.r_float(config_section, "detail_texture_scaler");
				}


				if (texture_ltx.line_exist(config_section, "usage"))
				{
					CheckAndCreate_Assoc(texture_desc);

					LPCSTR ussage_type = texture_ltx.r_string(config_section, "usage");

					if (xr_strcmp(ussage_type, "diffuse_or_bump") == 0)
						texture_desc->m_assoc->usage = (1 << 0) | (1 << 1);
					else

						if (xr_strcmp(ussage_type, "bump") == 0)
							texture_desc->m_assoc->usage = (1 << 1);
						else

							if (xr_strcmp(ussage_type, "diffuse") == 0)
								texture_desc->m_assoc->usage = (1 << 0);

					R_ASSERT2((texture_desc->m_assoc->usage & (1 << 0) || texture_desc->m_assoc->usage & (1 << 1)), make_string<const char*>("Variable 'usage' is specified but not set in mini ltx of texture %s", fn));
					
				}


				if (texture_ltx.line_exist(config_section, "tessellation_method"))
				{
					CheckAndCreate_Assoc(texture_desc);

					LPCSTR ussage_type = texture_ltx.r_string(config_section, "tessellation_method");

					if (xr_strcmp(ussage_type, "none") == 0)
						texture_desc->m_assoc->m_tesselation_method = 0;
					else

						if (xr_strcmp(ussage_type, "hight_map") == 0)
							texture_desc->m_assoc->m_tesselation_method = 1;
						else

							if (xr_strcmp(ussage_type, "smoothing") == 0)
								texture_desc->m_assoc->m_tesselation_method = 2;
							else

								if (xr_strcmp(ussage_type, "both") == 0)
									texture_desc->m_assoc->m_tesselation_method = 3;
				}


				if (texture_ltx.line_exist(config_section, "bump_texture"))
				{
					CheckAndCreate_Spec(texture_desc);

					texture_desc->m_spec->m_bump_name = texture_ltx.r_string(config_section, "bump_texture");
				}


				if (texture_ltx.line_exist(config_section, "parallax"))
				{
					CheckAndCreate_Spec(texture_desc);

					BOOL parallax = texture_ltx.r_bool(config_section, "parallax");
					texture_desc->m_spec->m_use_steep_parallax = (parallax == TRUE) ? true : false;
				}

				if (texture_ltx.line_exist(config_section, "material_type"))
				{
					CheckAndCreate_Spec(texture_desc);

					LPCSTR mat_type = texture_ltx.r_string(config_section, "material_type");

					if (xr_strcmp(mat_type, "orennayar_blin") == 0)
						texture_desc->m_spec->m_material += 0;
					else

						if (xr_strcmp(mat_type, "blin_phong") == 0)
							texture_desc->m_spec->m_material += 1;
						else

							if (xr_strcmp(mat_type, "phong_metal") == 0)
								texture_desc->m_spec->m_material += 2;
							else

								if (xr_strcmp(mat_type, "metal_orennayar") == 0)
									texture_desc->m_spec->m_material += 3;
				}

				if (texture_ltx.line_exist(config_section, "material_weight"))
				{
					CheckAndCreate_Spec(texture_desc);

					texture_desc->m_spec->m_material += texture_ltx.r_float(config_section, "material_weight");
				}

				if (texture_ltx.line_exist(config_section, "gloss_coef") || texture_ltx.line_exist(config_section, "gloss_offset"))
				{
					CheckAndCreate_Spec(texture_desc);

					cl_gloss_coef_and_offset*&	glossparams = texture_desc->m_spec->textureglossparams;

					if (!glossparams)
						glossparams = new cl_gloss_coef_and_offset(1.0f, 0.0f);

					R_ASSERT(texture_desc->m_spec->textureglossparams);

					if (texture_ltx.line_exist(config_section, "gloss_coef"))
						glossparams->coef = texture_ltx.r_float(config_section, "gloss_coef");

					if (texture_ltx.line_exist(config_section, "gloss_offset"))
						glossparams->offset = texture_ltx.r_float(config_section, "gloss_offset");

				}

				//Msg("%s %f %u %u %s %d %f %f %f", texture_desc->m_assoc->detail_name.c_str(), m_detail_scalers[fn] ? m_detail_scalers[fn]->scale : 0.0f, texture_desc->m_assoc->usage,
				//	texture_desc->m_assoc->m_tesselation_method, texture_desc->m_spec->m_bump_name.c_str(), texture_desc->m_spec->m_use_steep_parallax, texture_desc->m_spec->m_material,
				//	texture_desc->m_spec->textureglossparams ? texture_desc->m_spec->textureglossparams->coef : 0.f, texture_desc->m_spec->textureglossparams ? texture_desc->m_spec->textureglossparams->offset : 0.f);

			}
		}
	}
}
void CTextureDescrMngr::Load()
{
#ifdef DEBUG
	CTimer					TT;
	TT.Start				();
#endif // #ifdef DEBUG

	LoadTHM					("$game_textures$");
	LoadTHM					("$level$");
	if (!xr_strcmp(EngineExternal().GetEngineMode().c_str(), "la"))
	{
		LoadLTX();
		LoadMiniLTX();
	}
#ifdef DEBUG
	Msg("load time=%d ms",TT.GetElapsed_ms());
#endif // #ifdef DEBUG
}

void CTextureDescrMngr::UnLoad()
{
	map_TD::iterator I = m_texture_details.begin();
	map_TD::iterator E = m_texture_details.end();
	for(;I!=E;++I)
	{
		xr_delete(I->second.m_assoc);
		xr_delete(I->second.m_spec);
	}
	m_texture_details.clear	();
}

CTextureDescrMngr::~CTextureDescrMngr()
{
	map_CS::iterator I = m_detail_scalers.begin();
	map_CS::iterator E = m_detail_scalers.end();

	for(;I!=E;++I)
		xr_delete(I->second);

	m_detail_scalers.clear	();
}

shared_str CTextureDescrMngr::GetBumpName(const shared_str& tex_name) const
{
	map_TD::const_iterator I = m_texture_details.find	(tex_name);
	if (I!=m_texture_details.end())
	{
		if(I->second.m_spec)
		{
			return I->second.m_spec->m_bump_name;
		}	
	}
	return "";
}

BOOL CTextureDescrMngr::UseSteepParallax(const shared_str& tex_name) const
{
	map_TD::const_iterator I = m_texture_details.find	(tex_name);
	if (I!=m_texture_details.end())
	{
		if(I->second.m_spec)
		{
			return I->second.m_spec->m_use_steep_parallax;
		}	
	}
	return FALSE;
}

BOOL CTextureDescrMngr::UsePBRTexures(const shared_str& tex_name) const {
	auto I = m_texture_details.find(tex_name);
	if(I != m_texture_details.end()) {
		if(I->second.m_spec) {
			return I->second.m_spec->m_use_pbr;
		}
	}
	return FALSE;
}

float CTextureDescrMngr::GetMaterial(const shared_str& tex_name) const
{
	map_TD::const_iterator I = m_texture_details.find	(tex_name);
	if (I!=m_texture_details.end())
	{
		if(I->second.m_spec)
		{
			return I->second.m_spec->m_material;
		}
	}
	return 1.0f;
}

void CTextureDescrMngr::GetTextureUsage	(const shared_str& tex_name, BOOL& bDiffuse, BOOL& bBump) const
{
	map_TD::const_iterator I = m_texture_details.find	(tex_name);
	if (I!=m_texture_details.end())
	{
		if(I->second.m_assoc)
		{
			u8 usage	= I->second.m_assoc->usage;
			bDiffuse	= !!(usage & (1<<0));
			bBump		= !!(usage & (1<<1));
		}	
	}
}

BOOL CTextureDescrMngr::GetDetailTexture(const shared_str& tex_name, LPCSTR& res, R_constant_setup* &CS) const
{
	map_TD::const_iterator I = m_texture_details.find	(tex_name);
	if (I!=m_texture_details.end())
	{
		if(I->second.m_assoc)
		{
            texture_assoc* TA = I->second.m_assoc;
			res	= TA->detail_name.c_str();
			map_CS::const_iterator It2 = m_detail_scalers.find(tex_name);
			CS	= It2==m_detail_scalers.end()?0:It2->second;//TA->cs;
			return TRUE;
		}
	}
	return FALSE;
}

