#include "stdafx.h"

#include "TextureDescrManager.h"
#include "dx10FixedConstants.h"
// eye-params
float					r__dtex_range	= 50;
class cl_dt_scaler		: public RHIShaderConstant::Setup {
public:
	float				scale;

	cl_dt_scaler		(float s) : scale(s)	{};

	virtual void setup	(RHIShaderConstant* C) override
	{
		RCache.set_c	(C,scale,scale,scale,1/r__dtex_range);
		FixedConstants::SetDtParamsScale(scale);
	}
};

static shared_str NormalizeTexKey(const shared_str& Name)
{
#ifdef IXR_WINDOWS
	return Name;
#else
	return Name.size() ? shared_str(Platform::ValidPath(Name.c_str())) : Name;
#endif
}

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
void CTextureDescrMngr::LoadLTX()
{
	string_path				fname;		
	FS.update_path			(fname,_game_textures_,"textures.ltx");

	if (FS.exist(fname))
	{
		CInifile			ini(fname);
		if (ini.section_exist("association"))
		{
			CInifile::Sect& data	= ini.r_section("association");
			CInifile::SectCIt I		= data.Data.begin();
			CInifile::SectCIt E		= data.Data.end();
			for ( ; I!=E; ++I)	
			{
				const CInifile::Item& item	= *I;

				shared_str key = NormalizeTexKey(item.first);
				texture_desc& desc		= m_texture_details[key];
				cl_dt_scaler*& dts = m_detail_scalers[key];
				desc.m_assoc			= new texture_assoc();

				string_path				T;
				float					s;

				int res = sscanf					(*item.second,"%[^,],%f",T,&s);
				R_ASSERT(res==2);
				desc.m_assoc->detail_name = T;
				if (dts)
					dts->scale = s;
				else
					dts = new cl_dt_scaler(s);

//				desc.m_assoc->cs		= new cl_dt_scaler(s);

				desc.m_assoc->usage		= 0;
				if(strstr(item.second.c_str(),"usage[diffuse_or_bump]"))
					desc.m_assoc->usage	= (1<<0)|(1<<1);
				else
				if(strstr(item.second.c_str(),"usage[bump]"))
					desc.m_assoc->usage	= (1<<1);
				else
				if(strstr(item.second.c_str(),"usage[diffuse]"))
					desc.m_assoc->usage	= (1<<0);
			}
		}//"association"

		if (ini.section_exist("specification"))
		{
			CInifile::Sect& 	sect = ini.r_section("specification");
			for (CInifile::SectCIt I2=sect.Data.begin(); I2!=sect.Data.end(); ++I2)	
			{
				const CInifile::Item& item	= *I2;

				texture_desc& desc		= m_texture_details[NormalizeTexKey(item.first)];
				desc.m_spec				= new texture_spec();

				string_path				bmode, bparallax;
				int res = sscanf		(item.second.c_str(),"bump_mode[%[^]]], material[%f], parallax[%[^]]",bmode,&desc.m_spec->m_material,bparallax);
				R_ASSERT(res>=2);
				if ((bmode[0]=='u')&&(bmode[1]=='s')&&(bmode[2]=='e')&&(bmode[3]==':'))
				{
					// bump-map specified
					desc.m_spec->m_bump_name	=	bmode+4;
				}
				// parallax
				if (res == 3)
				{
					if ((bparallax[0] == 'y') && (bparallax[1] == 'e') && (bparallax[2] == 's'))
					{
						desc.m_spec->m_use_steep_parallax = true;
					}
					else
					{
						desc.m_spec->m_use_steep_parallax = false;
					}
				}
				else
				{
					desc.m_spec->m_use_steep_parallax = false;
				}
			}
		}//"specification"
		if (Device.IsEditorMode() && ini.section_exist("types"))
		{
			CInifile::Sect& 	data = ini.r_section("types");
			for (CInifile::SectCIt I=data.Data.begin(); I!=data.Data.end(); I++)	
			{
				CInifile::Item item	= *I;

				texture_desc& desc		= m_texture_details[NormalizeTexKey(item.first)];
				desc.m_type				= (STextureParams::ETType)atoi(item.second.c_str());
			}
		}//"types"
	}//file-exist
}

void CTextureDescrMngr::CheckAndCreate_Assoc(texture_desc& desc)
{
	if (desc.m_assoc == nullptr)
	{
		desc.m_assoc = new texture_assoc();
	}
}

void CTextureDescrMngr::CheckAndCreate_Spec(texture_desc& desc)
{
	if (desc.m_spec == nullptr)
	{
		desc.m_spec = new texture_spec();
	}
}

void CTextureDescrMngr::LoadMiniLTX()
{
	FS_FileSet flist;
	FS.file_list(flist, _game_textures_, FS_ListFiles, "*.ltx");

#ifdef DEBUG
	Msg("count of texture .ltx files=%d", flist.size());
#endif

	for (const FS_File& file : flist)
	{
		string_path ltx_path;
		FS.update_path(ltx_path, _game_textures_, file.name.c_str());

		string_path dds_path;
		xr_strcpy(dds_path, ltx_path);
		LPSTR dds_ext = strext(dds_path);
		if (dds_ext == nullptr)
		{
			continue;
		}

		*dds_ext = 0;
		xr_strcat(dds_path, ".dds");
		if (!FS.exist(dds_path))
		{
			continue;
		}

		string_path texture_name;
		xr_strcpy(texture_name, file.name.c_str());
		LPSTR texture_ext = strext(texture_name);
		if (texture_ext == nullptr)
		{
			continue;
		}
		*texture_ext = 0;

		CInifile mini_ltx(ltx_path);
		if (!mini_ltx.section_exist("config") && !mini_ltx.section_exist("shaders_options"))
		{
			continue;
		}

		const shared_str key = NormalizeTexKey(shared_str(texture_name));
		texture_desc& desc = m_texture_details[key];

		if (mini_ltx.line_exist("config", "detail_texture"))
		{
			CheckAndCreate_Assoc(desc);
			desc.m_assoc->detail_name = mini_ltx.r_string("config", "detail_texture");
		}

		if (mini_ltx.line_exist("config", "detail_texture_scaler"))
		{
			cl_dt_scaler*& scaler = m_detail_scalers[key];
			const float scale = mini_ltx.r_float("config", "detail_texture_scaler");
			if (scaler == nullptr)
			{
				scaler = new cl_dt_scaler(scale);
			}
			else
			{
				scaler->scale = scale;
			}
		}

		if (mini_ltx.line_exist("config", "usage"))
		{
			CheckAndCreate_Assoc(desc);

			const char* usage = mini_ltx.r_string("config", "usage");
			desc.m_assoc->usage = 0;
			if (xr_strcmp(usage, "diffuse_or_bump") == 0)
			{
				desc.m_assoc->usage = (1 << 0) | (1 << 1);
			}
			else if (xr_strcmp(usage, "bump") == 0)
			{
				desc.m_assoc->usage = (1 << 1);
			}
			else if (xr_strcmp(usage, "diffuse") == 0)
			{
				desc.m_assoc->usage = (1 << 0);
			}
		}

		float MaterialValue = -1.0f;
		bool MaterialTypeFound = false;
		bool MaterialIsPbr = false;

		if (mini_ltx.line_exist("config", "material_type"))
		{
			const char* material_type = mini_ltx.r_string("config", "material_type");
			if (xr_strcmp(material_type, "orennayar_blin") == 0)
			{
				MaterialValue = 0.0f;
				MaterialTypeFound = true;
			}
			else if (xr_strcmp(material_type, "blin_phong") == 0)
			{
				MaterialValue = 1.0f;
				MaterialTypeFound = true;
			}
			else if (xr_strcmp(material_type, "phong_metal") == 0)
			{
				MaterialValue = 2.0f;
				MaterialTypeFound = true;
			}
			else if (xr_strcmp(material_type, "metal_orennayar") == 0)
			{
				MaterialValue = 3.0f;
				MaterialTypeFound = true;
			}
			else if (xr_strcmp(material_type, "pbr") == 0)
			{
				MaterialValue = 4.0f;
				MaterialTypeFound = true;
				MaterialIsPbr = true;
			}
		}

		if (mini_ltx.line_exist("config", "material_weight"))
		{
			if (!MaterialTypeFound)
			{
				MaterialValue = 0.0f;
			}
			MaterialValue += mini_ltx.r_float("config", "material_weight");
			MaterialTypeFound = true;
		}

		if (MaterialTypeFound)
		{
			CheckAndCreate_Spec(desc);
			desc.m_spec->m_material = MaterialValue;

			if (mini_ltx.line_exist("config", "material_type"))
			{
				desc.m_spec->m_use_pbr = MaterialIsPbr;
			}
		}

		if (mini_ltx.line_exist("config", "bump_texture"))
		{
			CheckAndCreate_Spec(desc);
			desc.m_spec->m_bump_name = mini_ltx.r_string("config", "bump_texture");
		}

		if (mini_ltx.line_exist("config", "parallax"))
		{
			CheckAndCreate_Spec(desc);
			desc.m_spec->m_use_steep_parallax = mini_ltx.r_bool("config", "parallax");
		}

		if (mini_ltx.section_exist("shaders_options"))
		{
			CheckAndCreate_Spec(desc);
			for (auto& Line : mini_ltx.r_section("shaders_options").Data)
			{
				desc.m_spec->ShadersOptions[*Line.first] = *Line.second;
			}
		}
	}
}

void CTextureDescrMngr::LoadTHM(const char* initial)
{
	FS_FileSet				flist;
	FS.file_list			(flist, initial, FS_ListFiles, "*.thm");
#ifdef DEBUG
	Msg						("count of .thm files=%d", flist.size());
#endif // #ifdef DEBUG
	FS_FileSetIt It			= flist.begin();
	FS_FileSetIt It_e		= flist.end();
	xr_parallel_foreach(It, It_e, [&](const FS_File& elem)
	{
		STextureParams			tp;
		string_path				fn;

		FS.update_path(fn, initial, elem.name.c_str());
		IReader* F = FS.r_open(fn);

		shared_str InitialPath = fn;

		xr_strcpy(fn, elem.name.c_str());
		fix_texture_thm_name(fn);

		bool FoundedChunk = !!F->find_chunk(THM_CHUNK_TYPE);
		R_ASSERT2(FoundedChunk, "Not found chunk THM_CHUNK_TYPE");

		[[maybe_unused]] u32 ThmType = F->r_u32();
		tp.Clear();
		[[maybe_unused]] bool NeedSave = tp.Load(*F);
		FS.r_close(F);

//#ifdef _EDITOR
		if (NeedSave)
		{
			string_path		query;
			FS.update_path	(query,"$app_data_root$","fix_texture_thm\\");

			xr_string path(query);
			path.append(fn);
			path.append(".thm");

			IWriter* W = FS.w_open(path.c_str());

			W->open_chunk(THM_CHUNK_VERSION);
			W->w_u16(0x0012);
			W->close_chunk();

			W->open_chunk(THM_CHUNK_TYPE);
			W->w_u32(ThmType);
			W->close_chunk();

			tp.Save(*W);
			FS.w_close(W);
		}
//#endif

		if (STextureParams::ttImage		== tp.type ||
			STextureParams::ttTerrain	== tp.type ||
			STextureParams::ttNormalMap	== tp.type	)
		{
			shared_str key = NormalizeTexKey(shared_str(fn));
			MapAccessCS.Enter();
			texture_desc& desc = m_texture_details[key];
			cl_dt_scaler*& dts = m_detail_scalers[key];
			MapAccessCS.Leave();

			if (tp.detail_name.size() &&
				tp.flags.is_any(STextureParams::flDiffuseDetail | STextureParams::flBumpDetail))
			{
#ifndef MASTER_GOLD
				string_path src_detail_name;
				FS.update_path(src_detail_name, _game_textures_, tp.detail_name.c_str());
				xr_strcat(src_detail_name, ".dds");

				if (!FS.exist(src_detail_name))
				{
					Msg("! Detail texture not found\n"
						"  Detail texture name: %s\n"
						"  Expected path: %s\n"
						"  Source texture: %s\n",
						tp.detail_name.c_str(),
						src_detail_name,
						fn);

				}
#endif // !MASTER_GOLD

				if(desc.m_assoc)
				{
					xr_delete(desc.m_assoc);
				}

				desc.m_assoc = new texture_assoc();
				desc.m_assoc->detail_name = tp.detail_name;

				if (dts)
				{
					dts->scale = tp.detail_scale;
				}
				else
				{
					dts = new cl_dt_scaler(tp.detail_scale);
				}

				desc.m_assoc->usage = 0;

				if (tp.flags.is(STextureParams::flDiffuseDetail))
				{
					desc.m_assoc->usage |= (1 << 0);
				}

				if (tp.flags.is(STextureParams::flBumpDetail))
				{
					desc.m_assoc->usage |= (1 << 1);
				}
			}

			if (desc.m_spec)
			{
				xr_delete(desc.m_spec);
			}

			desc.m_spec = new texture_spec();
			desc.m_spec->m_material = static_cast<float>(tp.material) + tp.material_weight;
			desc.m_spec->m_use_steep_parallax = tp.bump_mode == STextureParams::tbmUseParallax;
			desc.m_spec->m_use_pbr = (tp.material == STextureParams::tmPBR_Material);

			string_path thm_overrider;

			if (FS.exist(thm_overrider, initial, fn, ".ltx"))
			{
				CInifile pOptions(thm_overrider);

				if (pOptions.section_exist("shaders_options"))
				{
					for (auto& Line : pOptions.r_section("shaders_options").Data)
					{
						desc.m_spec->ShadersOptions[*Line.first] = *Line.second;
					}
				}
			}
			
			if (tp.bump_mode == STextureParams::tbmUse || tp.bump_mode == STextureParams::tbmUseParallax)
			{
#ifndef MASTER_GOLD
				string_path pathBumpDds{}, pathBumpThm{};
				FS.update_path(pathBumpDds, _game_textures_, tp.bump_name.c_str());
				FS.update_path(pathBumpThm, _game_textures_, tp.bump_name.c_str());
				xr_strcat(pathBumpDds, ".dds");
				xr_strcat(pathBumpThm, ".thm");

				if (tp.bump_name == "")
				{
					Msg("! Missing bumpmap reference\n"
						"  Texture: %s\n"
						"  Reason: Bumpmap name is empty\n",
						(*It).name.c_str(), fn);
				}
				else if (!FS.exist(pathBumpDds) && !FS.exist(pathBumpThm))
				{
					Msg("! Bumpmap texture not found\n"
						"  Bumpmap name: %s\n"
						"  Source texture: %s\n"
						"  Material requires: %s\n",
						tp.bump_name.c_str(),
						fn,
						(tp.bump_mode == STextureParams::tbmUseParallax) ? "parallax bump" : "bump");
				}
#endif // !MASTER_GOLD

				desc.m_spec->m_bump_name = tp.bump_name;
			}
		}
	});
}

void CTextureDescrMngr::Load()
{
#ifdef DEBUG
	CTimer					TT;
	TT.Start				();
#endif // #ifdef DEBUG

	LoadLTX					();
	LoadTHM					(_game_textures_);
	LoadTHM					("$level$");
	LoadMiniLTX				();

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
	map_TD::const_iterator I = m_texture_details.find(NormalizeTexKey(tex_name));
	if (I!=m_texture_details.end())
	{
		if(I->second.m_spec)
		{
			return I->second.m_spec->m_bump_name;
		}	
	}
	return "";
}

bool CTextureDescrMngr::UseSteepParallax(const shared_str& tex_name) const
{
	map_TD::const_iterator I = m_texture_details.find(NormalizeTexKey(tex_name));
	if (I!=m_texture_details.end())
	{
		if(I->second.m_spec)
		{
			return I->second.m_spec->m_use_steep_parallax;
		}	
	}
	return false;
}

bool CTextureDescrMngr::UsePBRTexures(const shared_str& tex_name) const {
	auto I = m_texture_details.find(NormalizeTexKey(tex_name));
	if(I != m_texture_details.end()) {
		if(I->second.m_spec) {
			return I->second.m_spec->m_use_pbr;
		}
	}
	return false;
}

ShaderExternalMap* CTextureDescrMngr::GetShaderExternal(const shared_str& tex_name) const
{
	if (auto I = m_texture_details.find(NormalizeTexKey(tex_name)); I != m_texture_details.end() && I->second.m_spec != nullptr)
	{
		return &I->second.m_spec->ShadersOptions;
	}

	return nullptr;
}

float CTextureDescrMngr::GetMaterial(const shared_str& tex_name) const
{
	map_TD::const_iterator I = m_texture_details.find(NormalizeTexKey(tex_name));
	if (I!=m_texture_details.end())
	{
		if(I->second.m_spec)
		{
			return I->second.m_spec->m_material;
		}
	}
	return 1.0f;
}

void CTextureDescrMngr::GetTextureUsage	(const shared_str& tex_name, bool& bDiffuse, bool& bBump) const
{
	map_TD::const_iterator I = m_texture_details.find(NormalizeTexKey(tex_name));
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

bool CTextureDescrMngr::GetDetailTexture(const shared_str& tex_name, const char*& res, RHIShaderConstant::Setup* &CS) const
{
	map_TD::const_iterator I = m_texture_details.find(NormalizeTexKey(tex_name));
	if (I!=m_texture_details.end())
	{
		if(I->second.m_assoc)
		{
            texture_assoc* TA = I->second.m_assoc;
			res	= TA->detail_name.c_str();
			map_CS::const_iterator It2 = m_detail_scalers.find(NormalizeTexKey(tex_name));
			CS	= It2==m_detail_scalers.end()?nullptr:It2->second;//TA->cs;
			return true;
		}
	}
	return false;
}

