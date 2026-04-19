#include "stdafx.h"
#include "ETextureParams.h"

STextureParams::TFillPropImpl STextureParams::FillPropImpl;

ENGINE_API xr_token tfmt_token[] =
{
	{ "DXT1",				STextureParams::tfDXT1		},
	{ "DXT1 Alpha",			STextureParams::tfADXT1		},
	{ "DXT3",				STextureParams::tfDXT3		},
	{ "DXT5",				STextureParams::tfDXT5		},
	{ "DBC7",				STextureParams::tfBC7		},
	{ "16 bit (1:5:5:5)",	STextureParams::tf1555		},
	{ "16 bit (5:6:5)",		STextureParams::tf565		},
	{ "32 bit (8:8:8:8)",	STextureParams::tfRGBA		},
	{ "8 bit (alpha)",		STextureParams::tfA8		},
	{ "8 bit (luminance)",	STextureParams::tfL8		},
	{ "16 bit (alpha:luminance)",STextureParams::tfA8L8	},
	{ 0,					0							}
};

ENGINE_API xr_token tmtl_token[] =
{
	{ "OrenNayar <-> Blin",	STextureParams::tmOrenNayar_Blin			},
	{ "Blin <-> Phong",		STextureParams::tmBlin_Phong				},
	{ "Phong <-> Metal",	STextureParams::tmPhong_Metal				},
	{ "Metal <-> OrenNayar",STextureParams::tmMetal_OrenNayar			},
	{ "PBR",                STextureParams::tmPBR_Material			    },
	{ 0,					0											}
};

ENGINE_API xr_token tparam_token[] =
{
	{ "Advanced",			STextureParams::kMIPFilterAdvanced			},

	{ "Point",				STextureParams::kMIPFilterPoint				},
	{ "Box",				STextureParams::kMIPFilterBox				},
	{ "Triangle",			STextureParams::kMIPFilterTriangle			},
	{ "Quadratic",			STextureParams::kMIPFilterQuadratic			},
	{ "Cubic",				STextureParams::kMIPFilterCubic				},

	{ "Catrom",				STextureParams::kMIPFilterCatrom			},
	{ "Mitchell",			STextureParams::kMIPFilterMitchell			},

	{ "Gaussian",			STextureParams::kMIPFilterGaussian			},
	{ "Sinc",				STextureParams::kMIPFilterSinc				},
	{ "Bessel",				STextureParams::kMIPFilterBessel			},

	{ "Hanning",			STextureParams::kMIPFilterHanning			},
	{ "Hamming",			STextureParams::kMIPFilterHamming			},
	{ "Blackman",			STextureParams::kMIPFilterBlackman			},
	{ "Kaiser",				STextureParams::kMIPFilterKaiser			},
	{ 0,					0											}
};

ENGINE_API xr_token ttype_token[] =
{
	{ "2D Texture",			STextureParams::ttImage						},
	{ "Cube Map",  			STextureParams::ttCubeMap					},
	{ "Bump Map",			STextureParams::ttBumpMap					},
	{ "Normal Map",			STextureParams::ttNormalMap					},
	{ "Terrain",			STextureParams::ttTerrain					},
	{ 0,					0											}
};

ENGINE_API xr_token tbmode_token[] =
{
	{ "None",				STextureParams::tbmNone						},
	{ "Use",				STextureParams::tbmUse						},
	{ "Use parallax",		STextureParams::tbmUseParallax				},
	{ 0,					0											}
};

static bool FindAndValidateChunk(IReader& F, u32 ID, bool& IncorrectChunk)
{
	u32 dwSize, dwType;
	bool success = false;
	if (F.m_last_pos != 0)
	{
		F.seek(F.m_last_pos);
		dwType = F.r_u32();
		dwSize = F.r_u32();
		if ((dwType & (~CFS_CompressMark)) == ID)
		{
			success = true;
		}
	}
	if (!success)
	{
		F.rewind();
		while (!F.eof())
		{
			dwType = F.r_u32();
			dwSize = F.r_u32();
			if ((dwType & (~CFS_CompressMark)) == ID)
			{
				success = true;
				break;
			}
			else
			{
				//if (ID == THM_CHUNK_FADE_DELAY)
				{
					const u32 pos = F.tell();
					const u32 size = F.length();
					u32 length = dwSize;
					if (pos + length != size)
					{
						bool TestSize = pos + length <= size - 8;

						if (TestSize)
						{
							F.seek(pos + length);
							TestSize = F.r_u32() == ID;
						}

						if (!TestSize)
						{
							length = 0;
							while (pos + length < size)
							{
								F.seek(pos + length);

								if (pos + length <= size - 8 && F.r_u32() == ID)
									break;

								length++;
							}
							
							Msg("! THM chunk THM_CHUNK_... fixed, wrong size = %d, correct size = %d", dwSize, length);
							IncorrectChunk = true;
						}
					}

					F.seek(pos);
					dwSize = length;
				}
				F.advance(dwSize);
			}
		}
		if (!success)
		{
			F.m_last_pos = 0;
			return 0;
		}
	}

	const u32 dwPos = F.tell();
	if (dwPos + dwSize < F.length())
	{
		F.m_last_pos = dwPos + dwSize;
	}
	else
	{
		F.m_last_pos = 0;
	}
	return dwSize;
}

STextureParams::STextureParams()
{
	Clear();
}

bool STextureParams::HasAlphaChannel()
{
	switch (fmt)
	{
	case tfADXT1:
	case tfDXT3:
	case tfDXT5:
	case tf4444:
	case tf1555:
	case tfRGBA:
		return true;
	default:
		return false;
	}
}
bool STextureParams::Load(IReader& F)
{
	bool FoundedChunk = !!F.find_chunk(THM_CHUNK_TEXTUREPARAM);
	R_ASSERT2(FoundedChunk, "Not found chunk THM_CHUNK_TEXTUREPARAM");

	F.r(&fmt, sizeof(ETFormat));
	flags.assign(F.r_u32());
	border_color = F.r_u32();
	fade_color = F.r_u32();
	fade_amount = F.r_u32();
	mip_filter = F.r_u32();
	width = F.r_u32();
	height = F.r_u32();

	if (F.find_chunk(THM_CHUNK_TEXTURE_TYPE)) 
	{
		type = (ETType)F.r_u32();
	}

	if (F.find_chunk(THM_CHUNK_DETAIL_EXT)) 
	{
		F.r_stringZ(detail_name);
		detail_scale = F.r_float();
	}

	if (F.find_chunk(THM_CHUNK_MATERIAL)) 
	{
		material = (ETMaterial)F.r_u32();
		material_weight = F.r_float();
	}

	bool IncorrectChunk = false;
	if (FindAndValidateChunk(F, THM_CHUNK_BUMP, IncorrectChunk))
	{
		bump_virtual_height = F.r_float();
		bump_mode = (ETBumpMode)F.r_u32();

		if (bump_mode < STextureParams::tbmNone)
		{
			bump_mode = STextureParams::tbmNone; //.. временно (до полного убирания Autogen)
		}
		F.r_stringZ(bump_name);
	}

	if (FindAndValidateChunk(F, THM_CHUNK_EXT_NORMALMAP, IncorrectChunk))
		F.r_stringZ(ext_normal_map_name);

	if (FindAndValidateChunk(F, THM_CHUNK_FADE_DELAY, IncorrectChunk))
		fade_delay = F.r_u8();

	return IncorrectChunk;
}


void STextureParams::Save(IWriter& F)
{
	F.open_chunk	(THM_CHUNK_TEXTUREPARAM);
	F.w				(&fmt,sizeof(ETFormat));
	F.w_u32			(flags.get());
	F.w_u32			(border_color);
	F.w_u32			(fade_color);
	F.w_u32			(fade_amount);
	F.w_u32			(mip_filter);
	F.w_u32			(width);
	F.w_u32			(height);
	F.close_chunk	();

	F.open_chunk	(THM_CHUNK_TEXTURE_TYPE);
	F.w_u32			(type);
	F.close_chunk	();


	F.open_chunk	(THM_CHUNK_DETAIL_EXT);
	F.w_stringZ		(detail_name);
	F.w_float		(detail_scale);
	F.close_chunk	();

	F.open_chunk	(THM_CHUNK_MATERIAL);
	F.w_u32			(material);
	F.w_float		(material_weight);
	F.close_chunk	();

	F.open_chunk	(THM_CHUNK_BUMP);
	F.w_float		(bump_virtual_height);
	F.w_u32			(bump_mode);
	F.w_stringZ		(bump_name);
	F.close_chunk	();

	F.open_chunk	(THM_CHUNK_EXT_NORMALMAP);
	F.w_stringZ		(ext_normal_map_name);
	F.close_chunk	();

	F.open_chunk	(THM_CHUNK_FADE_DELAY);
	F.w_u8			(fade_delay);
	F.close_chunk	();
}

void STextureParams::OnTypeChange(PropValue* prop)
{
	switch (type){
	case ttImage:	
	case ttCubeMap:	
	break;
	case ttBumpMap:	
		flags.set			(flGenerateMipMaps,false);
	break;
	case ttNormalMap:
		flags.set			(flImplicitLighted|flBinaryAlpha|flAlphaBorder|flColorBorder|flFadeToColor
							|flFadeToAlpha|flDitherColor|flDitherEachMIPLevel|flBumpDetail,false);
		flags.set			(flGenerateMipMaps,true);
		mip_filter			= kMIPFilterKaiser;
		fmt					= tfRGBA;
	break;
	case ttTerrain:
		flags.set			(flGenerateMipMaps|flBinaryAlpha|flAlphaBorder|flColorBorder|flFadeToColor
							|flFadeToAlpha|flDitherColor|flDitherEachMIPLevel|flBumpDetail,false);
		flags.set			(flImplicitLighted,true);
		fmt					= tfDXT1;
	break;
	}
	if (!OnTypeChangeEvent.empty())
		OnTypeChangeEvent(prop);
}

void STextureParams::FillProp(const char* base_name, xr_vector<PropItem*>& items, TOnChange OnChangeEvent)
{
	VERIFY(FillPropImpl);
	FillPropImpl(this, base_name, items, OnChangeEvent);
}

bool STextureParams::similar(STextureParams& tp1, xr_vector<xr_string>& sel_params)
{
	bool res 				= true;
	
	xr_vector<xr_string>::iterator it = sel_params.begin();
	xr_vector<xr_string>::iterator it_e = sel_params.end();

	for(;it!=it_e;++it)
	{
	   const xr_string& par_name = *it;
		if(par_name=="Type")
		{
			res = (type==tp1.type);
		}else
		if(par_name=="Source\\Width")
		{
			res = (width==tp1.width);
		}else
		if(par_name=="Source\\Height")
		{
			res = (height==tp1.height);
		}else
		if(par_name=="Source\\Alpha")
		{
			res = (HasAlpha()==tp1.HasAlpha());
		}else
		if(par_name=="Format")
		{
			res = (fmt==tp1.fmt);
		}else
		if(par_name=="MipMaps\\Enabled")
		{
			res = (flags.test(flGenerateMipMaps)==tp1.flags.test(flGenerateMipMaps));
		}else
		if(par_name=="MipMaps\\Filter")
		{
			res = (mip_filter==tp1.mip_filter);
		}else
		if(par_name=="Bump\\Mode")
		{
			res = (bump_mode==tp1.bump_mode);
		}else
		if(par_name=="Bump\\Texture")
		{
			res = (bump_name==tp1.bump_name);
		}else
		if(par_name=="Details\\Use As Diffuse")
		{           
			res = (flags.test(flDiffuseDetail)==tp1.flags.test(flDiffuseDetail));
		}else
		if(par_name=="Details\\Use As Bump (R2)")
		{
			res = (flags.test(flBumpDetail)==tp1.flags.test(flBumpDetail));
		}else
		if(par_name=="Details\\Texture")
		{
			res = (detail_name==tp1.detail_name);
		}else
		if(par_name=="Details\\Scale")
		{
			res = (fsimilar(detail_scale,tp1.detail_scale) );
		}else
		if(par_name=="Material\\Base")
		{
			res = (material==tp1.material);
		}else
		if(par_name=="Material\\Weight")
		{
			res = (fsimilar(material_weight,tp1.material_weight) );
		}else
		if(par_name=="Flags\\Binary Alpha")
		{                  
			res = (flags.test(flBinaryAlpha)==tp1.flags.test(flBinaryAlpha));
		}else
		if(par_name=="Flags\\Dither")
		{
			res = (flags.test(flDitherColor)==tp1.flags.test(flDitherColor));
		}else
		if(par_name=="Flags\\Dither Each MIP")
		{
			res = (flags.test(flDitherEachMIPLevel)==tp1.flags.test(flDitherEachMIPLevel));
		}else
		if(par_name=="Flags\\Implicit Lighted")
		{
			res = (flags.test(flImplicitLighted)==tp1.flags.test(flImplicitLighted));
		}else
		if(par_name=="Fade\\Enable Color")
		{
			res = (flags.test(flFadeToColor)==tp1.flags.test(flFadeToColor));
		}else
		if(par_name=="Fade\\Enabled Alpha")
		{
			res = (flags.test(flFadeToAlpha)==tp1.flags.test(flFadeToAlpha));
		}else
		if(par_name=="Fade\\Delay 'n' MIP")
		{
			res = (fade_delay==tp1.fade_delay);
		}else
		if(par_name=="Fade\\% of color to fade in")
		{
			res = (fade_amount==tp1.fade_amount);
		}else
		if(par_name=="Fade\\Color")
		{
			res = (fade_color==tp1.fade_color);
		}else
		if(par_name=="Fade\\Alpha")
		{
			res = (color_get_A(fade_color)==color_get_A(tp1.fade_color));
		}else
		if(par_name=="Border\\Enabled Color")
		{
			res = (flags.test(flColorBorder)==tp1.flags.test(flColorBorder));
		}else
		if(par_name=="Border\\Enabled Alpha")
		{
			res = (flags.test(flAlphaBorder)==tp1.flags.test(flAlphaBorder));
		}else
		if(par_name=="Border\\Color")
		{
			res = (border_color==tp1.border_color);
		}else
		if(par_name=="Bump\\Special NormalMap")
		{
			res = (ext_normal_map_name==tp1.ext_normal_map_name);
		}else
		if(par_name=="Bump\\Virtual Height (m)")
		{
			res = ( fsimilar(bump_virtual_height,tp1.bump_virtual_height));
		}else
			Msg("! unknown filter [%s]", par_name.c_str());
	   if(!res)
		break;
	}

	return res;
}

const char* STextureParams::FormatString	()
{
	return get_token_name(tfmt_token,fmt);
}

u32 STextureParams::MemoryUsage(const char* base_name)
{
	u32 mem_usage	= width*height*4;
	if (flags.test(flGenerateMipMaps))	{ mem_usage*=3ul; mem_usage/=2ul; }
	switch (fmt){
	case STextureParams::tfDXT1:
	case STextureParams::tfADXT1: 	mem_usage/=6; break;
	case STextureParams::tfDXT3:
	case STextureParams::tfDXT5: 	mem_usage/=4; break;
	case STextureParams::tf4444:
	case STextureParams::tf1555:
	case STextureParams::tf565: 	mem_usage/=2; break;
	case STextureParams::tfRGBA:	break;
	}
	string_path fn;
	FS.update_path	(fn,_game_textures_,EFS.ChangeFileExt(base_name,".seq").c_str());
	if (FS.exist(fn))
	{
		string128		buffer;
		IReader* F		= FS.r_open(0,fn);
		F->r_string		(buffer,sizeof(buffer));
		int cnt = 0;
		while (!F->eof()){
			F->r_string(buffer,sizeof(buffer));
			cnt++;
		}
		FS.r_close		(F);
		mem_usage *= cnt?cnt:1;
	}
	return mem_usage;
}