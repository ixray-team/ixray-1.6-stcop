#include "StdAfx.h"
#include "ELight_def.h"

#include "Build.h"
#include "src/utils/Shader_xrLC_Compilers.h"

#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"
#include "../xrLC_Light/xrMU_Model.h"
#include "../xrLC_Light/xrMU_Model_Reference.h"

extern u32	version;
template <class T>
void transfer(const char *name, xr_vector<T> &dest, IReader& F, u32 chunk)
{
	IReader*	O	= F.open_chunk(chunk);
	u32		count	= O?(O->length()/sizeof(T)):0;
	clMsg			("* %16s: %d",name,count);
	if (count)  
	{
		dest.reserve(count);
		dest.insert	(dest.begin(), (T*)O->pointer(), (T*)O->pointer() + count);
	}
	if (O)		O->close	();
}

struct R_Control
{
	string64				name;
	xr_vector<u32>			data;
};

struct R_Layer
{
	R_Control				control;
	xr_vector<R_Light>		lights;
};

inline bool Surface_Detect(string_path& F, LPSTR N)
{
	FS.update_path(F, "$game_textures$", xr_strconcat(F, N, ".dds"));
	FILE* file = fopen(F, "rb");
	if (file)
	{
		fclose(file);
		return true;
	}

	return false;
}

void CBuild::Load	(const b_params& Params, const IReader& _in_FS)
{
	IReader&	fs	= const_cast<IReader&>(_in_FS);

	u32				i			= 0;

	float			p_total		= 0;
	float			p_cost		= 1.f/3.f;
	
	IReader*		F			= 0;

	// 
	string_path				sh_name;
	FS.update_path			(sh_name,"$game_data$","shaders_xrlc.xr");
	shaders().Load			(sh_name);


	//*******

	Status("Vertices...");
 
	size_t pre = GetHeapMemory();
	{
		F = fs.open_chunk(EB_Vertices);
		u32 v_count = F->length() / sizeof(b_vertex);
		scene_bb.invalidate();

		Status("* %16s: %d", "vertices", lc_global_data()->g_vertices().size());

		size_t LastStatCalls = 0;
		for (i = 0; i < v_count; i++)
		{
			Vertex* pV = lc_global_data()->create_vertex();
			F->r_fvector3(pV->P);
			pV->N.set(0, 0, 0);
			scene_bb.modify(pV->P);
		}
		
		Progress(p_total += p_cost);
		Status("* %16s: %d", "vertices", lc_global_data()->g_vertices().size());
		F->close();
	}

	//*******
	Status					("Faces...");
	{
		F = fs.open_chunk		(EB_Faces);
		R_ASSERT				(F);
		u32 f_count			=	F->length()/sizeof(b_face);
		lc_global_data()->g_faces().reserve			(f_count);

		for (i=0; i<f_count; i++)
		{
			Face*	_F			= lc_global_data()->create_face();
			
			b_face	B;
			B.v[0] = F->r_u32();
			B.v[1] = F->r_u32();
			B.v[2] = F->r_u32();
			F->r(&B.t[0], sizeof(Fvector2));
			F->r(&B.t[1], sizeof(Fvector2));
			F->r(&B.t[2], sizeof(Fvector2));
			B.dwMaterial = F->r_u16();
			F->r(&B.flags, sizeof(b_face_flags));
			B.dwMaterialGame = F->r_u32();
			R_ASSERT			(B.dwMaterialGame<65536);

			_F->dwMaterial		= u16(B.dwMaterial);
			_F->dwMaterialGame	= B.dwMaterialGame;
			_F->flags.bSharedMaterial = !!(B.flags & b_face_flags::UseSharedMaterial);

			// Vertices and adjacement info
			for (u32 it=0; it<3; ++it)
			{
				int id			= B.v[it];
				R_ASSERT		(id<(int)lc_global_data()->g_vertices().size());
				_F->SetVertex	(it, lc_global_data()->g_vertices()[id]);
			}

			// transfer TC
			Fvector2				uv1,uv2,uv3;
			uv1.set				(B.t[0].x,B.t[0].y);
			uv2.set				(B.t[1].x,B.t[1].y);
			uv3.set				(B.t[2].x,B.t[2].y);
			_F->AddChannel		( uv1, uv2, uv3 );
		}
		Progress			(p_total+=p_cost);
		clMsg				("* %16s: %d","faces",lc_global_data()->g_faces().size());
		F->close			();

		if(g_using_smooth_groups)
		{
			F = fs.open_chunk		(EB_SmoothGroups);
			
			R_ASSERT2				(F,"EB_SmoothGroups chunk not found.");
			
			u32* sm_groups			= NULL;
			u32 sm_count			=	F->length()/sizeof(u32);
		
			R_ASSERT				( sm_count == lc_global_data()->g_faces().size() );
			sm_groups				= xr_alloc<u32>(sm_count);
			F->r					(sm_groups, F->length());
			F->close				();
		
			for(u32 idx=0; idx<sm_count; ++idx)
				lc_global_data()->g_faces()[idx]->sm_group = sm_groups[idx];
		
			xr_free					(sm_groups);
		}
		
		if (InvalideFaces())	
		{
			// err_save		();
			if (gCompilerMode.LC_SkipInvalidFaces) 
			{
				clMsg("* Total %d invalid faces. Do something.", InvalideFaces());
			} else {
				Debug.fatal(DEBUG_INFO, "* FATAL: %d invalid faces. Compilation aborted", InvalideFaces());
			}
		}
	}
	 
	//*******
	Status	("Models and References");
	F = fs.open_chunk		(EB_MU_models);
	if (F)
	{
		auto F_LODs = fs.open_chunk(EB_MU_Mesh_LODs);
		while (!F->eof())
		{
			mu_models().push_back				(new xrMU_Model());
			auto Model = mu_models().back();
			Model->Load			(*F, version );
			if (F_LODs)
			{
				Model->UseBillboard = !F_LODs->r_u8();
				if (Model->UseBillboard)
				{
					continue;
				}
				auto LODRead = [&](int ID)
				{
					Model->LODsID[ID] = F_LODs->r_u32();
					if (Model->LODsID[ID] != u32(-1))
					{
						mu_models().push_back(new xrMU_Model());
						auto LOD = mu_models().back();
						LOD->Load(*F, version );
						LOD->UseBillboard = false;
						LOD->IsLOD = true;
					}
				};
				LODRead(0);
				LODRead(1);
				LODRead(2);
				LODRead(3);				
			}
		}
		F->close				();
		if (F_LODs)
		{
			F_LODs->close();
		}
	}
	
	F = fs.open_chunk		(EB_MU_refs);
	if (F)
	{
		auto& vec = mu_refs();
		while (!F->eof())
		{
			vec.push_back				(new xrMU_Reference());
			vec.back()->Load			( *F, mu_models() );
		}		
		F->close				();
	}

	F = fs.open_chunk(EB_MU_refs_debug);
	if (F)
	{
		auto& vec = mu_refs();
		for (auto ref : vec)
		{
			F->r_stringZ(ref->debug_name);
		}
	}


	//*******
	Status("Other transfer...");
	transfer("materials", materials(), fs, EB_Materials);
	transfer("materials_shared", materials_shared(), fs, EB_MaterialsShared);
	transfer("shaders", shader_render, fs, EB_Shaders_Render);
	transfer("shaders_xrlc", shader_compile, fs, EB_Shaders_Compile);
	transfer("glows", glows, fs, EB_Glows);
	transfer("portals", portals, fs, EB_Portals);
	transfer("LODs", lods, fs, EB_LOD_models);

	// Load lights
	Status	("Loading lights...");
	{
		xr_vector<R_Layer>			L_layers;
		xr_vector<BYTE>				L_control_data;

		// Controlles/Layers
		{
			F = fs.open_chunk		(EB_Light_control);
			L_control_data.assign	(LPBYTE(F->pointer()),LPBYTE(F->pointer())+F->length());

			R_Layer					temp;

			while (!F->eof())
			{
				F->r				(temp.control.name,sizeof(temp.control.name));
				u32 cnt				= F->r_u32();
				temp.control.data.resize(cnt);
				F->r(temp.control.data.data(), cnt * sizeof(u32));

				L_layers.push_back	(temp);
			}

			F->close		();
		}
		// Static
		{
			F = fs.open_chunk	(EB_Light_static);
			b_light_static		temp;
			u32 cnt				= F->length()/sizeof(temp);
			for	(i=0; i<cnt; i++)
			{
				R_Light		RL;
				F->r		(&temp,sizeof(temp));
				Flight	L	= temp.data;

				// type
				if			(L.type == D3DLIGHT_DIRECTIONAL)	RL.type	= LT_DIRECT;
				else											
					RL.type = LT_POINT;
				RL.level	= 0;

				// split energy/color
				float			_e		=	(L.diffuse.r+L.diffuse.g+L.diffuse.b)/3.f;
				Fvector			_c		=	{L.diffuse.r,L.diffuse.g,L.diffuse.b};
				if (std::abs(_e)>EPS_S)		_c.div	(_e);
				else					{ _c.set(0,0,0); _e=0; }

				// generic properties
				RL.diffuse.set				(_c);
				RL.position.set				(L.position);
				RL.direction.normalize_safe	(L.direction);
				RL.range				=	L.range*1.1f;
				RL.range2				=	RL.range*RL.range;
				RL.attenuation0			=	L.attenuation0;
				RL.attenuation1			=	L.attenuation1;
				RL.attenuation2			=	L.attenuation2;
				RL.falloff				=   1.0f/(RL.range*(RL.attenuation0 + RL.attenuation1*RL.range + RL.attenuation2*RL.range2));
				RL.energy				=	_e;

				// place into layer
				R_ASSERT	(temp.controller_ID<L_layers.size());
				L_layers	[temp.controller_ID].lights.push_back	(RL);
			}
			F->close		();
		}

		// ***Search LAYERS***
		for (u32 LH=0; LH<L_layers.size(); LH++)
		{
			R_Layer&	TEST	= L_layers[LH];
			if (0==_stricmp(TEST.control.name,LCONTROL_HEMI))
			{
				// Hemi found
				L_static().hemi			= TEST.lights;
			}
			if (0==_stricmp(TEST.control.name,LCONTROL_SUN))
			{
				// Sun found
				L_static().sun			= TEST.lights;
			}
			if (0==_stricmp(TEST.control.name,LCONTROL_STATIC))
			{
				// Static found
				L_static().rgb			= TEST.lights;
			}
		}
		clMsg	("*lighting*: HEMI:   %d lights",L_static().hemi.size());
		clMsg	("*lighting*: SUN:    %d lights",L_static().sun.size());
		clMsg	("*lighting*: STATIC: %d lights",L_static().rgb.size());
		R_ASSERT(L_static().hemi.size());
		R_ASSERT(L_static().sun.size());
		R_ASSERT(L_static().rgb.size());

		// Dynamic
		transfer("d-lights",	L_dynamic,			fs,		EB_Light_dynamic);
	}
	
	// process textures
	Status			("Processing textures...");
	{
		bool is_thm_missing = false;
		bool is_tga_missing = false;

		auto TextureProcess = [&](b_BuildTexture& BT)
		{
			BT.pSurface.Clear();

			// load thumbnail
			LPSTR N = BT.name;
			if (strchr(N,'.'))
			{
				*(strchr(N,'.')) = 0;
			}
			_strlwr(N);

			if (0==xr_strcmp(N,"level_lods")) 
			{
				// HACK for merged lod textures
				BT.dwWidth		= 1024;
				BT.dwHeight		= 1024;
				BT.bHasAlpha	= true;
				BT.SetHasSurface(false);
 			} 
			else 
			{
				string_path			th_name;
				FS.update_path	(th_name,"$game_textures$", xr_strconcat(th_name,N,".thm"));
 				IReader* THM	= FS.r_open(th_name);
 				// se7kills Не трогать Можно нормально скипать отсуцтвие THM
				if (!THM)
				{
					Msg("! cannot find thm: %s", th_name);
 					is_thm_missing = true;
					BT.dwWidth = 1024;
					BT.dwHeight = 1024;
					BT.bHasAlpha = false;
					BT.SetHasSurface(false);
 				}
				else
				{
					// version
					u32 version = 0;
					R_ASSERT2(THM->r_chunk(THM_CHUNK_VERSION, &version), th_name);

					// analyze thumbnail information
					R_ASSERT2(THM->find_chunk(THM_CHUNK_TEXTUREPARAM), th_name);
					THM->r(&BT.THM.fmt, sizeof(STextureParams::ETFormat));
					BT.THM.flags.assign(THM->r_u32());
					BT.THM.border_color = THM->r_u32();
					BT.THM.fade_color = THM->r_u32();
					BT.THM.fade_amount = THM->r_u32();
					BT.THM.mip_filter = THM->r_u32();
					BT.THM.width = THM->r_u32();
					BT.THM.height = THM->r_u32();
					bool			bLOD = false;
					if (N[0] == 'l' && N[1] == 'o' && N[2] == 'd' && N[3] == '\\') bLOD = true;

					// load surface if it has an alpha channel or has "implicit lighting" flag
					BT.dwWidth = BT.THM.width;
					BT.dwHeight = BT.THM.height;
					BT.bHasAlpha = BT.THM.HasAlphaChannel();
					if (!bLOD)
					{
						if (BT.bHasAlpha || BT.THM.flags.test(STextureParams::flImplicitLighted)  )
						{
							clMsg("- loading: %s W[%u] H[%u]", N, BT.dwWidth, BT.dwHeight);
							BT.SetHasSurface(true);

							string_path OutName;
							if (!Surface_Detect(OutName, N) || !BT.pSurface.LoadFromFile(OutName))
							{
								clMsg("! cannot find dds texture: %s", N);
								is_tga_missing = true;

								BT.SetHasSurface(false);
								return;
							}

							BT.pSurface.ClearMipLevels();
							BT.pSurface.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);
							BT.pSurface.SwapRB();

							if ((BT.pSurface.GetWidth() != BT.dwWidth) || (BT.pSurface.GetHeight() != BT.dwHeight))
							{
								Msg("! THM doesn't correspond to the texture: %dx%d -> %dx%d", BT.dwWidth, BT.dwHeight, BT.pSurface.GetWidth(), BT.pSurface.GetHeight());
								BT.dwWidth = BT.THM.width = BT.pSurface.GetWidth();
								BT.dwHeight = BT.THM.height = BT.pSurface.GetHeight();
							}
						}
					}
				}

			} 
		};

		u32 SharedMaterialsCount = materials_shared().size();
		u32 tex_count = SharedMaterialsCount;
		if (F = fs.open_chunk(EB_Textures); F)
		{
			tex_count += F->length() / sizeof(b_texture_real);
			for (u32 t=0; t<tex_count-SharedMaterialsCount; t++)
			{
				Progress(float(t)/float(tex_count));

				b_texture_real TEX;
				F->r(&TEX, sizeof(TEX));
				b_BuildTexture	BT;

				// ptr should be copied separately
				CopyMemory(&BT, &TEX, sizeof(TEX) - 4);

				TextureProcess(BT);
			
				// save all the stuff we've created
				textures().push_back	(BT);
			}
		}

		for (u32 i = 0; i < materials_shared().size(); i++)
		{
			auto& elem = materials_shared()[i];
			
			Progress(float(i + SharedMaterialsCount)/float(tex_count));

			textures_shared()[&elem] = {};
			b_BuildTexture&	BT = textures_shared()[&elem];
			auto Data = CSharedMaterialLibrary::Instance().GetData(elem.Name);
			R_ASSERT(Data);
			CopyMemory(BT.name, Data->m_Texture.c_str(), Data->m_Texture.size()+1);

			TextureProcess(BT);
			
		}

		if (!gCompilerMode.SkipTHM)
		{
			R_ASSERT2(!is_thm_missing, "Some of required thm's are missing. See log for details.");
			R_ASSERT2(!is_tga_missing, "Some of required tga_textures are missing. See log for details.");
		}
	}

	// post-process materials
	Status	("Post-process materials...");
	post_process_materials( shaders(), shader_compile, materials() );
	post_process_materials_shared( shaders(), materials_shared() );

	Progress(p_total+=p_cost);

	// Parameter block
	CopyMemory(&g_params(),&Params,sizeof(b_params));

	// 
	clMsg	("* sizes: V(%d),F(%d)",sizeof(Vertex),sizeof(Face));
}



