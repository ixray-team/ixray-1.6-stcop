#include "stdafx.h"
#include "global_calculation_data.h"

#include "../Shader_xrLC.h"
#include "embree_raytracing/EmbreeRayTrace.h"
#include "../xrForms/CompilersUI.h"
 
global_claculation_data	gl_data;

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

void global_claculation_data::xrLoad(bool skipThm)
{
 	string_path					SharesN;
	FS.update_path(SharesN, "$game_data$", "shaders_xrlc.xr");
	g_shaders_xrlc = new Shader_xrLC_LIB();
	g_shaders_xrlc->Load(SharesN);
 
	// Load CFORM

	Phase("Loading slots ...");
 	slots_data.Load( );

 	// Lights
	{
		IReader*			fs = FS.r_open("$level$","build.lights");
		IReader*			F;	u32 cnt; R_Light* L;

		// rgb
		F		=			fs->open_chunk		(0);
		cnt		=			F->length()/sizeof(R_Light);
		L		=			(R_Light*)F->pointer();
		g_lights.rgb.assign	(L,L+cnt);
		F->close			();

		// hemi
		F		=			fs->open_chunk		(1);
		cnt		=			F->length()/sizeof(R_Light);
		L		=			(R_Light*)F->pointer();
		g_lights.hemi.assign(L,L+cnt);
		F->close			();

		// sun
		F		=			fs->open_chunk		(2);
		cnt		=			F->length()/sizeof(R_Light);
		L		=			(R_Light*)F->pointer();
		g_lights.sun.assign	(L,L+cnt);
		F->close			();

		FS.r_close			(fs);
	}

	
	// Load level data
	IReader* fs = FS.r_open("$level$", "build.prj");

	{
 		// Version
		u32 version;
		fs->r_chunk			(EB_Version,&version);
		R_ASSERT(XRCL_CURRENT_VERSION==version);

		// Header
		fs->r_chunk			(EB_Parameters,&g_params);

		// Load level data
		transfer("materials",	g_materials,			*fs,		EB_Materials);
		transfer("shaders_xrlc",g_shader_compile,		*fs,		EB_Shaders_Compile);
		post_process_materials( *g_shaders_xrlc, g_shader_compile, g_materials );
		// process textures

		Status("Processing textures...");
		
		IReader* F;
		{
			F = fs->open_chunk	(EB_Textures);

			u32 tex_count = F->length() / sizeof(b_texture_real);

			bool is_thm_missing = false;
			bool is_tga_missing = false;

			for (u32 t=0; t<tex_count; t++)
			{
				Progress(float(t)/float(tex_count));

				b_texture_real	TEX;
				F->r(&TEX, sizeof(TEX));

				b_BuildTexture	BT;

				// ptr should be copied separately
				CopyMemory(&BT, &TEX, sizeof(TEX) - 4);
				BT.pSurface.Clear();

				// load thumbnail
				LPSTR N			= BT.name;
				if (strchr(N,'.')) *(strchr(N,'.')) = 0;
				_strlwr			(N);

				if (0==xr_strcmp(N,"level_lods"))
				{
					// HACK for merged lod textures
					BT.dwWidth	 = 1024;
					BT.dwHeight	 = 1024;
					BT.bHasAlpha = false;
					BT.SetHasSurface(false);
				}
				else
				{
					string_path NameThm;
					xr_strcpy(NameThm, BT.name);
					xr_strcat(NameThm, ".thm");
					IReader* THM = FS.r_open("$game_textures$", NameThm);

					if (!THM)
					{
						clMsg("cannot find thm: %s", N);
						is_thm_missing = true;

						BT.bHasAlpha = false;
 						BT.SetHasSurface(false);
					}
					else
					{
						// version
						u32 version = 0;
						R_ASSERT(THM->r_chunk(THM_CHUNK_VERSION, &version));

						// analyze thumbnail information
						R_ASSERT(THM->find_chunk(THM_CHUNK_TEXTUREPARAM));
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
						BT.SetHasSurface(false);

						if (!bLOD)
						{
							if (BT.bHasAlpha || BT.THM.flags.test(STextureParams::flImplicitLighted))
							{
								clMsg("- loading: %s", N);
								BT.SetHasSurface(true);

								string_path OutName;
								if (!Surface_Detect(OutName, N) || !BT.pSurface.LoadFromFile(OutName))
								{
									clMsg("cannot find tga texture: %s", N);
									is_tga_missing = true;

									BT.bHasAlpha = false;
									BT.SetHasSurface(false);
								}
								else
								{
									BT.pSurface.ClearMipLevels();
									BT.pSurface.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);
									BT.pSurface.SwapRB();

									if ((BT.pSurface.GetWidth() != BT.dwWidth) || (BT.pSurface.GetHeight() != BT.dwHeight))
									{
										Msg("! THM doesn't correspond to the texture: %dx%d -> %dx%d", BT.dwWidth, BT.dwHeight, BT.pSurface.GetWidth(), BT.pSurface.GetHeight());
										BT.dwWidth  = BT.THM.width  = BT.pSurface.GetWidth();
										BT.dwHeight = BT.THM.height = BT.pSurface.GetHeight();
									}
								}
							}
						}
					}
					
				}

				// save all the stuff we've created
				g_textures.push_back	(BT);
			}

			if (!skipThm)
			{
				R_ASSERT2(!is_thm_missing, "Some of required thm's are missing. See log for details.");
				R_ASSERT2(!is_tga_missing, "Some of required tga_textures are missing. See log for details.");
			}
		}
	}

	// Load Geometry New 
	xrLoadGeometry(fs);
} 

void global_claculation_data::xrCalculateOpacity()
{
	for (auto& F : building_embree_faces)	
	{
 		F.bOpaque = true;
		 
		b_material& M = gl_data.g_materials[F.dwMaterial];
		b_BuildTexture& T = gl_data.g_textures[M.surfidx];
		F.bOpaque = !T.bHasAlpha;

		// pSurface was possible deleted
		if (!F.bOpaque && (!T.HasSurface()))
		{
			F.bOpaque = true;
			clMsg("Strange face detected... Has alpha without texture... [%s]", T.name);
		}
	}
}

void global_claculation_data::xrLoadGeometry(IReader* fs)
{
 	auto GetShader = [](u32 dwMaterial) -> const Shader_xrLC&
	{
		return shader(dwMaterial, *gl_data.g_shaders_xrlc, gl_data.g_materials);
	};


	Status("Loading Vertices...");
	xr_vector<Fvector> vertexs;
  	{
		IReader* CHVertex = fs->open_chunk(EB_Vertices);
	
		u32 v_count = CHVertex->length() / sizeof(b_vertex);

		vertexs.resize(v_count);
 		for (u32 i = 0; i < v_count; i++)
  			CHVertex->r_fvector3(vertexs[i]);
 
		CHVertex->close();
	}

	//*******
	Status("Loading Faces...");
	{
		IReader * ChunkFaces = fs->open_chunk(EB_Faces);
		R_ASSERT(ChunkFaces);
		u32 f_count = ChunkFaces->length() / sizeof(b_face);
 		
		for (u32 i = 0; i < f_count; i++)
		{
 			b_face	B;
			ChunkFaces->r(&B, sizeof(B));
			R_ASSERT(B.dwMaterialGame < 65536);

			const Shader_xrLC& SH = GetShader(B.dwMaterial);
			if (!SH.flags.bLIGHT_CastShadow) continue;

			FaceDataEmbree& bFace = building_embree_faces.emplace_back();
			bFace.dwMaterial	 = u16(B.dwMaterial);
			bFace.dwMaterialGame = B.dwMaterialGame;
			bFace.ptr = &bFace;

			// Vertices and adjacement info
			bFace.v1 = vertexs[ B.v[0] ];
			bFace.v2 = vertexs[ B.v[1] ];
			bFace.v3 = vertexs[ B.v[2] ];

			// transfer TC
			bFace.TC[0].set(B.t[0].x, B.t[0].y);
			bFace.TC[1].set(B.t[1].x, B.t[1].y);
			bFace.TC[2].set(B.t[2].x, B.t[2].y);
 
 		}
		ChunkFaces->close();
	}


	//*******
	Status("Models and References");
	IReader* MUChunk = fs->open_chunk(EB_MU_models);

	xr_map<u16, xr_vector<FaceDataEmbree>> mu_faces;
	if (MUChunk)
	{
		int ModelID = 0;
		while (!MUChunk->eof())
		{
			xrMU_Model().Load_Embree(*MUChunk, mu_faces[ModelID]);
			ModelID++;
 		}
		MUChunk->close();
	}

	IReader* MUChunkRef = fs->open_chunk(EB_MU_refs);
	if (MUChunkRef)
	{
		while (!MUChunkRef->eof())
		{
			b_mu_reference		R;
			MUChunkRef->r(&R, sizeof(R));
			
			Fmatrix xform = R.transform;				// Transformation !
			auto& faces = mu_faces[R.model_index];		// Model Buffer by Index !
			for (auto& F : faces)
			{
				const Shader_xrLC& SH = GetShader(F.dwMaterial);
				if (!SH.flags.bLIGHT_CastShadow) continue;
 				
				auto& F = building_embree_faces.emplace_back();

 				Fvector					P[3];
				xform.transform_tiny(P[0], F.v1);
				xform.transform_tiny(P[1], F.v2);
				xform.transform_tiny(P[2], F.v3);
				
				F.SetFace(P[0], P[1], P[2], &F);
				F.SetMaterial(F.dwMaterial, F.dwMaterialGame, F.getTC0());			
			}
   		}
		MUChunkRef->close();
	}

	xrCalculateOpacity();
 
	// Изза сраного BOX-QUERY Для расщета t_n !
	if (true) // Rcast - Model
	{
		TriangleContainer container;
		for (auto& F : building_embree_faces)
		{
			container.AddFaceRaw(&F, F.v1, F.v2, F.v3);
		}
		container.useMsg = false;
		container.RemoveDublicates();

		RCAST_Model = new CDB::MODEL();
		xr_vector<Fvector>& verts = RCAST_Model->get_verts();
		xr_vector<CDB::TRI>& triangles = RCAST_Model->get_tris();
	
		verts = container.vertex();
		for (auto& F : container.faces())
		{
			triangles.push_back(F.Get());
		}

		Msg("RayQuery Box Model: Faces : %u | Vertex: %u", triangles.size(), verts.size());
		RCAST_Model->build(verts.data(), verts.size(), triangles.data(), triangles.size(), 
			nullptr, nullptr, nullptr, false , false);
	}
}


void global_claculation_data::xrUnload()
{
	slots_data.Free();

	xr_delete(RCAST_Model);
	xr_delete(g_shaders_xrlc);

	// lights 
	g_lights.clear();

	// vectors
	g_shader_compile.clear(); g_shader_compile.shrink_to_fit();
	g_materials.clear();	  g_materials.shrink_to_fit();
	g_textures.clear();		  g_textures.shrink_to_fit();

	building_embree_faces.clear(); building_embree_faces.shrink_to_fit();

}
