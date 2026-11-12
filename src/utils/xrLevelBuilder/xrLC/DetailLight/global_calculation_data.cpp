#include "stdafx.h"
#include "../Base/BuildTextureLoader.h"
#include "global_calculation_data.h"

#include "../Shader_xrLC_Compilers.h"
#include "../Light/Embree/EmbreeRayTrace.h"
#include "../../UI/CompilersUI.h"
#include "Build/Build.h"

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
	if (O)
	{
		O->close	();
	}
}

void global_claculation_data::xrLoad()
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
		transfer("materials_shared", g_materials_shared, *fs, EB_MaterialsShared);
		transfer("shaders_xrlc",g_shader_compile,		*fs,		EB_Shaders_Compile);
		post_process_materials( *g_shaders_xrlc, g_shader_compile, g_materials );
		// process textures

		Status("Processing textures...");
		
		{
			CBuildTextureLoader::SOptions LoaderOptions;
			LoaderOptions.LevelLodsHasAlpha = false;
			LoaderOptions.DropAlphaWithoutSurface = true;
			CBuildTextureLoader TextureLoader(LoaderOptions);
			u32 TotalTextureCount = 0;
			u32 SharedMaterialCount = g_materials_shared.size();
			{
				IReader* F = fs->open_chunk(EB_Textures);

				TotalTextureCount = F->length() / sizeof(b_texture_real);
			
				for (u32 t=0; t<TotalTextureCount; t++)
				{
					Progress(float(t)/float(TotalTextureCount+SharedMaterialCount));

					b_texture_real	TEX;
					F->r(&TEX, sizeof(TEX));

					b_BuildTexture	BT;

					// ptr should be copied separately
					CopyMemory(&BT, &TEX, sizeof(TEX) - 4);

					TextureLoader.Load(BT);
				
					// save all the stuff we've created
					g_textures.push_back	(BT);
				}
			}

			for (u32 i = 0; i < SharedMaterialCount; i++)
			{
				auto& elem = g_materials_shared[i];
				
				Progress(float(TotalTextureCount+i)/float(TotalTextureCount+SharedMaterialCount));
				
				b_BuildTexture	BT;
				CopyMemory(BT.name, elem.Name, sizeof(elem.Name));

				TextureLoader.Load(BT);

				g_textures_shared[&elem] = BT;
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
		b_BuildTexture& T = gl_data.FindTexture(F.dwMaterial, F.bSharedMaterial);
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
	auto GetShader = [](u16 ID, bool SharedMaterial) -> const Shader_xrLC&
	{
		if(SharedMaterial)
		{
			auto Slot = gl_data.g_shaders_xrlc->Get(CSharedMaterialLibrary::Instance().GetData(gl_data.g_materials_shared[ID].Name)->m_ShaderXRLCName.c_str());
			R_ASSERT(Slot);
			return *Slot;
		}
		return gl_data.g_shaders_xrlc->Get(gl_data.g_materials[ID].shader_xrlc);
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

			const Shader_xrLC& SH = GetShader(B.dwMaterial, bool(B.flags&b_face_flags::UseSharedMaterial));
			if (!SH.flags.bLIGHT_CastShadow)
			{
				continue;
			}

			FaceDataEmbree& bFace = building_embree_faces.emplace_back();
			bFace.bSharedMaterial = bool(B.flags&b_face_flags::UseSharedMaterial);
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
				const Shader_xrLC& SH = GetShader(F.dwMaterial, F.bSharedMaterial);
				if (!SH.flags.bLIGHT_CastShadow)
				{
					continue;
				}
 				
  				Fvector					P[3];
				xform.transform_tiny(P[0], F.v1);
				xform.transform_tiny(P[1], F.v2);
				xform.transform_tiny(P[2], F.v3);
				
				// ����� �� ��������� MU! 
				auto& Fnew = building_embree_faces.emplace_back();
 				Fnew.SetFace(P[0], P[1], P[2], &F);
				Fnew.SetMaterial(F.dwMaterial, F.dwMaterialGame, F.getTC0(), F.bSharedMaterial);			
			}
   		}
		MUChunkRef->close();
	}

	xrCalculateOpacity();
 
	// ���� ������� BOX-QUERY ��� ������� t_n !
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

		Msg("RayQuery Box Model: Faces : %zu | Vertex: %zu", triangles.size(), verts.size());
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
	g_materials_shared.clear(); g_materials_shared.shrink_to_fit();
	g_textures.clear();		  g_textures.shrink_to_fit();
	g_textures_shared.clear();

	building_embree_faces.clear(); building_embree_faces.shrink_to_fit();
}

b_BuildTexture& global_claculation_data::FindTexture(u16 Index, bool Shared)
{
	if (Shared)
	{
		return g_textures_shared[&g_materials_shared[Index]];
	}
	return g_textures[g_materials[Index].surfidx];
}
