#include "StdAfx.h"
#include "compiler.h"
#include "../xrForms/CompilersUI.h"
#include "compiler_embree.h"
#include "src/xrCore/SharedMaterialLibrary.h"
#include "src/xrCore/Collision/override/Model.h"

template <class T>
void transfer(const char* name, xr_vector<T>& dest, IReader& F, u32 chunk)
{
	IReader* O = F.open_chunk(chunk);
	u32 count = O ? (O->length() / sizeof(T)) : 0;
	clMsg("* %16s: %d", name, count);
	if (count)
	{
		dest.reserve(count);
		dest.insert(dest.begin(), (T*)O->pointer(), (T*)O->pointer() + count);
	}
	if (O)
	{
		O->close();
	}
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
 
void IComputeData::xrLoadData(const char* name, bool draft_mode, bool skipThm)
{
	string256 N;
	if (!draft_mode)
	{
		// shaders
		string_path N__;
		FS.update_path(N__, "$game_data$", "shaders_xrlc.xr");
		comp_data.g_shaders_xrlc = new Shader_xrLC_LIB();
		comp_data.g_shaders_xrlc->Load(N__);

		// Load level data
		{
			xr_strconcat(N__, name, "build.prj");
			IReader* fs = FS.r_open(N__);

			// Version
			u32 version;
			fs->r_chunk(EB_Version, &version);
			R_ASSERT(XRCL_CURRENT_VERSION >= 17);
			R_ASSERT(XRCL_CURRENT_VERSION <= 18);

			// Header
			b_params Params;
			fs->r_chunk(EB_Parameters, &Params);

			// Load level data
			transfer("materials", comp_data.g_materials, *fs, EB_Materials);
			transfer("materials", comp_data.g_materials_shared, *fs, EB_MaterialsShared);
			transfer("shaders_xrlc", comp_data.g_shader_compile, *fs, EB_Shaders_Compile);

			// process textures
			bool is_thm_missing = false;
			bool is_tga_missing = false;

			Status("Processing textures...");
			{
				auto TextureProcess = [&](b_BuildTexture& BT)
				{
					string128& N_ = BT.name;
					LPSTR extension = strext(N_);

					if (extension)
					{
						*extension = 0;
					}

					xr_strlwr(N_);
					

					if (0 == xr_strcmp(N_, "level_lods"))
					{
						// HACK for merged lod textures
						BT.dwWidth = 1024;
						BT.dwHeight = 1024;
						BT.bHasAlpha = true;
					}
					else
					{
						string_path th_name;
						xr_strconcat(th_name, N_, ".thm");
						IReader* THM = FS.r_open("$game_textures$", th_name);

						if (!THM)
						{
							BT.dwWidth = 1024;
							BT.dwHeight = 1024;
							BT.bHasAlpha = false;
							clMsg("cannot find thm: %s", th_name);
							is_thm_missing = true;
							return;
						}

						// version
						u32 version_ = 0;
						R_ASSERT(THM->r_chunk(THM_CHUNK_VERSION, &version_));
						// if( version!=THM_CURRENT_VERSION )	FATAL	("Unsupported version of THM file.");

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
						bool bLOD = false;
						if (N_[0] == 'l' && N_[1] == 'o' && N_[2] == 'd' && N_[3] == '\\')
						{
							bLOD = true;
						}

						// load surface if it has an alpha channel or has "implicit lighting" flag
						BT.dwWidth = BT.THM.width;
						BT.dwHeight = BT.THM.height;
						BT.bHasAlpha = BT.THM.HasAlphaChannel();
						if (!bLOD)
						{
							if (BT.bHasAlpha || BT.THM.flags.test(STextureParams::flImplicitLighted))
							{
								clMsg("- loading: %s", N_);

								string_path OutName;
								if (!Surface_Detect(OutName, N_) || !BT.pSurface.LoadFromFile(OutName))
								{
									clMsg("cannot find tga texture: %s", N_);
									is_tga_missing = true;
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
				};
				
				u32 TotalCount = g_materials_shared.size();
				u32 tex_count = 0;
				if (auto F = fs->open_chunk(EB_Textures); F)
				{
					u32 tex_count = F->length() / sizeof(b_texture_real);
					TotalCount += tex_count;
					for (u32 t = 0; t < tex_count; t++)
					{
						Progress(float(t) / float(tex_count));

						b_texture_real TEX;
						F->r(&TEX, sizeof(TEX));
						b_BuildTexture BT;

						// ptr should be copied separately
						CopyMemory(&BT, &TEX, sizeof(TEX) - 4);

						// load thumbnail
						TextureProcess(BT);

						// save all the stuff we've created
						comp_data.g_textures.push_back(BT);
					}
				}
				
				for (auto& elem : g_materials_shared)
				{
					Progress(float(tex_count++) / float(TotalCount));
					
					auto Data = CSharedMaterialLibrary::Instance().GetData(elem.Name);
					b_BuildTexture& BT = g_textures_shared.try_emplace(&elem).first->second;
					xr_strcpy(BT.name, Data->m_Texture.c_str());
					
					TextureProcess(BT);
				}

				if (!skipThm)
				{
					R_ASSERT2(!is_thm_missing, "Some of required thm's are missing. See log for details.");
					R_ASSERT2(!is_tga_missing, "Some of required tga_textures are missing. See log for details.");
				}
			}

			Status("Loading Geometry ...");
			xrLoadGeometry(fs);

		}
	}

	// Load lights
	{
		xr_strconcat(N, name, "build.prj");

		IReader* F = FS.r_open(N);
		R_ASSERT2(F, "There is no file 'build.prj'!");
		IReader& fs = *F;

		// Version
		u32 version;
		fs.r_chunk(EB_Version, &version);
		R_ASSERT(XRCL_CURRENT_VERSION >= 17);
		R_ASSERT(XRCL_CURRENT_VERSION <= 18);

		// Header
		b_params Params;
		fs.r_chunk(EB_Parameters, &Params);

		// Lights (Static)
		{
			F = fs.open_chunk(EB_Light_static);
			b_light_static temp;
			u32 cnt = F->length() / sizeof(temp);
			for (u32 i = 0; i < cnt; i++)
			{
				R_Light_Fast RL;
				F->r(&temp, sizeof(temp));
				Flight& L = temp.data;
				if (std::abs(L.range) > 10000.f)
				{
					Msg("! BAD light range : %f", L.range);
					L.range = L.range > 0.f ? 10000.f : -10000.f;
				}

				// type
				RL.type = (L.type == D3DLIGHT_DIRECTIONAL) ? LT_DIRECT : LT_POINT;

				// generic properties
				RL.position.set(L.position);
				RL.direction.normalize_safe(L.direction);
				RL.range = L.range * 1.1f;
				RL.range2 = RL.range * RL.range;
				RL.attenuation0 = L.attenuation0;
				RL.attenuation1 = L.attenuation1;
				RL.attenuation2 = L.attenuation2;

				RL.amount = L.diffuse.magnitude_rgb();
				RL.tri[0].set(0, 0, 0);
				RL.tri[1].set(0, 0, 0);
				RL.tri[2].set(0, 0, 0);

				// place into layer
				if (0 == temp.controller_ID)
				{
					g_lights.push_back(RL);
				}
			}
			F->close();
		}
	}
}

void IComputeData::xrLoadGeometry(IReader* fs)
{
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
		IReader* ChunkFaces = fs->open_chunk(EB_Faces);
		R_ASSERT(ChunkFaces);
		u32 f_count = ChunkFaces->length() / sizeof(b_face);
 		for (u32 i = 0; i < f_count; i++)
		{
			b_face B;
			ChunkFaces->r(&B, sizeof(B));
			R_ASSERT(B.dwMaterialGame < 65536);

			const Shader_xrLC& SH = GetShaderXRLC(B.dwMaterial, (bool)(B.flags&b_face_flags::UseSharedMaterial));
			if (!SH.flags.bLIGHT_CastShadow) continue;
 			 
			FaceDataEmbree& bFace = build_faces.emplace_back();
			bFace.SetFaceBuild(B, vertexs[B.v[0]], vertexs[B.v[1]], vertexs[B.v[2]]);
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
	
		auto LoadModel = [](IReader& F, xr_vector<FaceDataEmbree>& faces)
		{
			u16 lodID = 0;

			shared_str name;
			F.r_stringZ(name);

			// READ: vertices
			xr_vector<b_vertex> b_vertices;
			b_vertices.resize(F.r_u32());
			F.r(&*b_vertices.begin(), (u32)b_vertices.size() * sizeof(b_vertex));

			// READ: faces
			xr_vector<b_face> b_faces;
			b_faces.resize(F.r_u32());
			F.r(&*b_faces.begin(), (u32)b_faces.size() * sizeof(b_face));

 			// READ: lod-ID
			F.r(&lodID, 2);

			xr_vector<u32> sm_groups;
			sm_groups.resize(b_faces.size());
			F.r(&*sm_groups.begin(), (u32)sm_groups.size() * sizeof(u32));

			for (auto& bFace : b_faces)
			{
				FaceDataEmbree Fnew;
				Fnew.SetFaceBuild(bFace, b_vertices[bFace.v[0]], b_vertices[bFace.v[1]], b_vertices[bFace.v[2]]);
				faces.push_back(Fnew);
 			}

			clMsg("* Loading model: '%s' - v(%d), f(%d)", *name, b_vertices.size(), b_faces.size());
		};

		while (!MUChunk->eof())
		{
			LoadModel(*MUChunk, mu_faces[ModelID]);
			ModelID++;
		}
		MUChunk->close();
	}

	IReader* MUChunkRef = fs->open_chunk(EB_MU_refs);
	if (MUChunkRef)
	{
		while (!MUChunkRef->eof())
		{
			b_mu_reference R;
			MUChunkRef->r(&R, sizeof(R));
			
			Fmatrix xform = R.transform;		   // Transformation !
			auto& faces = mu_faces[R.model_index]; // Model Buffer by Index !
			for (auto& F : faces)
			{
				const Shader_xrLC& SH = GetShaderXRLC(F.dwMaterial, F.bSharedMaterial);
				if (!SH.flags.bLIGHT_CastShadow) continue;
 
				//   MU-Models !
				Fvector P[3];
				xform.transform_tiny(P[0], F.v1);
				xform.transform_tiny(P[1], F.v2);
				xform.transform_tiny(P[2], F.v3);

				//  !
				auto& Fnew = build_faces.emplace_back();
				Fnew.SetFace(P[0], P[1], P[2], nullptr);
				Fnew.SetMaterial(F.dwMaterial, F.dwMaterialGame, F.getTC0(), F.bSharedMaterial);
			}
		}
		MUChunkRef->close();
	}
	xrCalculateOpacity();

	// ???? ??????? BOX-QUERY ??? ??????? t_n !
	auto& CDATA = CAIRayTrace.static_geom;
 	CDATA.ClearAll();

	for (u32 ID = 0; ID < build_faces.size(); ID++)
	{
		FaceDataEmbree& F = build_faces[ID];
		CDATA.AddFaceRaw(&F, F.v1, F.v2, F.v3);
	}

	CDATA.useMsg = true;
	CDATA.RemoveDublicates();
 
 	CAIRayTrace.Initialize();
 
}


void IComputeData::xrUnload()
{
 	xr_delete(g_shaders_xrlc);

	// lights
	g_lights.clear();

	// vectors
	g_shader_compile.clear();
	g_shader_compile.shrink_to_fit();
	g_materials.clear();
	g_materials.shrink_to_fit();
	g_textures.clear();
	g_textures.shrink_to_fit();

	build_faces.clear();
	build_faces.shrink_to_fit();

 	CAIRayTrace.Deinitialize(); // ???????? !
}
