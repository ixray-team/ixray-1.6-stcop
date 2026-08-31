#include "xrCore.h"
#define SCRIPTS_API
#include "../../xrEngine/_d3d_extensions.h"
#include "../xrLC/vbm.h"
#include "FormatParsers/LevelCForm/CFormIO.h"
#include "FormatParsers/LevelGeom/GeomIO.h"
#include "ConverterUI.h"
#include "graph_abstract.h"
#include "server_entity_wrapper.h"
#include "xrServer_Object_Base.h"
#include "luabind/luabind_memory.h"
#include "Save/MemoryBuffer.h"
#include "Save/SaveManager.h"
#include "utils/xrAI/factory_api.h"

// SWIContainer g_SWI, x_SWI;
// VBContainer g_VB, x_VB;
// IBContainer g_IB, x_IB;

static LPVOID __cdecl luabind_allocator(luabind::memory_allocation_function_parameter const, void const* const pointer, size_t const size)
{
	if (!size)
	{
		LPVOID	non_const_pointer = const_cast<LPVOID>(pointer);
		xr_free(non_const_pointer);
		return	(0);
	}

	if (!pointer)
	{
		return	(Memory.mem_alloc(size));
	}

	LPVOID non_const_pointer = const_cast<LPVOID>(pointer);
	return (Memory.mem_realloc(non_const_pointer, size));
}

void setup_luabind_allocator()
{
	luabind::allocator = &luabind_allocator;
	luabind::allocator_parameter = nullptr;
}

SEFactory_Create* create_entity = nullptr;
SEFactory_Destroy* destroy_entity = nullptr;

bool UseAdvancedSerialization = false;

bool IsAdvancedSerialization()
{
	return UseAdvancedSerialization;
}

void StartupLevel()
{
	for (const auto& [Name, Selected] : CFormConverter::GetConverterSettings().Files)
	{
		if (!Selected)
			continue;

		string4096 name;
		strcpy(name, Name.data());

		if (xr_strlen(name))
			xr_strcat(name, "\\");

		//string_path prjName;
		//prjName[0] = 0;
		xr_stack_string_path prjName;
		//bool can_use_name = false;

		if (xr_strlen(name) < sizeof(string_path))
		{
			//can_use_name = true;
			FS.update_path(prjName, "$game_levels$", name);
		}

		prjName.append("level");

		if (CFormConverter::GetConverterSettings().CForm)
		{
			auto CForm = XRay::CForm::Read(prjName.c_str());

			switch (CFormConverter::GetConverterSettings().LC_CformType)
			{
				case CFormVersions::Vanilla:
				{
					xr_vector<Fvector> Verts;
					xr_vector<CDB::TRI> Tris;
					CForm->GetStaticGeom(Verts, Tris);
			
					XRay::CForm::CFormatVanilla TargetCForm;
					TargetCForm.AddStaticGeom(Verts, Tris);
					XRay::CForm::Write(prjName.c_str(), TargetCForm);
					break;
				}
				case CFormVersions::VanillaChunked:
				{
					xr_vector<Fvector> Verts;
					xr_vector<CDB::TRI> Tris;
					CForm->GetStaticGeom(Verts, Tris);

					size_t mem_bytes = Tris.size()*sizeof(CDB::TRI) + Verts.size()*sizeof(Fvector);
					u32 Number = (mem_bytes / (1024ull*1024ull)) / CFormConverter::GetConverterSettings().LC_CFormChunkSize;
					if (!Number)
					{
						XRay::CForm::CFormatVanilla TargetCForm;
						TargetCForm.AddStaticGeom(Verts, Tris);
						XRay::CForm::Write(prjName.c_str(), TargetCForm);
					} else
					{
						XRay::CForm::CFormatVanillaChunked TargetCForm(Number+1);
						TargetCForm.AddStaticGeom(Verts, Tris);
						XRay::CForm::Write(prjName.c_str(), TargetCForm);
					}
					break;
				}
				default:
				{
					FATAL("Invalid target CForm type!");
				}
			}
		}

		if (CFormConverter::GetConverterSettings().Geom)
		{
			auto func = [&](xr_string_view Ext)
			{
				auto Geom = XRay::Geom::Read(prjName, Ext);

				if (!I_ASSERT(Geom))
				{
					return;
				}
			
				xr_vector<u8> VB, IB, SWI;

				auto read_func = [&](xr_vector<u8>& Buff, IReaderBase& Data)
				{
					Buff.resize(Data.length());
					Data.r(Buff.data(), Buff.size());
				};
				if (Geom->HasVBData())
				{
					read_func(VB, Geom->GetVBData());
				}
				if (Geom->HasIBData())
				{
					read_func(IB, Geom->GetIBData());
				}
				if (Geom->HasSWIData())
				{
					read_func(SWI, Geom->GetSWIData());
				}

				Geom.reset();
			
				xr_unique_ptr<XRay::Geom::IFormat> FormatPtr = nullptr;
				switch (CFormConverter::GetConverterSettings().LC_GeomType)
				{
					case GeomVanillaType::Vanilla:
					{
						FormatPtr.reset(new XRay::Geom::CGeomVanillaFormat);
						break;
					}
					case GeomVanillaType::Chunked:
					{
						size_t mem_bytes = VB.size() + IB.size() + SWI.size();
						u32 Number = (mem_bytes/(1024ull*1024ull))/CFormConverter::GetConverterSettings().LC_GeomChunkSize;
						if (!Number)
						{
							FormatPtr.reset(new XRay::Geom::CGeomVanillaFormat);
						} else
						{
							FormatPtr.reset(new XRay::Geom::CGeomVanillaChunkedFormat(Number+1));
						}
						break;
					}
					default:
					{
						FATAL("Invalid Geom type!");
					}
				}
				IVERIFY(FormatPtr);

				FormatPtr->AddVBData(VB);
				FormatPtr->AddIBData(IB);
				FormatPtr->AddSWIData(SWI);

				Write(prjName, Ext, *FormatPtr);
			};
			func(".geom");
			func(".geomx");
		}
	}
}

void StartupSpawn()
{
	setup_luabind_allocator();
	const char* g_name = "xrSE_Factory.dll";
	Msg("Loading DLL: %s", g_name);
	auto hFactory = LoadLibraryA(g_name);

	if (!hFactory)
	{
		R_CHK(GetLastError());
		R_ASSERT2(hFactory, "Factory DLL raised exception during loading or there is no factory DLL at all");
		return;
	}
	xr_scope_exit hFactoryGuard = [&](){FreeModule(hFactory);};

	create_entity = (SEFactory_Create*)GetProcAddress(hFactory, "create_entity"); R_ASSERT(create_entity);
	destroy_entity = (SEFactory_Destroy*)GetProcAddress(hFactory, "destroy_entity");	R_ASSERT(destroy_entity);
	
	string_path file_name;
	bool file_exists = !!FS.exist(file_name, "$game_spawn$", CFormConverter::GetConverterSettings().SpawnOrig.c_str(), ".spawn");
	if (!I_ASSERT_M(file_exists, "Can't find spawn file:", CFormConverter::GetConverterSettings().SpawnOrig.c_str()))
	{
		return;
	}
	
	auto file = FS.rg_open(file_name);
	if (!I_ASSERT(file->find_chunk(SpawnFileChunks::SpawnGraphOld)))
	{
		return;
	}
	
	UseAdvancedSerialization = false;
	CMemoryWriter stream;
	auto CopyChunk = [&]<XRay::Concepts::Enum E>(E ChunkID)
	{
		stream.make_chunk(ChunkID, [&file, ChunkID](IWriter& stream)
		{
			auto OrigChunk = file->open_chunk(ChunkID);
			stream.w(OrigChunk->pointer(), OrigChunk->length());
			OrigChunk->close();
		});
	};
	CopyChunk(SpawnFileChunks::Header);
	CopyChunk(SpawnFileChunks::LevelPoints);
	CopyChunk(SpawnFileChunks::PatrolPathStorage);
	CopyChunk(SpawnFileChunks::GameGraph);
	
	
	CGraphAbstractSerialize<CServerEntityWrapper*,float,ALife::_SPAWN_ID> SpawnGraph;
	auto OldGraphData = file->open_chunk(SpawnFileChunks::SpawnGraphOld);
	IVERIFY(OldGraphData);
	load_data(SpawnGraph, *OldGraphData);
	OldGraphData->close();
	
	UseAdvancedSerialization = true;
	stream.make_chunk(SpawnFileChunks::SpawnGraphNew, [&SpawnGraph](IWriter& stream){
		save_data(SpawnGraph, stream);
	});
	/*stream.make_chunk(SpawnFileChunks::SpawnGraphNew, [&SpawnGraph](IWriter& stream){
		stream.make_chunk(GraphAbstractChunks::VerticesNum, [&SpawnGraph](IWriter& stream)
		{
			stream.w_u32(SpawnGraph.vertex_count());
		});
	
		stream.make_chunk(GraphAbstractChunks::VerticesData, [&SpawnGraph](IWriter& stream)
		{
			auto I = SpawnGraph.vertices().begin();
			auto E = SpawnGraph.vertices().end();
			SSaveTask dummy;
			for (int i=0; I != E; ++I)
			{
				stream.make_chunk(i, [&I, &dummy](IWriter& stream)
				{
					stream.make_chunk(GraphAbstractVertexChunks::ID, [&I, &dummy](IWriter& stream)
					{
						save_data(I->second->vertex_id(),stream);
					});
		
					stream.make_chunk(GraphAbstractVertexChunks::Data, [&I, &dummy](IWriter& stream)
					{
						auto& obj = I->second->data()->object();
						auto SaveObjPtr = CSaveManager::GetInstance().EditorBeginSave();
						auto& SaveObj = *SaveObjPtr;
						shared_str temp = obj.name();
						SaveObj << temp;
						obj.Spawn_Serialize(SaveObj, true);
						obj.UPDATE_Serialize(SaveObj);
						CMemoryBuffer buff;
						buff.Write(ESaveVariableType::t_chunk);
						SaveObj.Write(&buff, &dummy);
						buff.Write(&stream);
						xr_delete(SaveObjPtr);
					});
				});
			}
		});
	
		stream.make_chunk(GraphAbstractChunks::Edges, [&SpawnGraph](IWriter& stream)
		{
			for (auto& val : SpawnGraph.vertices() | std::views::values)
			{
				if (val->edges().empty())
				{
					continue;
				}
				save_data(val->vertex_id(),stream);
	
				stream.w_u32(val->edges().size());
				for (auto& Edge : val->edges())
				{
					save_data(Edge.vertex_id(),stream);
					save_data(Edge.weight(),stream);
				}
			}
		});
	});*/
	
	FS.update_path(file_name, "$game_spawn$", CFormConverter::GetConverterSettings().SpawnDest.c_str());
	xr_strcat(file_name, ".spawn");
	stream.save_to(file_name);
}

void StartupConv()
{
	SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);
	
	// Load project
	//FS.update_path(INI_FILE, "$game_config$", GAME_CONFIG);

	if (CFormConverter::GetConverterSettings().Geom || CFormConverter::GetConverterSettings().CForm)
	{
		StartupLevel();
	}
	if (CFormConverter::GetConverterSettings().Spawn)
	{
		StartupSpawn();
	}
}