#include "StdAfx.h"
#include "LevelSelfTests.h"

#include "../Sandbox/SandboxTerrain.h"
#include "../Sandbox/SandboxLevelBuilder.h"
#include "../Sandbox/EmbeddedLevelResources.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrEngine/device.h"
#include "../xrEngine/x_ray.h"
#include "../xrEngine/std_classes.h"
#include "../level_graph.h"
#include "../ParticlesObject.h"
#include "../level_path_manager.h"
#include "../Level.h"
#include "../map_location.h"
#include "../ai_object_location.h"
#include "../ai_space.h"
#include "../alife_smart_terrain_task.h"
#include "../alife_simulator.h"

extern CALifeSimulator* alife();

#include "../patrol_path.h"
#include "../patrol_point.h"
#include "../patrol_path_storage.h"
#include "../cover_manager.h"
#include "../smart_cover_object.h"
#include "../xrCore/Collision/xr_area.h"
#include "../xrCore/Collision/ISpatial.h"
#include "../xrCore/FormatParsers/LevelCForm/CFormIO.h"
#include "../xrCore/OffsetAllocator.h"
#include "../xrCore/TaskPool.h"
#include "../xrSound/Sound.h"
#include "../xrSound/New/SoundMixer.h"
#include "../xrEngine/xr_object.h"
#include "../xrEngine/Fmesh.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "../ImUtils/Viewer/ViewerGpuPool.h"
#include "../ImUtils/Viewer/ViewerSession.h"
#include "../ImUtils/Viewer/ViewerViewport.h"
#include "../ImUtils/Viewer/ViewerEditor.h"
#include "../xrNetServer/BaseClient.h"
#include "../xrScripts/script_engine.h"
#include "../script_binder.h"
#include "../saved_game_wrapper.h"
#include "../entity_alive.h"
#include "../xrNetServer/BaseServer.h"
#include "../xrServerEntities/xrServer_Objects_ALife_All.h"
#include "../xrServerEntities/object_factory.h"
#include "../xrServerEntities/object_item_script.h"
#include "../ui/UIGameTutorial.h"
#include "../RocketLauncher.h"
#include "../space_restriction_manager.h"
#include "../InventoryOwner.h"

#include <filesystem>
#include <cmath>

extern ENGINE_API ref_light precache_light;

// ---------------------------------------------------------------------------
// framework
// ---------------------------------------------------------------------------
namespace SelfTest
{

struct STestRecord
{
	const char*	Name;
	TestFn		Fn;
	bool		NeedsDeviceReady;
};

// Function-local static registry: no globals, and by xrGame DLL load time
// xrCore (memory, FS) is fully initialized.
static xr_vector<STestRecord>& TestRegistry()
{
	static xr_vector<STestRecord> Registry;
	return Registry;
}

void Register(const char* name, TestFn fn, bool needsDeviceReady)
{
	TestRegistry().push_back({ name, fn, needsDeviceReady });
}

struct SAutoRegister
{
	SAutoRegister(const char* name, TestFn fn, bool needsDeviceReady)
	{
		Register(name, fn, needsDeviceReady);
	}
};

int RunAll(bool includeDeviceTests, const char* onlyName)
{
	int Ran = 0;
	int Failed = 0;
	for (const STestRecord& Test : TestRegistry())
	{
		if (onlyName != nullptr && onlyName[0] != 0 && 0 != xr_strcmp(Test.Name, onlyName))
			continue;
		if (Test.NeedsDeviceReady && !includeDeviceTests)
			continue;

		++Ran;
		Msg("* [selftest] begin %s", Test.Name);
		xrLogger::FlushLog();
		const bool Ok = Test.Fn();
		Msg("* [selftest] %-24s: %s", Test.Name, Ok ? "pass" : "FAIL");
		xrLogger::FlushLog();
		if (!Ok)
			++Failed;
	}
	Msg("* [selftest] %d ran, %d failed, %d skipped", Ran, Failed, int(TestRegistry().size()) - Ran);
	return Failed;
}

bool StartupFlagPresent()
{
	return strstr(Core.Params, "-level_selftest") != nullptr;
}

void RunStartupTests()
{
	if (!StartupFlagPresent())
		return;

	Msg("* [selftest] -level_selftest on the command line, running startup tests");
	int Failed = 0;
	if (!Device.b_is_Ready)
	{
		constexpr const char* BootTests[] =
		{
			"mask_string_roundtrip", "desc_validate_rules", "terrain_meshes", "heightmap_pgm", "cform_roundtrip",
			"aimap_roundtrip", "aimap_out_of_box_clamps", "offset_allocator", "viewer_history_stream", "viewer_cpu_pool",
			"tutorial_render_ordering", "loading_screen_render_ordering", "network_processor_registration", "parallel_job_removal", "viewport_resize_crop",
		};
		for (const char* name : BootTests)
			Failed += RunAll(false, name);
	}
	else
	{
		Failed = RunAll(true);
	}
	if (Failed)
		Msg("! [selftest] startup: %d test(s) FAILED", Failed);
	else
		Msg("* [selftest] startup: all tests passed");
}

} // namespace SelfTest

#define SELFTEST_CHECK(condition, ...) \
	do \
	{ \
		if (!(condition)) \
		{ \
			Msg("! [selftest] %s(%d): check failed: %s", __FILE__, __LINE__, #condition); \
			Msg("! [selftest] " __VA_ARGS__); \
			return false; \
		} \
	} while (0)

#define SELFTEST(TestName, NeedsDeviceReady) \
	static bool SelfTestFn_##TestName(); \
	static const SelfTest::SAutoRegister SelfTestReg_##TestName(#TestName, &SelfTestFn_##TestName, NeedsDeviceReady); \
	static bool SelfTestFn_##TestName()

// ---------------------------------------------------------------------------
// shared helpers
// ---------------------------------------------------------------------------
namespace SelfTestDetail
{

static bool CheckFileExists(const char* levelDir, const char* fileName, bool expected)
{
	const xr_string Path = xr_string(levelDir) + "\\" + fileName;
	const bool Exists = std::filesystem::exists(Path.c_str());
	if (Exists != expected)
	{
		Msg("! [selftest] file '%s' was %s but %s was expected", Path.c_str(), Exists ? "present" : "missing", expected ? "present" : "missing");
		return false;
	}
	return true;
}

static xr_unique_ptr<CDB::MODEL> CreateSoundGeometryModel()
{
	CMemoryWriter stream;
	stream.open_chunk(0);
	stream.w_u32(0);
	stream.close_chunk();
	stream.open_chunk(1);
	struct SSomRecord
	{
		Fvector3 V1;
		Fvector3 V2;
		Fvector3 V3;
		u32 TwoSided;
		float Occ;
	};
	SSomRecord records[2] = {};
	records[0].V1.set(0.f, 0.f, 0.f);
	records[0].V2.set(0.f, 0.f, 1.f);
	records[0].V3.set(1.f, 0.f, 0.f);
	records[1].V1.set(1.f, 0.f, 0.f);
	records[1].V2.set(0.f, 0.f, 1.f);
	records[1].V3.set(1.f, 0.f, 1.f);
	for (SSomRecord& record : records)
	{
		record.TwoSided = 1;
		record.Occ = 0.65f;
	}
	stream.w(records, sizeof(records));
	stream.close_chunk();
	IReader reader(stream.pointer(), stream.size());
	return xr_unique_ptr<CDB::MODEL>(Sound->create_geometry_som(&reader));
}

} // namespace SelfTestDetail

// ---------------------------------------------------------------------------
// 1. resource mask string round trip
// ---------------------------------------------------------------------------
SELFTEST(mask_string_roundtrip, false)
{
	const ELevelResource Bits[] =
	{
		ELevelResource::LevelConfig, ELevelResource::RenderScene, ELevelResource::Collision, ELevelResource::AiMap,
		ELevelResource::GameGraph, ELevelResource::CrossTable, ELevelResource::PatrolPaths, ELevelResource::Spawn,
		ELevelResource::StaticParticles, ELevelResource::StaticSounds, ELevelResource::SoundEnv, ELevelResource::Ambience,
		ELevelResource::EnvModifiers, ELevelResource::PhysicsWorld, ELevelResource::Hud, ELevelResource::LevelScripts,
	};

	for (const ELevelResource Bit : Bits)
	{
		Flags32 Mask;
		Mask.zero();
		Mask.set(u32(Bit), true);
		const xr_string Text = LevelResourceMaskToString(Mask);
		Flags32 Parsed;
		SELFTEST_CHECK(LevelResourceMaskFromString(Text.c_str(), Parsed), "ToString output '%s' must parse", Text.c_str());
		SELFTEST_CHECK(Parsed.flags == Mask.flags, "single-bit round trip mismatch for '%s'", Text.c_str());
	}

	// all
	{
		Flags32 Mask;
		Mask.assign(u32(ELevelResource::All));
		const xr_string Text = LevelResourceMaskToString(Mask);
		SELFTEST_CHECK(Text == "all", "all mask must stringify to 'all', got '%s'", Text.c_str());
		Flags32 Parsed;
		SELFTEST_CHECK(LevelResourceMaskFromString(Text.c_str(), Parsed) && Parsed.flags == Mask.flags, "'all' round trip failed");
	}

	// none
	{
		Flags32 Mask;
		Mask.zero();
		const xr_string Text = LevelResourceMaskToString(Mask);
		SELFTEST_CHECK(Text == "none", "empty mask must stringify to 'none', got '%s'", Text.c_str());
		Flags32 Parsed;
		SELFTEST_CHECK(LevelResourceMaskFromString(Text.c_str(), Parsed) && Parsed.flags == 0, "'none' round trip failed");
	}

	// several bits, whitespace and case tolerance
	{
		Flags32 Mask;
		Mask.zero();
		Mask.set(u32(ELevelResource::Collision) | u32(ELevelResource::AiMap) | u32(ELevelResource::PatrolPaths), true);
		const xr_string Text = LevelResourceMaskToString(Mask);
		SELFTEST_CHECK(Text == "collision,ai_map,patrol_paths", "unexpected combo string '%s'", Text.c_str());
		Flags32 Parsed;
		SELFTEST_CHECK(LevelResourceMaskFromString(Text.c_str(), Parsed) && Parsed.flags == Mask.flags, "combo round trip failed");
		SELFTEST_CHECK(LevelResourceMaskFromString(" Patrol_Paths ,COLLISION", Parsed), "parser must trim and be case-insensitive");
		SELFTEST_CHECK(Parsed.flags == (u32(ELevelResource::Collision) | u32(ELevelResource::PatrolPaths)), "trim/case parse mismatch");
	}

	// unknown token must fail
	{
		Msg("* [selftest] the following 'unknown resource' log line is expected");
		Flags32 Parsed;
		SELFTEST_CHECK(!LevelResourceMaskFromString("collision,not_a_resource", Parsed), "unknown token must fail the parse");
	}
	return true;
}

// ---------------------------------------------------------------------------
// 2. SLevelDesc::Validate rules
// ---------------------------------------------------------------------------
SELFTEST(desc_validate_rules, false)
{
	// CrossTable without GameGraph is stripped.
	{
		SLevelDesc Desc;
		Desc.Resources.zero();
		Desc.Resources.set(u32(ELevelResource::CrossTable), true);
		const Flags32 Validated = Desc.Validate();
		SELFTEST_CHECK(!Validated.test(u32(ELevelResource::CrossTable)), "CrossTable without GameGraph must be cleared");
		SELFTEST_CHECK(Validated.flags == 0, "expected an empty mask, got 0x%x", Validated.flags);
	}

	// CrossTable with GameGraph stays.
	{
		SLevelDesc Desc;
		Desc.Resources.zero();
		Desc.Resources.set(u32(ELevelResource::CrossTable) | u32(ELevelResource::GameGraph), true);
		const Flags32 Validated = Desc.Validate();
		SELFTEST_CHECK(Validated.flags == Desc.Resources.flags, "CrossTable+GameGraph must survive validation");
	}

	// The full mask stays unchanged.
	{
		SLevelDesc Desc;
		const Flags32 Validated = Desc.Validate();
		SELFTEST_CHECK(Validated.flags == u32(ELevelResource::All), "full mask must stay unchanged");
	}
	return true;
}

// ---------------------------------------------------------------------------
// 3. terrain meshes
// ---------------------------------------------------------------------------
namespace SelfTestDetail
{

static bool CheckTerrainMesh(const Sandbox::STerrainParams& params, const char* what)
{
	Sandbox::STerrainMesh Mesh = Sandbox::BuildTerrainMesh(params);
	const int Cells = std::max(1, iFloor(params.Size / params.Cell + 0.5f));
	const u32 ExpectedVerts = u32((Cells + 1) * (Cells + 1));
	const u32 ExpectedTris = u32(Cells * Cells * 2);
	SELFTEST_CHECK(Mesh.Verts.size() == ExpectedVerts, "%s: expected %u verts, got %u", what, ExpectedVerts, u32(Mesh.Verts.size()));
	SELFTEST_CHECK(Mesh.Tris.size() == ExpectedTris, "%s: expected %u tris, got %u", what, ExpectedTris, u32(Mesh.Tris.size()));

	const float Half = float(Cells) * params.Cell * 0.5f;
	SELFTEST_CHECK(std::fabs(Mesh.Bounds.min.x + Half) < 1e-4f && std::fabs(Mesh.Bounds.max.x - Half) < 1e-4f, "%s: X bounds mismatch", what);
	SELFTEST_CHECK(std::fabs(Mesh.Bounds.min.z + Half) < 1e-4f && std::fabs(Mesh.Bounds.max.z - Half) < 1e-4f, "%s: Z bounds mismatch", what);

	for (const Fvector& Vertex : Mesh.Verts)
	{
		const float Sampled = Sandbox::SampleHeight(params, Vertex.x, Vertex.z);
		SELFTEST_CHECK(std::fabs(Vertex.y - Sampled) < 1e-4f, "%s: mesh vertex y %f != SampleHeight %f at (%.2f, %.2f)", what, Vertex.y, Sampled, Vertex.x, Vertex.z);
	}
	for (const CDB::TRI& Tri : Mesh.Tris)
	{
		SELFTEST_CHECK(Tri.verts[0] < ExpectedVerts && Tri.verts[1] < ExpectedVerts && Tri.verts[2] < ExpectedVerts, "%s: triangle index out of range", what);
		SELFTEST_CHECK(Tri.material == 0 && Tri.suppress_shadows == 0 && Tri.suppress_wm == 0, "%s: triangle flags must be zeroed", what);
	}
	return true;
}

} // namespace SelfTestDetail

SELFTEST(terrain_meshes, false)
{
	Sandbox::STerrainParams Params;
	Params.Size = 40.f;
	Params.Cell = 2.f;

	Params.Kind = Sandbox::ETerrainKind::Flat;
	if (!SelfTestDetail::CheckTerrainMesh(Params, "flat"))
		return false;

	Params.Kind = Sandbox::ETerrainKind::Analytic;
	for (u32 Preset = 0; Preset < 5; ++Preset)
	{
		Params.Analytic = Sandbox::EAnalyticPreset(Preset);
		if (!SelfTestDetail::CheckTerrainMesh(Params, Sandbox::AnalyticPresetName(Params.Analytic)))
			return false;
	}
	return true;
}

// ---------------------------------------------------------------------------
// 4. heightmap PGM loading
// ---------------------------------------------------------------------------
SELFTEST(heightmap_pgm, false)
{
	// P2 ascii with a comment line.
	string_path PathP2;
	FS.update_path(PathP2, _app_data_root_, "_selftest\\test_ascii.pgm");
	{
		IWriter* Writer = FS.w_open(PathP2);
		SELFTEST_CHECK(Writer != nullptr && Writer->valid(), "cannot write '%s'", PathP2);
		Writer->w_printf("P2\n# comment\n4 3\n255\n");
		for (u32 I = 0; I < 12; ++I)
			Writer->w_printf("%u ", I);
		FS.w_close(Writer);
	}

	xr_vector<float> Heights;
	u32 Width = 0, Height = 0;
	SELFTEST_CHECK(Sandbox::LoadHeightmap(PathP2, Heights, Width, Height), "P2 load failed");
	SELFTEST_CHECK(Width == 4 && Height == 3, "P2 dimensions mismatch: %ux%u", Width, Height);
	SELFTEST_CHECK(Heights.size() == 12, "P2 value count mismatch: %u", u32(Heights.size()));
	for (u32 I = 0; I < 12; ++I)
		SELFTEST_CHECK(std::fabs(Heights[I] - float(I) / 255.f) < 1e-6f, "P2 value %u mismatch", I);

	// P5 binary, 16-bit big-endian samples.
	string_path PathP5;
	FS.update_path(PathP5, _app_data_root_, "_selftest\\test_binary.pgm");
	{
		IWriter* Writer = FS.w_open(PathP5);
		SELFTEST_CHECK(Writer != nullptr && Writer->valid(), "cannot write '%s'", PathP5);
		Writer->w_printf("P5\n2 2\n65535\n");
		const u16 Samples[4] = { 0, 32768, 65535, 1000 };
		for (u32 I = 0; I < 4; ++I)
		{
			Writer->w_u8(u8(Samples[I] >> 8));
			Writer->w_u8(u8(Samples[I] & 0xff));
		}
		FS.w_close(Writer);
	}
	SELFTEST_CHECK(Sandbox::LoadHeightmap(PathP5, Heights, Width, Height), "P5 load failed");
	SELFTEST_CHECK(Width == 2 && Height == 2, "P5 dimensions mismatch: %ux%u", Width, Height);
	SELFTEST_CHECK(std::fabs(Heights[0] - 0.f) < 1e-6f, "P5 sample 0 mismatch");
	SELFTEST_CHECK(std::fabs(Heights[1] - 32768.f / 65535.f) < 1e-6f, "P5 sample 1 mismatch");
	SELFTEST_CHECK(std::fabs(Heights[2] - 1.f) < 1e-6f, "P5 sample 2 mismatch");
	SELFTEST_CHECK(std::fabs(Heights[3] - 1000.f / 65535.f) < 1e-6f, "P5 sample 3 mismatch");

	// Missing file must fail cleanly.
	string_path PathMissing;
	FS.update_path(PathMissing, _app_data_root_, "_selftest\\definitely_missing.pgm");
	SELFTEST_CHECK(!Sandbox::LoadHeightmap(PathMissing, Heights, Width, Height), "missing file must fail");

	FS.file_delete(PathP2);
	FS.file_delete(PathP5);
	return true;
}

// ---------------------------------------------------------------------------
// 5. cform round trip + CDB ray query
// ---------------------------------------------------------------------------
SELFTEST(cform_roundtrip, false)
{
	Sandbox::STerrainParams Terrain;
	Terrain.Kind = Sandbox::ETerrainKind::Flat;
	Terrain.Size = 20.f;
	Terrain.Cell = 2.f;

	Sandbox::STerrainMesh Mesh = Sandbox::BuildTerrainMesh(Terrain);
	SELFTEST_CHECK(!Mesh.Verts.empty() && !Mesh.Tris.empty(), "terrain mesh must not be empty");

	XRay::CForm::CFormatVanilla Format;
	Format.AddStaticGeom(Mesh.Verts, Mesh.Tris);
	// XRay::CForm::Write appends ".cform" to the file name itself.
	XRay::CForm::Write(_app_data_root_, "_selftest\\test_cform", Format);

	string_path CformPath;
	FS.update_path(CformPath, _app_data_root_, "_selftest\\test_cform.cform");
	// Register the file in the VFS so the FS-based reader finds it in this frame.
	FS.file_update(CformPath);

	xr_unique_ptr<XRay::CForm::IFormat> Parsed = XRay::CForm::Read(_app_data_root_, "_selftest\\test_cform");
	SELFTEST_CHECK(Parsed != nullptr, "CForm read-back failed");

	xr_vector<Fvector> Verts;
	xr_vector<CDB::TRI> Tris;
	Parsed->GetStaticGeom(Verts, Tris);
	SELFTEST_CHECK(Verts.size() == Mesh.Verts.size(), "vert count mismatch: %u != %u", u32(Verts.size()), u32(Mesh.Verts.size()));
	SELFTEST_CHECK(Tris.size() == Mesh.Tris.size(), "tri count mismatch: %u != %u", u32(Tris.size()), u32(Mesh.Tris.size()));
	SELFTEST_CHECK(Verts[0].similar(Mesh.Verts[0]) && Verts.back().similar(Mesh.Verts.back()), "corner verts mismatch");

	// CDB ray query straight down at a known (x, z) must hit at the terrain height.
	CDB::MODEL Model;
	Model.build(Verts.data(), Verts.size(), Tris.data(), Tris.size(), nullptr, nullptr, nullptr, false, false);

	CDB::COLLIDER Collider;
	Collider.ray_options(CDB::OPT_ONLYNEAREST);
	const Fvector Start = Fvector().set(3.f, 10.f, 4.f);
	const Fvector Dir = Fvector().set(0.f, -1.f, 0.f);
	Collider.ray_query(Model, Start, Dir, 100.f);
	SELFTEST_CHECK(Collider.r_count() > 0, "ray must hit the terrain");
	const float HitY = Start.y - Collider.r_any().range;
	SELFTEST_CHECK(std::fabs(HitY - Sandbox::SampleHeight(Terrain, Start.x, Start.z)) < 1e-3f, "hit y %f != terrain height", HitY);

	FS.file_delete(CformPath);
	return true;
}

SELFTEST(collision_swap, true)
{
	Sandbox::STerrainParams terrain;
	terrain.Kind = Sandbox::ETerrainKind::Flat;
	terrain.Size = 20.f;
	terrain.Cell = 2.f;
	Sandbox::STerrainMesh mesh = Sandbox::BuildTerrainMesh(terrain);
	SELFTEST_CHECK(!mesh.Verts.empty() && !mesh.Tris.empty(), "terrain must be nonempty");

	XRay::CForm::CFormatVanilla format;
	format.AddStaticGeom(mesh.Verts, mesh.Tris);
	CObjectSpace candidate;
	SELFTEST_CHECK(candidate.Create(format, nullptr, nullptr, false, false), "candidate collision must build");

	CObjectSpace live;
	live.SwapStaticGeometry(candidate);
	const Fvector start = Fvector().set(3.f, 10.f, 4.f);
	const Fvector direction = Fvector().set(0.f, -1.f, 0.f);
	collide::ray_cache cache;
	SELFTEST_CHECK(live.RayTest(start, direction, 100.f, collide::rqtStatic, &cache, nullptr), "swapped collision must hit the terrain");
	live.ClearStaticGeometry();
	SELFTEST_CHECK(!live.RayTest(start, direction, 100.f, collide::rqtStatic, nullptr, nullptr), "cleared collision must not hit");
	SELFTEST_CHECK(!live.RayTest(start, direction, 100.f, collide::rqtStatic, &cache, nullptr), "cleared collision must not reuse a cached hit");

	CObjectSpace restored;
	SELFTEST_CHECK(restored.Create(format, nullptr, nullptr, false, false), "restored collision must build");
	live.SwapStaticGeometry(restored);
	SELFTEST_CHECK(live.RayTest(start, direction, 100.f, collide::rqtStatic, nullptr, nullptr), "restored collision must hit again");
	CObjectSpace replacement;
	SELFTEST_CHECK(replacement.Create(format, nullptr, nullptr, false, false), "replacement collision must build");
	live.SwapStaticGeometry(replacement);
	SELFTEST_CHECK(live.RayTest(start, direction, 100.f, collide::rqtStatic, nullptr, nullptr), "populated swap destination must hit");
	SELFTEST_CHECK(replacement.RayTest(start, direction, 100.f, collide::rqtStatic, nullptr, nullptr), "populated swap source must hit");
	return true;
}

// ---------------------------------------------------------------------------
// 6. level.ai write + CLevelGraph read round trip
// ---------------------------------------------------------------------------
SELFTEST(aimap_roundtrip, false)
{
	Sandbox::STerrainParams Terrain;
	Terrain.Kind = Sandbox::ETerrainKind::Flat;
	Terrain.Size = 14.f;	// 7 cells
	Terrain.Cell = 2.f;		// -> 8x8 nodes over [-7, +7]

	string_path AiPath;
	FS.update_path(AiPath, _app_data_root_, "_selftest\\level.ai");
	SELFTEST_CHECK(Sandbox::WriteLevelAi(Terrain, AiPath), "WriteLevelAi failed");
	// Register the file in the VFS so FS.r_open (used by CLevelGraph) finds it.
	FS.file_update(AiPath);

	CLevelGraph Graph(AiPath);
	SELFTEST_CHECK(Graph.has_nodes(), "graph must have nodes");
	SELFTEST_CHECK(Graph.header().vertex_count() == 64, "expected 64 nodes, got %u", Graph.header().vertex_count());
	SELFTEST_CHECK(Graph.valid_vertex_id(0) && Graph.valid_vertex_id(63), "edge ids must be valid");
	SELFTEST_CHECK(!Graph.valid_vertex_id(64), "id 64 must be invalid");

	for (u32 X = 0; X < 8; ++X)
	{
		for (u32 Z = 0; Z < 8; ++Z)
		{
			const u32 Id = X * 8 + Z;
			const Fvector Position = Graph.vertex_position(Id);
			const float ExpectedX = -7.f + 2.f * float(X);
			const float ExpectedZ = -7.f + 2.f * float(Z);
			SELFTEST_CHECK(std::fabs(Position.x - ExpectedX) < 1.f && std::fabs(Position.z - ExpectedZ) < 1.f,
				"node %u position (%.2f, %.2f) != expected (%.2f, %.2f)", Id, Position.x, Position.z, ExpectedX, ExpectedZ);
			SELFTEST_CHECK(std::fabs(Position.y) < 0.01f, "node %u must sit on the flat terrain", Id);
			// Plane round trip: vertex_plane_y at the node center equals the terrain height.
			SELFTEST_CHECK(std::fabs(Graph.vertex_plane_y(Id)) < 0.01f, "node %u plane y mismatch", Id);
		}
	}

	// Interior nodes: 4 distinct neighbor links in the -X/+Z/+X/-Z convention.
	for (u32 X = 1; X < 7; ++X)
	{
		for (u32 Z = 1; Z < 7; ++Z)
		{
			const u32 Id = X * 8 + Z;
			const ILevelGraph::CVertex* Vertex = Graph.vertex(Id);
			SELFTEST_CHECK(Vertex->link(0) == Id - 8, "node %u link0 (-X) must be %u", Id, Id - 8);
			SELFTEST_CHECK(Vertex->link(1) == Id + 1, "node %u link1 (+Z) must be %u", Id, Id + 1);
			SELFTEST_CHECK(Vertex->link(2) == Id + 8, "node %u link2 (+X) must be %u", Id, Id + 8);
			SELFTEST_CHECK(Vertex->link(3) == Id - 1, "node %u link3 (-Z) must be %u", Id, Id - 1);
		}
	}

	// Corner node (0, 0): the -X/-Z links must be the no-link sentinel.
	const ILevelGraph::CVertex* Corner = Graph.vertex(u32(0));
	SELFTEST_CHECK(!Graph.valid_vertex_id(Corner->link(0)), "corner link0 must be invalid");
	SELFTEST_CHECK(Corner->link(1) == 1, "corner link1 must be node 1");
	SELFTEST_CHECK(Corner->link(2) == 8, "corner link2 must be node 8");
	SELFTEST_CHECK(!Graph.valid_vertex_id(Corner->link(3)), "corner link3 must be invalid");

	FS.file_delete(AiPath);
	return true;
}

// ---------------------------------------------------------------------------
// 7b. out-of-box positions clamp into the AI map instead of asserting
// ---------------------------------------------------------------------------
SELFTEST(aimap_out_of_box_clamps, false)
{
	Sandbox::STerrainParams Terrain;
	Terrain.Kind = Sandbox::ETerrainKind::Flat;
	Terrain.Size = 14.f;	// 7 cells
	Terrain.Cell = 2.f;		// -> 8x8 nodes over [-7, +7]

	string_path AiPath;
	FS.update_path(AiPath, _app_data_root_, "_selftest\\level_oob.ai");
	SELFTEST_CHECK(Sandbox::WriteLevelAi(Terrain, AiPath), "WriteLevelAi failed");
	FS.file_update(AiPath);

	CLevelGraph Graph(AiPath);
	SELFTEST_CHECK(Graph.has_nodes(), "graph must have nodes");
	// A position far outside the box clamps to the nearest edge, no assertion.
	ILevelGraph::CPosition Packed;
	Graph.vertex_position(Packed, Fvector().set(1000.f, 0.f, 1000.f));
	const Fvector Clamped = Graph.vertex_position(Packed);
	SELFTEST_CHECK(Clamped.x >= -8.f && Clamped.x <= 8.f && Clamped.z >= -8.f && Clamped.z <= 8.f,
		"out-of-box position must clamp into the box, got (%.2f, %.2f)", Clamped.x, Clamped.z);

	FS.file_delete(AiPath);
	return true;
}

// ---------------------------------------------------------------------------
// 7c. invalid path destinations recover through the object's nearest node
// ---------------------------------------------------------------------------
SELFTEST(nearest_vertex_recovery, false)
{
	Sandbox::STerrainParams Terrain;
	Terrain.Kind = Sandbox::ETerrainKind::Flat;
	Terrain.Size = 14.f;
	Terrain.Cell = 2.f;
	string_path AiPath;
	FS.update_path(AiPath, _app_data_root_, "_selftest\\movement_recovery.ai");
	SELFTEST_CHECK(Sandbox::WriteLevelAi(Terrain, AiPath), "WriteLevelAi failed");
	FS.file_update(AiPath);
	CLevelGraph Graph(AiPath);
	SELFTEST_CHECK(Graph.has_nodes(), "the recovery graph must have nodes");
	const u32 Recovered = Graph.FindNearestVertex(Fvector().set(10000.f, 0.f, 10000.f));
	SELFTEST_CHECK(Graph.valid_vertex_id(Recovered), "an off-map NPC must recover a valid nearest vertex");
	SELFTEST_CHECK(Recovered == Graph.header().vertex_count() - 1, "nearest recovery must select the NPC's closest corner instead of a hardcoded vertex");
	SELFTEST_CHECK(_valid(Graph.vertex_position(Recovered)), "the recovered destination must have a finite position");
	SELFTEST_CHECK(!Graph.valid_vertex_id(u32(-1)), "an unset destination must remain distinguishable from a real vertex");
	FS.file_delete(AiPath);
	return true;
}

// ---------------------------------------------------------------------------
// 7d. missing level.ai degrades to an empty graph
// ---------------------------------------------------------------------------
SELFTEST(aimap_missing_degrades, false)
{
	string_path AiPath;
	FS.update_path(AiPath, _app_data_root_, "_selftest\\definitely_missing.ai");

	CLevelGraph Graph(AiPath);
	SELFTEST_CHECK(!Graph.has_nodes(), "missing AI map must produce an empty graph");
	SELFTEST_CHECK(Graph.header().vertex_count() == 0, "empty graph must have zero vertices");
	SELFTEST_CHECK(!Graph.valid_vertex_id(0), "no vertex id may be valid in an empty graph");
	SELFTEST_CHECK(_valid(Graph.vertex_position(1234)), "an old vertex id must have a finite empty-graph position");
	SELFTEST_CHECK(_valid(Graph.vertex_position(*Graph.vertex(0u))), "the empty vertex must not divide by a zero row length");
	SELFTEST_CHECK(Graph.SearchNearestVertex(1234, Fvector().set(1.f, 0.f, 1.f), 30.f) == u32(-1), "nearest-node search must return no vertex without an AI map");
	SELFTEST_CHECK(Graph.vertex_id(Fvector().set(1.f, 0.f, 1.f)) == u32(-1), "position lookup must return no vertex without an AI map");
	SELFTEST_CHECK(Graph.vertex_id(Graph.vertex(0u)) == u32(-1), "the empty vertex must not produce a valid ID");
	SELFTEST_CHECK(Graph.check_position_in_direction(1234, Fvector().set(0.f, 0.f, 0.f), Fvector().set(1.f, 0.f, 1.f)) == u32(-1), "direction checks must return no vertex without an AI map");
	SELFTEST_CHECK(!Graph.inside(1234, Fvector().set(0.f, 0.f, 0.f)), "invalid IDs must not be treated as graph membership");

	CAI_Space First;
	CAI_Space Second;
	First.init(false);
	Second.init(false);
	SELFTEST_CHECK(&First.level_graph() != &Second.level_graph(), "fallback level graphs must belong to their AI spaces");
	SELFTEST_CHECK(&First.game_graph() != &Second.game_graph(), "fallback game graphs must belong to their AI spaces");
	SELFTEST_CHECK(_valid(First.level_graph().vertex_position(1234)), "fallback graph queries must degrade safely");
	return true;
}

// ---------------------------------------------------------------------------
// 7d. detached path graphs retain a pending destination without asserting
// ---------------------------------------------------------------------------
SELFTEST(path_without_graph, false)
{
	using GraphPath = CAbstractPathManager<ILevelGraph, SBaseParameters<float, u32, u32>, u32, u32>;
	using PathManager = CBasePathManager<ILevelGraph, SBaseParameters<float, u32, u32>, u32, u32>;
	class CDetachedPathManager final : public PathManager
	{
	public:
		using PathManager::PathManager;
		void Build(u32 StartVertex, u32 DestinationVertex) { GraphPath::build_path(StartVertex, DestinationVertex); }
	};
	CDetachedPathManager Path(nullptr);
	Path.reinit(nullptr);
	Path.set_dest_vertex(42);
	SELFTEST_CHECK(Path.dest_vertex_id() == 42, "the requested destination must be retained for a later graph binding");
	SELFTEST_CHECK(Path.failed(), "an unattached path must report failure instead of dereferencing a null graph");
	SELFTEST_CHECK(!Path.GraphPath::actual(0, 42), "an unattached path must not report itself as current");
	Path.Build(0, 42);
	SELFTEST_CHECK(Path.failed(), "building without a graph must fail safely");
	SELFTEST_CHECK(Path.path().empty(), "a failed path without a graph must not invent vertices");
	return true;
}

// ---------------------------------------------------------------------------
// 7e. independent transport settings and session ownership
// ---------------------------------------------------------------------------
SELFTEST(session_ownership, false)
{
	CTimer_paused* timer = Device.GetTimerGlobal();
	BaseClient directClient(timer);
	BaseClient remoteClient(timer);
	directClient.SetDirectConnection(true);
	remoteClient.SetDirectConnection(false);
	SELFTEST_CHECK(directClient.UseDirectConnection() && !remoteClient.UseDirectConnection(), "client transport modes must be independent");

	BaseServer directServer(timer, false);
	BaseServer remoteServer(timer, false);
	directServer.SetDirectConnection(true);
	remoteServer.SetDirectConnection(false);
	SELFTEST_CHECK(directServer.UseDirectConnection() && !remoteServer.UseDirectConnection(), "server transport modes must be independent");

	ImUtils::Viewer::CViewerSessionManager manager;
	SELFTEST_CHECK(manager.Sessions[0].Index() == 0 && manager.Sessions[1].Index() == 1, "viewer session slots must have stable identities");
	SELFTEST_CHECK(manager.Sessions[0].GetLevel() == nullptr && manager.Sessions[1].GetLevel() == nullptr, "a new manager must not create a level or process globals");
	manager.SelectSession(1);
	SELFTEST_CHECK(&manager.SelectSession() == &manager.Sessions[1], "no-argument commands must preserve the selected client session");
	manager.SelectSession(0);
	SELFTEST_CHECK(&manager.SelectSession() == &manager.Sessions[0], "no-argument commands must preserve the selected server session");
	CCoverManager covers;
	SELFTEST_CHECK(covers.smart_cover("missing_cover") == nullptr, "a missing cover must not dereference the registry end");

	CAI_Space space;
	space.init(false);
	CMemoryWriter invalid;
	invalid.w_u8(0);
	IReader invalidReader(invalid.pointer(), invalid.size());
	SELFTEST_CHECK(!space.LoadGameGraph(invalidReader), "invalid graph data must be rejected without an assertion");
	space.SetGameGraphEnabled(false);
	SELFTEST_CHECK(space.get_game_graph() == nullptr, "disabling a graph must remove it from movement queries");
	SELFTEST_CHECK(space.get_cross_table() == nullptr, "disabling a graph must detach its cross table");
	SELFTEST_CHECK(space.game_graph().header().vertex_count() == 0, "the fallback game graph must be empty and per-instance");
	SELFTEST_CHECK(!space.game_graph().accessible(0), "an absent graph must not be accessible");
	SELFTEST_CHECK(space.game_graph().vertex(0) == nullptr, "an absent graph must not return a vertex");
	IGameGraph::const_iterator begin = nullptr;
	IGameGraph::const_iterator end = nullptr;
	space.game_graph().begin(0, begin, end);
	SELFTEST_CHECK(begin == nullptr && end == nullptr, "an absent graph must return an empty edge range");
	return true;
}

SELFTEST(smart_cover_lifetime, true)
{
	CAI_Space Space;
	Space.init(false);
	struct SContextRestore
	{
		CAI_Space* Ai;
		CScriptEngine* Script;
		~SContextRestore()
		{
			g_ai_space = Ai;
			g_pScriptEngine = Script;
		}
	} Context{ g_ai_space, g_pScriptEngine };
	g_ai_space = &Space;
	g_pScriptEngine = Space.get_script_engine();
	SELFTEST_CHECK(g_pScriptEngine != nullptr, "the stock script engine must provide smart-cover descriptions");

	CCoverManager ObjectFirstRegistry;
	ObjectFirstRegistry.compute_static_cover();
	{
		smart_cover::object Object;
		luabind::object NoLoopholes;
		const smart_cover::cover* Cover = ObjectFirstRegistry.add_smart_cover("animpoint_stay_wall", Object, true, true, NoLoopholes);
		SELFTEST_CHECK(Cover != nullptr && Object.HasCover(), "the stock cover must bind its owning object without an AI-map");
		SELFTEST_CHECK(ObjectFirstRegistry.SmartCoverCount() == 1, "the registry must own exactly one cover");
		ObjectFirstRegistry.clear(true);
		SELFTEST_CHECK(Object.HasCover() && ObjectFirstRegistry.SmartCoverCount() == 1, "an AI-map unload must retain the live cover");
		ObjectFirstRegistry.compute_static_cover();
		SELFTEST_CHECK(Object.HasCover() && ObjectFirstRegistry.SmartCoverCount() == 1, "an AI-map reload must rebind without duplicating the cover");
	}
	SELFTEST_CHECK(ObjectFirstRegistry.SmartCoverCount() == 0, "object destruction must unregister its cover");
	ObjectFirstRegistry.clear(false);

	smart_cover::object RegistryFirstObject;
	CCoverManager RegistryFirst;
	RegistryFirst.compute_static_cover();
	{
		luabind::object NoLoopholes;
		const smart_cover::cover* Cover = RegistryFirst.add_smart_cover("animpoint_stay_wall", RegistryFirstObject, true, true, NoLoopholes);
		SELFTEST_CHECK(Cover != nullptr && RegistryFirstObject.HasCover(), "the second destruction-order fixture must bind");
	}
	RegistryFirst.clear(false);
	SELFTEST_CHECK(!RegistryFirstObject.HasCover(), "registry teardown must detach the surviving object back-pointer");
	SELFTEST_CHECK(RegistryFirst.SmartCoverCount() == 0, "registry teardown must release every owned cover");
	RegistryFirst.clear(false);
	return true;
}

// ---------------------------------------------------------------------------
// 8. patrol paths: CPatrolPathStorage save/load + level.game raw round trip
// ---------------------------------------------------------------------------
SELFTEST(patrol_roundtrip, false)
{
	// CPatrolPathStorage::save(IWriter) -> load(IReader) round trip.
	CPatrolPathStorage Storage;
	CPatrolPath* Path = new CPatrolPath("zz_selftest_loop");
	const Fvector Positions[3] =
	{
		{ 0.f, 1.f, 0.f },
		{ 10.f, 2.f, 0.f },
		{ 10.f, 3.f, 10.f },
	};
	for (u32 I = 0; I < 3; ++I)
	{
		string16 PointName;
		xr_sprintf(PointName, "point_%u", I);
		// Null level graph/cross/game graph: the point stays exactly as given.
		Path->add_vertex(CPatrolPoint(nullptr, nullptr, nullptr, Path, Positions[I], 0, 0, PointName), I);
	}
	Path->add_edge(0, 1, 1.f);
	Path->add_edge(1, 2, 0.5f);
	Path->add_edge(2, 0, 0.25f);
	Storage.add_path("zz_selftest_loop", Path);

	CMemoryWriter MemoryWriter;
	Storage.save(MemoryWriter);

	CPatrolPathStorage Loaded;
	IReader Reader(MemoryWriter.pointer(), MemoryWriter.size());
	Loaded.load(Reader);

	const CPatrolPath* LoadedPath = Loaded.path("zz_selftest_loop", true);
	SELFTEST_CHECK(LoadedPath != nullptr, "saved path must load back");
	SELFTEST_CHECK(LoadedPath->vertex_count() == 3, "expected 3 points, got %u", LoadedPath->vertex_count());
	SELFTEST_CHECK(LoadedPath->edge_count() == 3, "expected 3 edges, got %u", LoadedPath->edge_count());
	for (u32 I = 0; I < 3; ++I)
	{
		const CPatrolPath::CVertex* Vertex = LoadedPath->vertex(I);
		SELFTEST_CHECK(Vertex != nullptr, "vertex %u must exist", I);
		SELFTEST_CHECK(Vertex->data().position().similar(Positions[I]), "vertex %u position mismatch", I);
	}
	SELFTEST_CHECK(LoadedPath->edge(0, 1) != nullptr && std::fabs(LoadedPath->edge(0, 1)->weight() - 1.f) < EPS_S, "edge 0->1 mismatch");
	SELFTEST_CHECK(LoadedPath->edge(1, 2) != nullptr && std::fabs(LoadedPath->edge(1, 2)->weight() - 0.5f) < EPS_S, "edge 1->2 mismatch");
	SELFTEST_CHECK(LoadedPath->edge(2, 0) != nullptr && std::fabs(LoadedPath->edge(2, 0)->weight() - 0.25f) < EPS_S, "edge 2->0 mismatch");

	xr_shared_ptr<const CPatrolPath> Retained = Loaded.shared_path("zz_selftest_loop", true);
	std::weak_ptr<const CPatrolPath> Observer = Retained;
	Loaded.unload();
	SELFTEST_CHECK(!Observer.expired() && Retained->vertex_count() == 3, "a path user must retain valid data after registry unload");
	SELFTEST_CHECK(Loaded.path("zz_selftest_loop", true) == nullptr, "unloaded path must not remain discoverable");
	Retained.reset();
	SELFTEST_CHECK(Observer.expired(), "the path must be released after its last owner is gone");

	// level.game raw format (CPatrolPathStorage::load_raw) round trip.
	Sandbox::STerrainParams Terrain;
	Terrain.Kind = Sandbox::ETerrainKind::Flat;
	Terrain.Size = 20.f;

	CMemoryWriter RawWriter;
	Sandbox::WriteLevelGame(RawWriter, Terrain);

	CPatrolPathStorage RawStorage;
	IReader RawReader(RawWriter.pointer(), RawWriter.size());
	// Null level graph/cross/game graph are tolerated: points keep their raw
	// positions and an invalid level vertex id.
	RawStorage.load_raw(nullptr, nullptr, nullptr, RawReader);

	const CPatrolPath* RawPath = RawStorage.path("viewer_patrol", true);
	SELFTEST_CHECK(RawPath != nullptr, "raw path must load");
	SELFTEST_CHECK(RawPath->vertex_count() == 4, "expected 4 raw points, got %u", RawPath->vertex_count());
	SELFTEST_CHECK(RawPath->edge_count() == 4, "expected 4 raw edges, got %u", RawPath->edge_count());
	SELFTEST_CHECK(RawPath->vertex(0)->data().game_vertex_id(nullptr, nullptr, nullptr) == GameGraph::_GRAPH_ID(-1), "raw path without a game graph must have an invalid game vertex sentinel");

	const float Offset = Terrain.Size * 0.3f;
	const Fvector Expected[4] =
	{
		{ -Offset, 0.f, -Offset },
		{ Offset, 0.f, -Offset },
		{ Offset, 0.f, Offset },
		{ -Offset, 0.f, Offset },
	};
	for (u32 I = 0; I < 4; ++I)
	{
		const CPatrolPath::CVertex* Vertex = RawPath->vertex(I);
		SELFTEST_CHECK(Vertex != nullptr, "raw vertex %u must exist", I);
		SELFTEST_CHECK(Vertex->data().position().similar(Expected[I]), "raw vertex %u position mismatch", I);
	}
	return true;
}

// ---------------------------------------------------------------------------
// 9. resource mask -> produced files matrix
// ---------------------------------------------------------------------------
SELFTEST(resource_mask_matrix, false)
{
	Sandbox::STerrainParams Terrain;
	Terrain.Kind = Sandbox::ETerrainKind::Flat;
	Terrain.Size = 8.f;
	Terrain.Cell = 2.f;

	xr_vector<Flags32> Matrix;
	{
		Flags32 None;
		None.zero();
		Matrix.push_back(None);
	}
	for (u32 Bit = 0; Bit < 16; ++Bit)
	{
		Flags32 Single;
		Single.zero();
		Single.set(1u << Bit, true);
		Matrix.push_back(Single);
	}
	{
		Flags32 Combo;
		Combo.zero();
		Combo.set(u32(ELevelResource::Collision) | u32(ELevelResource::AiMap), true);
		Matrix.push_back(Combo);
		Combo.set(u32(ELevelResource::PatrolPaths), true);
		Matrix.push_back(Combo);
	}
	{
		Flags32 All;
		All.assign(u32(ELevelResource::All));
		Matrix.push_back(All);
			for (u32 Bit = 0; Bit < 16; ++Bit)
			{
				Flags32 MissingOne = All;
				MissingOne.set(1u << Bit, false);
				Matrix.push_back(MissingOne);
			}
	}

	for (u32 I = 0; I < Matrix.size(); ++I)
	{
		SLevelDesc Desc;
		Desc.Resources = Matrix[I];
		const Flags32 Effective = Desc.Validate();

		string64 Folder;
		xr_sprintf(Folder, "zz_selftest_matrix_%02u", I);
		string_path LevelDir;
		FS.update_path(LevelDir, _game_levels_, Folder);

		const xr_string MaskText = LevelResourceMaskToString(Effective);
		Msg("* [selftest] matrix step %u [%s]", I, MaskText.c_str());
		xrLogger::FlushLog();
		SELFTEST_CHECK(Sandbox::BuildLevel(Terrain, Folder, Effective), "build failed for mask [%s]", MaskText.c_str());
		xrLogger::FlushLog();

		bool Ok = SelfTestDetail::CheckFileExists(LevelDir, "level.ltx", true);
		Ok &= SelfTestDetail::CheckFileExists(LevelDir, "level.cform", Effective.test(u32(ELevelResource::Collision)));
		Ok &= SelfTestDetail::CheckFileExists(LevelDir, "level.ai", Effective.test(u32(ELevelResource::AiMap)));
		Ok &= SelfTestDetail::CheckFileExists(LevelDir, "level.game", Effective.test(u32(ELevelResource::PatrolPaths)));

		Sandbox::RemoveLevel(Folder);
		Ok &= !std::filesystem::exists(LevelDir);

		SELFTEST_CHECK(Ok, "file set mismatch for mask [%s]", MaskText.c_str());
	}
	return true;
}

// ---------------------------------------------------------------------------
// 10. empty CObjectSpace tolerates ray queries
// ---------------------------------------------------------------------------
SELFTEST(object_space_empty, true)
{
	if (g_SpatialSpace == nullptr || g_SpatialSpacePhysic == nullptr)
	{
		Msg("* [selftest] object_space_empty: spatial spaces do not exist yet, skipped");
		return true;
	}

	// An empty static collision model does not need to reinitialize spatial databases.
	CObjectSpace ObjectSpace;
	ObjectSpace.CreateEmpty(false);
	Msg("* [selftest] object_space_empty: empty databases created");
	xrLogger::FlushLog();
	const bool Hit = ObjectSpace.RayTest(Fvector().set(0.f, 10.f, 0.f), Fvector().set(0.f, -1.f, 0.f), 100.f, collide::rqtStatic, nullptr, nullptr);
	SELFTEST_CHECK(!Hit, "empty object space must not report ray hits");
	return true;
}

// ---------------------------------------------------------------------------
// 11. offset allocator (viewer GPU pool foundation)
// ---------------------------------------------------------------------------
SELFTEST(offset_allocator, false)
{
	COffsetAllocator Allocator;

	// Uninitialized allocator must not crash and must refuse.
	SELFTEST_CHECK(Allocator.Alloc(16) == COffsetAllocator::InvalidOffset, "alloc before Init must fail");

	Allocator.Init(1024);
	SELFTEST_CHECK(Allocator.GetStats().Free == 1024, "fresh pool must be fully free");

	// Simple allocation + alignment.
	const u32 A = Allocator.Alloc(100);
	SELFTEST_CHECK(A != COffsetAllocator::InvalidOffset && (A % 16) == 0, "A must be 16-aligned");
	const u32 B = Allocator.Alloc(200);
	SELFTEST_CHECK(B != COffsetAllocator::InvalidOffset && B >= A + 100, "B must follow A");
	SELFTEST_CHECK(Allocator.GetStats().Used == 300, "used bytes mismatch");
	SELFTEST_CHECK(Allocator.GetStats().AllocationCount == 2, "live allocation count mismatch");

	// Free in the middle, then re-allocate fitting and non-fitting sizes.
	Allocator.Free(A);
	SELFTEST_CHECK(Allocator.GetStats().FreeBlockCount == 2, "freeing A must split free space into two blocks");
	// A was at offset 0 (100 bytes); B at offset 112 (16-aligned, 200 bytes) with
	// 12 bytes of alignment padding. After freeing A: used = 200 (B), free = 824,
	// split as [0,112) and [312,1024).
	const COffsetAllocator::SStats AfterFreeA = Allocator.GetStats();
	SELFTEST_CHECK(AfterFreeA.Free == 1024 - 200, "total free bytes mismatch after freeing A (got %u)", AfterFreeA.Free);
	SELFTEST_CHECK(AfterFreeA.LargestFreeBlock == 712, "largest free block mismatch (got %u)", AfterFreeA.LargestFreeBlock);

	const u32 C = Allocator.Alloc(50);
	SELFTEST_CHECK(C == 0, "50 bytes must fit into the freed head block");	// A was at 0
	SELFTEST_CHECK(Allocator.Owns(B) && Allocator.Owns(C), "Owns() bookkeeping mismatch");

	// Coalescing: free B and C, the whole range must become one block again.
	Allocator.Free(B);
	Allocator.Free(C);
	SELFTEST_CHECK(Allocator.GetStats().FreeBlockCount == 1 && Allocator.GetStats().LargestFreeBlock == 1024, "free space must coalesce back to one block");
	SELFTEST_CHECK(Allocator.GetStats().Used == 0, "all memory must be returned");

	// Exhaustion must fail gracefully, never crash.
	SELFTEST_CHECK(Allocator.Alloc(1025) == COffsetAllocator::InvalidOffset, "over-capacity alloc must fail");
	const u32 Whole = Allocator.Alloc(1024);
	SELFTEST_CHECK(Whole != COffsetAllocator::InvalidOffset, "full-range alloc must succeed");
	SELFTEST_CHECK(Allocator.Alloc(1) == COffsetAllocator::InvalidOffset, "exhausted pool must refuse");
	Allocator.Free(Whole);

	// Double free must be ignored (and logged), not corrupt state.
	Allocator.Free(Whole);
	Msg("* [selftest] the 'unknown offset' log line above is expected");
	SELFTEST_CHECK(Allocator.GetStats().Free == 1024, "double free must not corrupt the pool");

	// Fragmentation: fill the pool completely with 8x128, then free two
	// non-adjacent blocks - a 256 request must fail while a 128 succeeds.
	xr_vector<u32> Fills;
	for (u32 i = 0; i < 8; ++i)
	{
		const u32 p = Allocator.Alloc(128);
		SELFTEST_CHECK(p != COffsetAllocator::InvalidOffset, "pool fill %u failed", i);
		Fills.push_back(p);
	}
	Allocator.Free(Fills[1]);
	Allocator.Free(Fills[3]);
	SELFTEST_CHECK(Allocator.Alloc(256) == COffsetAllocator::InvalidOffset, "fragmented pool must refuse 256");
	SELFTEST_CHECK(Allocator.Alloc(128) != COffsetAllocator::InvalidOffset, "fragmented pool must serve 128");
	for (u32 p : Fills)
		Allocator.Free(p);
	SELFTEST_CHECK(Allocator.GetStats().Used == 0, "all blocks freed");
	Allocator.Reset();
	SELFTEST_CHECK(Allocator.GetStats().FreeBlockCount == 1 && Allocator.GetStats().Used == 0, "Reset must restore the pool");
	return true;
}

SELFTEST(viewer_history_stream, false)
{
	string_path path;
	FS.update_path(path, _app_data_root_, "_selftest\\viewer_history\\history.bin");
	IWriter* fixture = FS.w_open(path);
	SELFTEST_CHECK(fixture != nullptr && fixture->valid(), "history fixture must be writable");
	FS.w_close(fixture);
	ImUtils::Viewer::CViewerEditorHistory history;
	SELFTEST_CHECK(history.Open(path), "history file must open");
	for (u32 index = 0; index < 25; ++index)
	{
		CMemoryWriter state;
		state.w_u32(index * 17 + 3);
		char label[32];
		xr_sprintf(label, "state %02u", index);
		SELFTEST_CHECK(history.Append(label, state), "history append %u failed", index);
	}
	SELFTEST_CHECK(history.EntryCount() == 10, "history must retain only ten in-memory records");
	SELFTEST_CHECK(history.CurrentId() == 25 && !history.CanRedo(), "append cursor must be after the newest record");
	u32 undone = 0;
	while (history.CanUndo())
	{
		const u64 expected = history.CurrentId() - 2;
		const u64 expectedCursor = history.CurrentId() - 1;
		CMemoryWriter state;
		SELFTEST_CHECK(history.Undo(state), "undo must read state %llu", (unsigned long long)expected);
		IReader reader(state.pointer(), state.size());
		SELFTEST_CHECK(reader.r_u32() == expected * 17 + 3, "undo returned the wrong state");
		SELFTEST_CHECK(history.SetCursor(expectedCursor), "undo must move the cursor");
		++undone;
	}
	SELFTEST_CHECK(undone == 24, "25 states must allow 24 undo transitions");
	u32 redone = 0;
	while (history.CanRedo())
	{
		const u64 expected = history.CurrentId();
		CMemoryWriter state;
		SELFTEST_CHECK(history.Redo(state), "redo must read state %llu", (unsigned long long)expected);
		IReader reader(state.pointer(), state.size());
		SELFTEST_CHECK(reader.r_u32() == expected * 17 + 3, "redo returned the wrong state");
		SELFTEST_CHECK(history.SetCursor(expected + 1), "redo must move the cursor");
		++redone;
	}
	SELFTEST_CHECK(redone == 24, "25 states must allow 24 redo transitions");
	SELFTEST_CHECK(history.Open(path) && history.CurrentId() == 25 && history.EntryCount() == 10, "history must reopen with the same bounded window and cursor");
	CMemoryWriter state;
	SELFTEST_CHECK(history.Undo(state) && history.SetCursor(24), "reopened history must restore a previous state");
	SELFTEST_CHECK(history.Redo(state) && history.SetCursor(25), "reopened history must restore the newest state");

	string_path malformed;
	FS.update_path(malformed, _app_data_root_, "_selftest\\viewer_history\\malformed.bin");
	{
		IWriter* writer = FS.w_open(malformed);
		SELFTEST_CHECK(writer != nullptr, "cannot create malformed history fixture");
		writer->w_u32(u32(-1));
		FS.w_close(writer);
	}
	ImUtils::Viewer::CViewerEditorHistory recovered;
	SELFTEST_CHECK(recovered.Open(malformed), "malformed history must be handled without asserting");
	SELFTEST_CHECK(recovered.Append("recovered", state), "the next valid record must replace a malformed stream");
	SELFTEST_CHECK(recovered.CurrentId() == 1 && !recovered.CanUndo(), "a single recorded state must not undo itself");
	SELFTEST_CHECK(!recovered.SetCursor(0), "the cursor cannot move before the first recorded state");
	SELFTEST_CHECK(recovered.Open(malformed) && recovered.Restore(0, state), "recovered history must reopen correctly");
	return true;
}

SELFTEST(viewer_cpu_pool, false)
{
	ImUtils::Viewer::CViewerCpuPool pool;
	pool.Init(64);
	SELFTEST_CHECK(pool.Capacity() == 64, "scratch pool capacity mismatch");
	SELFTEST_CHECK(pool.Allocate(24) != nullptr && pool.UsedBytes() == 24, "first allocation must be aligned");
	SELFTEST_CHECK(pool.Allocate(32) != nullptr && pool.UsedBytes() == 64, "second allocation must use the aligned offset");
	SELFTEST_CHECK(pool.Allocate(1) == nullptr, "exhausted scratch pool must refuse without crashing");
	SELFTEST_CHECK(!pool.Resize(128, false), "resize must reject disabled reallocation");
	SELFTEST_CHECK(!pool.Resize(128, true), "resize must reject outstanding allocations");
	pool.Reset();
	SELFTEST_CHECK(pool.Resize(128, true) && pool.Capacity() == 128, "idle scratch pool must resize when allowed");
	SELFTEST_CHECK(pool.Resize(128, false), "an unchanged size is valid even when resizing is disabled");
	pool.Shutdown();
	return true;
}

SELFTEST(particle_definition_clone, true)
{
	CMemoryWriter stream;
	stream.open_chunk(u32(PS::PE::Chunks::VERSION));
	stream.w_u16(u16(PS::PE::Version::Original));
	stream.close_chunk();
	stream.open_chunk(u32(PS::PE::Chunks::NAME));
	stream.w_stringZ("selftest_effect");
	stream.close_chunk();
	stream.open_chunk(u32(PS::PE::Chunks::EFFECTDATA));
	stream.w_u32(128);
	stream.close_chunk();
	stream.open_chunk(u32(PS::PE::Chunks::ACTIONLIST));
	stream.w_u32(0);
	stream.close_chunk();
	{
		u32 flags = PS::dfSprite | PS::dfTimeLimit;
		stream.w_chunk(u32(PS::PE::Chunks::FLAGS), &flags, sizeof(flags));
	}
	stream.open_chunk(u32(PS::PE::Chunks::SPRITE));
	stream.w_stringZ("particle\\particle");
	stream.w_stringZ("pfx\\pfx_smoke");
	stream.close_chunk();
	stream.open_chunk(u32(PS::PE::Chunks::TIMELIMIT));
	stream.w_float(4.f);
	stream.close_chunk();

	IReader source(stream.pointer(), stream.size());
	IParticleDefinition* original = Render->create_particle_definition(source);
	SELFTEST_CHECK(original != nullptr, "valid source definition must load through the neutral renderer API");
	SELFTEST_CHECK(original->SetSourceRate(16.f), "the real source-action serializer must create a default source action");
	CMemoryWriter clonedStream;
	original->Save(clonedStream);
	IReader clonedReader(clonedStream.pointer(), clonedStream.size());
	SELFTEST_CHECK(Render->validate_particle_definition(clonedReader), "the serialized definition must pass bounded runtime validation");
	IParticleDefinition* clone = Render->create_particle_definition(clonedReader);
	SELFTEST_CHECK(clone != nullptr && clone != original, "clone must own a separate definition");
	SELFTEST_CHECK(clone->GetCompiledActionBytes() == original->GetCompiledActionBytes(), "compiled action bytes must be preserved");
	clone->SetMaxParticles(256);
	clone->SetTimeLimit(9.f);
	SELFTEST_CHECK(original->GetMaxParticles() == 128 && original->GetTimeLimit() == 4.f, "editing the clone must not mutate the source");
	SELFTEST_CHECK(clone->GetMaxParticles() == 256 && clone->GetTimeLimit() == 9.f, "clone parameter edits must be applied");
	IRenderVisual* visual = Render->model_CreateParticleDefinition(clone);
	SELFTEST_CHECK(visual != nullptr, "the clone must compile through the stock particle factory");
	SELFTEST_CHECK(Render->particle_source_rate(visual) == 16.f, "source-rate action data must survive cloning");
	SELFTEST_CHECK(Render->set_particle_source_rate(visual, 24.f), "source-rate editing must use the real particle action");
	CMemoryWriter edited;
	clone->Save(edited);
	IReader editedReader(edited.pointer(), edited.size());
	IParticleDefinition* restored = Render->create_particle_definition(editedReader);
	SELFTEST_CHECK(restored != nullptr && restored->GetMaxParticles() == 256, "saved clone must reload its parameter edits");
	IRenderVisual* restoredVisual = Render->model_CreateParticleDefinition(restored);
	SELFTEST_CHECK(restoredVisual != nullptr && Render->particle_source_rate(restoredVisual) == 24.f, "saved source-rate action must reload");
	Render->model_Delete(visual);
	Render->model_Delete(restoredVisual);
	Render->destroy_particle_definition(original);
	Render->destroy_particle_definition(clone);
	Render->destroy_particle_definition(restored);
	return true;
}

SELFTEST(tutorial_render_ordering, false)
{
	CUISequencer sequencer;
	SELFTEST_CHECK(!sequencer.ShouldRender(true), "intro/tutorial UI must not overwrite an active main menu");
	SELFTEST_CHECK(sequencer.ShouldRender(false), "intro/tutorial UI must render without the main menu");
	sequencer.m_flags.set(CUISequencer::etsOverMainMenu, true);
	SELFTEST_CHECK(sequencer.ShouldRender(true), "over_main_menu must retain its explicit overlay behavior");
	return true;
}

SELFTEST(loading_screen_render_ordering, false)
{
	CLoadScreenRenderer loader;
	SELFTEST_CHECK(!loader.ShouldRender(false), "unregistered loading UI must not clear the scene");
	loader.b_registered = true;
	SELFTEST_CHECK(loader.ShouldRender(false), "registered loading UI must render outside the main menu");
	SELFTEST_CHECK(!loader.ShouldRender(true), "loading UI must not clear an active main menu");
	return true;
}

SELFTEST(network_processor_registration, false)
{
	const pureFrame* callback = nullptr;
	{
		CNetworkProcessor processor(nullptr);
		callback = &processor;
		Device.seqFrameMT.Add(&processor);
	}
	const bool registered = std::any_of(Device.seqFrameMT.pure_objects.begin(), Device.seqFrameMT.pure_objects.end(), [callback](const PureReg<pureFrame>& entry)
	{
		return entry.object == callback;
	});
	SELFTEST_CHECK(!registered, "network processor destructor must unregister the game-thread callback");
	return true;
}

SELFTEST(parallel_job_removal, false)
{
	struct SJob
	{
		static void Run() {}
	};
	const xr_delegate<void()> job(&SJob::Run);
	const size_t before = Device.seqParallel.size();
	SeqParallelPush(job);
	SeqParallelRemove(job);
	SELFTEST_CHECK(Device.seqParallel.size() == before, "parallel job removal must clear the owning queue");
	return true;
}

SELFTEST(missing_vertex_fallback, true)
{
	CAI_ObjectLocation location;
	location.reinit();
	SELFTEST_CHECK(location.level_vertex() == ai().level_graph().empty_vertex(), "an invalid cached vertex must use the graph's empty vertex without asserting");
	return true;
}

SELFTEST(script_engine_ownership, true)
{
	if (g_pGameLevel == nullptr)
	{
		SELFTEST_CHECK(alife() == nullptr, "alife() must not dereference a missing level");
	}
	struct SScriptContextRestore
	{
		CAI_Space* PreviousAi = g_ai_space;
		CScriptEngine* PreviousScript = g_pScriptEngine;
		~SScriptContextRestore()
		{
			g_ai_space = PreviousAi;
			g_pScriptEngine = PreviousScript;
		}
	} Context;

	const CObjectItemScript* item = dynamic_cast<const CObjectItemScript*>(object_factory().FindItem(TEXT2CLSID("MAIN_MNU")));
	SELFTEST_CHECK(item != nullptr, "main-menu script class is not registered");
	ImUtils::Viewer::EnsureViewerSession();
	CScriptEngine* secondary = nullptr;
	{
		CAI_Space Secondary;
		g_ai_space = &Secondary;
		g_pScriptEngine = nullptr;
		Secondary.init(true);
		secondary = Secondary.get_script_engine();
		SELFTEST_CHECK(Context.PreviousScript != nullptr, "primary script engine must exist");
		SELFTEST_CHECK(secondary != Context.PreviousScript, "dedicated session reused the primary Lua VM");
		SELFTEST_CHECK(&ai().script_engine() == secondary, "AI space does not use its owned Lua VM");
		SELFTEST_CHECK(item->HasCreators(Context.PreviousScript) && item->HasCreators(secondary), "script creators are not isolated per Lua VM");
		SELFTEST_CHECK(CScriptEngine::EngineFromState(secondary->lua()) == secondary, "a VM must resolve its owning engine independently of the active viewer");
		SELFTEST_CHECK(CScriptEngine::EngineFromState(Context.PreviousScript->lua()) == Context.PreviousScript, "the primary VM must retain its own engine");
		SELFTEST_CHECK(CScriptEngine::EngineFromState(nullptr) == nullptr, "an absent VM must not resolve an engine");
		g_ai_space = &Secondary;
		ImUtils::Viewer::RestoreViewerPrimaryContext();
		SELFTEST_CHECK(g_pScriptEngine == Context.PreviousScript && g_ai_space == Context.PreviousAi, "viewer work must restore the primary script context");
		g_ai_space = &Secondary;
		g_pScriptEngine = secondary;
		SELFTEST_CHECK(!ScriptBinding::HasActor(), "a fresh Lua VM must not report an actor binding");
		luabind::functor<void> previousFramework = g_pGamePersistent->ixr_framework_onframe;
		CScriptEngine* previousOwner = g_pGamePersistent->FrameworkScriptEngine;
		{
			const luabind::object globals = luabind::get_globals(secondary->lua());
			globals["db"] = luabind::newtable(secondary->lua());
			const luabind::object db = globals["db"];
			db["actor"] = luabind::newtable(secondary->lua());
			SELFTEST_CHECK(ScriptBinding::HasActor(), "the script actor binding must be visible to script-dependent UI");
			lua_pushnil(secondary->lua());
			const luabind::object nil(secondary->lua());
			nil.set();
			db["actor"] = nil;
			SELFTEST_CHECK(!ScriptBinding::HasActor(), "removing the script actor must make script-dependent UI idle");
			SELFTEST_CHECK(secondary->functor("___ixr_engine_callbacks.on_frame", g_pGamePersistent->ixr_framework_onframe), "secondary framework callback must be available");
		}
		previousFramework.reset();
		g_pGamePersistent->FrameworkScriptEngine = secondary;
		secondary->init();
		SELFTEST_CHECK(!g_pGamePersistent->ixr_framework_onframe.is_valid(), "script reinitialization must release its callback before closing the VM");
		SELFTEST_CHECK(g_pGamePersistent->FrameworkScriptEngine == nullptr, "reinitialization must clear the callback owner");
		if (previousOwner != nullptr)
			previousOwner->functor("___ixr_engine_callbacks.on_frame", g_pGamePersistent->ixr_framework_onframe);
		else
			g_pGamePersistent->ixr_framework_onframe.reset();
		g_pGamePersistent->FrameworkScriptEngine = previousOwner;
	}
	SELFTEST_CHECK(!item->HasCreators(secondary), "secondary Lua VM left a dangling creator");
	SELFTEST_CHECK(g_pScriptEngine == nullptr, "AI space must not retain its destroyed script engine globally");
	SELFTEST_CHECK(item->HasCreators(Context.PreviousScript), "secondary teardown removed the primary creator");
	return true;
}

SELFTEST(sound_geometry_validation, true)
{
	xr_unique_ptr<CDB::MODEL> model = SelfTestDetail::CreateSoundGeometryModel();
	SELFTEST_CHECK(model != nullptr, "valid two-sided SOM polygon must load");
	SELFTEST_CHECK(model->get_tris().size() == 4, "two distinct two-sided SOM polygons must produce four triangles");

	u8 invalidVersion[] = { 1, 0, 0, 0 };
	IReader invalidReader(invalidVersion, sizeof(invalidVersion));
	SELFTEST_CHECK(Sound->create_geometry_som(&invalidReader) == nullptr, "unsupported SOM version must be rejected without an assertion");
	u8 truncatedHeader[] = { 0, 0, 0, 0, 4, 0, 0 };
	IReader truncatedReader(truncatedHeader, sizeof(truncatedHeader));
	SELFTEST_CHECK(Sound->create_geometry_som(&truncatedReader) == nullptr, "truncated chunk header must be rejected without an assertion");
	u8 invalidSize[] = { 0, 0, 0, 0, 100, 0, 0, 0 };
	IReader invalidSizeReader(invalidSize, sizeof(invalidSize));
	SELFTEST_CHECK(Sound->create_geometry_som(&invalidSizeReader) == nullptr, "out-of-bounds chunk size must be rejected without an assertion");
	SoundZoneVector zones;
	SELFTEST_CHECK(Sound->create_geometry_env(nullptr, zones) == nullptr, "missing sound environment data must degrade safely");
	return true;
}

SELFTEST(sound_geometry_concurrent_replacement, true)
{
	CDB::MODEL* Previous = Sound->swap_geometry_som(nullptr);
	xr_task_group Queries{ ETaskPriority::Background };
	xr_atomic_bool Finished = false;
	xr_atomic_bool Valid = true;
	Queries.run([&]()
	{
		const Fvector Listener = { 0.f, 2.f, 0.f };
		const Fvector Emitter = { 0.f, 2.f, 4.f };
		for (u32 Iteration = 0; Iteration < 200000 && !Finished.load(); ++Iteration)
		{
			const float Occlusion = Sound->get_occlusion_to(Listener, Emitter);
			if (!_valid(Occlusion) || Occlusion < 0.f || Occlusion > 1.f)
				Valid.store(false);
		}
	});
	for (u32 Iteration = 0; Iteration < 64; ++Iteration)
	{
		CDB::MODEL* Model = SelfTestDetail::CreateSoundGeometryModel().release();
		if (Model == nullptr)
		{
			Valid.store(false);
			break;
		}
		if (Sound->swap_geometry_som(Model) != nullptr)
			Valid.store(false);
		CDB::MODEL* Detached = Sound->swap_geometry_som(nullptr);
		if (Detached != Model)
			Valid.store(false);
		xr_delete(Detached);
	}
	Finished.store(true);
	Queries.wait();
	CDB::MODEL* Pending = Sound->swap_geometry_som(Previous);
	SELFTEST_CHECK(Pending == nullptr, "the concurrent test must detach every temporary model before restoration");
	SELFTEST_CHECK(Valid.load(), "geometry replacement must preserve valid occlusion queries during concurrent unload and deletion");
	return true;
}

SELFTEST(sound_pending_emitter_lifetime, true)
{
	string_path path;
	SELFTEST_CHECK(FS.exist(path, _game_sounds_, "weapons\\n_ak74_shot", ".ogg"), "the lifetime test requires the stock weapon sound");
	ref_sound sound;
	CObject emitter;
	if (Core.ParamsData.test(ECoreParams::autotest) || Core.ParamsData.test(ECoreParams::nosound))
	{
		sound.create("weapons\\n_ak74_shot", st_Effect, sg_SourceType);
		SELFTEST_CHECK(sound._p == nullptr, "disabled audio must not allocate sound data");
		sound.play(&emitter, sm_2D, 0.f);
		SELFTEST_CHECK(sound.slot() == 0, "disabled audio must not allocate a playback slot");
		return true;
	}
	sound.create("weapons\\n_ak74_shot", st_Effect, sg_SourceType);
	SELFTEST_CHECK(sound._p != nullptr, "the stock sound must be created");
	sound.play(&emitter, sm_2D, 0.f);
	const u32 cancelledSlot = sound.slot();
	SELFTEST_CHECK(cancelledSlot != 0, "the test must allocate a playback slot");
	CObject* removed = &emitter;
	XRay::Sound::Mixer::DereferenceObjects(&removed, 1);
	XRay::Sound::Mixer::Destroy(cancelledSlot);
	XRay::Sound::Mixer::Update(nullptr, 1.f, 1.f, 1.f, Fvector().set(0.f, 0.f, 0.f),
		Fvector().set(0.f, 0.f, 1.f), Fvector().set(0.f, 1.f, 0.f));
	const u32 first = XRay::Sound::Mixer::Create();
	const u32 second = XRay::Sound::Mixer::Create();
	SELFTEST_CHECK(first != second, "cancelled playback must not recycle the same slot twice");
	XRay::Sound::Mixer::Destroy(first);
	XRay::Sound::Mixer::Destroy(second);
	sound.destroy();
	return true;
}

SELFTEST(gpu_pool_offset_storage, true)
{
	ImUtils::Viewer::CViewerGpuPool pool;
	SELFTEST_CHECK(pool.Init(64 * 1024), "small GPU pool must initialize");
	const u8 vertexData[] = { 1, 2, 3, 4 };
	const u8 indexData[] = { 5, 6, 7, 8 };
	auto vertex = pool.CreateVertexBuffer(vertexData, sizeof(vertexData));
	auto secondVertex = pool.CreateVertexBuffer(vertexData, sizeof(vertexData));
	auto index = pool.CreateIndexBuffer(indexData, sizeof(indexData));
	SELFTEST_CHECK(vertex.IsValid() && secondVertex.IsValid() && index.IsValid(), "offset allocations must succeed");
	SELFTEST_CHECK(vertex.Buffer == secondVertex.Buffer && vertex.Buffer != index.Buffer, "each buffer class must have its own shared backing storage");
	SELFTEST_CHECK(vertex.Offset != secondVertex.Offset, "same-class allocations must not overlap");
	SELFTEST_CHECK(pool.VertexStats().AllocationCount == 2 && pool.IndexStats().AllocationCount == 1, "all allocations must be live");

	u8 firstReadback[sizeof(vertexData)] = {};
	u8 secondReadback[sizeof(vertexData)] = {};
	u8 indexReadback[sizeof(indexData)] = {};
	const bool readbackOk = vertex.Buffer->Read(firstReadback, vertex.Offset, sizeof(firstReadback)) &&
		secondVertex.Buffer->Read(secondReadback, secondVertex.Offset, sizeof(secondReadback)) &&
		index.Buffer->Read(indexReadback, index.Offset, sizeof(indexReadback));
	SELFTEST_CHECK(readbackOk, "backing buffers must support staging readback");
	if (memcmp(firstReadback, vertexData, sizeof(vertexData)) != 0 || memcmp(secondReadback, vertexData, sizeof(vertexData)) != 0)
	{
		Msg("! [selftest] gpu offsets %u/%u, first %u,%u,%u,%u second %u,%u,%u,%u",
			vertex.Offset, secondVertex.Offset, firstReadback[0], firstReadback[1], firstReadback[2], firstReadback[3],
			secondReadback[0], secondReadback[1], secondReadback[2], secondReadback[3]);
	}
	SELFTEST_CHECK(memcmp(firstReadback, vertexData, sizeof(vertexData)) == 0 && memcmp(secondReadback, vertexData, sizeof(vertexData)) == 0,
		"each vertex allocation must retain its own bytes in shared backing storage");
	SELFTEST_CHECK(memcmp(indexReadback, indexData, sizeof(indexData)) == 0, "index upload must retain its bytes in index backing storage");

	auto constant = pool.CreateConstantBuffer(48);
	auto secondConstant = pool.CreateConstantBuffer(96);
	SELFTEST_CHECK(constant.IsValid() && secondConstant.IsValid(), "constant-buffer allocations must succeed");
	SELFTEST_CHECK(constant.Buffer == secondConstant.Buffer, "constant allocations must share one backing buffer");
	SELFTEST_CHECK(constant.Offset % 256 == 0 && secondConstant.Offset % 256 == 0, "constant allocations must be 256-byte aligned");
	SELFTEST_CHECK(constant.Offset != secondConstant.Offset, "constant allocations must not overlap");
	SELFTEST_CHECK(pool.ConstantStats().AllocationCount == 2, "both constant allocations must be live");
	SELFTEST_CHECK(pool.VertexStats().Capacity + pool.IndexStats().Capacity + pool.ConstantStats().Capacity == pool.BudgetBytes(),
		"vertex, index and constant capacities must stay within the total byte budget");
	pool.Destroy(constant, false, true);
	SELFTEST_CHECK(pool.ConstantStats().AllocationCount == 1 && pool.IsReady(), "freeing one constant slice must preserve the shared backing buffer");
	pool.Destroy(secondConstant, false, true);
	SELFTEST_CHECK(pool.ConstantStats().Used == 0, "both constant slices must return their bytes to the allocator");

	pool.Destroy(vertex);
	pool.Destroy(secondVertex);
	pool.Destroy(index, true);
	SELFTEST_CHECK(pool.IsReady(), "freeing an allocation must not destroy backing storage");
	pool.Shutdown();
	SELFTEST_CHECK(!pool.IsReady(), "shutdown must release backing storage");
	return true;
}

SELFTEST(patrol_task_rebind, false)
{
	CPatrolPathStorage& Storage = ai().patrol_paths_raw();
	const u32 InitialRevision = Storage.Revision();
	const shared_str PathName("zz_selftest_rebind");
	auto AddPath = [&Storage, &PathName](const Fvector& Position)
	{
		CPatrolPath* Path = new CPatrolPath(PathName);
		Path->add_vertex(CPatrolPoint(nullptr, nullptr, nullptr, Path, Position, 0, 0, "point0"), 0);
		Storage.add_path(PathName, Path);
	};
	CALifeSmartTerrainTask Task(PathName, 0);
	const Fvector First = Fvector().set(1.f, 2.f, 3.f);
	AddPath(First);
	SELFTEST_CHECK(Task.position().similar(First), "a patrol task must resolve the path inserted after its construction");
	Storage.remove_path(PathName);
	const Fvector Empty = Fvector().set(0.f, 0.f, 0.f);
	SELFTEST_CHECK(Task.position().similar(Empty), "removing a path must release the task's point without asserting");
	SELFTEST_CHECK(Task.game_vertex_id() == GameGraph::_GRAPH_ID(-1), "a removed path must not retain a stale game vertex");
	SELFTEST_CHECK(Task.level_vertex_id() == u32(-1), "a removed path must not retain a stale level vertex");
	const Fvector Replacement = Fvector().set(4.f, 5.f, 6.f);
	AddPath(Replacement);
	SELFTEST_CHECK(Task.position().similar(Replacement), "restoring a path must rebind to its new point data");
	SELFTEST_CHECK(Storage.Revision() > InitialRevision, "path mutations must advance the storage revision");
	Storage.remove_path(PathName);
	SELFTEST_CHECK(Task.position().similar(Empty), "final path removal must leave a safe fallback");
	return true;
}

SELFTEST(square_viewport, false)
{
	ImUtils::Viewer::CViewerSessionManager manager;
	ImUtils::Viewer::CViewerSession& session = manager.Sessions[0];
	const u32 Side = static_cast<u32>(std::max(1, std::min(Device.Width, Device.Height)));
	SELFTEST_CHECK(session.ViewportDimension() == Side / 2, "the default square viewport must use half of the shorter screen dimension");
	session.ViewerState.Session = ImUtils::Viewer::EViewerSessionState::Starting;
	ImUtils::Viewer::SViewerRequest request;
	request.Type = ImUtils::Viewer::EViewerRequest::SetViewportSize;
	request.IntValue = 0;
	session.RequestHandler_LevelViewer(request);
	SELFTEST_CHECK(session.ViewerState.ViewportSize == 0, "the one-third viewport option must be selected through the request queue");
	SELFTEST_CHECK(session.Renderer.ViewportWidth() == Side / 3 && session.Renderer.ViewportHeight() == Side / 3,
		"the one-third viewport target must remain square");
	session.Renderer.ClearScene();
	SELFTEST_CHECK(session.Renderer.VerifyReadback(nullptr) >= 0.f, "readback must pack pixels when the GPU row pitch has padding");
	request.IntValue = 1;
	session.RequestHandler_LevelViewer(request);
	SELFTEST_CHECK(session.Renderer.ViewportWidth() == Side / 2 && session.Renderer.ViewportHeight() == Side / 2,
		"the half-screen viewport target must remain square");
	request.IntValue = 730;
	request.HeightValue = 410;
	session.RequestHandler_LevelViewer(request);
	SELFTEST_CHECK(session.ViewerState.ViewportSize == 2, "docked resizing must use explicit pixel dimensions");
	SELFTEST_CHECK(session.Renderer.ViewportWidth() == 730 && session.Renderer.ViewportHeight() == 410,
		"the render target must preserve both dimensions instead of forcing a square");
	session.Renderer.ClearScene();
	SELFTEST_CHECK(session.Renderer.VerifyReadback(nullptr) >= 0.f, "rectangular readback must handle row pitch correctly");
	session.Renderer.Shutdown();
	return true;
}

SELFTEST(viewport_resize_crop, false)
{
	using namespace ImUtils::Viewer::ViewerViewport;
	const auto Square = CenteredCoverCrop(1920.f, 1200.f, 600.f, 600.f);
	SELFTEST_CHECK(std::abs(Square.Min.x - 0.1875f) < 0.0001f && std::abs(Square.Max.x - 0.8125f) < 0.0001f,
		"the original square target must crop only the horizontal edges");
	SELFTEST_CHECK(Square.Min.y == 0.f && Square.Max.y == 1.f, "the square target must preserve the full vertical field");
	const auto Wide = CenteredCoverCrop(600.f, 600.f, 1000.f, 500.f);
	SELFTEST_CHECK(Wide.Min.x == 0.f && Wide.Max.x == 1.f && std::abs(Wide.Min.y - 0.25f) < 0.0001f && std::abs(Wide.Max.y - 0.75f) < 0.0001f,
		"a wider window must crop the square source without stretching it");
	const auto Tall = CenteredCoverCrop(600.f, 600.f, 400.f, 800.f);
	SELFTEST_CHECK(std::abs(Tall.Min.x - 0.25f) < 0.0001f && std::abs(Tall.Max.x - 0.75f) < 0.0001f && Tall.Min.y == 0.f && Tall.Max.y == 1.f,
		"a taller window must crop horizontally without stretching the scene");
	const auto Center = ApplyCrop(Wide, Fvector2{ 0.5f, 0.5f });
	SELFTEST_CHECK(Center.x == 0.5f && Center.y == 0.5f, "crop mapping must preserve the center for overlays");
	const Fvector2 LeftPanel = FitToSquare({ 0.f, 0.5f }, 1920.f, 1200.f);
	const Fvector2 TopPanel = FitToSquare({ 0.5f, 0.f }, 1920.f, 1200.f);
	SELFTEST_CHECK(LeftPanel.x == 0.f && LeftPanel.y == 0.5f, "fitting a wide scene must retain the stock UI's left panel");
	SELFTEST_CHECK(std::abs(TopPanel.x - 0.5f) < 0.0001f && std::abs(TopPanel.y - 0.1875f) < 0.0001f,
		"fitting a wide scene must use symmetric letterbox bars rather than cropping its content");
	const Fvector2 InvalidFit = FitToSquare({ 0.5f, 0.5f }, 0.f, 0.f);
	SELFTEST_CHECK(InvalidFit.x == 0.5f && InvalidFit.y == 0.5f, "zero-sized source data must retain a safe centered mapping");
	const auto Empty = CenteredCoverCrop(0.f, 0.f, 0.f, 0.f);
	SELFTEST_CHECK(Empty.Min.x == 0.f && Empty.Min.y == 0.f && Empty.Max.x == 1.f && Empty.Max.y == 1.f,
		"zero-sized layout data must use a safe full-frame crop");
	return true;
}

SELFTEST(missing_visual_fallback, false)
{
	IRender_interface* renderer = Render != nullptr ? Render : dynamic_cast<IRender_interface*>(Device.m_pRender);
	SELFTEST_CHECK(renderer != nullptr, "the stock renderer must be available for the missing-model check");
	IRenderVisual* visual = renderer->model_Create("meshes\\__viewer_missing_visual__.ogf");
	SELFTEST_CHECK(visual != nullptr, "a missing model must produce a bounded empty visual instead of a fatal error");
	if (visual == nullptr)
		return false;
	SELFTEST_CHECK(visual->getType() == MT_NORMAL, "the fallback must be a plain static visual");
	const Fbox& bounds = visual->getVisData().box;
	SELFTEST_CHECK(bounds.min.x <= bounds.max.x && bounds.min.y <= bounds.max.y && bounds.min.z <= bounds.max.z,
		"fallback bounds must be valid for physics and object code");
	IRenderVisual* duplicate = renderer->model_Duplicate(visual);
	SELFTEST_CHECK(duplicate != nullptr, "an empty visual must support the normal model duplication path");
	renderer->model_Delete(duplicate);
	renderer->model_Delete(visual);
	SELFTEST_CHECK(visual == nullptr && duplicate == nullptr, "empty visuals must release through the normal model pool");
	return true;
}

SELFTEST(request_wait_during_loading, false)
{
	ImUtils::Viewer::CViewerSessionManager manager;
	ImUtils::Viewer::CViewerSession& session = manager.Sessions[0];
	session.ViewerState.Session = ImUtils::Viewer::EViewerSessionState::Starting;
	ImUtils::Viewer::SViewerRequest request;
	request.Type = ImUtils::Viewer::EViewerRequest::SpawnActor;
	request.IntValue = 599;
	session.RequestHandler_LevelViewer(request);
	SELFTEST_CHECK(session.PendingRequests() == 1, "an active load must retain the spawn request even past its old retry budget");
	ImUtils::Viewer::SViewerRequest pending;
	session.DrainRequests([&](const ImUtils::Viewer::SViewerRequest& current)
	{
		pending = current;
		session.RequestHandler_LevelViewer(current);
	});
	SELFTEST_CHECK(pending.IntValue == 599 && session.PendingRequests() == 1,
		"waiting for the level to become active must not consume the retry budget");
	session.ViewerState.Session = ImUtils::Viewer::EViewerSessionState::Idle;
	session.DrainRequests([&](const ImUtils::Viewer::SViewerRequest& current)
	{
		session.RequestHandler_LevelViewer(current);
	});
	session.DrainRequests([&](const ImUtils::Viewer::SViewerRequest& current)
	{
		session.RequestHandler_LevelViewer(current);
	});
	SELFTEST_CHECK(session.PendingRequests() == 0, "an unavailable session must finish with a bounded rejection instead of waiting forever");
	return true;
}

SELFTEST(map_without_alife, false)
{
	struct CTestLocation : CRelationMapLocation
	{
		CTestLocation() : CRelationMapLocation("friend_location", 1, 0) {}
		void SetOwner(CSE_ALifeDynamicObject* owner) { m_owner_se_object = owner; }
	};
	SLevelDesc desc;
	desc.Parallel = true;
	desc.Sandbox = true;
	desc.Resources.zero();
	pApp->SetNextLevelDesc(desc);
	IGame_Level* previous = g_pGameLevel;
	CLevel* level = static_cast<CLevel*>(NEW_INSTANCE(CLSID_GAME_LEVEL));
	SELFTEST_CHECK(level != nullptr, "a level context must be available for map-location construction");
	if (level == nullptr)
	{
		g_pGameLevel = previous;
		return false;
	}
	g_pGameLevel = level;
	CAI_Space* previousAi = g_ai_space;
	g_ai_space = nullptr;
	bool actual = false;
	{
		CSE_ALifeCreatureActor owner("actor");
		CTestLocation location;
		location.SetOwner(&owner);
		actual = location.Update();
	}
	g_ai_space = previousAi;
	DEL_INSTANCE(level);
	g_pGameLevel = previous;
	SELFTEST_CHECK(actual, "relation-only map work must tolerate a missing ALife simulator without deleting the location");
	return true;
}

SELFTEST(sequencer_lifetime, false)
{
	CUISequencer* globalIntro = new CUISequencer();
	globalIntro->Destroy();
	globalIntro->Destroy();
	delete globalIntro;

	SLevelDesc desc;
	desc.Parallel = true;
	desc.Sandbox = true;
	desc.Resources.zero();
	pApp->SetNextLevelDesc(desc);
	IGame_Level* previous = g_pGameLevel;
	CLevel* level = static_cast<CLevel*>(NEW_INSTANCE(CLSID_GAME_LEVEL));
	SELFTEST_CHECK(level != nullptr, "a level owner must be available for sequencer cleanup");
	if (level == nullptr)
	{
		g_pGameLevel = previous;
		return false;
	}
	g_pGameLevel = level;
	CUISequencer* intro = new CUISequencer(level);
	level->Flow.Intro = intro;
	level->Flow.Tutorial = intro;
	level->Flow.RetiringTutorial = intro;
	intro->Destroy();
	SELFTEST_CHECK(level->Flow.Intro == nullptr && level->Flow.Tutorial == nullptr && level->Flow.RetiringTutorial == nullptr,
		"sequencer cleanup must clear every alias before its completion callback");
	DEL_INSTANCE(level);
	g_pGameLevel = previous;
	SELFTEST_CHECK(g_pGameLevel == previous, "sequencer and owner teardown must restore the previous level context");
	return true;
}

SELFTEST(embedded_level_resources, false)
{
	for (const char* folder : { "viewer_flat", "viewer_plane" })
	{
		const Sandbox::EmbeddedLevel* level = Sandbox::FindEmbeddedLevel(folder);
		SELFTEST_CHECK(level != nullptr && level->Count == 5, "embedded preset '%s' must contain all five resources", folder);
		Sandbox::EmbeddedLevelRegistration registration;
		SELFTEST_CHECK(registration.Install(*level), "embedded preset '%s' must register in the VFS", folder);
		string_path diskFolder;
		FS.update_path(diskFolder, "$game_levels$", folder);
		SELFTEST_CHECK(!std::filesystem::exists(diskFolder), "embedded preset '%s' must not require a disk folder", folder);
		string_path expectedFolder;
		xr_strconcat(expectedFolder, folder, "\\");
		xr_vector<char*>* folders = FS.file_list_open("$game_levels$", FS_ListFolders | FS_RootOnly);
		bool listed = false;
		if (folders != nullptr)
		{
			for (const char* entry : *folders)
			{
				listed = listed || _stricmp(entry, expectedFolder) == 0;
			}
		}
		FS.file_list_close(folders);
		SELFTEST_CHECK(listed, "the normal VFS enumeration must list embedded preset '%s'", folder);
		pApp->Level_Scan();
		SELFTEST_CHECK(pApp->Level_ID(folder, "1.0", false) >= 0, "embedded preset '%s' must appear in the normal level scan", folder);
		for (const Sandbox::EmbeddedFile& file : xr_span<const Sandbox::EmbeddedFile>(level->Files, level->Count))
		{
			string_path path;
			xr_strconcat(path, folder, "\\", file.Name);
			IReader* reader = FS.r_open("$game_levels$", path);
			SELFTEST_CHECK(reader != nullptr, "embedded resource '%s' must open through the VFS", path);
			if (reader == nullptr)
				continue;
			SELFTEST_CHECK(reader->length() == file.Size, "embedded resource '%s' must preserve its exact length", path);
			SELFTEST_CHECK(reader->length() == 0 || memcmp(reader->pointer(), file.Bytes, file.Size) == 0,
				"embedded resource '%s' must preserve its exact bytes", path);
			if (file.Size == level->Files[0].Size && reader->length() == file.Size)
			{
				const LevelGraph::CHeader* header = static_cast<const LevelGraph::CHeader*>(reader->pointer());
				SELFTEST_CHECK(header->version() == XRAI_LARGE_VERSION, "embedded preset '%s' must preserve its AI-map version", folder);
				SELFTEST_CHECK(header->vertex_count() == 1089, "embedded preset '%s' must preserve its 64m / 2m AI-map grid", folder);
			}
			FS.r_close(reader);
		}
		string_path path;
		xr_strconcat(path, folder, "\\level.ai");
		registration.Reset();
		SELFTEST_CHECK(!FS.exist("$game_levels$", path), "stopping an embedded preset must unregister its VFS resources");
	}
	return true;
}

SELFTEST(smart_task_missing_path, false)
{
	CALifeSmartTerrainTask task("definitely_missing_patrol_path");
	SELFTEST_CHECK(task.level_vertex_id() == u32(-1), "a missing path must not dereference a null patrol point");
	SELFTEST_CHECK(task.game_vertex_id() == GameGraph::_GRAPH_ID(-1), "a missing path must retain the empty game vertex");
	const Fvector expected = Fvector().set(0.f, 0.f, 0.f);
	SELFTEST_CHECK(task.position().similar(expected), "a missing path must use the neutral fallback position");
	return true;
}

namespace SelfTestDetail
{

static u32 SpawnStateSizeOffset(const NET_Packet& packet)
{
	NET_Packet header;
	header.construct(packet.B.data.data(), u32(packet.B.data.size()));
	u16 message = 0;
	header.r_begin(message);
	if (message != M_SPAWN)
		return 0;
	shared_str name;
	header.r_stringZ(name);
	header.r_stringZ(name);
	header.r_advance(2 + 24 + 2 + 6);
	const u16 flags = header.r_u16();
	u16 version = 0;
	if (flags & M_SPAWN_VERSION)
		header.r_u16(version);
	if (version != SPAWN_VERSION)
		return 0;
	if (version >= SPAWN_VERSION_HEADER_U32_ID)
		header.r_advance(6);
	header.r_advance(2 + 2);
	if (version >= SPAWN_VERSION_CLIENT_DATA)
		header.r_advance(sizeof(u16));
	const u16 clientDataSize = header.r_u16();
	header.r_advance(clientDataSize + 2);
	return header.r_tell();
}

static bool RewriteSpawnState(const NET_Packet& packet, u32 sizeOffset, s32 sizeDelta, const u8* extension, u32 extensionSize, NET_Packet& result)
{
	if (sizeOffset + sizeof(u16) > packet.B.data.size())
		return false;
	u16 stateSize = 0;
	memcpy(&stateSize, packet.B.data.data() + sizeOffset, sizeof(stateSize));
	const s32 newSize = s32(stateSize) + sizeDelta;
	const u32 updateOffset = sizeOffset + stateSize;
	if (newSize < 0 || updateOffset > packet.B.data.size() || (extensionSize != 0 && extension == nullptr))
		return false;
	result.construct(packet.B.data.data(), sizeOffset);
	result.w_u16(u16(newSize));
	if (updateOffset > sizeOffset + sizeof(u16))
		result.w(packet.B.data.data() + sizeOffset + sizeof(u16), updateOffset - sizeOffset - sizeof(u16));
	if (extensionSize != 0)
		result.w(extension, extensionSize);
	if (updateOffset < packet.B.data.size())
		result.w(packet.B.data.data() + updateOffset, u32(packet.B.data.size()) - updateOffset);
	return true;
}

} // namespace SelfTestDetail

SELFTEST(actor_spawn_roundtrip, false)
{
	CSE_ALifeCreatureActor server("actor");
	server.o_Position.set(12.f, 34.f, 56.f);
	server.ID = 0x12345;
	server.set_health(1.f);
	server.s_flags.assign(M_SPAWN_UPDATE);
	server.m_holderID = ALife::INVALID_OBJECT_ID;
	server.mstate = 0;
	server.m_u16NumItems = 0;

	NET_Packet packet;
	server.Spawn_Write(packet, true);
	server.UPDATE_Write(packet);
	const u32 sizeOffset = SelfTestDetail::SpawnStateSizeOffset(packet);
	SELFTEST_CHECK(sizeOffset != 0, "spawn state size field must be found");
	u16 stateSize = 0;
	memcpy(&stateSize, packet.B.data.data() + sizeOffset, sizeof(stateSize));

	NET_Packet spawnOnly;
	spawnOnly.w(packet.B.data.data(), sizeOffset + stateSize);
	spawnOnly.read_start();
	CSE_ALifeCreatureActor spawnOnlyClient("actor");
	SELFTEST_CHECK(spawnOnlyClient.Spawn_Read(spawnOnly), "a state ending at the packet boundary must load without seeking past it");
	SELFTEST_CHECK(spawnOnly.r_eof(), "the exact-end state must leave the reader at packet end");

	const u8 extension[] = { 0xde, 0xad, 0xbe };
	NET_Packet extended;
	SELFTEST_CHECK(SelfTestDetail::RewriteSpawnState(packet, sizeOffset, sizeof(extension), extension, sizeof(extension), extended), "state extension must be constructed");
	CSE_ALifeCreatureActor client("actor");
	SELFTEST_CHECK(client.Spawn_Read(extended), "actor spawn header must load");
	SELFTEST_CHECK(extended.r_tell() == sizeOffset + stateSize + sizeof(extension), "unknown state extension bytes must be skipped before update data");
	client.UPDATE_Read(extended);
	Msg("* [selftest] actor spawn: expected [%.3f %.3f %.3f], actual [%.3f %.3f %.3f], remaining=%u",
		VPUSH(server.o_Position), VPUSH(client.o_Position), u32(extended.B.data.size()) - extended.r_tell());
	SELFTEST_CHECK(client.o_Position.similar(server.o_Position), "actor position must survive spawn and update");
	SELFTEST_CHECK(extended.r_eof(), "actor update left %u bytes", u32(extended.B.data.size()) - extended.r_tell());

	NET_Packet truncated;
	SELFTEST_CHECK(SelfTestDetail::RewriteSpawnState(packet, sizeOffset, -3, nullptr, 0, truncated), "truncated state must be constructed");
	CSE_ALifeCreatureActor truncatedClient("actor");
	SELFTEST_CHECK(!truncatedClient.Spawn_Read(truncated), "a state reader that consumes beyond the declared boundary must be rejected");

	NET_Packet oversized;
	SELFTEST_CHECK(SelfTestDetail::RewriteSpawnState(packet, sizeOffset, 256, nullptr, 0, oversized), "oversized state must be constructed");
	CSE_ALifeCreatureActor oversizedClient("actor");
	SELFTEST_CHECK(!oversizedClient.Spawn_Read(oversized), "a declared state beyond the packet must be rejected");
	return true;
}

SELFTEST(entity_save_version_bridge, false)
{
	class CEntitySaveProbe final : public CEntityAlive
	{
	public:
		CEntitySaveProbe()
		{
			_construct();
		}
		~CEntitySaveProbe() override
		{
			xr_delete(CEntity::m_entity_condition);
		}
		float ffGetFov() const override { return 0.f; }
		float ffGetRange() const override { return 0.f; }
		void HitSignal(float, Fvector&, CObject*, s16) override {}
	};

	CEntitySaveProbe source;
	source.SetWeaponModifier(WeaponModifier::eHitPowerCoeff, 1.75f);
	NET_Packet current;
	source.save(current);
	constexpr u32 AliveConditionSize = 18;
	const u32 conditionOffset = current.B.data.size() - source.WeaponModifierCoefficients.size() * sizeof(float) - AliveConditionSize;
	SELFTEST_CHECK(current.B.data.data()[conditionOffset] == 3, "new saves must carry condition version 1 after the physics state");

	CEntitySaveProbe restored;
	IReader currentReader(current.B.data.data(), current.B.data.size());
	restored.load(currentReader);
	SELFTEST_CHECK(currentReader.elapsed() == 0, "current entity save must be consumed completely");
	SELFTEST_CHECK(restored.GetWeaponModifier(WeaponModifier::eHitPowerCoeff) == 1.75f, "current modifier must round trip");

	NET_Packet legacy;
	legacy.w(current.B.data.data(), current.B.data.size() - source.WeaponModifierCoefficients.size() * sizeof(float));
	legacy.B.data.data()[conditionOffset] &= 1;
	CEntitySaveProbe legacyRestored;
	legacyRestored.SetWeaponModifier(WeaponModifier::eHitPowerCoeff, 4.f);
	IReader legacyReader(legacy.B.data.data(), legacy.B.data.size());
	legacyRestored.load(legacyReader);
	SELFTEST_CHECK(legacyReader.elapsed() == 0, "legacy entity save must be consumed completely");
	SELFTEST_CHECK(legacyRestored.conditions().SaveVersion() == 0, "legacy save must use version zero");
	for (const float coefficient : legacyRestored.WeaponModifierCoefficients)
	{
		SELFTEST_CHECK(coefficient == 1.0f, "legacy saves must retain the original default modifier");
	}
	return true;
}

SELFTEST(script_object_id_version_bridge, false)
{
	u8 data[sizeof(u32) + sizeof(u16)];
	constexpr u32 CurrentId = 0x12345678;
	constexpr u16 LegacyId = 0xfedc;
	memcpy(data, &CurrentId, sizeof(CurrentId));
	memcpy(data + sizeof(CurrentId), &LegacyId, sizeof(LegacyId));
	IReader reader(data, sizeof(data));
	SELFTEST_CHECK(reader.r_object_id() == CurrentId, "current script saves must read 32-bit object IDs");
	SELFTEST_CHECK(reader.tell() == sizeof(CurrentId), "current ID must consume exactly four bytes");
	reader.set_script_object_id_size(sizeof(u16));
	SELFTEST_CHECK(reader.r_object_id() == LegacyId, "legacy script saves must preserve 16-bit object IDs");
	SELFTEST_CHECK(reader.eof(), "both script ID representations must be consumed exactly");
	u16 LegacyInvalidId = u16(-1);
	IReader invalidReader(&LegacyInvalidId, sizeof(LegacyInvalidId));
	invalidReader.set_script_object_id_size(sizeof(u16));
	SELFTEST_CHECK(invalidReader.r_object_id() == u32(-1), "the legacy invalid ID must map to the current 32-bit sentinel");
	class CScriptBinderProbe final : public CScriptBinder
	{
	public:
		u32 ObjectIdSize() const { return m_object_id_size; }
	};
	CScriptBinderProbe binder;
	binder.set_client_data_version(SPAWN_VERSION_SCRIPT_U32_ID - 1);
	SELFTEST_CHECK(binder.ObjectIdSize() == sizeof(u16), "legacy spawn data must select 16-bit script IDs");
	binder.set_client_data_version(SPAWN_VERSION_SCRIPT_U32_ID);
	SELFTEST_CHECK(binder.ObjectIdSize() == sizeof(u32), "current spawn data must select 32-bit script IDs");

	CSE_ALifeCreatureActor original("actor");
	original.ID = 0;
	original.set_health(0.f);
	original.m_holderID = ALife::INVALID_OBJECT_ID;
	original.mstate = 0;
	original.m_u16NumItems = 0;
	original.client_data.resize(sizeof(CurrentId) + 1);
	memcpy(original.client_data.data(), &CurrentId, sizeof(CurrentId));
	original.client_data.back() = 0x5a;
	NET_Packet legacySpawn;
	{
		SpawnWrite::ScopedVersion legacyVersion(SPAWN_VERSION_HEADER_U32_ID);
		original.Spawn_Write(legacySpawn, true);
	}
	CSE_ALifeCreatureActor stored("actor");
	SELFTEST_CHECK(stored.Spawn_ReadWithOptions(legacySpawn, false), "legacy spawn must load");
	SELFTEST_CHECK(stored.m_client_data_version == SPAWN_VERSION_HEADER_U32_ID, "the embedded legacy payload version must survive loading");

	NET_Packet respawn;
	stored.Spawn_Write(respawn, true);
	CSE_ALifeCreatureActor restoredLegacy("actor");
	SELFTEST_CHECK(restoredLegacy.Spawn_Read(respawn), "current spawn headers must carry their embedded legacy payload version");
	SELFTEST_CHECK(restoredLegacy.m_wVersion == SPAWN_VERSION, "the runtime spawn header must be current");
	SELFTEST_CHECK(restoredLegacy.m_client_data_version == SPAWN_VERSION_HEADER_U32_ID, "upgrading a spawn header must not upgrade its untouched payload");
	SELFTEST_CHECK(0 == xr_strcmp(restoredLegacy.m_sCharacterProfile.c_str(), original.m_sCharacterProfile.c_str()), "the 32-bit native fields must remain aligned before the trader profile");
	binder.set_client_data_version(restoredLegacy.m_client_data_version);
	SELFTEST_CHECK(binder.ObjectIdSize() == sizeof(u32), "the binder must use the payload version, not the upgraded header version");
	IReader embeddedLegacy(restoredLegacy.client_data.data(), restoredLegacy.client_data.size());
	embeddedLegacy.set_script_object_id_size(binder.ObjectIdSize());
	SELFTEST_CHECK(embeddedLegacy.r_object_id() == CurrentId && embeddedLegacy.r_u8() == 0x5a && embeddedLegacy.eof(), "the legacy payload must remain byte-aligned after a current respawn");

	NET_Packet clientSave;
	clientSave.w_u16(sizeof(CurrentId) + 1);
	clientSave.w_u32(CurrentId);
	clientSave.w_u8(0x5a);
	stored.CSE_Abstract::load(clientSave);
	SELFTEST_CHECK(stored.m_client_data_version == SPAWN_VERSION, "a current client save must advance the embedded payload version");
	NET_Packet currentSpawn;
	stored.Spawn_Write(currentSpawn, true);
	CSE_ALifeCreatureActor restoredCurrent("actor");
	SELFTEST_CHECK(restoredCurrent.Spawn_Read(currentSpawn), "the current client payload must survive a respawn");
	SELFTEST_CHECK(restoredCurrent.m_client_data_version == SPAWN_VERSION, "the current payload version must round trip");
	IReader embeddedCurrent(restoredCurrent.client_data.data(), restoredCurrent.client_data.size());
	SELFTEST_CHECK(embeddedCurrent.r_object_id() == CurrentId && embeddedCurrent.r_u8() == 0x5a && embeddedCurrent.eof(), "the current payload must remain byte-aligned after a respawn");
	return true;
}

SELFTEST(scheduler_pending_cancellation, true)
{
	class CScheduleProbe final : public ISheduled
	{
	public:
		float shedule_Scale() override { return 1.f; }
		bool shedule_Needed() override { return false; }
		shared_str shedule_Name() const override { return "selftest_scheduler"; }
	};

	CScheduleProbe probe;
	probe.shedule_register();
	probe.shedule_unregister();
#ifdef DEBUG
	SELFTEST_CHECK(!Engine.Sheduler.Registered(&probe), "register/unregister before a pump must cancel both operations");
#endif
	Engine.Sheduler.Update();
#ifdef DEBUG
	SELFTEST_CHECK(!Engine.Sheduler.Registered(&probe), "the scheduler must not retain an expired registration");
#endif
	return true;
}

SELFTEST(precache_light_cleanup, true)
{
	SELFTEST_CHECK(::Render != nullptr, "the real renderer is required");
	precache_light = ::Render->light_create();
	SELFTEST_CHECK(precache_light, "the renderer must create a precache light");
	precache_light->set_active(true);
	Device.ReleasePrecacheLight();
	SELFTEST_CHECK(!precache_light, "precache light must be released before renderer teardown");
	return true;
}

SELFTEST(local_transport_roundtrip, false)
{
	CTimer_paused* timer = Device.GetTimerGlobal();
	BaseClient client(timer);
	client.SetDirectConnection(false);
	MSYS_CONFIG config = {};
	config.sign1 = 0x12071980;
	config.sign2 = 0x26111975;
	client.ReceiveLocalPacket(&config, sizeof(config));
	SELFTEST_CHECK(client.net_isCompleted_Connect(), "local connection control must complete the client connection");
	NET_Packet challenge;
	challenge.w_begin(M_AUTH_CHALLENGE);
	client.ReceiveLocalPacket(challenge.B.data.data(), u32(challenge.B.data.size()));
	bool received = false;
	client.StartProcessQueue();
	if (NET_Packet* packet = client.net_msg_Retreive())
	{
		u16 type = 0;
		packet->r_begin(type);
		received = type == M_AUTH_CHALLENGE;
		client.net_msg_Release();
	}
	client.EndProcessQueue();
	SELFTEST_CHECK(received, "raw local packets must reach the client message queue without wire framing");
	return true;
}

SELFTEST(spawn_protocol_version_bridge, false)
{
	CSE_ALifeItemWeaponMagazined Server("wpn_ak74");
	Server.m_wVersion = 126;
	Server.ID = 0x12345;
	Server.s_flags.assign(M_SPAWN_UPDATE);
	Server.m_fCondition = 0.75f;
	Server.a_elapsed.MagazineElapsed = 17;
	Server.m_u8CurFireMode = 2;
	Server.misfire = TRUE;
	Server.rt_zoom_factor = 3.25f;
	Server.cur_scope = 1;
	Server.chamber_ammo_type = 1;
	Server.a_chamber_elapsed = 9;

	NET_Packet Packet;
	Server.Spawn_Write(Packet, true);
	Server.UPDATE_Write(Packet);

	NET_Packet LegacySpawn;
	LegacySpawn.construct(Packet.B.data.data(), Packet.w_tell());
	NET_Packet Header;
	Header.construct(LegacySpawn.B.data.data(), LegacySpawn.B.data.size());
	u16 Message;
	Header.r_begin(Message);
	shared_str Name;
	Header.r_stringZ(Name);
	Header.r_stringZ(Name);
	Header.r_advance(2 + 24 + 2 + 6 + 2);
	const u32 VersionOffset = Header.r_tell();
	const u16 LegacyVersion = 126;
	memcpy(LegacySpawn.B.data.data() + VersionOffset, &LegacyVersion, sizeof(LegacyVersion));
	memmove(LegacySpawn.B.data.data() + VersionOffset + 2, LegacySpawn.B.data.data() + VersionOffset + 10, Packet.w_tell() - VersionOffset - 10);
	LegacySpawn.B.data.resize(Packet.w_tell() - 8);
	const u32 SizeOffset = SelfTestDetail::SpawnStateSizeOffset(Packet);
	SELFTEST_CHECK(SizeOffset >= 8, "current spawn state size field must be found");
	const u32 LegacySizeOffset = SizeOffset - 8;
	u16 StateSize = 0;
	memcpy(&StateSize, LegacySpawn.B.data.data() + LegacySizeOffset, sizeof(StateSize));
	constexpr u32 BaseAlifeStateSize = 27;
	const u32 ScriptStoryOffset = LegacySizeOffset + sizeof(u16) + BaseAlifeStateSize;
	SELFTEST_CHECK(ScriptStoryOffset < LegacySpawn.B.data.size() && LegacySpawn.B.data.data()[ScriptStoryOffset] == 0, "the current script-story field must precede the visual");
	memmove(LegacySpawn.B.data.data() + ScriptStoryOffset, LegacySpawn.B.data.data() + ScriptStoryOffset + 1, LegacySpawn.B.data.size() - ScriptStoryOffset - 1);
	LegacySpawn.B.data.resize(LegacySpawn.B.data.size() - 1);
	const u16 LegacyStateSize = StateSize - 1;
	memcpy(LegacySpawn.B.data.data() + LegacySizeOffset, &LegacyStateSize, sizeof(LegacyStateSize));
	CSE_ALifeItemWeaponMagazined Migrated("wpn_ak74");
	SELFTEST_CHECK(Migrated.Spawn_Read(LegacySpawn), "legacy saved state must be readable");
	SELFTEST_CHECK(Migrated.m_wVersion == SPAWN_VERSION, "saved-state version must not leak into runtime updates");
	NET_Packet CurrentUpdate;
	Server.UPDATE_Write(CurrentUpdate);
	Migrated.UPDATE_Read(CurrentUpdate);
	SELFTEST_CHECK(CurrentUpdate.r_eof(), "migrated server must consume the full current update");

	CSE_ALifeItemWeaponMagazined FileObject("wpn_ak74");
	SELFTEST_CHECK(FileObject.Spawn_ReadWithOptions(LegacySpawn, false), "file wrapper must read legacy saved state");
	SELFTEST_CHECK(FileObject.m_wVersion == LegacyVersion, "file wrapper must retain the legacy version for its update");
	NET_Packet LegacyUpdate;
	{
		SpawnWrite::ScopedVersion legacyUpdateVersion(LegacyVersion);
		Server.UPDATE_Write(LegacyUpdate);
	}
	LegacyUpdate.B.data.resize(LegacyUpdate.B.data.size() - (sizeof(u8) + sizeof(float) + 2 * sizeof(u8) + sizeof(u16)));
	FileObject.UPDATE_Read(LegacyUpdate);
	SELFTEST_CHECK(LegacyUpdate.r_eof(), "legacy update was not consumed with its own version");
	FileObject.m_wVersion = SPAWN_VERSION;
	NET_Packet PromotedUpdate;
	Server.UPDATE_Write(PromotedUpdate);
	FileObject.UPDATE_Read(PromotedUpdate);
	SELFTEST_CHECK(PromotedUpdate.r_eof(), "promoted file object must consume current updates");

	CSE_ALifeItemWeaponMagazined Client("wpn_ak74");
	SELFTEST_CHECK(Client.Spawn_Read(Packet), "spawn header must be readable");
	Msg("* [selftest] spawn protocol: server version %u, client version %u, packet %u bytes, update offset %u", Server.m_wVersion, Client.m_wVersion, u32(Packet.B.data.size()), Packet.r_tell());
	SELFTEST_CHECK(Client.m_wVersion == SPAWN_VERSION, "network version must replace the level-file version");
	SELFTEST_CHECK(Client.ID == Server.ID, "spawn ID mismatch");
	Client.UPDATE_Read(Packet);
	SELFTEST_CHECK(Packet.r_eof(), "update reader left %u of %u bytes", u32(Packet.B.data.size()) - Packet.r_tell(), u32(Packet.B.data.size()));
	SELFTEST_CHECK(Client.a_elapsed.MagazineElapsed == 17, "magazine data mismatch");
	SELFTEST_CHECK(Client.m_u8CurFireMode == 2, "fire mode mismatch");
	SELFTEST_CHECK(Client.misfire != FALSE && Client.rt_zoom_factor == 3.25f, "current-version weapon fields were not read");
	SELFTEST_CHECK(Client.cur_scope == 1 && Client.chamber_ammo_type == 1 && Client.a_chamber_elapsed == 9, "weapon update tail mismatch");
	return true;
}

SELFTEST(spawn_v134_legacy_ids, false)
{
	CSE_ALifeDynamicObject server("actor");
	server.ID = 0x12345;
	server.m_tGraphID = 7;
	server.m_script_story_ID = "";
	server.s_flags.assign(M_SPAWN_UPDATE);
	NET_Packet packet;
	server.Spawn_Write(packet, true);
	server.UPDATE_Write(packet);

	const u32 sizeOffset = SelfTestDetail::SpawnStateSizeOffset(packet);
	SELFTEST_CHECK(sizeOffset != 0, "current spawn state size field must be found");
	u16 stateSize = 0;
	memcpy(&stateSize, packet.B.data.data() + sizeOffset, sizeof(stateSize));
	SELFTEST_CHECK(sizeOffset + stateSize == packet.B.data.size(), "test state must have an empty script-story terminator");
	SELFTEST_CHECK(packet.B.data.data()[sizeOffset + stateSize - 1] == 0, "empty script-story id must terminate the legacy state");

	constexpr u16 LegacyVersion = 134;
	NET_Packet adjusted;
	{
		SpawnWrite::ScopedVersion legacyWriter(LegacyVersion);
		server.Spawn_Write(adjusted, true);
	}
	const u16 adjustedStateSize = stateSize - 1;
	const u32 legacySizeOffset = sizeOffset - sizeof(u16);
	memcpy(adjusted.B.data.data() + legacySizeOffset, &adjustedStateSize, sizeof(adjustedStateSize));

	CSE_ALifeDynamicObject loaded("actor");
	SELFTEST_CHECK(loaded.Spawn_ReadWithOptions(adjusted, false), "version 134 high-word IDs must be read");
	SELFTEST_CHECK(loaded.m_wVersion == LegacyVersion, "the saved version must be retained for the adjacent update");
	SELFTEST_CHECK(loaded.ID == server.ID, "the 32-bit actor ID must survive version 134 loading");
	SELFTEST_CHECK(loaded.m_tGraphID == server.m_tGraphID, "the actor graph vertex must not be shifted");
	SELFTEST_CHECK(adjusted.r_eof(), "the legacy spawn must consume its complete packet: pos=%u size=%u expected=%u", adjusted.r_tell(), u32(adjusted.B.data.size()), legacySizeOffset + adjustedStateSize);
	return true;
}

SELFTEST(saved_game_wrapper, false)
{
	if (!CSavedGameWrapper::saved_game_exist("viewer_parallel_autosave"))
	{
		Msg("* [selftest] saved_game_wrapper: optional viewer_parallel_autosave is absent, skipped");
		return true;
	}
	CSavedGameWrapper wrapper("viewer_parallel_autosave");
	Msg("* [selftest] saved_game_wrapper: level=%s, id=%d", wrapper.level_name(), wrapper.level_id());
	SELFTEST_CHECK(wrapper.level_id() != GameGraph::_LEVEL_ID(-1), "saved actor graph mapping must be valid");
	SELFTEST_CHECK(0 == xr_strcmp(wrapper.level_name(), "zaton"), "the controlled save must map to zaton");
	return true;
}

SELFTEST(talk_partner_lifetime, false)
{
	class CTalkPartnerProbe final : public CInventoryOwner
	{
	public:
		bool unlimited_ammo() override { return false; }
		bool infinite_fire() override { return false; }
	};
	CTalkPartnerProbe first;
	CTalkPartnerProbe second;
	first.StartTalk(&second);
	second.StartTalk(&first);
	first.DetachTalkPartner();
	SELFTEST_CHECK(!first.IsTalking() && first.GetTalkPartner() == nullptr, "the destroyed speaker must leave dialogue locally");
	SELFTEST_CHECK(!second.IsTalking() && second.GetTalkPartner() == nullptr, "a surviving speaker must not retain a dangling partner pointer");
	return true;
}

SELFTEST(restriction_cleanup_lifetime, false)
{
	CSpaceRestrictionManager manager;
	manager.restrict(42, "", "");
	manager.clear();
	manager.unrestrict(42);
	return true;
}

SELFTEST(missing_rocket_cleanup, false)
{
	CRocketLauncher launcher;
	launcher.AttachRocket(ALife::INVALID_OBJECT_ID, nullptr);
	launcher.DetachRocket(ALife::INVALID_OBJECT_ID, false);
	SELFTEST_CHECK(launcher.getRocketCount() == 0, "absent rockets must not create a tracked attachment");
	return true;
}

// ---------------------------------------------------------------------------
// console commands
// ---------------------------------------------------------------------------
namespace SelfTest
{

class CCC_LevelSelfTest : public IConsole_Command
{
public:
	CCC_LevelSelfTest(const char* N) : IConsole_Command(N)
	{
		bEmptyArgsHandled = true;
	}
	virtual void Execute(const char* args)
	{
		const int Failed = RunAll(true, args);
		if (Failed == 0)
			Msg("* level_selftest: all tests passed");
		else
			Msg("! level_selftest: %d test(s) FAILED", Failed);
	}
	virtual void Info(TInfo& I)
	{
		xr_strcpy(I, "run the sandbox/level-resource self tests");
	}
};

class CCC_SandboxBuild : public IConsole_Command
{
public:
	CCC_SandboxBuild(const char* N) : IConsole_Command(N)
	{
	}
	virtual void Execute(const char* args)
	{
		if (args == nullptr || args[0] == 0)
		{
			Msg("! sandbox_build <flat|sine|slope|crater|steps|prebuilt:<level>> [resources_mask]");
			return;
		}

		string64 Preset;
		_GetItem(args, 0, Preset, ' ');

		Sandbox::STerrainParams Terrain;
		string64 Folder;
		if (0 == xr_strcmp(Preset, "flat"))
		{
			Terrain.Kind = Sandbox::ETerrainKind::Flat;
			xr_strcpy(Folder, "viewer_flat");
		}
		else if (0 == strncmp(Preset, "prebuilt:", 9) && Preset[9])
		{
			Terrain.Kind = Sandbox::ETerrainKind::PrebuiltLevel;
			Terrain.PrebuiltLevel = Preset + 9;
			xr_sprintf(Folder, "viewer_prebuilt_%s", Preset + 9);
		}
		else if (Sandbox::AnalyticPresetFromString(Preset, Terrain.Analytic) && Terrain.Analytic != Sandbox::EAnalyticPreset::Plane)
		{
			Terrain.Kind = Sandbox::ETerrainKind::Analytic;
			xr_sprintf(Folder, "viewer_%s", Preset);
		}
		else
		{
			Msg("! sandbox_build: unknown preset '%s'", Preset);
			return;
		}

		// Optional mask: everything after the first token.
		Flags32 Mask;
		const char* MaskBegin = strchr(args, ' ');
		if (MaskBegin && MaskBegin[1])
		{
			if (!LevelResourceMaskFromString(MaskBegin + 1, Mask))
			{
				Msg("! sandbox_build: bad resources mask '%s'", MaskBegin + 1);
				return;
			}
		}
		else
		{
			Mask.zero();
			Mask.set(u32(ELevelResource::Collision) | u32(ELevelResource::AiMap) | u32(ELevelResource::PatrolPaths), true);
		}

		SLevelDesc Desc;
		Desc.Resources = Mask;
		const Flags32 Effective = Desc.Validate();

		if (!Sandbox::BuildLevel(Terrain, Folder, Effective))
		{
			Msg("! sandbox_build: build failed");
			return;
		}

		Msg("* sandbox_build: wrote level '%s' with resources [%s]:", Folder, LevelResourceMaskToString(Effective).c_str());
		string_path LevelDir;
		FS.update_path(LevelDir, _game_levels_, Folder);
		std::error_code Ec;
		for (const std::filesystem::directory_entry& Entry : std::filesystem::directory_iterator(LevelDir, Ec))
			Msg("*   %s", Entry.path().filename().string().c_str());
	}
	virtual void Info(TInfo& I)
	{
		xr_strcpy(I, "<flat|sine|slope|crater|steps|prebuilt:<level>> [resources_mask] - build a sandbox level bundle");
	}
};

class CCC_SandboxRemove : public IConsole_Command
{
public:
	CCC_SandboxRemove(const char* N) : IConsole_Command(N)
	{
	}
	virtual void Execute(const char* args)
	{
		string64 Folder;
		_GetItem(args, 0, Folder, ' ');
		if (Folder[0] == 0)
		{
			Msg("! sandbox_remove <foldername>");
			return;
		}
		Sandbox::RemoveLevel(Folder);
		Msg("* sandbox_remove: removed '%s'", Folder);
	}
	virtual void Info(TInfo& I)
	{
		xr_strcpy(I, "<foldername> - remove a generated sandbox level");
	}
};

void RegisterConsoleCommands()
{
	CMD1(CCC_LevelSelfTest, "level_selftest");
	CMD1(CCC_SandboxBuild, "sandbox_build");
	CMD1(CCC_SandboxRemove, "sandbox_remove");
}

} // namespace SelfTest
