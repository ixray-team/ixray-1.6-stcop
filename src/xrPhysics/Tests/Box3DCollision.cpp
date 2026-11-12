#include "StdAfx.h"
#include "Box3D/B3World.h"
#include "Box3D/B3Math.h"
#include "IPHCharacter.h"
#include "../../xrCore/Collision/xr_area.h"
#include "../../xrCore/Collision/ISpatial.h"
#include "../../xrEngine/GameMtlLib.h"
#include "../../xrEngine/device.h"

namespace
{
u32 g_Failures = 0;
u32 g_ShotMarks = 0;

bool Check(bool Value, const char* Description)
{
	if (!Value)
	{
		std::fprintf(stderr, "FAIL: %s\n", Description);
		++g_Failures;
	}
	return Value;
}

void ShotMark(const CDB::TRI& Tri, Fvector* Verts, const SPHShotMarkContact& Contact)
{
	++g_ShotMarks;
	Check(Tri.material == 1 && Contact.IsCharacter, "character passable callback uses the source material");
	Check(Verts[0].x >= 22.f && Verts[0].y == 1.f, "shot mark vertices include the instance transform");
}

void AddMaterial(const char* Name, u32 Flags)
{
	auto* Material = new SGameMtl();
	Material->ID = int(GMLibrary().GetMaterials().size());
	Material->m_Name = Name;
	Material->Flags.assign(Flags);
	Material->fPHFriction = 1.f;
	Material->fBounceDamageFactor = 0.f;
	Material->fFlotationFactor = 0.5f;
	GMLibrary().GetMaterials().push_back(Material);
}

void SetTriangle(CDB::TRI& Tri, u32 Base, u16 Material, u32 StreamingSector = 0)
{
	Tri.verts[0] = Base;
	Tri.verts[1] = Base + 1;
	Tri.verts[2] = Base + 2;
	Tri.dummy = 0;
	Tri.material = Material;
	Tri.StreamedSectorID = StreamingSector;
}

void AddInstance(XRay::CForm::IFormat& Format, const char* Name, CDB::MODEL& Prototype, const Fmatrix& Transform)
{
	Fbox Bounds;
	Bounds.invalidate();
	for (const Fvector& Vertex : Prototype.verts)
	{
		Fvector World;
		Transform.transform_tiny(World, Vertex);
		Bounds.modify(World);
	}
	Format.AddInstanceRef(Name, Transform, Bounds, Prototype, 7, 0);
}

void WriteFixture(const xr_string& Base, bool Streaming)
{
	xr_vector<Fvector> Vertices{
		{-20.f, 0.f, -20.f}, {-20.f, 0.f, 40.f}, {40.f, 0.f, -20.f}, {1.f, 3.f, 1.f}, {1.f, 3.f, 4.f}, {4.f, 3.f, 1.f}, {32.f, 1.f, 2.f}, {32.f, 1.f, 5.f}, {35.f, 1.f, 2.f}
	};
	xr_vector<CDB::TRI> Triangles(3);
	SetTriangle(Triangles[0], 0, 0);
	SetTriangle(Triangles[1], 3, 0, 99);
	SetTriangle(Triangles[2], 6, 2);
	xr_unique_ptr<XRay::CForm::IFormat> Format;
	if (Streaming)
	{
		Format.reset(new XRay::CForm::CFormatStreamedInstanced(10.f));
	}
	else
	{
		Format.reset(new XRay::CForm::CFormatInstanced());
	}
	Format->AddStaticGeom(Vertices, Triangles);
	CDB::MODEL Prototype;
	Prototype.verts = {{0.f, 0.f, 0.f}, {0.f, 0.f, 3.f}, {3.f, 0.f, 0.f}};
	Prototype.tris.resize(1);
	SetTriangle(Prototype.tris[0], 0, 0);
	Fmatrix Transform{Fmatrix::EIdentity::Identity};
	Transform.i.set(0.f, 0.f, -2.f);
	Transform.k.set(3.f, 0.f, 0.f);
	Transform.c.set(2.f, 5.f, 2.f);
	AddInstance(*Format, "solid-instance", Prototype, Transform);
	CDB::MODEL Liquid;
	Liquid.verts = Prototype.verts;
	Liquid.tris.resize(1);
	SetTriangle(Liquid.tris[0], 0, 1);
	Transform.identity();
	Transform.c.set(22.f, 1.f, 2.f);
	AddInstance(*Format, "liquid-instance", Liquid, Transform);
	Check(Format->Write(Base.c_str()), "write collision fixture");
}

void TestWorld(const xr_string& Base, bool Streaming)
{
	const s32 NativeBytesBefore = b3GetByteCount();
	WriteFixture(Base, Streaming);
	CObjectSpace Space;
	Space.Load(nullptr, Base.c_str(), nullptr);
	CB3World World;
	g_B3World = &World;
	World.Create(false, &Space, nullptr);
	SB3RayHit Hit;
	const Fvector Down{0.f, -1.f, 0.f};
	const Fvector AboveInstance{3.5f, 10.f, -1.f};
	if (Streaming)
	{
		Check(!World.RayPick(AboveInstance, Down, 20.f, B3Category::Level, Hit), "unloaded collision has no native shapes");
		Check(Space.LoadStreamedTile(0, 0), "load initial tile");
	}
	if (Check(World.RayPick(AboveInstance, Down, 20.f, B3Category::Level, Hit), "ray hits transformed instance"))
	{
		Check(!World.IsStaticGeometryPending(), "first nonempty collision snapshot is initialized synchronously");
		Check(std::abs(Hit.Distance - 5.f) < EPS, "nearest hit has world-space distance");
		Check(Hit.Triangle.Valid() && Hit.ModelOwner && Hit.Triangle.model != Hit.ModelOwner.get(), "ray identifies and pins the prototype inside its owning tile");
		Fvector Verts[3];
		Hit.Triangle.GetVerts(Verts);
		Check(Verts[0].similar(Fvector{2.f, 5.f, 2.f}) && Verts[1].similar(Fvector{11.f, 5.f, 2.f}) && Verts[2].similar(Fvector{2.f, 5.f, -4.f}), "instance rotation and nonuniform scale survive native cooking");
		Check(Hit.Normal.y > 0.99f, "native winding preserves the upward normal");
	}
	SB3RayHit Saved = Hit;
	const b3ShapeId OriginalShape = Hit.Shape;
	b3BodyDef BodyDef = b3DefaultBodyDef();
	BodyDef.type = b3_dynamicBody;
	BodyDef.position = B3Math::ToB3(Fvector{AboveInstance.x, 8.f, AboveInstance.z});
	const b3BodyId FallingBody = b3CreateBody(World.Id(), &BodyDef);
	b3ShapeDef ShapeDef = b3DefaultShapeDef();
	ShapeDef.filter.categoryBits = B3Category::Dynamic;
	ShapeDef.filter.maskBits = B3Category::Level;
	const b3Sphere Sphere{b3Vec3_zero, 0.25f};
	b3CreateSphereShape(FallingBody, &ShapeDef, &Sphere);
	for (u32 Step = 0; Step < 100; ++Step)
	{
		World.Step();
	}
	Check(std::abs(B3Math::FromB3(b3Body_GetPosition(FallingBody)).y - 5.25f) < 0.1f, "dynamic body rests on the transformed instance");
	if (Streaming)
	{
		Check(Space.LoadStreamedTile(0, -1), "load duplicate boundary geometry");
		World.SyncStaticGeometry();
		Check(World.IsStaticGeometryPending(), "subsequent snapshots are prepared as background tasks");
		Check(b3Shape_IsValid(OriginalShape), "old native shapes stay alive while replacement data is prepared");
		World.WaitForStaticGeometry();
		Check(World.RayPick(AboveInstance, Down, 20.f, B3Category::Level, Hit) && B3_ID_EQUALS(Hit.Shape, OriginalShape), "unchanged tiles keep their native shape");
		const b3BodyId LevelBody = b3Shape_GetBody(Hit.Shape);
		Check(b3Body_GetShapeCount(LevelBody) == 1, "boundary duplicates do not create duplicate native collision");
		Check(Space.UnloadStreamedTile(0, 0), "unload original boundary owner");
		World.SyncStaticGeometry();
		Check(World.IsStaticGeometryPending() && b3Shape_IsValid(OriginalShape), "boundary ownership changes keep the old support until publication");
		World.WaitForStaticGeometry();
		Check(World.RayPick(AboveInstance, Down, 20.f, B3Category::Level, Hit), "remaining tile takes over boundary collision");
		Check(!b3Shape_IsValid(OriginalShape) && Saved.Triangle.Valid(), "unloaded shape is destroyed but saved CDB result remains alive");
		Check(Space.LoadStreamedSector(99), "load manual sector");
		World.SyncStaticGeometry();
		Check(World.IsStaticGeometryPending(), "manual sector changes use background cooking too");
		Check(Space.UnloadStreamedSector(99), "invalidate a pending sector update");
		World.WaitForStaticGeometry();
		Check(World.RayPick(Fvector{1.2f, 10.f, 1.2f}, Down, 20.f, B3Category::Level, Hit) && std::abs(Hit.Distance - 10.f) < EPS, "stale cooked snapshots cannot resurrect an unloaded sector");
		Check(Space.LoadStreamedSector(99), "reload manual sector after discarding stale work");
		World.WaitForStaticGeometry();
		Check(World.RayPick(Fvector{1.2f, 10.f, 1.2f}, Down, 20.f, B3Category::Level, Hit) && std::abs(Hit.Distance - 7.f) < EPS, "manual sector participates in native queries");
		Check(Space.LoadStreamedTile(2, 0) && Space.LoadStreamedTile(3, 0), "load passable and actor-obstacle geometry");
		World.WaitForStaticGeometry();
		for (u32 Iteration = 0; Iteration < 4; ++Iteration)
		{
			Check(Space.UnloadStreamedTile(2, 0), "churn unloads the liquid tile");
			World.SyncStaticGeometry();
			Check(World.IsStaticGeometryPending(), "churn starts only one pending snapshot");
			Check(Space.LoadStreamedTile(2, 0), "churn reloads a new CDB model before native preparation is published");
			World.Step();
			World.WaitForStaticGeometry();
			Check(World.RayPick(Fvector{22.5f, 10.f, 2.5f}, Down, 20.f, B3Category::Liquid, Hit) && Hit.ModelOwner == Space.GetStaticStreamedTileModel(Fvector{22.5f, 0.f, 2.5f}), "churn publishes only the latest model and preserves its source owner");
		}
	}
	Check(World.RayPick(Fvector{22.5f, 10.f, 2.5f}, Down, 20.f, B3Category::Liquid, Hit) && Hit.Material == 1 && Hit.Triangle.Valid(), "liquid ray maps to an instanced triangle");
	Check(!World.RayPick(Fvector{32.5f, 10.f, 2.5f}, Down, 20.f, B3Category::Level, Hit), "actor obstacle is not a solid level shape");
	Check(World.RayPick(Fvector{32.5f, 10.f, 2.5f}, Down, 20.f, B3Category::ActorObstacle, Hit) && Hit.Material == 2, "actor obstacle preserves its material and triangle mapping");
	World.set_default_character_contact_shotmark(&ShotMark);
	IPHCharacter* Character = create_actor_character(true);
	Character->SetMaterial(0);
	Character->SetPosition(Fvector{22.5f, 0.f, 2.5f});
	Character->Create(Fvector{0.6f, 1.8f, 0.6f});
	Character->SetApplyGravity(false);
	Character->SetVelocity(Fvector{0.2f, 0.f, 0.f});
	const u32 MarksBefore = g_ShotMarks;
	Character->step(0.02f);
	Check(Character->InjuriousMaterialIDX() == 1 && g_ShotMarks > MarksBefore, "character detects transformed passables and calls the ODE-compatible shot-mark interface");
	Character->Destroy();
	xr_delete(Character);
	if (Streaming)
	{
		Space.UnloadAllStreamedTiles();
		World.WaitForStaticGeometry();
		Check(!World.RayPick(AboveInstance, Down, 20.f, B3Category::Level, Hit) && !Hit.Triangle.Valid() && !Hit.ModelOwner, "empty query resets the previous prototype result");
		Check(b3Body_IsAwake(FallingBody), "removing a supporting shape wakes its body");
		for (u32 Step = 0; Step < 100; ++Step)
		{
			World.Step();
		}
		Check(B3Math::FromB3(b3Body_GetPosition(FallingBody)).y < 4.f, "unloading native collision wakes a resting body");
		Check(World.RayPick(Fvector{1.2f, 10.f, 1.2f}, Down, 20.f, B3Category::Level, Hit), "unloading automatic tiles preserves manual collision");
		Check(Space.UnloadStreamedSector(99), "unload manual sector");
		World.WaitForStaticGeometry();
		Check(!World.RayPick(Fvector{1.2f, 10.f, 1.2f}, Down, 20.f, B3Category::Level, Hit), "native world drops the last unloaded sector");
	}
	b3DestroyBody(FallingBody);
	if (Streaming)
	{
		const b3WorldId OldWorld = World.Id();
		Check(Space.LoadStreamedTile(2, 0), "queue collision work before world destruction");
		World.SyncStaticGeometry();
		Check(World.IsStaticGeometryPending(), "world teardown test has an outstanding task");
		World.Destroy();
		Check(!World.IsStaticGeometryPending() && !b3World_IsValid(OldWorld), "destroy joins cooking tasks before releasing the native world");
		World.Create(false, &Space, nullptr);
		Check(!World.IsStaticGeometryPending() && World.RayPick(Fvector{22.5f, 10.f, 2.5f}, Down, 20.f, B3Category::Liquid, Hit), "recreated worlds synchronously initialize resident tiles without stale task state");
	}
	World.Destroy();
	Check(b3GetByteCount() == NativeBytesBefore, "world destruction releases all native meshes, including stale and cancelled cooking results");
	g_B3World = nullptr;
}

void TestCookedTriangleOrder(const xr_string& Base)
{
	const s32 NativeBytesBefore = b3GetByteCount();
	xr_vector<Fvector> Vertices;
	xr_vector<CDB::TRI> Triangles;
	// Deliberately not spatially sorted: native BVH cooking reorders these faces.
	const float Positions[]{21.f, 3.f, 33.f, 9.f, 27.f, 15.f, 39.f, -3.f};
	for (float X : Positions)
	{
		const u32 BaseVertex = u32(Vertices.size());
		Vertices.push_back(Fvector{X, 2.f, 0.f});
		Vertices.push_back(Fvector{X, 2.f, 2.f});
		Vertices.push_back(Fvector{X + 2.f, 2.f, 0.f});
		SetTriangle(Triangles.emplace_back(), BaseVertex, 0);
	}
	XRay::CForm::CFormatInstanced Format;
	Format.AddStaticGeom(Vertices, Triangles);
	Check(Format.Write(Base.c_str()), "write native triangle ordering fixture");
	CObjectSpace Space;
	Space.Load(nullptr, Base.c_str(), nullptr);
	CB3World World;
	g_B3World = &World;
	World.Create(false, &Space, nullptr);
	for (u32 Index = 0; Index < std::size(Positions); ++Index)
	{
		SB3RayHit Hit;
		if (Check(World.RayPick(Fvector{Positions[Index] + 0.25f, 10.f, 0.25f}, Fvector{0.f, -1.f, 0.f}, 20.f, B3Category::Level, Hit), "native ray hits a spatially sorted mesh face"))
		{
			Check(Hit.Triangle.Valid() && Hit.Triangle.tris_id == Index, "cooked triangle indices map back to the correct CDB face");
		}
	}
	World.Destroy();
	Check(b3GetByteCount() == NativeBytesBefore, "world destruction releases all native meshes, including stale and cancelled cooking results");
	g_B3World = nullptr;
}
} // namespace

int main(int Count, char** Arguments)
{
	if (Count != 2)
	{
		std::fprintf(stderr, "Usage: TestBox3DCollision <empty fixture directory>\n");
		return 2;
	}
	const auto Directory = std::filesystem::absolute(Arguments[1]);
	std::filesystem::create_directories(Directory);
	if (!std::filesystem::is_empty(Directory))
	{
		std::fprintf(stderr, "Fixture directory must be empty\n");
		return 2;
	}
	Core._initialize("Box3DCollisionTests", [](const char* Text)
					 { std::fprintf(stderr, "%s\n", Text); },
					 false);
	FS._initialize(CLocatorAPI::flTargetFolderOnly, Directory.string().c_str());
	DevicePtr = new CRenderDevice();
	Device.fTimeDelta = 0.02f;
	Device.dwTimeGlobal = 0;
	Device.dwFrame = 0;
	g_SpatialSpace = new ISpatial_DB();
	g_SpatialSpacePhysic = new ISpatial_DB();
	AddMaterial("default", 0);
	AddMaterial("test-liquid", SGameMtl::flPassable | SGameMtl::flLiquid | SGameMtl::flSlowDown | SGameMtl::flInjurious);
	AddMaterial("test-obstacle", SGameMtl::flPassable | SGameMtl::flActorObstacle);
	{
		const xr_string Base = (Directory / "collision").string().c_str();
		TestWorld(Base + "_instanced", false);
		TestWorld(Base + "_streamed", true);
		TestCookedTriangleOrder(Base + "_triangle_order");
	}
	GMLibrary().Unload();
	xr_delete(g_SpatialSpace);
	xr_delete(g_SpatialSpacePhysic);
	xr_delete(DevicePtr);
	std::printf("Box3D collision: %u failure(s)\n", g_Failures);
	Core._destroy();
	return g_Failures == 0 ? 0 : 1;
}
