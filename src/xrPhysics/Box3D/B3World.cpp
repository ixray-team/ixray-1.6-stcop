#include "StdAfx.h"
#include "B3World.h"
#include "B3Math.h"
#include "PHContact.h"
#include "console_vars.h"
#include "../../xrCore/Collision/xrCDB.h"
#include "../../xrCore/Collision/xr_area.h"
#include "../../xrEngine/GameMtlLib.h"
#include "../../xrEngine/device.h"
#include "../../xrCore/Collision/ISpatial.h"

extern Fbox phBoundaries;

CB3World* g_B3World = nullptr;

static struct SEmptyUpdateCallback : public IPHWorldUpdateCallbck
{
	virtual void update_step() override {}
	virtual void phys_shell_relcase(CPhysicsShell* Shell) override {}
} g_EmptyUpdateCallback;

IPHWorld* physics_world()
{
	return g_B3World;
}

void create_physics_world(bool mt, CObjectSpace* os, CObjectList* lo)
{
	VERIFY(os);
	g_B3World = new CB3World();
	g_B3World->Create(mt, os, lo);
}

void destroy_physics_world()
{
	if (g_B3World == nullptr)
	{
		return;
	}

	g_B3World->Destroy();
	xr_delete(g_B3World);
}

CObjectSpace* create_object_space()
{
	CObjectSpace* os = new CObjectSpace();
	g_SpatialSpace = new ISpatial_DB();
	g_SpatialSpacePhysic = new ISpatial_DB();
	os->Load(nullptr, "ActorEditorLevel", nullptr);
	return os;
}

CObjectSpace* mesh_create_object_space(const XRay::CForm::IFormat& CForm, CDB::build_callback build_callback)
{
	CObjectSpace* os = new CObjectSpace();
	g_SpatialSpace = new ISpatial_DB();
	g_SpatialSpacePhysic = new ISpatial_DB();
	os->Create(CForm, build_callback, nullptr, false);
	return os;
}

void destroy_object_space(CObjectSpace*& os)
{
	xr_delete(os);
}

CB3World::CB3World() :
	UpdateCallback(&g_EmptyUpdateCallback)
{
}

CB3World::~CB3World()
{
	Destroy();
}

void CB3World::Create(bool Mt, CObjectSpace* InObjectSpace, CObjectList* InLevelObjects)
{
	ObjectSpacePtr = InObjectSpace;
	LevelObjects = InLevelObjects;

	b3WorldDef Def = b3DefaultWorldDef();
	Def.gravity = B3Math::ToB3(Fvector().set(0.f, -GravityValue, 0.f));
	Def.maximumLinearSpeed = default_l_limit;
	World = b3CreateWorld(&Def);
	b3World_SetPreSolveCallback(World, &CB3World::PreSolve, this);
	b3World_SetCustomFilterCallback(World, &CB3World::CustomFilter, this);

	auto DefaultIt = GMLibrary().GetMaterialIt("default");
	DefaultMaterial = DefaultIt == GMLibrary().GetMaterials().end() ? 0 : u16(DefaultIt - GMLibrary().FirstMaterial());

	CreateStaticMesh(ObjectSpacePtr->GetStaticVerts().data(), ObjectSpacePtr->GetStaticVerts().size(),
		ObjectSpacePtr->GetStaticTris().data(), ObjectSpacePtr->GetStaticTris().size());

	phBoundaries.set(ObjectSpacePtr->GetBoundingVolume());
	phBoundaries.y1 -= 30.f;

	static_cast<IRenderDevice&>(Device).AddSeqFrame(this, Mt);
	Exists = true;

	StepNumIterations(phIterations);
	SetStep(ph_console::ph_step_time);
	Msg("* [Box3D] physics world created");
}

void CB3World::Destroy()
{
	if (!Exists)
	{
		return;
	}

	static_cast<IRenderDevice&>(Device).RemoveSeqFrame(this);
	DestroyStaticMesh();
	Ground = b3_nullBodyId;
	if (b3World_IsValid(World))
	{
		b3DestroyWorld(World);
	}
	World = b3_nullWorldId;
	Exists = false;
}

bool CB3World::CreateStaticMesh(const Fvector* Verts, size_t VertCount, const CDB::TRI* Tris, size_t TriCount)
{
	DestroyStaticMesh();
	if (VertCount < 3 || TriCount < 1)
	{
		return false;
	}

	xr_vector<b3Vec3> Vertices(VertCount);
	for (size_t I = 0; I < VertCount; ++I)
	{
		Vertices[I] = B3Math::ToB3(Verts[I]);
	}

	b3BodyDef BodyDef = b3DefaultBodyDef();
	BodyDef.type = b3_staticBody;
	StaticBody = b3CreateBody(World, &BodyDef);

	const bool Solid = BuildMesh(Vertices, Tris, TriCount, false);
	BuildMesh(Vertices, Tris, TriCount, true);
	return Solid;
}

// Solid mesh: every non-passable triangle. Liquid mesh: passable liquid triangles, which in ODE
// make contacts that never collide but splash and slow bodies down.
bool CB3World::BuildMesh(const xr_vector<b3Vec3>& Vertices, const CDB::TRI* Tris, size_t TriCount, bool Liquid)
{
	// Box3D takes up to 256 materials per shape: map game materials to slots.
	xr_vector<b3SurfaceMaterial> Materials;
	xr_hash_map<u32, u8> MaterialSlots;
	xr_vector<int32_t> Indices;
	xr_vector<uint8_t> MaterialIndices;
	xr_vector<int>& ToCdb = Liquid ? LiquidToCdbTriangle : MeshToCdbTriangle;
	ToCdb.clear();
	for (size_t I = 0; I < TriCount; ++I)
	{
		const u32 GameMaterial = Tris[I].material;
		SGameMtl* Mtl = GMLibrary().GetMaterialByIdx(u16(GameMaterial));
		const bool Passable = Mtl && Mtl->Flags.test(SGameMtl::flPassable);
		const bool IsLiquid = Passable && Mtl->Flags.test(SGameMtl::flLiquid);
		if (Liquid ? !IsLiquid : Passable)
		{
			continue;
		}

		auto It = MaterialSlots.find(GameMaterial);
		if (It == MaterialSlots.end())
		{
			if (Materials.size() >= 256)
			{
				Msg("! [Box3D] level uses more than 256 materials, material %u falls back to slot 0", GameMaterial);
				It = MaterialSlots.emplace(GameMaterial, u8(0)).first;
			}
			else
			{
				b3SurfaceMaterial Material = b3DefaultSurfaceMaterial();
				Material.userMaterialId = GameMaterial;
				if (Mtl)
				{
					Material.friction = Mtl->fPHFriction;
					Material.restitution = Mtl->Flags.test(SGameMtl::flBounceable) ? Mtl->fPHBouncing : 0.f;
				}
				It = MaterialSlots.emplace(GameMaterial, u8(Materials.size())).first;
				Materials.push_back(Material);
			}
		}

		// Mirroring Z reverses the winding.
		Indices.push_back(int32_t(Tris[I].verts[0]));
		Indices.push_back(int32_t(Tris[I].verts[2]));
		Indices.push_back(int32_t(Tris[I].verts[1]));
		MaterialIndices.push_back(It->second);
		ToCdb.push_back(int(I));
	}

	if (MaterialIndices.empty())
	{
		return false;
	}

	b3MeshDef MeshDef = {};
	MeshDef.vertices = const_cast<b3Vec3*>(Vertices.data());
	MeshDef.vertexCount = int(Vertices.size());
	MeshDef.indices = Indices.data();
	MeshDef.triangleCount = int(MaterialIndices.size());
	MeshDef.materialIndices = MaterialIndices.data();
	MeshDef.identifyEdges = !Liquid;
	// Median split builds the tree much faster than SAH on a level-sized mesh.
	MeshDef.useMedianSplit = true;

	b3MeshData* Mesh = b3CreateMesh(&MeshDef, nullptr, 0);
	if (Mesh == nullptr)
	{
		Msg("! [Box3D] failed to build the %s mesh", Liquid ? "liquid" : "level");
		return false;
	}

	b3ShapeDef ShapeDef = b3DefaultShapeDef();
	ShapeDef.materials = Materials.data();
	ShapeDef.materialCount = int(Materials.size());
	// Box3D never calls pre-solve for mesh contacts, so liquid cannot be a disabled contact:
	// it takes part only in queries (see ProcessLiquidContacts).
	ShapeDef.filter.categoryBits = Liquid ? B3Category::Liquid : B3Category::Level;
	ShapeDef.filter.maskBits = Liquid ? B3Category::LiquidQuery : B3Category::Dynamic;
	const b3ShapeId Shape = b3CreateMeshShape(StaticBody, &ShapeDef, Mesh, b3Vec3{1.f, 1.f, 1.f});
	if (Liquid)
	{
		LiquidMesh = Mesh;
		LiquidShape = Shape;
	}
	else
	{
		StaticMesh = Mesh;
	}
	Msg("* [Box3D] %s mesh: %d triangles, %d materials", Liquid ? "liquid" : "level", int(MaterialIndices.size()), int(Materials.size()));
	return true;
}

void CB3World::DestroyStaticMesh()
{
	if (b3Body_IsValid(StaticBody))
	{
		b3DestroyBody(StaticBody);
	}
	StaticBody = b3_nullBodyId;
	LiquidShape = b3_nullShapeId;

	if (StaticMesh != nullptr)
	{
		b3DestroyMesh(StaticMesh);
		StaticMesh = nullptr;
	}
	if (LiquidMesh != nullptr)
	{
		b3DestroyMesh(LiquidMesh);
		LiquidMesh = nullptr;
	}
}

struct SB3RayContext
{
	SB3RayHit* Hit;
	bool Found = false;
};

static float RayCallback(b3ShapeId ShapeId, b3Pos Point, b3Vec3 Normal, float Fraction, uint64_t UserMaterialId, int TriangleIndex, int ChildIndex, void* Context)
{
	SB3RayContext& Ray = *static_cast<SB3RayContext*>(Context);
	Ray.Found = true;
	Ray.Hit->Position = B3Math::FromB3(Point);
	Ray.Hit->Normal = B3Math::FromB3(Normal);
	Ray.Hit->Distance = Fraction;
	Ray.Hit->Material = u16(UserMaterialId);
	Ray.Hit->Triangle = TriangleIndex;
	Ray.Hit->Geom = static_cast<CODEGeom*>(b3Shape_GetUserData(ShapeId));
	Ray.Hit->Shape = ShapeId;
	// Clip the ray to find the closest hit.
	return Fraction;
}

bool CB3World::RayPick(const Fvector& Start, const Fvector& Dir, float Range, u64 Mask, SB3RayHit& Hit) const
{
	Fvector Translation;
	Translation.mul(Dir, Range);

	b3QueryFilter Filter = b3DefaultQueryFilter();
	// Shapes test their mask against the query category: query as a dynamic body.
	Filter.categoryBits = (Mask & B3Category::Ladder) ? B3Category::Ladder : ((Mask & B3Category::Liquid) ? B3Category::LiquidQuery : B3Category::Dynamic);
	Filter.maskBits = Mask;

	SB3RayContext Context{&Hit};
	b3World_CastRay(World, B3Math::ToB3(Start), B3Math::ToB3(Translation), Filter, &RayCallback, &Context);
	if (!Context.Found)
	{
		return false;
	}

	Hit.Distance *= Range;
	const xr_vector<int>& ToCdb = B3_ID_EQUALS(Hit.Shape, LiquidShape) ? LiquidToCdbTriangle : MeshToCdbTriangle;
	if (Hit.Geom == nullptr && Hit.Triangle >= 0 && Hit.Triangle < int(ToCdb.size()))
	{
		Hit.Triangle = ToCdb[Hit.Triangle];
	}
	return true;
}

b3BodyId CB3World::GroundBody()
{
	if (!b3Body_IsValid(Ground))
	{
		b3BodyDef Def = b3DefaultBodyDef();
		Def.type = b3_staticBody;
		Ground = b3CreateBody(World, &Def);
	}
	return Ground;
}

// Elements of one shell collide only with self collision; shells of one collision group never collide.
bool CB3World::CustomFilter(b3ShapeId ShapeA, b3ShapeId ShapeB, void* Context)
{
	CODEGeom* GeomA = static_cast<CODEGeom*>(b3Shape_GetUserData(ShapeA));
	CODEGeom* GeomB = static_cast<CODEGeom*>(b3Shape_GetUserData(ShapeB));
	if (GeomA == nullptr || GeomB == nullptr || GeomA->Owner == nullptr || GeomB->Owner == nullptr)
	{
		return true;
	}

	CPHShell* ShellA = GeomA->Owner->Shell;
	CPHShell* ShellB = GeomB->Owner->Shell;
	if (ShellA == nullptr || ShellB == nullptr)
	{
		return true;
	}
	if (ShellA == ShellB)
	{
		return ShellA->IsSelfCollision();
	}
	return !(ShellA->IsGroupObject() && ShellA->GetCLGroup() == ShellB->GetCLGroup());
}

float CB3World::InterpolationFactor() const
{
	return fixed_step > 0.f ? clampr(FrameTimeValue / fixed_step, 0.f, 1.f) : 1.f;
}

void CB3World::AddShell(CPHShell* Shell)
{
	Shells.PushBack(Shell);
}

void CB3World::RemoveShell(CPHShell* Shell)
{
	Shells.Erase(Shell);
}

void CB3World::AddUpdateObject(CPHUpdateObject* Object)
{
	UpdateObjects.PushBack(Object);
}

void CB3World::RemoveUpdateObject(CPHUpdateObject* Object)
{
	UpdateObjects.Erase(Object);
}

void CB3World::NetRelcase(CPhysicsShell* Shell)
{
	UpdateCallback->phys_shell_relcase(Shell);
	for (CPHUpdateObject* Object : UpdateObjects)
	{
		Object->NetRelcase(Shell);
	}
}

void CB3World::set_update_callback(IPHWorldUpdateCallbck* Callback)
{
	VERIFY(Callback);
	UpdateCallback = Callback;
}

void CB3World::SetGravity(float G)
{
	GravityValue = G;
	if (b3World_IsValid(World))
	{
		b3World_SetGravity(World, B3Math::ToB3(Fvector().set(0.f, -G, 0.f)));
	}
}

void CB3World::SetStep(float S)
{
	fixed_step = S;
	if (Exists)
	{
		const u32 Steps = iFloor(Device.fTimeDelta / fixed_step);
		FrameTimeValue = Device.fTimeDelta - Steps * fixed_step;
	}
}

void CB3World::StepNumIterations(int Iterations)
{
	// ODE iterations map loosely to Box3D sub-steps.
	SubSteps = clampr(Iterations / 4, 1, 8);
}

u32 CB3World::CalcNumSteps(u32 DeltaTime)
{
	if (DeltaTime < FrameTimeValue * 1000)
	{
		return 0;
	}

	return iCeil((float(DeltaTime) - FrameTimeValue * 1000) / (fixed_step * 1000));
}

void CB3World::Freeze()
{
	R_ASSERT2(!IsFrozen, "already freezed!!!");
	IsFrozen = true;
}

void CB3World::UnFreeze()
{
	R_ASSERT2(IsFrozen, "is not freezed!!!");
	IsFrozen = false;
}

void CB3World::OnFrame()
{
	PROF_EVENT("CB3World::OnFrame");
	FrameStep(Device.fTimeDelta);
}

void CB3World::FrameStep(float DeltaTime)
{
	if (IsFrozen)
	{
		return;
	}

	DeltaTime *= phTimefactor;
	float FrameTime = FrameTimeValue + DeltaTime;
	if (FrameTime < fixed_step)
	{
		FrameTimeValue = FrameTime;
		return;
	}

	const u32 StepsNumber = iFloor(FrameTime / fixed_step);
	FrameTimeValue = FrameTime - StepsNumber * fixed_step;

	if (ph_console::g_bDebugDumpPhysicsStep && StepsNumber > 20)
	{
		Msg("!!!TOO MANY PHYSICS STEPS PER FRAME = %u !!!", StepsNumber);
	}

	IsProcessing = true;
	for (u32 I = 0; I < StepsNumber; ++I)
	{
		Step();
	}
	IsProcessing = false;
}

void CB3World::Step()
{
	PROF_EVENT("CB3World::Step");
	++StepCount;

	for (CPHShell* Shell : Shells)
	{
		Shell->PhTune(fixed_step);
	}
	for (CPHUpdateObject* Object : UpdateObjects)
	{
		Object->PhTune(fixed_step);
	}

	UpdateCallback->update_step();

	Stepping = true;
	b3World_Step(World, fixed_step, SubSteps);
	Stepping = false;
	RunDeferred();
	ProcessHitEvents();
	ProcessLiquidContacts();
	ProcessLevelContacts();

	// Joints over their force or torque threshold break; the shell splits on the next game update.
	const b3JointEvents JointEvents = b3World_GetJointEvents(World);
	for (int I = 0; I < JointEvents.count; ++I)
	{
		CPHJoint* Joint = static_cast<CPHJoint*>(JointEvents.jointEvents[I].userData);
		if (Joint && Joint->isBreakable() && Joint->OwnerShell && !Joint->OwnerShell->IsBreakingBlocked())
		{
			Joint->Broken = true;
		}
	}

	for (CPHShell* Shell : Shells)
	{
		Shell->PhDataUpdate(fixed_step);
	}
	for (CPHUpdateObject* Object : UpdateObjects)
	{
		Object->PhDataUpdate(fixed_step);
	}
}

static SGameMtl* MaterialOf(u16 Index)
{
	SGameMtl* Mtl = GMLibrary().GetMaterialByIdx(Index);
	return Mtl ? Mtl : GMLibrary().GetMaterialByIdx(0);
}

static void CallObjectCallbacks(const xr_vector<ObjectContactCallbackFun*>& Callbacks, bool& DoCollide, bool First, CPHContact& Contact, SGameMtl* M1, SGameMtl* M2)
{
	// Callbacks may remove themselves.
	for (size_t I = 0; I < Callbacks.size(); ++I)
	{
		ObjectContactCallbackFun* Callback = Callbacks[I];
		Callback(DoCollide, First, Contact, M1, M2);
	}
}

bool CB3World::PreSolve(b3ShapeId ShapeA, b3ShapeId ShapeB, b3Pos Point, b3Vec3 Normal, void* Context)
{
	CB3World& Self = *static_cast<CB3World*>(Context);


	dContact Native;
	Native.Geom[0] = static_cast<CODEGeom*>(b3Shape_GetUserData(ShapeA));
	Native.Geom[1] = static_cast<CODEGeom*>(b3Shape_GetUserData(ShapeB));
	if (Native.Geom[0] == nullptr && Native.Geom[1] == nullptr)
	{
		return true;
	}

	Native.Position = B3Math::FromB3(Point);
	Native.Normal = B3Math::FromB3(Normal);
	for (int I = 0; I < 2; ++I)
	{
		CODEGeom* Geom = Native.Geom[I];
		Native.Material[I] = Geom && Geom->Owner ? Geom->Owner->Material : Self.DefaultMaterial;
	}

	SGameMtl* M1 = MaterialOf(Native.Material[0]);
	SGameMtl* M2 = MaterialOf(Native.Material[1]);
	if (M1->Flags.test(SGameMtl::flPassable) || M2->Flags.test(SGameMtl::flPassable))
	{
		return false;
	}

	CPHContact Contact(Native);
	bool DoCollide = true;
	for (int I = 0; I < 2; ++I)
	{
		CODEGeom* Geom = Native.Geom[I];
		if (Geom == nullptr)
		{
			continue;
		}

		CallObjectCallbacks(Geom->ObjectCallbacks, DoCollide, I == 0, Contact, M1, M2);
		if (Geom->Owner)
		{
			CallObjectCallbacks(Geom->Owner->ObjectCallbacks, DoCollide, I == 0, Contact, M1, M2);
		}
	}
	return DoCollide;
}

bool CB3World::AllowContact(b3ShapeId ShapeA, b3ShapeId ShapeB, const Fvector& Point, const Fvector& Normal)
{
	const bool WasStepping = Stepping;
	Stepping = true;
	const bool Allow = PreSolve(ShapeA, ShapeB, B3Math::ToB3(Point), B3Math::ToB3(Normal), this);
	Stepping = WasStepping;
	return Allow;
}

void CB3World::ProcessHitEvents()
{
	const b3ContactEvents Events = b3World_GetContactEvents(World);
	for (int I = 0; I < Events.hitCount; ++I)
	{
		const b3ContactHitEvent& Event = Events.hitEvents[I];
		CODEGeom* GeomA = static_cast<CODEGeom*>(b3Shape_GetUserData(Event.shapeIdA));
		CODEGeom* GeomB = static_cast<CODEGeom*>(b3Shape_GetUserData(Event.shapeIdB));

		// Shot marks are for a dynamic geom hitting the level mesh.
		const bool LevelA = GeomA == nullptr && B3_ID_EQUALS(b3Shape_GetBody(Event.shapeIdA), StaticBody);
		const bool LevelB = GeomB == nullptr && B3_ID_EQUALS(b3Shape_GetBody(Event.shapeIdB), StaticBody);
		CODEGeom* Geom = LevelA ? GeomB : (LevelB ? GeomA : nullptr);
		if (Geom == nullptr || Geom->Owner == nullptr || Geom->Owner->ContactCallback == nullptr)
		{
			continue;
		}

		// Normal points from A to B; make it point from the level to the object.
		Fvector Normal = B3Math::FromB3(Event.normal);
		if (LevelB)
		{
			Normal.invert();
		}

		const Fvector Position = B3Math::FromB3(Event.point);
		Fvector Start;
		Start.mad(Position, Normal, 0.1f);
		SB3RayHit Hit;
		if (!RayPick(Start, Fvector().invert(Normal), 0.3f, B3Category::Level, Hit) || Hit.Triangle < 0)
		{
			continue;
		}

		CDB::TRI* Tri = &ObjectSpacePtr->GetStaticTris()[Hit.Triangle];
		SPHShotMarkContact Mark;
		Mark.Position = Position;
		Mark.Normal = Normal;
		Mark.Object = Geom->Owner->RefObject;
		Mark.Material = Geom->Owner->Material;
		// ODE scales the impact speed by sqrt(mass) (PHCallContactCallback); shot mark thresholds expect that.
		const b3BodyId HitBody = b3Shape_GetBody(LevelA ? Event.shapeIdB : Event.shapeIdA);
		Mark.VelCret = Event.approachSpeed * _sqrt(std::max(b3Body_GetMass(HitBody), 0.f));
		Mark.HasEffect = true;
		Mark.HasUserData = true;
		Mark.IsCharacter = Geom->Owner->Character != nullptr;
		Geom->Owner->ContactCallback(Tri, Mark);
	}
}

// Box3D never calls pre-solve for mesh contacts, so object contact callbacks get the level here,
// after the step, from the deepest touching point of each manifold. As in ODE the level side has
// no geom and takes the triangle material. DoCollide cannot undo a solved contact and is ignored.
void CB3World::ProcessLevelContacts()
{
	for (CPHShell* Shell : Shells)
	{
		if (!Shell->isActive())
		{
			continue;
		}

		for (CPHElement* Element : Shell->ElementsList)
		{
			for (CODEGeom* Geom : Element->Geoms)
			{
				const bool HasCallbacks = !Geom->ObjectCallbacks.empty() || (Geom->Owner && !Geom->Owner->ObjectCallbacks.empty());
				if (HasCallbacks && b3Shape_IsValid(Geom->Shape))
				{
					CallLevelContactCallbacks(Geom, Geom->Shape);
				}
			}
		}
	}
	RunDeferred();
}

void CB3World::CallLevelContactCallbacks(CODEGeom* Geom, b3ShapeId Shape)
{
	const int Capacity = b3Shape_GetContactCapacity(Shape);
	if (Capacity <= 0)
	{
		return;
	}

	xr_vector<b3ContactData> Contacts(Capacity);
	const int Count = b3Shape_GetContactData(Shape, Contacts.data(), Capacity);
	for (int I = 0; I < Count; ++I)
	{
		const b3ContactData& Data = Contacts[I];
		const bool LevelA = B3_ID_EQUALS(b3Shape_GetBody(Data.shapeIdA), StaticBody);
		const bool LevelB = B3_ID_EQUALS(b3Shape_GetBody(Data.shapeIdB), StaticBody);
		if (LevelA == LevelB)
		{
			continue;
		}

		for (int M = 0; M < Data.manifoldCount; ++M)
		{
			const b3Manifold& Manifold = Data.manifolds[M];
			const b3ManifoldPoint* Deepest = nullptr;
			for (int P = 0; P < Manifold.pointCount; ++P)
			{
				const b3ManifoldPoint& Point = Manifold.points[P];
				const bool Touching = Point.separation <= 0.f || Point.totalNormalImpulse > 0.f;
				if (Touching && (Deepest == nullptr || Point.separation < Deepest->separation))
				{
					Deepest = &Point;
				}
			}
			if (Deepest == nullptr)
			{
				continue;
			}

			const size_t MeshTriangle = size_t(Deepest->triangleIndex);
			if (Deepest->triangleIndex < 0 || MeshTriangle >= MeshToCdbTriangle.size())
			{
				continue;
			}
			const CDB::TRI& Tri = ObjectSpacePtr->GetStaticTris()[MeshToCdbTriangle[MeshTriangle]];

			// The geom goes first; the normal points from it to the level.
			const b3BodyId GeomBody = b3Shape_GetBody(Shape);
			const b3Vec3 Anchor = LevelA ? Deepest->anchorB : Deepest->anchorA;
			dContact Native;
			Native.Position = B3Math::FromB3(b3OffsetPos(b3Body_GetWorldCenterOfMass(GeomBody), Anchor));
			Native.Normal = B3Math::FromB3(Manifold.normal);
			if (LevelA)
			{
				Native.Normal.invert();
			}
			Native.Depth = std::max(-Deepest->separation, 0.f);
			Native.Geom[0] = Geom;
			Native.Material[0] = Geom->Owner ? Geom->Owner->Material : DefaultMaterial;
			Native.Material[1] = u16(Tri.material);

			SGameMtl* M1 = MaterialOf(Native.Material[0]);
			SGameMtl* M2 = MaterialOf(Native.Material[1]);
			CPHContact Contact(Native);
			bool DoCollide = true;
			Stepping = true; // world changes from callbacks wait for RunDeferred, as in pre-solve
			CallObjectCallbacks(Geom->ObjectCallbacks, DoCollide, true, Contact, M1, M2);
			if (Geom->Owner)
			{
				CallObjectCallbacks(Geom->Owner->ObjectCallbacks, DoCollide, true, Contact, M1, M2);
			}
			Stepping = false;
		}
	}
}

void CB3World::RunDeferred()
{
	// Deferred calls may defer more work only while stepping, so one pass is enough.
	xr_vector<std::function<void()>> Calls;
	Calls.swap(Deferred);
	for (std::function<void()>& Call : Calls)
	{
		Call();
	}
}

void CB3World::ProcessLiquidContacts()
{
	if (!b3Shape_IsValid(LiquidShape))
	{
		return;
	}

	// A body is in liquid when a liquid surface crosses its bounding sphere: probe with a vertical ray.
	for (CPHShell* Shell : Shells)
	{
		for (CPHElement* Element : Shell->ElementsList)
		{
			const b3BodyId Body = Element->Body;
			if (!b3Body_IsValid(Body) || b3Body_GetType(Body) != b3_dynamicBody || !b3Body_IsAwake(Body))
			{
				continue;
			}

			const float Radius = std::max(Element->getRadius(), 0.05f);
			Fvector Start = B3Math::FromB3(b3Body_GetWorldCenterOfMass(Body));
			Start.y += Radius;
			SB3RayHit Hit;
			if (!RayPick(Start, Fvector().set(0.f, -1.f, 0.f), Radius * 2.f, B3Category::Liquid, Hit) || Hit.Triangle < 0)
			{
				continue;
			}

			CDB::TRI* Tri = &ObjectSpacePtr->GetStaticTris()[Hit.Triangle];
			SGameMtl* Mtl = MaterialOf(u16(Tri->material));
			const Fvector Velocity = B3Math::FromB3(b3Body_GetLinearVelocity(Body));
			const float Mass = b3Body_GetMass(Body);

			// CPHContactBodyEffector::Apply for a passable material: drag against the whole velocity.
			// ODE adds the effector only for flSlowDown materials (Physics.cpp).
			if (Mtl->Flags.test(SGameMtl::flSlowDown))
			{
				const float RecipFlotation = 1.f - Mtl->fFlotationFactor;
				const float Drag = std::min(Velocity.magnitude() * 10000.f * RecipFlotation * RecipFlotation, Mass / fixed_step);
				if (!fis_zero(Drag))
				{
					Fvector Force;
					Force.mul(Velocity, -Drag);
					// The drag must not hold the item up against gravity: slow the sinking, do not stop it.
					Force.y = std::min(Force.y, Mass * GravityValue * 0.8f);
					B3_SAFE(b3Body_ApplyForceToCenter(Body, B3Math::ToB3(Force), true));
				}
			}

			if (Element->Owner.ContactCallback)
			{
				SPHShotMarkContact Mark;
				Mark.Position = Hit.Position;
				Mark.Normal = Hit.Normal;
				Mark.Object = Element->Owner.RefObject;
				Mark.Material = Element->Owner.Material;
				Mark.VelCret = std::abs(Velocity.dotproduct(Hit.Normal)) * _sqrt(std::max(Mass, 0.f));
				Mark.HasEffect = true;
				Mark.HasUserData = true;
				Element->Owner.ContactCallback(Tri, Mark);
			}
		}
	}
}

#ifdef DEBUG
u16 CB3World::ObjectsNumber(bool UpdateOnly)
{
	u16 Count = 0;
	if (!UpdateOnly)
	{
		for (CPHShell* Shell : Shells)
		{
			++Count;
		}
	}
	for (CPHUpdateObject* Object : UpdateObjects)
	{
		++Count;
	}
	return Count;
}
#endif

CPHUpdateObject::CPHUpdateObject()
{
	b_activated = false;
}

void CPHUpdateObject::Activate()
{
	if (b_activated || g_B3World == nullptr)
	{
		return;
	}
	g_B3World->AddUpdateObject(this);
	b_activated = true;
}

void CPHUpdateObject::Deactivate()
{
	if (!b_activated)
	{
		return;
	}
	if (g_B3World)
	{
		g_B3World->RemoveUpdateObject(this);
	}
	b_activated = false;
}
