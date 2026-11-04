#include "StdAfx.h"
#include "B3World.h"
#include "B3Math.h"
#include "PHContact.h"
#include "console_vars.h"
#include "IPhysicsShellHolder.h"
#include "icollisiondamagereceiver.h"
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

// ODE surface: mu = f1 * f2, and a bounce only when both materials bounce (non-bounceable shapes
// store restitution 0), at the lower value. A contact callback's override replaces the mix.
static float MixFriction(float A, uint64_t IdA, float B, uint64_t IdB)
{
	return (IdA & B3MaterialFlag::Friction) ? A : ((IdB & B3MaterialFlag::Friction) ? B : A * B);
}

static float MixRestitution(float A, uint64_t IdA, float B, uint64_t IdB)
{
	return (IdA & B3MaterialFlag::Bounce) ? A : ((IdB & B3MaterialFlag::Bounce) ? B : std::min(A, B));
}

void CB3World::Create(bool Mt, CObjectSpace* InObjectSpace, CObjectList* InLevelObjects)
{
	ObjectSpacePtr = InObjectSpace;
	LevelObjects = InLevelObjects;

	b3WorldDef Def = b3DefaultWorldDef();
	Def.gravity = B3Math::ToB3(Fvector().set(0.f, -GravityValue, 0.f));
	Def.maximumLinearSpeed = default_l_limit;
	Def.frictionCallback = &MixFriction;
	Def.restitutionCallback = &MixRestitution;
	// Shot mark thresholds (VelCret) do the gating, as ODE did for every contact.
	Def.hitEventThreshold = 0.2f;
	// Box3D makes static contacts twice as stiff: this keeps level contacts near the ODE ERP.
	Def.contactHertz = 30.f;
	Def.contactDampingRatio = 4.f;
	Def.contactSpeed = 3.f;
	// The bounce start velocity is per pair in ODE and global here: take the default object material's.
	if (auto It = GMLibrary().GetMaterialIt("objects\\small_box"); It != GMLibrary().GetMaterials().end() && (*It)->fPHBounceStartVelocity > 0.f)
	{
		Def.restitutionThreshold = (*It)->fPHBounceStartVelocity;
	}
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

	const bool Solid = BuildMesh(Vertices, Tris, TriCount, MeshSolid);
	BuildMesh(Vertices, Tris, TriCount, MeshPassable);
	BuildMesh(Vertices, Tris, TriCount, MeshActorObstacle);
	return Solid;
}

// Passable triangles in ODE make contacts that never collide but splash, slow bodies down and,
// for actor obstacles, block characters (CPHActorCharacter::InitContact). Box3D never calls
// pre-solve for mesh contacts, so they are query-only meshes whose masks no body category matches.
bool CB3World::BuildMesh(const xr_vector<b3Vec3>& Vertices, const CDB::TRI* Tris, size_t TriCount, EMesh Kind)
{
	static constexpr struct
	{
		u64 Category;
		u64 Mask;
		const char* Name;
	} Kinds[MeshCount] = {
		{B3Category::Level, B3Category::Dynamic, "level"},
		{B3Category::Liquid, B3Category::LiquidQuery, "passable"},
		{B3Category::ActorObstacle, B3Category::ActorObstacleQuery, "actor obstacle"}};

	// Box3D takes up to 256 materials per shape: map game materials to slots.
	xr_vector<b3SurfaceMaterial> Materials;
	xr_hash_map<u32, u8> MaterialSlots;
	xr_vector<int32_t> Indices;
	xr_vector<uint8_t> MaterialIndices;
	SLevelMesh& Target = Meshes[Kind];
	Target.ToCdb.clear();
	for (size_t I = 0; I < TriCount; ++I)
	{
		const u32 GameMaterial = Tris[I].material;
		SGameMtl* Mtl = GMLibrary().GetMaterialByIdx(u16(GameMaterial));
		const bool Passable = Mtl && Mtl->Flags.test(SGameMtl::flPassable);
		const bool Keep = Kind == MeshSolid ? !Passable : (Passable && (Kind == MeshPassable || Mtl->Flags.test(SGameMtl::flActorObstacle)));
		if (!Keep)
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
		Target.ToCdb.push_back(int(I));
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
	MeshDef.identifyEdges = Kind == MeshSolid;
	// Median split builds the tree much faster than SAH on a level-sized mesh.
	MeshDef.useMedianSplit = true;

	Target.Data = b3CreateMesh(&MeshDef, nullptr, 0);
	if (Target.Data == nullptr)
	{
		Msg("! [Box3D] failed to build the %s mesh", Kinds[Kind].Name);
		return false;
	}

	b3ShapeDef ShapeDef = b3DefaultShapeDef();
	ShapeDef.materials = Materials.data();
	ShapeDef.materialCount = int(Materials.size());
	ShapeDef.filter.categoryBits = Kinds[Kind].Category;
	ShapeDef.filter.maskBits = Kinds[Kind].Mask;
	Target.Shape = b3CreateMeshShape(StaticBody, &ShapeDef, Target.Data, b3Vec3{1.f, 1.f, 1.f});
	Msg("* [Box3D] %s mesh: %d triangles, %d materials", Kinds[Kind].Name, int(MaterialIndices.size()), int(Materials.size()));
	return true;
}

void CB3World::DestroyStaticMesh()
{
	if (b3Body_IsValid(StaticBody))
	{
		b3DestroyBody(StaticBody);
	}
	StaticBody = b3_nullBodyId;

	for (SLevelMesh& Mesh : Meshes)
	{
		if (Mesh.Data != nullptr)
		{
			b3DestroyMesh(Mesh.Data);
		}
		Mesh = SLevelMesh();
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
	Filter.categoryBits |= (Mask & B3Category::ActorObstacle) ? B3Category::ActorObstacleQuery : 0ull;
	Filter.maskBits = Mask;

	SB3RayContext Context{&Hit};
	b3World_CastRay(World, B3Math::ToB3(Start), B3Math::ToB3(Translation), Filter, &RayCallback, &Context);
	if (!Context.Found)
	{
		return false;
	}

	Hit.Distance *= Range;
	for (const SLevelMesh& Mesh : Meshes)
	{
		if (B3_ID_EQUALS(Hit.Shape, Mesh.Shape) && Hit.Triangle >= 0 && Hit.Triangle < int(Mesh.ToCdb.size()))
		{
			Hit.Triangle = Mesh.ToCdb[Hit.Triangle];
		}
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

// Characters are dynamic and character class, static geom shells have no class (CPHCollideValidator).
static CLClassBits CollideClass(const SB3GeomOwner& Owner)
{
	return Owner.Shell ? Owner.Shell->CollideClassBits.flags : (Owner.Character ? B3CollideClass::Dynamic | B3CollideClass::Character : 0);
}

// Elements of one shell collide only with self collision; shells of one collision group never collide;
// then the collide classes. Runs only when Box3D creates a pair: changes go through CPHShell::ResetPairs.
bool CB3World::CustomFilter(b3ShapeId ShapeA, b3ShapeId ShapeB, void* Context)
{
	const CODEGeom* GeomA = static_cast<const CODEGeom*>(b3Shape_GetUserData(ShapeA));
	const CODEGeom* GeomB = static_cast<const CODEGeom*>(b3Shape_GetUserData(ShapeB));
	if (GeomA == nullptr || GeomB == nullptr || GeomA->Owner == nullptr || GeomB->Owner == nullptr)
	{
		return true;
	}

	CPHShell* ShellA = GeomA->Owner->Shell;
	CPHShell* ShellB = GeomB->Owner->Shell;
	if (ShellA && ShellA == ShellB)
	{
		return ShellA->SelfCollides(GeomA->Owner, GeomB->Owner);
	}
	if (ShellA && ShellB && ShellA->IsGroupObject() && ShellA->GetCLGroup() == ShellB->GetCLGroup())
	{
		return false;
	}
	return B3CollideClass::DoCollide(CollideClass(*GeomA->Owner), CollideClass(*GeomB->Owner));
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
	std::erase_if(FrozenBodies, [Shell](const SFrozenBody& Entry) { return Entry.Shell == Shell; });
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
	// Box3D has no solver iteration count: the sub-steps are the only convergence knob.
	// ODE solves every constraint `Iterations` times per step, here every sub-step solves
	// them once, so a long ragdoll chain needs more sub-steps than the default four.
	SubSteps = clampr(Iterations / 2, 1, 8);
}

u32 CB3World::CalcNumSteps(u32 DeltaTime)
{
	if (DeltaTime < FrameTimeValue * 1000)
	{
		return 0;
	}

	return iCeil((float(DeltaTime) - FrameTimeValue * 1000) / (fixed_step * 1000));
}

void CB3World::FreezeBody(b3BodyId Body, CPHShell* Shell)
{
	FrozenBodies.push_back({Body, Shell, b3Body_GetLinearVelocity(Body), b3Body_GetAngularVelocity(Body), b3Body_GetGravityScale(Body)});
}

// ODE takes every object out of the step until a shell UnFreeze puts it back, so a FlyTo moves only
// the corpse. Box3D steps every body: while frozen, Step holds the bodies awake now still, character
// capsules included, unless their shell is thawed. UnFreeze gives them their motion back.
void CB3World::Freeze()
{
	R_ASSERT2(!Frozen, "already freezed!!!");
	Frozen = true;
	FrozenBodies.clear();
	for (CPHShell* Shell : Shells)
	{
		Shell->Thawed = false;
		for (CPHElement* Element : Shell->ElementsList)
		{
			if (Element->IsAwake())
			{
				FreezeBody(Element->Body, Shell);
			}
		}
	}

	// Move events list every body awake in the last step; capsule shapes belong to a character owner.
	const b3BodyEvents Events = b3World_GetBodyEvents(World);
	for (int I = 0; I < Events.moveCount; ++I)
	{
		const b3BodyId Body = Events.moveEvents[I].bodyId;
		b3ShapeId Shape;
		if (b3Body_IsValid(Body) && b3Body_IsAwake(Body) && b3Body_GetShapes(Body, &Shape, 1) == 1)
		{
			const CODEGeom* Geom = static_cast<const CODEGeom*>(b3Shape_GetUserData(Shape));
			if (Geom && Geom->Owner && Geom->Owner->Character)
			{
				FreezeBody(Body, nullptr);
			}
		}
	}
}

void CB3World::UnFreeze()
{
	R_ASSERT2(Frozen, "is not freezed!!!");
	Frozen = false;
	for (const SFrozenBody& Entry : FrozenBodies)
	{
		if (b3Body_IsValid(Entry.Body) && !(Entry.Shell && Entry.Shell->Thawed))
		{
			b3Body_SetLinearVelocity(Entry.Body, Entry.Linear);
			b3Body_SetAngularVelocity(Entry.Body, Entry.Angular);
			b3Body_SetGravityScale(Entry.Body, Entry.GravityScale);
		}
	}
	FrozenBodies.clear();
	for (CPHShell* Shell : Shells)
	{
		Shell->Thawed = false;
	}
}

void CB3World::OnFrame()
{
	PROF_EVENT("CB3World::OnFrame");
	FrameStep(Device.fTimeDelta);
}

void CB3World::FrameStep(float DeltaTime)
{
	if (Frozen)
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
		StepsRemaining = StepsNumber - I;
		Step();
	}
	StepsRemaining = 1;
	IsProcessing = false;
}

void CB3World::Step()
{
	PROF_EVENT("CB3World::Step");
	++StepCount;

	for (CPHShell* Shell : Shells)
	{
		if (Shell->SelfCollisionDirty)
		{
			Shell->BuildSelfCollisionPairs();
		}
		if (Simulated(Shell))
		{
			Shell->PhTune(fixed_step);
		}
	}
	if (!Frozen)
	{
		for (CPHUpdateObject* Object : UpdateObjects)
		{
			Object->PhTune(fixed_step);
		}
		UpdateCallback->update_step();
	}
	for (CPHShell* Shell : Shells)
	{
		if (Simulated(Shell))
		{
			for (CPHElement* Element : Shell->ElementsList)
			{
				Element->FlushForces(fixed_step);
			}
		}
	}
	for (const SFrozenBody& Entry : FrozenBodies)
	{
		if (b3Body_IsValid(Entry.Body) && !(Entry.Shell && Entry.Shell->Thawed))
		{
			b3Body_SetLinearVelocity(Entry.Body, b3Vec3_zero);
			b3Body_SetAngularVelocity(Entry.Body, b3Vec3_zero);
			b3Body_SetGravityScale(Entry.Body, 0.f);
		}
	}

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
		if (!Simulated(Shell))
		{
			continue;
		}

		Shell->PhDataUpdate(fixed_step);
		// ODE DisableObject: the corpse stops colliding with characters once it first comes to rest.
		if (Shell->RemoveCharCollOnDisable && !Shell->isEnabled())
		{
			Shell->RemoveCharCollOnDisable = false;
			Shell->DisableCharacterCollision();
		}
	}
	if (!Frozen)
	{
		for (CPHUpdateObject* Object : UpdateObjects)
		{
			Object->PhDataUpdate(fixed_step);
		}
	}
}

static SGameMtl* MaterialOf(u16 Index)
{
	SGameMtl* Mtl = GMLibrary().GetMaterialByIdx(Index);
	return Mtl ? Mtl : GMLibrary().GetMaterialByIdx(0);
}

static void CallObjectCallbacks(const xr_vector<ObjectContactCallbackFun*>& Callbacks, bool& DoCollide, int Side, CPHContact& Contact, SGameMtl* M1, SGameMtl* M2, ObjectContactCallbackFun* Skip)
{
	// Callbacks may remove themselves.
	for (size_t I = 0; I < Callbacks.size(); ++I)
	{
		ObjectContactCallbackFun* Callback = Callbacks[I];
		if (Callback != Skip)
		{
			Contact.Native().Side = u8(Side);
			Contact.Native().FrictionRead = false;
			Callback(DoCollide, Side == 0, Contact, M1, M2);
		}
	}
}

static void CallGeomCallbacks(dContact& Native, int Side, bool& DoCollide, ObjectContactCallbackFun* Skip = nullptr)
{
	CODEGeom* Geom = Native.Geom[Side];
	if (Geom == nullptr)
	{
		return;
	}

	CPHContact Contact(Native);
	SGameMtl* M1 = MaterialOf(Native.Material[0]);
	SGameMtl* M2 = MaterialOf(Native.Material[1]);
	CallObjectCallbacks(Geom->ObjectCallbacks, DoCollide, Side, Contact, M1, M2, Skip);
	if (Geom->Owner)
	{
		CallObjectCallbacks(Geom->Owner->ObjectCallbacks, DoCollide, Side, Contact, M1, M2, Skip);
	}
}

static bool HasCallbacks(const CODEGeom& Geom)
{
	return !Geom.ObjectCallbacks.empty() || (Geom.Owner && !Geom.Owner->ObjectCallbacks.empty());
}

static bool HasCallback(const CODEGeom& Geom, ObjectContactCallbackFun* Callback)
{
	const auto Has = [Callback](const xr_vector<ObjectContactCallbackFun*>& List) { return std::find(List.begin(), List.end(), Callback) != List.end(); };
	return Has(Geom.ObjectCallbacks) || (Geom.Owner && Has(Geom.Owner->ObjectCallbacks));
}

bool CB3World::PreSolve(b3ShapeId ShapeA, b3ShapeId ShapeB, b3Pos Point, b3Vec3 Normal, void* Context)
{
	CB3World& Self = *static_cast<CB3World*>(Context);
	if (!B3CharacterPreSolve(ShapeA, ShapeB))
	{
		return false;
	}

	dContact Native;
	Native.Geom[0] = static_cast<CODEGeom*>(b3Shape_GetUserData(ShapeA));
	Native.Geom[1] = static_cast<CODEGeom*>(b3Shape_GetUserData(ShapeB));
	if (Native.Geom[0] == nullptr && Native.Geom[1] == nullptr)
	{
		return true;
	}

	Native.Position = B3Math::FromB3(Point);
	Native.Normal = B3Math::FromB3(b3Neg(Normal));
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

	// ODE collides the geoms of one shell without object callbacks.
	const SB3GeomOwner* OwnerA = Native.Geom[0] ? Native.Geom[0]->Owner : nullptr;
	const SB3GeomOwner* OwnerB = Native.Geom[1] ? Native.Geom[1]->Owner : nullptr;
	if (OwnerA && OwnerB && OwnerA->Shell && OwnerA->Shell == OwnerB->Shell)
	{
		return true;
	}

	// As ODE: the second side first. Collision damage comes per contact point after the step.
	Native.Friction = M1->fPHFriction * M2->fPHFriction;
	bool DoCollide = true;
	CallGeomCallbacks(Native, 1, DoCollide, &DamageReceiverCollisionCallback);
	CallGeomCallbacks(Native, 0, DoCollide, &DamageReceiverCollisionCallback);
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

void CB3World::OverlapGeom(const CODEGeom& Geom, const Fmatrix& Form, float Inset, u64 Mask, b3OverlapResultFcn* Fcn, void* Context) const
{
	b3Vec3 Points[B3_MAX_SHAPE_CAST_POINTS];
	b3ShapeProxy Proxy = {Points, 0, 0.f};
	Proxy.count = Geom.MakeProxy(Form, Inset, Points, Proxy.radius);
	b3QueryFilter Filter = b3DefaultQueryFilter();
	Filter.categoryBits = B3Category::Dynamic;
	Filter.maskBits = Mask;
	b3World_OverlapShape(World, b3Vec3_zero, &Proxy, Filter, Fcn, Context);
}

// Only imotion reads the depth, against 1 cm: a proxy shrunk by 1 cm overlaps only deeper contacts.
void CB3World::CollideGeom(CODEGeom& Geom, const Fmatrix& Form, u64 Mask)
{
	struct SContext
	{
		CB3World* World;
		CODEGeom* Geom;
		Fvector Position;
	};
	const auto Callback = [](b3ShapeId ShapeId, void* Context) -> bool
	{
		const SContext& Self = *static_cast<SContext*>(Context);
		const SB3GeomOwner& Owner = *Self.Geom->Owner;
		CODEGeom* Other = static_cast<CODEGeom*>(b3Shape_GetUserData(ShapeId));
		if (Other && Other->Owner && ((Owner.Shell && Other->Owner->Shell == Owner.Shell) || (Owner.RefObject && Other->Owner->RefObject == Owner.RefObject)))
		{
			return true;
		}

		dContact Native;
		Native.Position = Self.Position;
		Native.Normal.set(0.f, 1.f, 0.f);
		Native.Depth = 0.02f;
		Native.ApproachSpeed = 0.f;
		Native.Geom[0] = Self.Geom;
		Native.Geom[1] = Other;
		Native.Material[0] = Owner.Material;
		Native.Material[1] = Other && Other->Owner ? Other->Owner->Material : Self.World->DefaultMaterial;
		bool DoCollide = true;
		CallGeomCallbacks(Native, 0, DoCollide);
		return true;
	};

	if (Geom.Owner == nullptr)
	{
		return;
	}
	SContext Context{this, &Geom};
	Form.transform_tiny(Context.Position, Geom.LocalForm.c);
	const bool WasStepping = Stepping;
	Stepping = true;
	OverlapGeom(Geom, Form, 0.01f, Mask, Callback, &Context);
	Stepping = WasStepping;
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

// CPHContactBodyEffector::Apply: drag against the velocity; on a solid surface only along it.
static void ApplyDrag(b3BodyId Body, float RecipFlotation, const Fvector* Normal)
{
	const Fvector Velocity = B3Math::FromB3(b3Body_GetLinearVelocity(Body));
	const float Drag = std::min(Velocity.magnitude() * 10000.f * RecipFlotation * RecipFlotation, b3Body_GetMass(Body) / fixed_step);
	if (!fis_zero(Drag))
	{
		Fvector Force;
		Force.mul(Velocity, -Drag);
		if (Normal)
		{
			Force.mad(*Normal, -Force.dotproduct(*Normal));
		}
		b3Body_ApplyForceToCenter(Body, B3Math::ToB3(Force), true);
	}
}

// Box3D never calls pre-solve for mesh contacts and calls it once per pair, so after the step the
// contact data gives, as ODE per touching point: object callbacks for the level, collision damage
// for every contact, solid slow-down drag. DoCollide cannot undo a solved contact and is ignored.
void CB3World::ProcessLevelContacts()
{
	for (CPHShell* Shell : Shells)
	{
		if (!Shell->isActive() || !Simulated(Shell))
		{
			continue;
		}

		for (CPHElement* Element : Shell->ElementsList)
		{
			if (!Element->IsAwake())
			{
				continue;
			}

			float SlowDown = 0.f;
			Fvector SlowDownNormal;
			for (CODEGeom* Geom : Element->Geoms)
			{
				if (b3Shape_IsValid(Geom->Shape))
				{
					ProcessGeomContacts(*Element, *Geom, SlowDown, SlowDownNormal);
				}
			}
			if (SlowDown > 0.f && !Shell->TracedGeoms)
			{
				ApplyDrag(Element->Body, SlowDown, &SlowDownNormal);
			}
		}
	}
	RunDeferred();
}

// As ODE the level is side 0 with the triangle material, and the normal points from side 1 to side 0.
void CB3World::ProcessGeomContacts(CPHElement& Element, CODEGeom& Geom, float& SlowDown, Fvector& SlowDownNormal)
{
	const b3ShapeId Shape = Geom.Shape;
	const int Capacity = b3Shape_GetContactCapacity(Shape);
	if (Capacity <= 0)
	{
		return;
	}
	if (ContactScratch.size() < size_t(Capacity))
	{
		ContactScratch.resize(Capacity);
	}

	const int Count = b3Shape_GetContactData(Shape, ContactScratch.data(), Capacity);
	const bool Callbacks = HasCallbacks(Geom);
	const bool Damage = Callbacks && HasCallback(Geom, &DamageReceiverCollisionCallback);
	// Without the bounce flag the solver did not bounce this step: the first SetBounce impact bounces here.
	const bool HadBounce = (b3Shape_GetSurfaceMaterial(Shape).userMaterialId & B3MaterialFlag::Bounce) != 0;
	float BounceSpeed = 0.f;
	float Bounce = 0.f;
	Fvector BounceNormal;
	for (int I = 0; I < Count; ++I)
	{
		const b3ContactData& Data = ContactScratch[I];
		const bool IsA = B3_ID_EQUALS(Data.shapeIdA, Shape);
		const b3ShapeId OtherShape = IsA ? Data.shapeIdB : Data.shapeIdA;
		const bool Level = B3_ID_EQUALS(b3Shape_GetBody(OtherShape), StaticBody);
		CODEGeom* Other = static_cast<CODEGeom*>(b3Shape_GetUserData(OtherShape));
		if (!Level && (!Damage || (Other && Other->Owner && Other->Owner->Shell == Element.Shell)))
		{
			continue;
		}

		const b3Pos CenterA = b3Body_GetWorldCenterOfMass(b3Shape_GetBody(Data.shapeIdA));
		for (int M = 0; M < Data.manifoldCount; ++M)
		{
			const b3Manifold& Manifold = Data.manifolds[M];
			for (int P = 0; P < Manifold.pointCount; ++P)
			{
				const b3ManifoldPoint& Point = Manifold.points[P];
				const size_t Triangle = size_t(Point.triangleIndex);
				if (Point.totalNormalImpulse <= 0.f || (Level && (Point.triangleIndex < 0 || Triangle >= Meshes[MeshSolid].ToCdb.size())))
				{
					continue;
				}

				dContact Native;
				Native.Position = B3Math::FromB3(b3OffsetPos(CenterA, Point.anchorA));
				Native.Depth = std::max(-Point.separation, 0.f);
				Native.ApproachSpeed = std::max(-Point.normalVelocity, 0.f);
				Native.Geom[0] = Level ? nullptr : (IsA ? &Geom : Other);
				Native.Geom[1] = Level || !IsA ? &Geom : Other;
				Native.Normal = B3Math::FromB3(Level && IsA ? Manifold.normal : b3Neg(Manifold.normal));
				for (int S = 0; S < 2; ++S)
				{
					Native.Material[S] = Native.Geom[S] && Native.Geom[S]->Owner ? Native.Geom[S]->Owner->Material : DefaultMaterial;
				}
				if (Level)
				{
					Native.Material[0] = u16(ObjectSpacePtr->GetStaticTris()[Meshes[MeshSolid].ToCdb[Triangle]].material);
					SGameMtl* Mtl = MaterialOf(Native.Material[0]);
					if (Mtl->Flags.test(SGameMtl::flSlowDown) && 1.f - Mtl->fFlotationFactor > SlowDown)
					{
						SlowDown = 1.f - Mtl->fFlotationFactor;
						SlowDownNormal = Native.Normal;
					}
					if (!Callbacks)
					{
						continue;
					}
				}

				SGameMtl* M1 = MaterialOf(Native.Material[0]);
				SGameMtl* M2 = MaterialOf(Native.Material[1]);
				Native.Friction = M1->fPHFriction * M2->fPHFriction;
				bool DoCollide = true;
				Stepping = true; // world changes from callbacks wait for RunDeferred, as in pre-solve
				if (Level)
				{
					CallGeomCallbacks(Native, 1, DoCollide);
				}
				else
				{
					CPHContact Contact(Native);
					Native.Side = IsA ? 0 : 1;
					DamageReceiverCollisionCallback(DoCollide, IsA, Contact, M1, M2);
				}
				Stepping = false;

				if (Level && Native.Bounce > 0.f && !HadBounce && Native.ApproachSpeed > std::max(Native.BounceVel, BounceSpeed))
				{
					BounceSpeed = Native.ApproachSpeed;
					Bounce = Native.Bounce;
					BounceNormal = Native.Normal;
				}
			}
		}
	}

	if (BounceSpeed > 0.f)
	{
		const b3BodyId Body = Element.Body;
		b3Body_SetLinearVelocity(Body, b3MulAdd(b3Body_GetLinearVelocity(Body), -Bounce * BounceSpeed, B3Math::ToB3(BounceNormal)));
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
	if (!b3Shape_IsValid(Meshes[MeshPassable].Shape))
	{
		return;
	}

	// A body touches a passable surface when it crosses its bounding sphere: probe with a vertical ray.
	for (CPHShell* Shell : Shells)
	{
		if (!Simulated(Shell))
		{
			continue;
		}

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

			// ODE adds the effector only for flSlowDown, and to ray-motion objects only in liquid (Physics.cpp).
			if (Mtl->Flags.test(SGameMtl::flSlowDown) && (Mtl->Flags.test(SGameMtl::flLiquid) || !Shell->TracedGeoms))
			{
				ApplyDrag(Body, 1.f - Mtl->fFlotationFactor, nullptr);
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
