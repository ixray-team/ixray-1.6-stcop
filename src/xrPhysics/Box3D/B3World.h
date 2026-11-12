#pragma once

#include <box3d/box3d.h>

#include "IPHWorld.h"
#include "PHUpdateObject.h"
#include "physics_scripted.h"
#include "../xrEngine/pure.h"
#include "B3Shell.h"
#include "B3StaticGeometry.h"

class CObjectSpace;
class CObjectList;

namespace CDB
{
	struct TRI;
}

struct SB3RayHit
{
	Fvector Position;
	Fvector Normal;
	float Distance = 0.f;
	u16 Material = 0;
	// The index belongs to Triangle.model, not to a single whole-level mesh.
	TriabgleCDBData Triangle;
	xr_shared_ptr<CDB::MODEL> ModelOwner;
	int ShapeTriangle = -1;
	CODEGeom* Geom = nullptr;
	b3ShapeId Shape = b3_nullShapeId;
};

// Collision categories of Box3D shapes.
namespace B3Category
{
	constexpr u64 Level = 1ull << 0;
	constexpr u64 Dynamic = 1ull << 1;
	constexpr u64 Static = 1ull << 2; // static geom shells
	constexpr u64 Character = 1ull << 3;
	constexpr u64 Ladder = 1ull << 4; // only found by character queries
	constexpr u64 Liquid = 1ull << 5; // every passable level triangle: splashes, drag and shot marks, no collision
	constexpr u64 LiquidQuery = 1ull << 6; // the only category liquid accepts, so it never makes contacts
	constexpr u64 ActorObstacle = 1ull << 7; // passable level triangles that block characters only
	constexpr u64 ActorObstacleQuery = 1ull << 8; // the only category actor obstacles accept
	constexpr u64 All = ~0ull;
}

// Defined in B3Character.cpp. Called first in CB3World::PreSolve; false cancels the contact.
bool B3CharacterPreSolve(b3ShapeId ShapeA, b3ShapeId ShapeB);

// Box3D world: owns the b3World, the static level mesh and the stepping.
class CB3World :
	public pureFrame,
	public IPHWorld,
	public cphysics_scripted
{
public:
	CB3World();
	virtual ~CB3World() override;

	CB3World(const CB3World&) = delete;
	CB3World& operator=(const CB3World&) = delete;

	void Create(bool Mt, CObjectSpace* ObjectSpace, CObjectList* LevelObjects);
	void Destroy();

	// Apply residency changes only outside Box3D queries/steps and contact callbacks.
	void SyncStaticGeometry();
	// Explicit loading/test barrier. Normal frame and query paths never wait for cooking.
	void WaitForStaticGeometry();
	bool IsStaticGeometryPending() const { return PendingStaticGeometry != nullptr; }
	void DestroyStaticMesh();

	bool RayPick(const Fvector& Start, const Fvector& Dir, float Range, u64 Mask, SB3RayHit& Hit);
	void ResolveRayHit(SB3RayHit& Hit) const;

	b3WorldId Id()
	{
		SyncStaticGeometry();
		return World;
	}
	bool IsStepping() const
	{
		return Stepping;
	}
	bool IsFrozen() const
	{
		return Frozen;
	}
	// Fixed steps left in the current frame, the running one included; 1 outside FrameStep.
	u32 StepsLeft() const
	{
		return StepsRemaining;
	}
	void Defer(std::function<void()>&& Call)
	{
		Deferred.push_back(std::move(Call));
	}
	CObjectSpace& ObjectSpace()
	{
		return *ObjectSpacePtr;
	}
	CObjectList* Objects() const
	{
		return LevelObjects;
	}
	float InterpolationFactor() const;
	// Static body for joints that attach an element to the world.
	b3BodyId GroundBody();
	u16 LevelDefaultMaterial() const
	{
		return DefaultMaterial;
	}

	// Object contact callbacks for a contact Box3D does not report through pre-solve (character
	// mover). Normal points from A to B, as in Box3D pre-solve; callbacks get the ODE direction.
	// World changes wait for RunDeferred. Returns false when a callback refuses the contact.
	bool AllowContact(b3ShapeId ShapeA, b3ShapeId ShapeB, const Fvector& Point, const Fvector& Normal);
	void RunDeferred();
	void OverlapGeom(const CODEGeom& Geom, const Fmatrix& Form, float Inset, u64 Mask, b3OverlapResultFcn* Fcn, void* Context);
	// ODE CPHShell::CollideAll for one geom at Form: object callbacks for every overlap, at once.
	void CollideGeom(CODEGeom& Geom, const Fmatrix& Form, u64 Mask);

	void AddShell(CPHShell* Shell);
	void RemoveShell(CPHShell* Shell);
	void AddUpdateObject(CPHUpdateObject* Object);
	void RemoveUpdateObject(CPHUpdateObject* Object);
	void NetRelcase(CPhysicsShell* Shell);

	ContactCallbackFun* DefaultContactShotMark() const
	{
		return ContactShotMark;
	}
	ContactCallbackFun* DefaultCharacterContactShotMark() const
	{
		return CharacterContactShotMark;
	}

	// IPHWorld
	virtual float Gravity() override
	{
		return GravityValue;
	}
	virtual void SetGravity(float G) override;
	virtual bool Processing() override
	{
		return IsProcessing;
	}
	virtual u32 CalcNumSteps(u32 DeltaTime) override;
	virtual u64& StepsNum() override
	{
		return StepCount;
	}
	virtual float FrameTime() override
	{
		return FrameTimeValue;
	}
	virtual void Freeze() override;
	virtual void UnFreeze() override;
	virtual void Step() override;
	virtual void SetStep(float S) override;
	virtual void StepNumIterations(int Iterations) override;
	virtual void set_default_contact_shotmark(ContactCallbackFun* F) override
	{
		ContactShotMark = F;
	}
	virtual void set_default_character_contact_shotmark(ContactCallbackFun* F) override
	{
		CharacterContactShotMark = F;
	}
	virtual void set_update_callback(IPHWorldUpdateCallbck* Callback) override;
#ifdef DEBUG
	virtual u16 ObjectsNumber(bool UpdateOnly = false) override;
	virtual void OnRender() override {}
#endif

	virtual void OnFrame() override;
	virtual iphysics_scripted& get_scripted() override
	{
		return *this;
	}

private:
	// Solid: non-passable triangles. Passable: every passable one (B3Category::Liquid).
	// ActorObstacle: passable actor obstacles, which block characters only.
	enum EMesh
	{
		MeshSolid,
		MeshPassable,
		MeshActorObstacle,
		MeshCount
	};

	struct SLevelMesh
	{
		SLevelMesh() = default;
		SLevelMesh(const SLevelMesh&) = delete;
		SLevelMesh& operator=(const SLevelMesh&) = delete;
		~SLevelMesh();
		void Reset();
		b3MeshData* Data = nullptr;
		b3ShapeId Shape = b3_nullShapeId;
		xr_vector<u32> ToCdb;
		xr_vector<b3SurfaceMaterial> Materials;
	};
	struct SLevelCollision
	{
		SLevelCollision(xr_shared_ptr<CDB::MODEL> InRoot, xr_shared_ptr<const CB3StaticGeometry> InGeometry)
			: Root(std::move(InRoot)), Geometry(std::move(InGeometry)) {}
		xr_shared_ptr<CDB::MODEL> Root;
		xr_shared_ptr<const CB3StaticGeometry> Geometry;
		xr_shared_ptr<const xr_vector<u32>> Selected;
		SLevelMesh Meshes[MeshCount];
	};
	struct SStaticGeometryEntry
	{
		xr_shared_ptr<CDB::MODEL> Root;
		xr_shared_ptr<const CB3StaticGeometry> Geometry;
		xr_shared_ptr<const xr_vector<u32>> Selected;
		xr_unique_ptr<SLevelCollision> Prepared;
	};
	struct SStaticGeometryUpdate
	{
		u64 Revision;
		xr_vector<SStaticGeometryEntry> Entries;
		xr_atomic_bool Cancelled = false;
		bool Success = false;
	};
	struct SStaticMaterial
	{
		b3SurfaceMaterial Surface;
		bool Passable;
		bool ActorObstacle;
	};

	struct SFrozenBody
	{
		b3BodyId Body;
		CPHShell* Shell; // null for character capsules
		b3Vec3 Linear;
		b3Vec3 Angular;
		float GravityScale;
	};

	void FrameStep(float DeltaTime);
	void ProcessHitEvents();
	void ProcessLiquidContacts();
	void ProcessLevelContacts();
	void ProcessGeomContacts(CPHElement& Element, CODEGeom& Geom, float& SlowDown, Fvector& SlowDownNormal);
	static bool BuildMesh(SLevelCollision& Collision, const xr_vector<b3Vec3>& Vertices, EMesh Kind, const xr_vector<SStaticMaterial>& Materials);
	static void PrepareStaticGeometry(SStaticGeometryUpdate& Update, const xr_vector<SStaticMaterial>& Materials);
	bool ApplyStaticGeometry(SStaticGeometryUpdate& Update);
	const SLevelMesh* FindLevelMesh(b3ShapeId Shape, const SLevelCollision*& Collision) const;
	void FreezeBody(b3BodyId Body, CPHShell* Shell);
	bool Simulated(const CPHShell* Shell) const
	{
		return !Frozen || Shell->Thawed;
	}
	static bool CustomFilter(b3ShapeId ShapeA, b3ShapeId ShapeB, void* Context);
	static bool PreSolve(b3ShapeId ShapeA, b3ShapeId ShapeB, b3Pos Point, b3Vec3 Normal, void* Context);

	b3WorldId World = b3_nullWorldId;
	b3BodyId StaticBody = b3_nullBodyId;
	b3BodyId Ground = b3_nullBodyId;
	xr_vector<xr_unique_ptr<SLevelCollision>> LevelCollisions;
	xr_task_group StaticGeometryTasks;
	xr_atomic_bool StaticGeometryTaskRunning = false;
	xr_unique_ptr<SStaticGeometryUpdate> PendingStaticGeometry;
	u64 CollisionRevision = u64(-1);
	bool SyncingCollision = false;
	bool StaticGeometryInitialized = false;
	xr_vector<b3ContactData> ContactScratch;
	xr_vector<SFrozenBody> FrozenBodies;

	PH_SHELL_STORAGE Shells;
	PH_UPDATE_OBJECT_STORAGE UpdateObjects;

	IPHWorldUpdateCallbck* UpdateCallback = nullptr;
	CObjectSpace* ObjectSpacePtr = nullptr;
	CObjectList* LevelObjects = nullptr;
	ContactCallbackFun* ContactShotMark = nullptr;
	ContactCallbackFun* CharacterContactShotMark = nullptr;

	u64 StepCount = 0;
	float FrameTimeValue = 0.f;
	float GravityValue = default_world_gravity;
	u16 DefaultMaterial = 0;
	int SubSteps = 4;
	bool IsProcessing = false;
	bool Stepping = false;
	xr_vector<std::function<void()>> Deferred;
	u32 StepsRemaining = 1;
	bool Frozen = false;
	bool Exists = false;
};

extern CB3World* g_B3World;

// Box3D locks the world during b3World_Step, while ODE let contact callbacks change bodies and
// shapes. Changes made from inside the step (pre-solve object callbacks) run right after it.
#define B3_SAFE(...)                                                   \
	do                                                                 \
	{                                                                  \
		if (g_B3World != nullptr && g_B3World->IsStepping())           \
		{                                                              \
			g_B3World->Defer([=]() { __VA_ARGS__; });                  \
		}                                                              \
		else                                                           \
		{                                                              \
			__VA_ARGS__;                                               \
		}                                                              \
	} while (false)
