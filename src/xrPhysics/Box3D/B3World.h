#pragma once

#include <box3d/box3d.h>

#include "IPHWorld.h"
#include "PHUpdateObject.h"
#include "physics_scripted.h"
#include "../xrEngine/pure.h"
#include "B3Shell.h"

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
	int Triangle = -1;
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
	constexpr u64 Liquid = 1ull << 5; // passable liquid level triangles: splashes and drag, no collision
	constexpr u64 LiquidQuery = 1ull << 6; // the only category liquid accepts, so it never makes contacts
	constexpr u64 All = ~0ull;
}

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

	// Level geometry from xrCDB; per-triangle game material goes to userMaterialId.
	bool CreateStaticMesh(const Fvector* Verts, size_t VertCount, const CDB::TRI* Tris, size_t TriCount);
	void DestroyStaticMesh();

	bool RayPick(const Fvector& Start, const Fvector& Dir, float Range, u64 Mask, SB3RayHit& Hit) const;

	b3WorldId Id() const
	{
		return World;
	}
	bool IsStepping() const
	{
		return Stepping;
	}
	void Defer(std::function<void()>&& Call)
	{
		Deferred.push_back(std::move(Call));
	}
	CObjectSpace& ObjectSpace()
	{
		return *ObjectSpacePtr;
	}
	float InterpolationFactor() const;
	// Static body for joints that attach an element to the world.
	b3BodyId GroundBody();
	u16 LevelDefaultMaterial() const
	{
		return DefaultMaterial;
	}

	// Object contact callbacks for a contact Box3D does not report through pre-solve (character
	// mover). World changes wait for RunDeferred. Returns false when a callback refuses the contact.
	bool AllowContact(b3ShapeId ShapeA, b3ShapeId ShapeB, const Fvector& Point, const Fvector& Normal);
	void RunDeferred();

	void AddShell(CPHShell* Shell);
	void RemoveShell(CPHShell* Shell);
	void AddUpdateObject(CPHUpdateObject* Object);
	void RemoveUpdateObject(CPHUpdateObject* Object);
	void NetRelcase(CPhysicsShell* Shell);

	ContactCallbackFun* DefaultContactShotMark() const
	{
		return ContactShotMark;
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
	void FrameStep(float DeltaTime);
	void ProcessHitEvents();
	void ProcessLiquidContacts();
	void ProcessLevelContacts();
	void CallLevelContactCallbacks(CODEGeom* Geom, b3ShapeId Shape);
	bool BuildMesh(const xr_vector<b3Vec3>& Vertices, const CDB::TRI* Tris, size_t TriCount, bool Liquid);
	static bool CustomFilter(b3ShapeId ShapeA, b3ShapeId ShapeB, void* Context);
	static bool PreSolve(b3ShapeId ShapeA, b3ShapeId ShapeB, b3Pos Point, b3Vec3 Normal, void* Context);

	b3WorldId World = b3_nullWorldId;
	b3BodyId StaticBody = b3_nullBodyId;
	b3BodyId Ground = b3_nullBodyId;
	b3MeshData* StaticMesh = nullptr;
	xr_vector<int> MeshToCdbTriangle;
	b3ShapeId LiquidShape = b3_nullShapeId;
	b3MeshData* LiquidMesh = nullptr;
	xr_vector<int> LiquidToCdbTriangle;


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
	bool IsFrozen = false;
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
