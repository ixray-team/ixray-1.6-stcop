#include "StdAfx.h"
#include "PHStaticCollisionCache.h"
#include "PHWorld.h"
#include "ExtendedGeom.h"
#include "dcylinder/dCylinder.h"
#include "tri-colliderknoopc/dTriCollideK.h"
#include "tri-colliderknoopc/dcTriListCollider.h"
#include "../3rd-party/ode/include/ode/odemath.h"
#include "../xrCore/Collision/xrCDB.h"

#ifdef DEBUG
#include "debug_output.h"
#endif

namespace StaticCollisionExecution
{
constexpr u32 MaxJobs = 32;
constexpr u32 MaxWorkers = 8;
constexpr u32 MaxContacts = 128;
constexpr u32 MaxTriangles = 4096;
constexpr u32 MaxHierarchyDepth = 16;

struct GeometryInput
{
	int Class = -1;
	dBodyID Body = nullptr;
	dVector3 Position{};
	dMatrix3 Rotation{};
	dVector3 Velocity{};
	dVector3 Size{};

	bool Read(dxGeom* Geometry, dBodyID EffectiveBody, const dReal* WorldPosition, const dReal* WorldRotation)
	{
		Class = dGeomGetClass(Geometry);
		Body = EffectiveBody;
		memcpy(Position, WorldPosition, 3 * sizeof(dReal));
		memcpy(Rotation, WorldRotation, sizeof(Rotation));
		if (Body)
		{
			memcpy(Velocity, dBodyGetLinearVel(Body), 3 * sizeof(dReal));
		}
		if (Class == dSphereClass && Body)
		{
			Size[0] = dGeomSphereGetRadius(Geometry);
		}
		else if (Class == dBoxClass)
		{
			dGeomBoxGetLengths(Geometry, Size);
		}
		else if (Class == dCylinderClassUser && Body)
		{
			dGeomCylinderGetParams(Geometry, &Size[0], &Size[1]);
		}
		else
		{
			return false;
		}
		return true;
	}

	bool Matches(dxGeom* Geometry) const
	{
		GeometryInput Current;
		return Current.Read(Geometry, dGeomGetBody(Geometry), dGeomGetPosition(Geometry), dGeomGetRotation(Geometry)) &&
			   Class == Current.Class && Body == Current.Body &&
			   memcmp(Position, Current.Position, 3 * sizeof(dReal)) == 0 &&
			   memcmp(Rotation, Current.Rotation, sizeof(Rotation)) == 0 &&
			   memcmp(Velocity, Current.Velocity, 3 * sizeof(dReal)) == 0 &&
			   memcmp(Size, Current.Size, 3 * sizeof(dReal)) == 0;
	}
};

static bool SameUserData(const dxGeomUserData& First, const dxGeomUserData& Second)
{
	return memcmp(First.last_pos, Second.last_pos, 3 * sizeof(dReal)) == 0 &&
		   First.neg_tri == Second.neg_tri && First.b_neg_tri == Second.b_neg_tri &&
		   First.ph_object == Second.ph_object && First.ph_ref_object == Second.ph_ref_object &&
		   First.material == Second.material && First.tri_material == Second.tri_material &&
		   First.element_position == Second.element_position && First.bone_id == Second.bone_id &&
		   First.callback == Second.callback && First.callback_data == Second.callback_data &&
		   First.object_callbacks == Second.object_callbacks && First.cashed_tries == Second.cashed_tries &&
		   memcmp(&First.last_aabb_pos, &Second.last_aabb_pos, sizeof(Fvector)) == 0 &&
		   memcmp(&First.last_aabb_size, &Second.last_aabb_size, sizeof(Fvector)) == 0 &&
		   First.pushing_neg == Second.pushing_neg && First.pushing_b_neg == Second.pushing_b_neg &&
		   First.b_static_colide == Second.b_static_colide;
}

struct CallbackEvent
{
	CDB::TRI* Triangle = nullptr;
	dContactGeom Contact{};
};

class Job final
{
public:
	dxGeom* Source = nullptr;
	ISpatialShared Owner;
	dGeomID Geometry = nullptr;
	dBodyID Body = nullptr;
	GeometryInput Input;
	dxGeomUserData InitialData{};
	dxGeomUserData WorkingData{};
	xr_array<dContact, MaxContacts> Contacts{};
	xr_array<CallbackEvent, MaxContacts> Events{};
	u32 EventCount = 0;
	int ContactCount = 0;
	bool Overflow = false;
	bool Complete = false;
	bool Consumed = false;

	~Job()
	{
		if (Geometry)
		{
			dGeomDestroy(Geometry);
		}
		if (Body)
		{
			dBodyDestroy(Body);
		}
	}

	void ConfigureClone()
	{
		if (Geometry && dGeomGetClass(Geometry) != Input.Class)
		{
			dGeomDestroy(Geometry);
			Geometry = nullptr;
		}
		if (!Geometry)
		{
			if (Input.Class == dSphereClass)
			{
				Geometry = dCreateSphere(nullptr, Input.Size[0]);
			}
			else if (Input.Class == dBoxClass)
			{
				Geometry = dCreateBox(nullptr, Input.Size[0], Input.Size[1], Input.Size[2]);
			}
			else
			{
				Geometry = dCreateCylinder(nullptr, Input.Size[0], Input.Size[1]);
			}
		}
		else if (Input.Class == dSphereClass)
		{
			dGeomSphereSetRadius(Geometry, Input.Size[0]);
		}
		else if (Input.Class == dBoxClass)
		{
			dGeomBoxSetLengths(Geometry, Input.Size[0], Input.Size[1], Input.Size[2]);
		}
		else
		{
			dGeomCylinderSetParams(Geometry, Input.Size[0], Input.Size[1]);
		}
		if (Input.Body && !Body)
		{
			Body = dBodyCreate(nullptr);
		}
		dGeomSetBody(Geometry, Input.Body ? Body : nullptr);
		if (Input.Body)
		{
			dBodySetPosition(Body, Input.Position[0], Input.Position[1], Input.Position[2]);
			dBodySetRotation(Body, Input.Rotation);
			// Keep the exact collision matrix; the body setter normalizes through a quaternion.
			memcpy(const_cast<dReal*>(dGeomGetRotation(Geometry)), Input.Rotation, sizeof(Input.Rotation));
			dBodySetLinearVel(Body, Input.Velocity[0], Input.Velocity[1], Input.Velocity[2]);
		}
		else
		{
			dGeomSetPosition(Geometry, Input.Position[0], Input.Position[1], Input.Position[2]);
			dGeomSetRotation(Geometry, Input.Rotation);
		}
		dGeomSetData(Geometry, &WorkingData);
	}
};

thread_local Job* g_CurrentJob = nullptr;

class RecordingScope final
{
	Job* Previous;

public:
	explicit RecordingScope(Job& Current) : Previous(g_CurrentJob) { g_CurrentJob = &Current; }
	~RecordingScope() { g_CurrentJob = Previous; }
	RecordingScope(const RecordingScope&) = delete;
	RecordingScope& operator=(const RecordingScope&) = delete;
};

static void RecordCallback(CDB::TRI* Triangle, dContactGeom* Contact)
{
	Job& Current = *g_CurrentJob;
	if (Current.EventCount >= MaxContacts)
	{
		Current.Overflow = true;
		return;
	}
	Current.Events[Current.EventCount++] = {Triangle, *Contact};
}

class Worker final
{
public:
	dcTriListCollider Collider;
	explicit Worker(dxGeom* Mesh) : Collider(Mesh) {}
};

static void RemapContact(dContactGeom& Contact, const Job& Current)
{
	if (Contact.g1 == Current.Geometry)
	{
		Contact.g1 = Current.Source;
	}
	if (Contact.g2 == Current.Geometry)
	{
		Contact.g2 = Current.Source;
	}
}
} // namespace StaticCollisionExecution

bool IsParallelStaticCollision()
{
	return StaticCollisionExecution::g_CurrentJob != nullptr;
}

void MarkParallelStaticCollisionOverflow()
{
	if (StaticCollisionExecution::g_CurrentJob)
	{
		StaticCollisionExecution::g_CurrentJob->Overflow = true;
	}
}

bool AcceptParallelStaticTriangleCount(size_t Count)
{
	if (IsParallelStaticCollision() && Count > StaticCollisionExecution::MaxTriangles)
	{
		MarkParallelStaticCollisionOverflow();
		return false;
	}
	return true;
}

class CPHStaticCollisionCache::Implementation final
{
public:
	dxGeom* Mesh;
	ContactCallbackFun* ShellCallback = nullptr;
	ContactCallbackFun* CharacterCallback = nullptr;
	xr_vector<xr_unique_ptr<StaticCollisionExecution::Job>> Jobs;
	xr_vector<xr_unique_ptr<StaticCollisionExecution::Worker>> Workers;
	xr_atomic_u32 NextJob{0};
	u32 JobCount = 0;
	u32 WorkerCount = 0;

	explicit Implementation(dxGeom* MeshGeometry) : Mesh(MeshGeometry) {}

	void Capture(dxGeom* Geometry, const ISpatialShared& Owner, const dReal* ParentPosition, const dReal* ParentRotation, dBodyID ParentBody, u32 Depth)
	{
		using namespace StaticCollisionExecution;
		if (!Geometry || JobCount >= MaxJobs || Depth >= MaxHierarchyDepth)
		{
			return;
		}
		if (dGeomIsSpace(Geometry))
		{
			const dSpaceID Space = (dSpaceID)Geometry;
			const int Count = dSpaceGetNumGeoms(Space);
			for (int Index = 0; Index < Count && JobCount < MaxJobs; ++Index)
			{
				Capture(dSpaceGetGeom(Space, Index), Owner, ParentPosition, ParentRotation, ParentBody, Depth + 1);
			}
			return;
		}
		const int Class = dGeomGetClass(Geometry);
		if (Class != dGeomTransformClass && Class != dSphereClass && Class != dBoxClass && Class != dCylinderClassUser)
		{
			return;
		}
		dVector3 Position{};
		dMatrix3 Rotation{};
		if (ParentPosition)
		{
			dMULTIPLY0_331(Position, ParentRotation, dGeomGetPosition(Geometry));
			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				Position[Axis] += ParentPosition[Axis];
			}
			dMULTIPLY0_333(Rotation, ParentRotation, dGeomGetRotation(Geometry));
		}
		else
		{
			memcpy(Position, dGeomGetPosition(Geometry), 3 * sizeof(dReal));
			memcpy(Rotation, dGeomGetRotation(Geometry), sizeof(Rotation));
		}
		dBodyID EffectiveBody = ParentPosition ? ParentBody : dGeomGetBody(Geometry);
		if (Class == dGeomTransformClass)
		{
			Capture(dGeomTransformGetGeom(Geometry), Owner, Position, Rotation, EffectiveBody, Depth + 1);
			return;
		}
		const dxGeomUserData* SourceData = dGeomGetUserData(Geometry);
		if (!SourceData || !Owner || !SourceData->ph_object || Owner->dcast_CPHObject() != SourceData->ph_object ||
			!SourceData->b_static_colide || SourceData->cashed_tries.size() > MaxTriangles ||
			(SourceData->callback && SourceData->callback != ShellCallback && SourceData->callback != CharacterCallback))
		{
			return;
		}
		GeometryInput Input;
		if (!Input.Read(Geometry, EffectiveBody, Position, Rotation))
		{
			return;
		}
		if (JobCount == Jobs.size())
		{
			Jobs.push_back(xr_make_unique<Job>());
		}
		Job& Current = *Jobs[JobCount++];
		Current.Source = Geometry;
		Current.Owner = Owner;
		Current.Input = Input;
		Current.InitialData = *SourceData;
		Current.WorkingData = *SourceData;
		Current.WorkingData.callback = SourceData->callback ? &RecordCallback : nullptr;
		Current.EventCount = 0;
		Current.ContactCount = 0;
		Current.Overflow = false;
		Current.Complete = false;
		Current.Consumed = false;
		Current.ConfigureClone();
	}
};

void CPHStaticCollisionCache::ImplementationDeleter::operator()(Implementation* Pointer) const
{
	xr_delete(Pointer);
}

CPHStaticCollisionCache::CPHStaticCollisionCache(dxGeom* Mesh) : Data(xr_make_unique<Implementation>(Mesh).release()) {}
CPHStaticCollisionCache::~CPHStaticCollisionCache() = default;

void ConfigureParallelStaticCallbacks(ContactCallbackFun* Shell, ContactCallbackFun* Character)
{
	if (ph_world)
	{
		ph_world->SetParallelStaticCallbacks(Shell, Character);
	}
}

void CPHStaticCollisionCache::SetCallbacks(ContactCallbackFun* Shell, ContactCallbackFun* Character)
{
	Data->ShellCallback = Shell;
	Data->CharacterCallback = Character;
}

u32 CPHStaticCollisionCache::Prepare(const xr_vector<SPHCollisionQuery>& Queries, u32 Workers)
{
	PROF_EVENT("PH Collide Snapshot");
	Data->JobCount = 0;
	Data->WorkerCount = 0;
	Data->NextJob.store(0);
	if (!g_PhysicsParallelNarrowphase)
	{
		EndBatch(true);
		return 0;
	}
#ifdef DEBUG
	// Drawing and detailed triangle counters retain the ordinary owner-thread path.
	if (debug_output().ph_dbg_draw_mask().get())
	{
		EndBatch(true);
		return 0;
	}
#endif
	for (const SPHCollisionQuery& Query : Queries)
	{
		Data->Capture(Query.Geometry, Query.Owner, nullptr, nullptr, nullptr, 0);
	}
	Data->WorkerCount = std::min({Workers, Data->JobCount, StaticCollisionExecution::MaxWorkers});
	while (Data->Workers.size() < Data->WorkerCount)
	{
		Data->Workers.push_back(xr_make_unique<StaticCollisionExecution::Worker>(Data->Mesh));
	}
	return Data->WorkerCount;
}

void CPHStaticCollisionCache::RunWorker(u32 Worker)
{
	using namespace StaticCollisionExecution;
	if (Worker >= Data->WorkerCount)
	{
		return;
	}
	PROF_EVENT("PH Collide Narrowphase");
	dcTriListCollider& Collider = Data->Workers[Worker]->Collider;
	for (;;)
	{
		const u32 Index = Data->NextJob.fetch_add(1);
		if (Index >= Data->JobCount)
		{
			break;
		}
		PROF_EVENT("PH Static Collision Job");
		Job& Current = *Data->Jobs[Index];
		RecordingScope Recording(Current);
		if (Current.Input.Class == dSphereClass)
		{
			Current.ContactCount = Collider.CollideSphere(Current.Geometry, MaxContacts, &Current.Contacts[0].geom, sizeof(dContact));
		}
		else if (Current.Input.Class == dBoxClass)
		{
			Current.ContactCount = Collider.CollideBox(Current.Geometry, MaxContacts, &Current.Contacts[0].geom, sizeof(dContact));
		}
		else
		{
			Current.ContactCount = Collider.CollideCylinder(Current.Geometry, MaxContacts, &Current.Contacts[0].geom, sizeof(dContact));
		}
		Current.Complete = !Current.Overflow && Current.ContactCount >= 0 && Current.ContactCount < (int)MaxContacts - 16;
		Collider.TrimScratch(MaxTriangles);
	}
}

void CPHStaticCollisionCache::EndBatch(bool Release)
{
	Data->JobCount = 0;
	Data->WorkerCount = 0;
	if (Release)
	{
		Data->Jobs.clear();
		Data->Workers.clear();
		return;
	}
	for (auto& Current : Data->Jobs)
	{
		Current->Source = nullptr;
		Current->Owner.reset();
		Current->InitialData.cashed_tries.clear();
		Current->WorkingData.cashed_tries.clear();
		if (Current->InitialData.cashed_tries.capacity() > StaticCollisionExecution::MaxTriangles)
		{
			xr_vector<int>().swap(Current->InitialData.cashed_tries);
		}
		if (Current->WorkingData.cashed_tries.capacity() > StaticCollisionExecution::MaxTriangles)
		{
			xr_vector<int>().swap(Current->WorkingData.cashed_tries);
		}
	}
}

bool CPHStaticCollisionCache::TryCollide(dxGeom* Mesh, dxGeom* Primitive, int Flags, dContactGeom* Contacts, int Stride, int& Count)
{
	using namespace StaticCollisionExecution;
	if (!g_PhysicsParallelNarrowphase || Mesh != Data->Mesh || Flags < (int)MaxContacts || Stride != sizeof(dContact))
	{
		return false;
	}
	for (u32 Index = 0; Index < Data->JobCount; ++Index)
	{
		Job& Current = *Data->Jobs[Index];
		if (Current.Source != Primitive || Current.Consumed)
		{
			continue;
		}
		Current.Consumed = true;
		dxGeomUserData* TargetData = dGeomGetUserData(Primitive);
		if (!Current.Complete || !TargetData || !Current.Owner || !TargetData->ph_object ||
			Current.Owner->dcast_CPHObject() != TargetData->ph_object || Current.Owner != TargetData->ph_object->SpatialComponent ||
			!Current.Input.Matches(Primitive) || !SameUserData(Current.InitialData, *TargetData))
		{
			return false;
		}
		PROF_EVENT("PH Static Cache Hit");
#ifdef DEBUG
		debug_output().dbg_total_saved_tries() -= (u32)TargetData->cashed_tries.size();
		debug_output().dbg_total_saved_tries() += (u32)Current.WorkingData.cashed_tries.size();
#endif
		memcpy(TargetData->last_pos, Current.WorkingData.last_pos, 3 * sizeof(dReal));
		TargetData->neg_tri = Current.WorkingData.neg_tri;
		TargetData->b_neg_tri = Current.WorkingData.b_neg_tri;
		TargetData->tri_material = Current.WorkingData.tri_material;
		TargetData->pushing_neg = Current.WorkingData.pushing_neg;
		TargetData->pushing_b_neg = Current.WorkingData.pushing_b_neg;
		TargetData->last_aabb_pos = Current.WorkingData.last_aabb_pos;
		TargetData->last_aabb_size = Current.WorkingData.last_aabb_size;
		TargetData->cashed_tries.swap(Current.WorkingData.cashed_tries);
		Count = Current.ContactCount;
		for (int ContactIndex = 0; ContactIndex < Count; ++ContactIndex)
		{
			dContact* Destination = (dContact*)((u8*)Contacts - offsetof(dContact, geom) + ContactIndex * Stride);
			*Destination = Current.Contacts[ContactIndex];
			RemapContact(Destination->geom, Current);
		}
		for (u32 EventIndex = 0; EventIndex < Current.EventCount; ++EventIndex)
		{
			CallbackEvent Event = Current.Events[EventIndex];
			RemapContact(Event.Contact, Current);
			Current.InitialData.callback(Event.Triangle, &Event.Contact);
		}
		return true;
	}
	return false;
}
