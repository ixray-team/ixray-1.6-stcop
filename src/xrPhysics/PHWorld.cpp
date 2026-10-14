#include "StdAfx.h"

#include "PHWorld.h"
#include "tri-colliderknoopc/dTriList.h"
#include "PhysicsCommon.h"
#include "ExtendedGeom.h"
#include "dRayMotions.h"
#include "PHCollideValidator.h"
#include "../xrEngine/GameMtlLib.h"

#include "params.h"
#include "../xrServerEntities/PHSynchronize.h"
#include "../xrServerEntities/PHNetState.h"
#include "GeometryBits.h"
#include "console_vars.h"
#include "../xrEngine/device.h"
#include "../xrEngine/defines.h"
#include "../xrCore/Collision/xr_area.h"
#include "../xrCore/FS_internal.h"

#ifdef DEBUG_DRAW
#	include "debug_output.h"
#endif

//////////////CPHMesh///////////////////////////////////////////
CPHWorld *ph_world = nullptr;

IPHWorld* physics_world()
{
	return ph_world;
}

void  create_physics_world(bool mt, CObjectSpace* os, CObjectList* lo)
{
	ph_world = new CPHWorld(); //&commander
	VERIFY(os);
	ph_world->Create(mt, os, lo);
}

void  destroy_physics_world()
{
	ph_world->Destroy();
	xr_delete(ph_world);
}

CObjectSpace*  create_object_space()
{
	//CFileReader* fr = new CFileReader("ActorEditorLevel.cform");
	CObjectSpace* os = new CObjectSpace();
	g_SpatialSpace = new ISpatial_DB();
	g_SpatialSpacePhysic = new ISpatial_DB();
	os->Load(nullptr, "ActorEditorLevel", nullptr);
	//os->Load(fr, 0);

	return os;
}

CObjectSpace* mesh_create_object_space(const XRay::CForm::IFormat& CForm, CDB::build_callback build_callback)
{
	CObjectSpace* os = new CObjectSpace();
	g_SpatialSpace				= new ISpatial_DB	();
	g_SpatialSpacePhysic		= new ISpatial_DB	();
	os->Create( CForm, build_callback, nullptr, false);
	return os;
}

void  destroy_object_space(CObjectSpace* &os)
{
	xr_delete(os);
}

void CPHMesh ::Create(dSpaceID space, dWorldID world)
{
	Geom = dCreateTriList(space, 0, 0);
	CPHGeometryBits::init_geom(*this);
}

void CPHMesh::Destroy()
{
	dGeomDestroy(Geom);
	dTriListClass = -1;
}

///////////CPHWorld/////////////////////////////////////////////////////////

#ifdef DEBUG_DRAW
void CPHWorld::OnRender()
{
	debug_output().PH_DBG_Render();
}
#endif

static struct sempty_update_callback:
public IPHWorldUpdateCallbck
{
	void		update_step			(){};
	void		phys_shell_relcase	(CPhysicsShell* sh){};
} empty_update_callback;

namespace PhysicsExecution
{
constexpr u32 MaxQueryObjects = 64;
constexpr u32 MaxQueryResults = 256;

class PhaseScope final
{
	bool& IsActive;

public:
	explicit PhaseScope(bool& Active) : IsActive(Active) { IsActive = true; }
	~PhaseScope() { IsActive = false; }
	PhaseScope(const PhaseScope&) = delete;
	PhaseScope& operator=(const PhaseScope&) = delete;
};

static u32 WorkerCount()
{
	if (phThreads == 1)
	{
		return 1;
	}
	const u32 Available = (u32)xr_max_concurrency();
	const u32 Requested = phThreads > 0 ? (u32)phThreads : 8u;
	return std::max(1u, std::min(Available, Requested));
}
}

CPHWorld::CPHWorld() :
	m_update_callback(&empty_update_callback),
	m_default_contact_shotmark(0),
	m_default_character_contact_shotmark(0),
	m_object_space(0),
	m_level_objects(0)
{
	disable_count = 0;
	m_frame_time = 0.f;
	m_previous_frame_time = 0.f;
	b_frame_mark = false;
	m_steps_num = 0;
	m_steps_short_num = 0;
	m_frame_sum = 0.f;
	m_delay = 0;
	m_previous_delay = 0;
	m_reduce_delay = 0;
	m_update_delay_count = 0;
	b_world_freezed = false;
	b_processing = false;
	m_gravity = default_world_gravity;
	b_exist = false;
}

void CPHWorld::SetStep(float s)
{
	fixed_step		= s;
	world_cfm		= Cfm(Spring(base_cfm,base_erp,base_fixed_step), Damping(base_cfm,base_erp));
	world_erp		= Erp(Spring(base_cfm,base_erp,base_fixed_step), Damping(base_cfm,base_erp));
	world_spring	= 1.0f* Spring(world_cfm,world_erp);
	world_damping	= 1.0f* Damping(world_cfm,world_erp);

	if(ph_world&&ph_world->Exist())
	{
		float	frame_time					=	Device.fTimeDelta;
		u32		it_number					=	iFloor	(frame_time /fixed_step);
		frame_time							-=	it_number*fixed_step;
		ph_world->m_previous_frame_time		=	frame_time;
		ph_world->m_frame_time				=	frame_time;
	}
}
void CPHWorld::Create(bool mt, CObjectSpace* os, CObjectList* lo)
{
	LoadParams();
	dWorldID phWorld = 0;
	m_object_space = os;
	m_level_objects = lo;


	Device.AddSeqFrame(this, mt);

#ifndef ODE_SLOW_SOLVER
	dWorldSetAutoEnableDepthSF1(phWorld, 100000000);
#endif
	ContactGroup = dJointGroupCreate(0);
	dWorldSetGravity(phWorld, 0, -Gravity(), 0);
	Mesh.Create(0, phWorld);

	dWorldSetERP(phWorld, Erp(world_spring, world_damping));
	dWorldSetCFM(phWorld, Cfm(world_spring, world_damping));

	disable_count = 0;
	m_motion_ray = dCreateRayMotions(0);
	phBoundaries.set(inl_ph_world().ObjectSpace().GetBoundingVolume());
	phBoundaries.y1 -= 30.f;
	CPHCollideValidator::Init();
	b_exist = true;

	StepNumIterations(phIterations);
	SetStep(ph_console::ph_step_time);
}

/////////////////////////////////////////////////////////////////////////////

void CPHWorld::Destroy()
{
	PhysicsTasks.wait();
	CollisionQueries.clear();
	SerialCollideContext.Spatial.clear();
	Mesh.Destroy();

#ifdef DEBUG
	debug_output().PH_DBG_Clear();
#endif
	dGeomDestroy(m_motion_ray);
	dJointGroupEmpty(ContactGroup);
	dJointGroupDestroy(ContactGroup);

	ContactFeedBacks.clear();
	ContactEffectors.clear();


	dCloseODE();

	dCylinderClassUser=-1;
	dRayMotionsClassUser=-1;

	Device.RemoveSeqFrame( this );
	b_exist=false;
}

void CPHWorld::SetGravity(float g)
{
	m_gravity = g;
	dWorldID phWorld = 0;
	dWorldSetGravity(phWorld, 0, -m_gravity, 0);
}

void CPHWorld::OnFrame()
{
	PROF_EVENT("CPHWorld::OnFrame");
	FrameStep(Device.fTimeDelta);
}

//////////////////////////////////////////////////////////////////////////////
void CPHWorld::Step()
{
	if (IsStepping || IsPhysicsPhaseActive)
	{
		return;
	}
	PhysicsExecution::PhaseScope StepScope(IsStepping);
	PROF_EVENT("CPHWorld::Step");
#ifdef DEBUG
	debug_output().dbg_reused_queries_per_step()	=0			;
	debug_output().dbg_new_queries_per_step()		=0			;
#endif
	
	VERIFY(b_processing||IsFreezed());

	PH_OBJECT_I			i_object;
	PH_UPDATE_OBJECT_I	i_update_object;

	if(disable_count==0)
	{
		disable_count=worldDisablingParams.objects_params.L2frames;
		for(i_object=m_recently_disabled_objects.begin();m_recently_disabled_objects.end() != i_object;)
		{
			CPHObject* obj=(*i_object);
			obj->check_recently_deactivated();
			++i_object;
		}
	}
	if(!IsFreezed())
		--disable_count;

	++m_steps_num;
	++m_steps_short_num;
	Device.StatPhysics()->ph_collision.Begin	();

	ProcessCollisions();

	Device.StatPhysics()->ph_collision.End	();

#ifdef DEBUG
	for(i_object=m_objects.begin();m_objects.end() != i_object;)
	{
		CPHObject* obj=(*i_object);
		if( debug_output().ph_dbg_draw_mask().test(phDbgDrawEnabledAABBS) )
		debug_output().DBG_DrawPHObject(obj);
		++i_object;
	}
#endif

	for(i_object=m_objects.begin();m_objects.end() != i_object;)
	{	
		CPHObject* obj=(*i_object);
		++i_object;

#ifdef	DEBUG
		debug_output().DBG_ObjBeforePhTune( obj );
#endif

		obj->PhTune(fixed_step);

#ifdef	DEBUG
		debug_output().DBG_ObjeAfterPhTune( obj );
#endif

	}

	for(i_update_object=m_update_objects.begin();m_update_objects.end() != i_update_object;)
	{	CPHUpdateObject* obj=(*i_update_object);
		++i_update_object;
		obj->PhTune(fixed_step);
	}

	Device.StatPhysics()->ph_core.Begin		();

#ifdef DEBUG
	debug_output().dbg_bodies_num()=0;
	debug_output().dbg_joints_num()=0;
	debug_output().dbg_islands_num()=0;
#endif
//////////////////////////////////////////////////////////////////////
	VERIFY( m_update_callback );
	m_update_callback->update_step();

//////////////////////////////////////////////////////////////////////
	StepIslands();

	Device.StatPhysics()->ph_core.End		();



	for(i_object=m_objects.begin();m_objects.end() != i_object;)
	{
		CPHObject* obj=(*i_object);
		++i_object;
		obj->IslandReinit();

#ifdef	DEBUG
		debug_output().DBG_ObjBeforePhDataUpdate( obj );
#endif

		obj->PhDataUpdate(fixed_step);

#ifdef	DEBUG
		debug_output().DBG_ObjAfterPhDataUpdate( obj );
#endif

		obj->spatial_move();
	}

	for(i_update_object=m_update_objects.begin();m_update_objects.end() != i_update_object;)
	{	
		CPHUpdateObject* obj=*i_update_object;
		++i_update_object;
		obj->PhDataUpdate(fixed_step);
	}

#ifdef DEBUG
	debug_output().dbg_contacts_num()=ContactGroup->num;
#endif
	dJointGroupEmpty(ContactGroup);//this is to be called after PhDataUpdate!!!-the order is critical!!!
	ContactFeedBacks.empty();
	ContactEffectors.empty();
}

void CPHWorld::StepTouch()
{
	if (IsStepping || IsPhysicsPhaseActive)
	{
		return;
	}
	PhysicsExecution::PhaseScope StepScope(IsStepping);
	PROF_EVENT("CPHWorld::StepTouch");
	PH_OBJECT_I i_object;
	ProcessCollisions();

	for (i_object = m_objects.begin(); m_objects.end() != i_object;)
	{
		CPHObject* obj = (*i_object);
		++i_object;
		obj->Island().Enable();
	}

	for (i_object = m_objects.begin(); m_objects.end() != i_object;)
	{
		CPHObject* obj = (*i_object);
		++i_object;
		obj->IslandReinit();
		obj->spatial_move();
	}

	dJointGroupEmpty(ContactGroup);
	ContactFeedBacks.empty();
	ContactEffectors.empty();
}

u32 CPHWorld::CalcNumSteps(u32 dTime)
{
	if (dTime < m_frame_time * 1000)
		return 0;

	u32 res = iCeil((float(dTime) - m_frame_time * 1000) / (fixed_step * 1000));

	return res;
}

void CPHWorld::FrameStep(dReal step)
{
	if (IsStepping || IsPhysicsPhaseActive)
	{
		return;
	}

	if (IsFreezed())
		return;

	VERIFY(_valid(step));
	step *= phTimefactor;

	// compute contact joints and forces
	u32 it_number;
	float frame_time = m_frame_time;
	frame_time += step;

#ifdef DEBUG
	if (debug_output().ph_dbg_draw_mask().test(phDbgDrawObjectStatistics))
	{
		static float dbg_iterations = 0.f;
		dbg_iterations = dbg_iterations * 0.9f + step / fixed_step * 0.1f;
		b_processing = true;
		debug_output().DBG_OutText("phys steps per frame %2.1f", dbg_iterations);
		b_processing = false;
	}
#endif

	if (!(frame_time < fixed_step))
	{
		it_number = iFloor(frame_time / fixed_step);
		frame_time -= it_number * fixed_step;
		m_previous_frame_time = m_frame_time;
		m_frame_time = frame_time;
		b_frame_mark = !b_frame_mark;
	}
	else
	{
		m_frame_time = frame_time;
		return;
	}

#ifdef DEBUG 
	debug_output().DBG_DrawFrameStart();
	debug_output().DBG_DrawStatBeforeFrameStep();
#endif

	b_processing = true;

	if (ph_console::g_bDebugDumpPhysicsStep && it_number > 20)
		Msg("!!!TOO MANY PHYSICS STEPS PER FRAME = %d !!!", it_number);

	for (UINT i = 0; i < it_number; ++i)
		Step();

	b_processing = false;
#ifdef DEBUG
	debug_output().DBG_DrawStatAfterFrameStep();
#endif
}

void CPHWorld::AddObject(CPHObject* object)
{
	m_objects.PushBack(object);
}

void CPHWorld::AddRecentlyDisabled(CPHObject* object)
{
	m_recently_disabled_objects.PushBack(object);
}

void CPHWorld::RemoveFromRecentlyDisabled(CPHObject* i)
{
	m_recently_disabled_objects.Erase(i);
}

void CPHWorld::AddUpdateObject(CPHUpdateObject* object)
{
	m_update_objects.PushBack(object);
}

void CPHWorld::RemoveUpdateObject(CPHUpdateObject* i)
{
	m_update_objects.Erase(i);
}

void CPHWorld::RemoveObject(CPHObject* i)
{
	m_objects.Erase(i);
}

void CPHWorld::AddFreezedObject(CPHObject* obj)
{
	m_freezed_objects.PushBack(obj);
}

void CPHWorld::RemoveFreezedObject(CPHObject* i)
{
	m_freezed_objects.Erase(i);
}

void CPHWorld::Freeze()
{
	R_ASSERT2(!b_world_freezed, "already freezed!!!");
	m_freezed_objects.MoveItems(m_objects);

	for (CPHObject* Obj : m_freezed_objects)
		Obj->FreezeContent();

	m_freezed_update_objects.MoveItems(m_update_objects);

	b_world_freezed = true;
}

void CPHWorld::UnFreeze()
{
	R_ASSERT2(b_world_freezed, "is not freezed!!!");

	for (CPHObject* Obj : m_freezed_objects)
		Obj->UnFreezeContent();

	m_objects.MoveItems(m_freezed_objects);

	m_update_objects.MoveItems(m_freezed_update_objects);
	b_world_freezed = false;
}

bool CPHWorld::IsFreezed()
{
	return b_world_freezed;
}

void CPHWorld::CutVelocity(float l_limit, float a_limit)
{
	for (CPHObject* Obj : m_objects)
	{
		Obj->CutVelocity(l_limit, a_limit);
	}
}

void CPHWorld::NetRelcase(CPhysicsShell* s)
{
	VERIFY(m_update_callback);
	m_update_callback->phys_shell_relcase(s);
	PH_UPDATE_OBJECT_I	i_update_object;

	for (CPHUpdateObject* Obj : m_update_objects)
	{
		Obj->NetRelcase(s);
	}
}

u16 CPHWorld::ObjectsNumber(bool UpdateOnly)
{
	return UpdateOnly ? m_update_objects.Count() : m_objects.Count();
}

void CPHWorld::GetState(V_PH_WORLD_STATE& state)
{
	state.clear();

	for (CPHObject* Obj : m_objects)
	{
		const u16 els = Obj->get_elements_number();
		for (u16 i = 0; els > i; ++i)
		{
			std::pair<CPHSynchronize*, SPHNetState> s;
			s.first = Obj->get_element_sync(i);
			s.first->get_State(s.second);
			state.push_back(s);
		}
	}
}

void CPHWorld::StepNumIterations(int num_it)
{
	dWorldSetQuickStepNumIterations(nullptr, num_it);
}

/////////////////////////////////////////////////////////////////////////////

SPHCollideContext& CPHWorld::GetCollideContext()
{
	VERIFY(ph_world);
	return ph_world->SerialCollideContext;
}

void CPHWorld::CollectCollisionQueries(u32 Workers)
{
	xr_atomic_u32 NextQuery(0);
	const u32 Count = (u32)CollisionQueries.size();
	for (SPHCollisionQuery& Query : CollisionQueries)
	{
		Query.Spatial.reserve(PhysicsExecution::MaxQueryResults);
	}
	const auto CollectLambda = [this, &NextQuery, Count]()
	{
		PROF_EVENT("PH Collide Collect");
		for (;;)
		{
			const u32 Index = NextQuery.fetch_add(1);
			if (Index >= Count)
			{
				break;
			}
			SPHCollisionQuery& Query = CollisionQueries[Index];
			Query.IsComplete = g_SpatialSpacePhysic->QueryBoxLimited(Query.Spatial, ESPATIAL_TYPE::PHYSIC, Query.Bounds, PhysicsExecution::MaxQueryResults, Query.Revision);
		}
	};
	for (u32 Worker = 0; Worker < std::min(Workers, Count); ++Worker)
	{
		PhysicsTasks.run(CollectLambda);
	}
	PhysicsTasks.wait();
}

void CPHWorld::ProcessCollisions()
{
	if (IsPhysicsPhaseActive)
	{
		return;
	}
	PhysicsExecution::PhaseScope Phase(IsPhysicsPhaseActive);
	const u32 Workers = g_PhysicsParallelBroadphase && m_objects.Count() >= 8 ? PhysicsExecution::WorkerCount() : 1;
	const bool IsParallel = Workers > 1;
	if (!IsParallel)
	{
		xr_vector<SPHCollisionQuery>().swap(CollisionQueries);
	}

	// Keep the live iterator: callbacks can wake objects or remove them.
	for (PH_OBJECT_I Iterator = m_objects.begin(); Iterator != m_objects.end();)
	{
		u32 BatchCount = 1;
		if (IsParallel)
		{
			BatchCount = std::min(PhysicsExecution::MaxQueryObjects, Workers * 4);
			CollisionQueries.resize(BatchCount);
			u32 Count = 0;
			for (PH_OBJECT_I Next = Iterator; Next != m_objects.end() && Count < BatchCount; ++Next)
			{
				(*Next)->PrepareCollisionQuery(CollisionQueries[Count++]);
			}
			CollisionQueries.resize(Count);
			BatchCount = Count;
			CollectCollisionQueries(Workers);
		}

		PROF_EVENT("PH Collide Apply");
		for (u32 Index = 0; Index < BatchCount && Iterator != m_objects.end(); ++Index)
		{
			CPHObject* Object = *Iterator;
			SerialCollideContext.Query = nullptr;
			if (IsParallel)
			{
				for (const SPHCollisionQuery& Query : CollisionQueries)
				{
					if (Query.Owner == Object->SpatialComponent)
					{
						SerialCollideContext.Query = &Query;
						break;
					}
				}
			}
#ifdef DEBUG
			debug_output().DBG_ObjBeforeCollision(Object);
#endif
			// Narrowphase, materials, callbacks, dirty flags and activation retain
			// their original ordering. Ray motions are consumed immediately.
			Object->Collide();
#ifdef DEBUG
			debug_output().DBG_ObjAfterCollision(Object);
#endif
			SerialCollideContext.Query = nullptr;
			++Iterator;
		}
		for (SPHCollisionQuery& Query : CollisionQueries)
		{
			Query.Spatial.clear();
			Query.Owner.reset();
		}
	}
	SerialCollideContext.Spatial.clear();
	if (SerialCollideContext.Spatial.capacity() > PhysicsExecution::MaxQueryResults)
	{
		qResultVec().swap(SerialCollideContext.Spatial);
	}
}

void CPHWorld::StepIslands()
{
	if (IsPhysicsPhaseActive)
	{
		return;
	}
	PhysicsExecution::PhaseScope Phase(IsPhysicsPhaseActive);
	xr_vector<CPHIsland*> Islands;
	for (CPHObject* Object : m_objects)
	{
		CPHIsland& Island = Object->Island();
		if (Island.IsActive())
		{
			Islands.push_back(&Island);
#ifdef DEBUG
			if (debug_output().ph_dbg_draw_mask().test(phDbgDrawObjectStatistics))
			{
				debug_output().dbg_islands_num()++;
				debug_output().dbg_joints_num() += Island.nj;
				debug_output().dbg_bodies_num() += Island.nb;
			}
#endif
		}
	}

	const u32 Count = (u32)Islands.size();
	const u32 Workers = g_PhysicsParallelIslands && Count >= 4 ? std::min(PhysicsExecution::WorkerCount(), Count) : 1;
	if (Workers > 1)
	{
#ifdef DEBUG
		for (CPHObject* Object : m_objects)
		{
			debug_output().DBG_ObjBeforeStep(Object);
		}
#endif
		// Start the expensive islands first; each world owns its solver seed.
		const auto IslandCostLambda = [](const CPHIsland* First, const CPHIsland* Second)
		{
			return First->nj + First->nb > Second->nj + Second->nb;
		};
		std::sort(Islands.begin(), Islands.end(), IslandCostLambda);
		xr_atomic_u32 NextIsland(0);
		const auto StepLambda = [&Islands, &NextIsland, Count]()
		{
			PROF_EVENT("PH Island Step");
			for (;;)
			{
				const u32 Index = NextIsland.fetch_add(1);
				if (Index >= Count)
				{
					break;
				}
				Islands[Index]->Step(fixed_step);
			}
		};
		for (u32 Worker = 0; Worker < Workers; ++Worker)
		{
			PhysicsTasks.run(StepLambda);
		}
		PhysicsTasks.wait();
#ifdef DEBUG
		for (CPHObject* Object : m_objects)
		{
			debug_output().DBG_ObjAfterStep(Object);
		}
#endif
		return;
	}

	PROF_EVENT("PH Island Step Serial");
	for (PH_OBJECT_I Iterator = m_objects.begin(); Iterator != m_objects.end();)
	{
		CPHObject* Object = *Iterator;
		++Iterator;
#ifdef DEBUG
		debug_output().DBG_ObjBeforeStep(Object);
#endif
		Object->IslandStep(fixed_step);
#ifdef DEBUG
		debug_output().DBG_ObjAfterStep(Object);
#endif
	}
}
