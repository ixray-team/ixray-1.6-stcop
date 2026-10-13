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
#include <thread>

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

CPHWorld::CPHWorld() :
	m_serial_collide_context(nullptr),
	m_parallel_phase_active(false),
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

	m_serial_collide_context = new SPHCollideContext();
	m_parallel_phase_active = false;

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
	Mesh.Destroy();

#ifdef DEBUG
	debug_output().PH_DBG_Clear();
#endif
	dGeomDestroy(m_motion_ray);
	dJointGroupEmpty(ContactGroup);
	dJointGroupDestroy(ContactGroup);

	ContactFeedBacks.clear();
	ContactEffectors.clear();

	DestroyCollideContexts();

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

SPHCollideContext::SPHCollideContext()
{
	Contacts.reserve(1024);
	ContactScratch.resize(800 + 10);
	Pairs.reserve(256);
	Objects.reserve(64);
	PairBounds.reserve(65);
	DirtyObjects.reserve(64);
}

SPHCollideContext::~SPHCollideContext()
{
	if (MotionRay)
	{
		dGeomDestroy(MotionRay);
		MotionRay = nullptr;
	}
}

dGeomID SPHCollideContext::GetMotionRay()
{
	if (!MotionRay)
		MotionRay = dCreateRayMotions(0);

	return MotionRay;
}

void SPHCollideContext::Reset()
{
	Spatial.clear();
	Contacts.clear();
	Pairs.clear();
	Objects.clear();
	PairBounds.clear();
	DirtyObjects.clear();
	Collecting = false;
}

static thread_local SPHCollideContext* t_current_collide_context = nullptr;

void CPHWorld::SetCurrentCollideContext(SPHCollideContext* ctx)
{
	t_current_collide_context = ctx;
}

SPHCollideContext& CPHWorld::GetCollideContext()
{
	if (t_current_collide_context)
		return *t_current_collide_context;

	VERIFY(ph_world);
	VERIFY(ph_world->m_serial_collide_context);
	return *ph_world->m_serial_collide_context;
}

static bool UseParallelPhysics()
{
	if (phThreads == 1)
		return false;

	return xr_max_concurrency() > 1;
}

static u32 ParallelPhysicsThreads()
{
	if (phThreads > 1)
		return (u32)phThreads;

	const size_t concurrency = xr_max_concurrency();
	return concurrency > 1 ? (u32)concurrency : 1u;
}

template <class F>
static void RunNamedWorker(std::thread::id caller, const char* name, F&& body)
{
	if (std::this_thread::get_id() == caller)
	{
		body();
		return;
	}

	PROF_THREAD(name);
	body();
}

void CPHWorld::EnsureCollideContexts(u32 count)
{
	while (m_collide_contexts.size() < count)
	{
		SPHCollideContext* ctx = new SPHCollideContext();
		ctx->GetMotionRay();
		m_collide_contexts.push_back(ctx);
	}
}

void CPHWorld::DestroyCollideContexts()
{
	for (SPHCollideContext* ctx : m_collide_contexts)
		delete ctx;

	m_collide_contexts.clear();

	delete m_serial_collide_context;
	m_serial_collide_context = nullptr;
	t_current_collide_context = nullptr;
}

void CPHWorld::CollectPair(CPHObject* obj1, CPHObject* obj2, dGeomID o1, dGeomID o2, bool static_pair)
{
	SPHCollideContext& ctx = GetCollideContext();

	SPHCollidePair& pair = ctx.Pairs.emplace_back();
	pair.Obj1 = obj1;
	pair.Obj2 = obj2;
	pair.O1 = o1;
	pair.O2 = o2;
	pair.Static = static_pair;
}

void CPHWorld::ProcessCollisions()
{
	xr_vector<CPHObject*> snapshot;
	snapshot.reserve(m_objects.Count());

	for (CPHObject* obj : m_objects)
		snapshot.push_back(obj);

	const u32 count = (u32)snapshot.size();

	for (u32 i = 0; i < count; ++i)
		snapshot[i]->CollidePhaseIndex = i;

	if (count >= 8 && !m_parallel_phase_active && UseParallelPhysics())
	{
		u32 chunks = ParallelPhysicsThreads() * 2;
		if (chunks > count)
			chunks = count;

		ProcessCollisionsParallel(snapshot, chunks);
		NarrowphaseCollisions(chunks);
		ApplyCollisions(chunks);
		return;
	}

	PROF_EVENT("PH Collide Serial");

	for (CPHObject* obj : snapshot)
	{
#ifdef	DEBUG
		debug_output().DBG_ObjBeforeCollision(obj);
#endif
		obj->Collide();
#ifdef	DEBUG
		debug_output().DBG_ObjAfterCollision(obj);
#endif
	}
}

void CPHWorld::ProcessCollisionsParallel(xr_vector<CPHObject*>& objects, u32 chunks)
{
	EnsureCollideContexts(chunks);

	m_parallel_phase_active = true;

	xr_vector<SPHCollideContext*>& contexts = m_collide_contexts;
	const u32 count = (u32)objects.size();

	u32 workers = ParallelPhysicsThreads();
	if (workers > chunks)
		workers = chunks;

	const std::thread::id caller = std::this_thread::get_id();
	xr_atomic_u32 next_chunk(0);

	for (u32 w = 0; w < workers; ++w)
	{
		m_physics_tasks.run([&contexts, &objects, &next_chunk, caller, count, chunks]()
		{
			RunNamedWorker(caller, "PH Collide Worker", [&]()
			{
				PROF_EVENT("PH Collide Collect");

				for (;;)
				{
					const u32 chunk = next_chunk.fetch_add(1);
					if (chunk >= chunks)
						break;

					SPHCollideContext& ctx = *contexts[chunk];
					ctx.Reset();
					ctx.Collecting = true;
					CPHWorld::SetCurrentCollideContext(&ctx);

					struct SContextReset
					{
						~SContextReset() { CPHWorld::SetCurrentCollideContext(nullptr); }
					} context_reset;

					const u32 begin = (u32)((u64)chunk * count / chunks);
					const u32 end = (u32)((u64)(chunk + 1) * count / chunks);

					for (u32 i = begin; i < end; ++i)
					{
						CPHObject* obj = objects[i];
						ctx.Objects.push_back(obj);
						ctx.PairBounds.push_back((u32)ctx.Pairs.size());
						obj->Collide();
					}

					ctx.PairBounds.push_back((u32)ctx.Pairs.size());
				}
			});
		});
	}

	m_physics_tasks.wait();
}

static void RunNarrowphase(SPHCollideContext& ctx, SPHCollidePair& pair)
{
	const int N = 800 + 10;
	int n = dCollide(pair.O1, pair.O2, N, &ctx.ContactScratch[0].geom, sizeof(dContact));
	if (n > N - 1)
		n = N - 1;

	const u32 first = (u32)ctx.Contacts.size();

	if (n > 0)
		ctx.Contacts.insert(ctx.Contacts.end(), ctx.ContactScratch.begin(), ctx.ContactScratch.begin() + n);

	pair.FirstContact = first;
	pair.ContactCount = (u32)n;
}

static void NarrowphasePair(SPHCollideContext& ctx, SPHCollidePair& pair)
{
	CPHObject* first = pair.Obj1;
	CPHObject* second = pair.Obj2;

	if (first == second)
	{
		xrSRWLockGuard guard(&first->CollideGuard, false);
		RunNarrowphase(ctx, pair);
		return;
	}

	if (first->CollidePhaseIndex < second->CollidePhaseIndex)
	{
		xrSRWLockGuard guard1(&first->CollideGuard, false);
		xrSRWLockGuard guard2(&second->CollideGuard, false);
		RunNarrowphase(ctx, pair);
	}
	else
	{
		xrSRWLockGuard guard2(&second->CollideGuard, false);
		xrSRWLockGuard guard1(&first->CollideGuard, false);
		RunNarrowphase(ctx, pair);
	}
}

void CPHWorld::NarrowphaseCollisions(u32 chunks)
{
	xr_vector<SPHCollideContext*>& contexts = m_collide_contexts;
	const std::thread::id caller = std::this_thread::get_id();
	xr_atomic_u32 next_chunk(0);

	u32 workers = ParallelPhysicsThreads();
	if (workers > chunks)
		workers = chunks;

	for (u32 w = 0; w < workers; ++w)
	{
		m_physics_tasks.run([&contexts, &next_chunk, caller, chunks]()
		{
			RunNamedWorker(caller, "PH Narrowphase Worker", [&]()
			{
				PROF_EVENT("PH Collide Narrowphase");

				for (;;)
				{
					const u32 chunk = next_chunk.fetch_add(1);
					if (chunk >= chunks)
						break;

					SPHCollideContext& ctx = *contexts[chunk];
					for (SPHCollidePair& pair : ctx.Pairs)
						NarrowphasePair(ctx, pair);
				}
			});
		});
	}

	m_physics_tasks.wait();
}

void CPHWorld::ApplyCollidePair(const SPHCollidePair& pair, dContact* contacts)
{
	if (pair.Static)
	{
		CPHIsland* island = pair.Obj2->DActiveIsland();
		ApplyContacts(contacts, (int)pair.ContactCount, GetMeshGeom(), pair.O2, ContactGroup, island, island->MaxJoints());
		return;
	}

	CPHIsland* island1 = pair.Obj1->DActiveIsland();
	CPHIsland* island2 = pair.Obj2->DActiveIsland();
	pair.Obj2->near_callback(pair.Obj1);
	int MAX_CONTACTS = -1;
	if (!island1->CanMerge(island2, MAX_CONTACTS))
		return;

	if (ApplyContacts(contacts, (int)pair.ContactCount, pair.O1, pair.O2, ContactGroup, island1, MAX_CONTACTS) != 0)
	{
		pair.Obj1->MergeIsland(pair.Obj2);
		if (!pair.Obj2->is_active())
			pair.Obj2->EnableObject(pair.Obj1);
	}
}

void CPHWorld::ApplyCollisions(u32 chunks)
{
	PROF_EVENT("PH Collide Apply");

	for (u32 chunk = 0; chunk < chunks; ++chunk)
	{
		SPHCollideContext& ctx = *m_collide_contexts[chunk];

		for (u32 k = 0; k < (u32)ctx.Objects.size(); ++k)
		{
			CPHObject* obj = ctx.Objects[k];
			const u32 pairBegin = ctx.PairBounds[k];
			const u32 pairEnd = ctx.PairBounds[k + 1];

			for (u32 p = pairBegin; p < pairEnd; ++p)
			{
				const SPHCollidePair& pair = ctx.Pairs[p];
				ApplyCollidePair(pair, ctx.Contacts.data() + pair.FirstContact);
			}

			obj->CollidePostProcess();
		}

		for (CPHObject* obj : ctx.DirtyObjects)
			obj->ClearDirty();
	}

	m_parallel_phase_active = false;
}

void CPHWorld::StepIslands()
{
	xr_vector<CPHIsland*> islands;
	islands.reserve(m_objects.Count());

	for (CPHObject* obj : m_objects)
	{
		CPHIsland* island = &obj->Island();
		if (island->IsActive())
			islands.push_back(island);
	}

#ifdef DEBUG
	if (debug_output().ph_dbg_draw_mask().test(phDbgDrawObjectStatistics))
	{
		for (CPHIsland* island : islands)
		{
			debug_output().dbg_islands_num()++;
			debug_output().dbg_joints_num() += island->nj;
			debug_output().dbg_bodies_num() += island->nb;
		}
	}
#endif

	const u32 count = (u32)islands.size();

	if (count >= 4 && !m_parallel_phase_active && UseParallelPhysics())
	{
		u32 workers = ParallelPhysicsThreads();
		if (workers > count)
			workers = count;

		const std::thread::id caller = std::this_thread::get_id();
		xr_atomic_u32 next_island(0);

		for (u32 w = 0; w < workers; ++w)
		{
			m_physics_tasks.run([&islands, &next_island, caller, count]()
			{
				RunNamedWorker(caller, "PH Island Worker", [&]()
				{
					PROF_EVENT("PH Island Step");

					for (;;)
					{
						const u32 i = next_island.fetch_add(1);
						if (i >= count)
							break;

						islands[i]->Step(fixed_step);
					}
				});
			});
		}

		m_physics_tasks.wait();
		return;
	}

	PROF_EVENT("PH Island Step Serial");

	for (CPHIsland* island : islands)
		island->Step(fixed_step);
}
