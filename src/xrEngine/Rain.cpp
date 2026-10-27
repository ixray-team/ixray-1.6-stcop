#include "stdafx.h"

#include "Rain.h"
#include "IGame_Persistent.h"
#include "IGame_Menu.h"
#include "Environment.h"
#include "GameFont.h"
#include "Editor/XrEditorSceneInterface.h"

#include "Render.h"
#include "IGame_Level.h"
#include "../xrCore/Collision/xr_area.h"
#include "xr_object.h"

ENGINE_API bool bIsRaindropCollision = false;
ENGINE_API bool bIsSndOnRoof = false;
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CEffect_Rain::CEffect_Rain()
{
	state = stIdle;
	snd_Ambient.create("ambient\\rain", st_Effect, sg_Undefined);

	string_path RainSounds = {};
	FS.update_path(RainSounds, _game_sounds_, "ambient\\rain_drops_roof.ogg");

	if (FS.exist(RainSounds))
	{
		snd_RoofDroplets.create("ambient\\rain_drops_roof", st_Effect, sg_Undefined);
		snd_RoofDropletsHard.create("ambient\\thunder_drops_roof", st_Effect, sg_Undefined);
	}

	m_rainVolume = 0.0f;
	p_create();
	Rain_ROS = ::Render->ros_create(nullptr);
	Rain_ROS->force_mode(IRender_ObjectSpecific::TRACE_HEMI | IRender_ObjectSpecific::TRACE_SUN);
}

CEffect_Rain::~CEffect_Rain()
{
	rainCollideObjects.clear();
	snd_Ambient.destroy();
	snd_RoofDroplets.destroy();
	snd_RoofDropletsHard.destroy();
	m_rainVolume = 0.0f;

	// Cleanup
	p_destroy();
	::Render->ros_destroy(Rain_ROS);
}

void CEffect_Rain::AddRainCollidableObject(CObject* obj)
{
	xrCriticalSectionGuard Guard(rainCS);
	rainCollideObjects.push_back(obj);
}

void CEffect_Rain::RemoveRainCollidableObject(CObject* obj)
{
	xrCriticalSectionGuard Guard(rainCS);
	auto iter = std::find(rainCollideObjects.begin(), rainCollideObjects.end(), obj);
	if (iter != rainCollideObjects.end())
	{
		rainCollideObjects.erase(iter);
	}
}

ICF bool RayPick(const Fvector& s, const Fvector& d, float& range, collide::rq_target tgt, Fvector* HitNormal = nullptr)
{
	if (HitNormal)
	{
		HitNormal->set(0.f, 1.f, 0.f);
	}
	if (Device.IsEditorMode())
	{
		if (bIsRaindropCollision)
		{
			EditorScene->SetPlayInEditorRayPickCall(true);
			bool value = EditorScene->RayPick(s, d, range);
			EditorScene->SetPlayInEditorRayPickCall(false);
			return value;
		}

		return false;
	}
	else
	{
		if (!g_pGameLevel || !g_pGameLevel->bReady)
			return false;
	}

	float rangeFirstTrace = 0.f;
	bool bRes = false;
	collide::rq_result RQ;

	bRes = !!g_pGameLevel->ObjectSpace.RayPick(s, d, range, tgt, RQ, g_pGameLevel->CurrentViewEntity());
	if (bRes)
	{
		range = RQ.range;
		rangeFirstTrace = RQ.range;
		if (HitNormal && !RQ.O && RQ.element >= 0)
		{
			const auto& Triangles = g_pGameLevel->ObjectSpace.GetStaticTris();
			const auto& Vertices = g_pGameLevel->ObjectSpace.GetStaticVerts();
			if (size_t(RQ.element) < Triangles.size())
			{
				const auto& Triangle = Triangles[RQ.element];
				HitNormal->mknormal(Vertices[Triangle.verts[0]], Vertices[Triangle.verts[1]], Vertices[Triangle.verts[2]]);
				if (HitNormal->square_magnitude() < EPS_S)
				{
					HitNormal->set(0.f, 1.f, 0.f);
				}
				if (HitNormal->dotproduct(d) > 0.f)
				{
					HitNormal->invert();
				}
			}
		}
		// Dynamic/editor queries provide no surface normal; use the explicit up fallback.

		if (g_pGamePersistent->Environment().eff_Rain->rainCollideObjects.empty())
		{
			return bRes;
		}

		Fvector scanPosition;
		Fvector contactPosition;

		for (CObject* obj : g_pGamePersistent->Environment().eff_Rain->rainCollideObjects)
		{
			scanPosition.mad(s, d, rangeFirstTrace - (obj->Radius() / 3));

			if (obj->XFORM().c.distance_to(scanPosition) <= obj->Radius())
			{
				contactPosition.mad(obj->XFORM().c, scanPosition.sub(obj->XFORM().c).normalize(), obj->Radius());
				obj->OnRainCollide(contactPosition);
				break;
			}
		}
	}

	return bRes;
}

ICF void RenewItem(CEffect_Rain::Item& dest, float height, bool bHit)
{
	dest.bHit = bHit;
	dest.RemainingDistance = std::max(height, 0.f);
	// A miss still has a finite endpoint at the local rain range, without an impact event.
	dest.Phit.mad(dest.P, dest.D, dest.RemainingDistance);
}

// Born
ICF void computeDirection(CEffect_Rain::Item& item, float variation, CEnvironment& env)
{
	Fvector axis = {};
	axis.set(0.f, -1.f, 0.f);
	float k = env.CurrentEnv->rain_angle / std::max(env.drop_max_wind_vel, EPS_L);
	float pitch = env.drop_max_angle * k - PI_DIV_2;

	axis.setHP(env.CurrentEnv->rain_angle_rotation, pitch);

	item.D.random_dir(axis, deg2rad(::Random.randF(0.f, variation)));
}

ICF void Born(CEffect_Rain::Item& dest, float NearDistance)
{
	static shared_str st_drizzle = "drizzle";
	static shared_str st_dense = "dense";
	static shared_str st_spherical = "spherical";
	CEnvironment& env = g_pGamePersistent->Environment();

	const Fvector& view = Device.vCameraPosition;
	float radius = std::max(env.source_rain_radius_render + env.add_const_dist_coefficient_render, EPS_L);
	shared_str& rainType = env.CurrentEnv->rain_type;
	float source_offset = env.source_offset;
	float rain_speed_max = env.CurrentEnv->rain_speed_max;
	float angle = ::Random.randF(0.f, PI_MUL_2);
	if (rainType.equal(st_drizzle))
	{
		float dist = _sqrt(::Random.randF()) * radius;
		float x = dist * std::cos(angle);
		float z = dist * std::sin(angle);

		computeDirection(dest, 10.f, env);

		dest.P.set(x + view.x - dest.D.x * source_offset,
			source_offset + view.y,
			z + view.z - dest.D.z * source_offset);

		dest.fSpeed = ::Random.randF(env.CurrentEnv->rain_speed_min * 0.5f, rain_speed_max * 0.7f);
	}
	else if (rainType.equal(st_dense))
	{
		float dist = _sqrt(::Random.randF()) * (radius * 0.5f);
		float x = dist * std::cos(angle);
		float z = dist * std::sin(angle);

		computeDirection(dest, 5.f, env);

		dest.P.set(x + view.x - dest.D.x * source_offset,
			source_offset + view.y,
			z + view.z - dest.D.z * source_offset);

		dest.fSpeed = ::Random.randF(env.CurrentEnv->rain_speed_min, rain_speed_max);
	}
	else if (rainType.equal(st_spherical))
	{
		float phi = ::Random.randF(0.f, PI_DIV_2);
		float r = ::Random.randF() * radius;

		float x = r * sinf(phi) * cosf(angle);
		float y = r * cosf(phi);
		float z = r * sinf(phi) * sinf(angle);

		computeDirection(dest, 15.f, env);

		dest.P.set(x + view.x, y + view.y, z + view.z);
		dest.fSpeed = ::Random.randF(env.CurrentEnv->rain_speed_min, rain_speed_max);
	}
	else//default
	{
		float dist = _sqrt(::Random.randF()) * radius;
		float x = dist * std::cos(angle);
		float z = dist * std::sin(angle);

		computeDirection(dest, 10.f, env);

		dest.P.set(x + view.x - dest.D.x * source_offset,
			source_offset + view.y,
			z + view.z - dest.D.z * source_offset);

		dest.fSpeed = ::Random.randF(env.CurrentEnv->rain_speed_min, rain_speed_max);
	}

	// Place new trajectories outside the camera volume before predicting their contact.
	if (NearDistance > 0.f)
	{
		Fvector Offset;
		Offset.sub(dest.P, view);
		const float DistanceSquared = Offset.square_magnitude();
		if (DistanceSquared < _sqr(NearDistance))
		{
			if (DistanceSquared > EPS_S)
			{
				Offset.mul(1.f / _sqrt(DistanceSquared));
			}
			else
			{
				Offset.set(std::cos(angle), 0.f, std::sin(angle));
			}
			dest.P.mad(view, Offset, NearDistance + EPS_L);
		}
	}
	float height = std::max(env.max_distance + env.add_const_dist_coefficient, EPS_L);

	// RayPick shortens height to the distance of the hit, so the two calls must be sequenced:
	// inside one expression the read of height and its update are unsequenced, and MSVC and clang
	// pick different orders. With clang the drop kept its full length and fell through roofs.
	dest.fSpeed = std::max(dest.fSpeed, EPS_L);
	dest.Seed = u32(Random.randI(0x7fffffff));
	const bool bHit = RayPick(dest.P, dest.D, height, collide::rqtBoth, &dest.HitNormal);
	RenewItem(dest, height, bHit);
}

void CEffect_Rain::Enable(bool status)
{
	xrCriticalSectionGuard Guard(rainCS);
	IsEnabled = status;
}

void CEffect_Rain::UpdateHudDrops(float rain, float sky)
{
	const float dt = Device.fTimeDelta;
	const float openSky = 0.15f;
	rain = clampr(rain, 0.f, 1.f);

	if (rain > EPS_L && sky > openSky)
	{
		const float speed = (1.5f - rain) * 10.f;
		m_hudDropsTime += 20.f * dt / std::max(speed, 0.01f);
		if (m_hudDropsTime > 99000.f)
			m_hudDropsTime = 0.f;

		m_hudDropsAmount = std::min(m_hudDropsAmount + 0.09f * dt, 1.f);
	}
	else
	{
		m_hudDropsAmount = std::max(m_hudDropsAmount - 0.01f * dt, 0.f);
	}
}

void CEffect_Rain::OnFrame()
{
	PROF_EVENT("CEffect_Rain::OnFrame");

	if (!g_pGameLevel && !Device.IsEditorMode())
	{
		return;
	}

	if (g_dedicated_server || !IsEnabled)
	{
		UpdateHudDrops(0.f, 0.f);
		return;
	}
	CEnvironment& env = g_pGamePersistent->Environment();
	// Parse states
	float factor = env.CurrentEnv->rain_density;
	float source_offset = env.source_offset;
	static float hemi_factor = 0.f;
	CObject *E = g_pGameLevel ? g_pGameLevel->CurrentViewEntity() : nullptr;
	if (E && !Device.IsEditorMode())
	{
		Rain_ROS->update_smooth(E);
		float* hemi_cube = Rain_ROS->get_luminocity_hemi_cube();
		float hemi_val = std::max(hemi_cube[0], hemi_cube[1]);
		hemi_val = std::max(hemi_val, hemi_cube[2]);
		hemi_val = std::max(hemi_val, hemi_cube[3]);
		hemi_val = std::max(hemi_val, hemi_cube[5]);

		//		float f					= 0.9f*hemi_factor + 0.1f*hemi_val;
		float f = hemi_val;
		float t = Device.fTimeDelta;
		clamp(t, 0.001f, 1.0f);
		hemi_factor = hemi_factor * (1.0f - t) + f * t;
	}
	else if (Device.IsEditorMode())
	{
		float Distance = 35.f;
		const Fvector Direction(0, 1, 0);
		Fvector Position = Device.vCameraPosition;
		hemi_factor = !RayPick(Position, Direction, Distance, collide::rqtBoth);
	}

	UpdateHudDrops(factor, hemi_factor);

	ref_sound& CurDropSnd = factor < 0.7f ? snd_RoofDroplets : snd_RoofDropletsHard;
	switch (state)
	{
	case stIdle:
	{
		if (factor < EPS_L)
		{
			if (snd_Ambient.is_playing())
			{
				snd_Ambient.stop();
				m_rainVolume = 0.0f;
			}
			return;
		}

		state = stWorking;
		if (snd_Ambient.handle())
		{
			snd_Ambient.play(nullptr, sm_Looped | sm_2D);
			if (!Device.IsEditorMode() || (Device.IsEditorMode() && bIsSndOnRoof))
				CurDropSnd.play(nullptr, sm_Looped | sm_2D);
			else
				CurDropSnd.stop();
			snd_Ambient.set_position(Fvector().set(0, 0, 0));
			snd_Ambient.set_range(source_offset, source_offset * 2.f);
		}

		break;
	}
	case stWorking:
		if (factor<EPS_L)
		{
			state				= stIdle;
			snd_Ambient.stop	();
			m_rainVolume = 0.0f;
			snd_RoofDroplets.stop();
			snd_RoofDropletsHard.stop();
			return;
		}
		break;
	}

	// ambient sound
	if (snd_Ambient.is_playing())
	{
		m_rainVolume = std::max(0.1f, factor) * hemi_factor;
		snd_Ambient.set_volume(m_rainVolume);
	}

	if (CurDropSnd.is_playing())
	{
		float Distance = 35.f;
		const Fvector Direction(0, 1, 0);
		Fvector Position = Device.vCameraPosition;

		if (RayPick(Position, Direction, Distance, collide::rqtBoth))
		{
			Fvector	sndP;
			sndP.mad(Position, Direction, Distance);
			CurDropSnd.set_position(sndP);
			CurDropSnd.set_volume(1);
		}
		else
		{
			CurDropSnd.set_volume(0.f);
		}
	}
}

void CEffect_Rain::UpdateItems()
{
	PROF_EVENT("CEffect_Rain::UpdateItems");
	// Simulation, instance preparation and rendering share the same containers.
	xrCriticalSectionGuard Guard(rainCS);
	if (g_dedicated_server || (!g_pGameLevel && !Device.IsEditorMode()))
	{
		return;
	}
	if (!Device.IsEditorMode() && g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive())
	{
		return;
	}

	const float Dt = std::max(Device.fTimeDelta, 0.f);
	for (Particle* Impact = particle_active; Impact;)
	{
		Particle* Next = Impact->next;
		Impact->Instance.Age += Dt;
		if (!IsEnabled || Impact->Instance.Age >= Impact->Instance.Lifetime)
		{
			p_free(Impact);
		}
		Impact = Next;
	}

	CEnvironment& Env = g_pGamePersistent->Environment();
	const float Density = clampr(Env.CurrentEnv->rain_density, 0.f, 1.f);
	RainColor = Env.CurrentEnv->rain_color;
	RainOpacity = 0.5f + 0.5f * Density;
	float VisualFactor = RainOpacity;
	if (LightingModeIsDynamicRender())
	{
		RainColor.mul(0.9f);
		VisualFactor *= 0.8f;
	}
	RainLength = std::max(Env.CurrentEnv->rain_length * VisualFactor, 0.f);
	RainWidth = std::max(Env.CurrentEnv->rain_width, 0.f);

	if (IsEnabled && Density >= EPS_L)
	{
		const Fvector& Eye = Device.vCameraPosition;
		const float Radius = std::max(Env.source_rain_radius_render + Env.add_const_dist_coefficient_render, EPS_L);
		const float WrapRadiusSquared = _sqr(Radius + 0.5f);
		// The quad is built in the plane through its head, perpendicular to the camera ray.
		const float NearDistance = std::max(RainNearDistance, 0.f);
		const float NearDistanceSquared = _sqr(NearDistance);
		const u32 Desired = iFloor(0.5f * (1.f + Density) * float(Env.max_desired_items));
		items.resize(Desired);

		Fplane SourcePlane;
		SourcePlane.build(Fvector().set(Eye.x, Eye.y , Eye.z), Fvector().set(0.f, -1.f, 0.f)); // + Env.source_offset
		for (Item& Drop : items)
		{
			if (Drop.RemainingDistance <= 0.f)
			{
				Born(Drop, NearDistance);
			}
			Fvector ToEnd;
			ToEnd.sub(Drop.Phit, Drop.P);
			const float Remaining = std::max(ToEnd.dotproduct(Drop.D), 0.f);
			const float Step = std::min(Drop.fSpeed * Dt, Remaining);
			if (NearDistance > 0.f)
			{
				// Test the whole movement segment, including a camera move into an existing trajectory.
				Fvector ToCamera, ClosestPoint;
				ToCamera.sub(Eye, Drop.P);
				const float ClosestStep = clampr(ToCamera.dotproduct(Drop.D), 0.f, Step);
				ClosestPoint.mad(Drop.P, Drop.D, ClosestStep);
				if (ClosestPoint.distance_to_sqr(Eye) < NearDistanceSquared)
				{
					Born(Drop, NearDistance);
					continue;
				}
			}
			Drop.P.mad(Drop.D, Step);
			Drop.RemainingDistance = std::max(Remaining - Step, 0.f);
			if (Drop.RemainingDistance == 0.f)
			{
				// Keep the actual endpoint as the current pose; respawn on the next update.
				Drop.P.set(Drop.Phit);
				if (Drop.bHit && Random.randI(2) == 0 && Env.particles_time > EPS_L)
				{
					if (Particle* Impact = p_allocate())
					{
						Impact->Instance = {Drop.Phit, 0.f, Drop.HitNormal, Env.particles_time, Drop.Seed, 1.f, {0.f, 0.f}};
					}
				}
				Drop.bHit = false;
				continue;
			}

			Fvector WrapDirection;
			WrapDirection.set(Drop.P.x - Eye.x, 0.f, Drop.P.z - Eye.z);
			const float DistanceSquared = WrapDirection.square_magnitude();
			if (DistanceSquared > WrapRadiusSquared)
			{
				if (Drop.P.y - Eye.y < Env.sink_offset)
				{
					Drop.bHit = false;
					Drop.RemainingDistance = -1.f;
					continue;
				}
				const float Distance = _sqrt(DistanceSquared);
				WrapDirection.div(Distance);
				Drop.P.mad(WrapDirection, -(Distance + Radius));
				if (NearDistance > 0.f && Drop.P.distance_to_sqr(Eye) < NearDistanceSquared)
				{
					Born(Drop, NearDistance);
					continue;
				}
				Fvector Reverse, Source;
				Reverse.invert(Drop.D);
				float Range = Env.max_distance;
				if (!SourcePlane.intersectRayPoint(Drop.P, Reverse, Source))
				{
					Drop.bHit = false;
					Drop.RemainingDistance = -1.f;
					continue;
				}
				const float Travelled = Drop.P.distance_to(Source);
				const bool Hit = RayPick(Source, Drop.D, Range, collide::rqtBoth, &Drop.HitNormal);
				if (Range <= Travelled)
				{
					Drop.bHit = false;
					Drop.RemainingDistance = -1.f;
					continue;
				}
				RenewItem(Drop, Range - Travelled, Hit);
			}
		}
	}
	else
	{
		// Restart trajectories on re-enable; old contacts must not emit bursts.
		items.clear();
	}
}

void CEffect_Rain::BuildInstances()
{
	// ViewBase is set by the current render; simulation runs separately on PreRenderThread.
	RainInstances.clear();
	ImpactInstances.clear();
	RainInstances.reserve(items.size());
	// The quad extends backward from its head by at most twice its configured full length.
	const float BoundsRadius = 2.f * _sqrt(_sqr(RainLength) + _sqr(RainWidth));
	for (const Item& Drop : items)
	{
		if (Drop.RemainingDistance < 0.f)
		{
			continue;
		}
		if (::Render->ViewBase.testSphere_dirty(Drop.P, BoundsRadius))
		{
			RainInstances.push_back({Drop.P, Drop.Seed, Drop.D, Drop.fSpeed});
		}
	}
	for (Particle* Impact = particle_active; Impact; Impact = Impact->next)
	{
		// Includes the 0.3 s ballistic flight at radius scale two, even for downward normals.
		if (::Render->ViewBase.testSphere_dirty(Impact->Instance.Position, 2.f))
		{
			ImpactInstances.push_back(Impact->Instance);
		}
	}
}

void CEffect_Rain::InvalidateState()
{
	xrCriticalSectionGuard Guard(rainCS);
	items.clear();
	RainInstances.clear();
	ImpactInstances.clear();
	while (particle_active)
	{
		p_free(particle_active);
	}
	state = stIdle;
	m_rainVolume = 0.0f;
}


void CEffect_Rain::Statistics(CGameFont& Font)
{
	xrCriticalSectionGuard Guard(rainCS);
	const auto IsActive = [](const Item& Drop)
	{
		return Drop.RemainingDistance >= 0.f;
	};
	const u32 ActiveDrops = IsEnabled ? u32(std::count_if(items.begin(), items.end(), IsActive)) : 0;
	Font.OutNext("Rain air: %u active / %u in view", ActiveDrops, IsEnabled ? u32(RainInstances.size()) : 0);
	Font.OutNext("Rain impacts: %d active / %u in view", IsEnabled ? p_size(particle_active) : 0, IsEnabled ? u32(ImpactInstances.size()) : 0);
}

void CEffect_Rain::Render()
{
	PROF_EVENT("CEffect_Rain::Render")
	if (!g_pGameLevel && !Device.IsEditorMode())
		return;

	xrCriticalSectionGuard Guard(rainCS);
	if (IsEnabled)
	{
		BuildInstances();
		m_pRender->Render(*this);
	}
}

// initialize particles pool
void CEffect_Rain::p_create()
{
	// pool
	particle_pool.resize(g_pGamePersistent->Environment().max_particles);
	for (u32 it=0; it<particle_pool.size(); it++)
	{
		Particle&	P	= particle_pool[it];
		P.prev			= it?(&particle_pool[it-1]):0;
		P.next			= (it<(particle_pool.size()-1))?(&particle_pool[it+1]):0;
	}
	
	// active and idle lists
	particle_active	= 0;
	particle_idle = particle_pool.empty() ? nullptr : &particle_pool.front();
}

// destroy particles pool
void CEffect_Rain::p_destroy()
{
	// active and idle lists
	particle_active	= 0;
	particle_idle	= 0;
	
	// pool
	particle_pool.clear();
}

// _delete_ node from _list_
void CEffect_Rain::p_remove(Particle* P, Particle* &LST)
{
	VERIFY		(P);
	Particle*	prev		= P->prev;	P->prev = nullptr;
	Particle*	next		= P->next;	P->next	= nullptr;
	if (prev) prev->next	= next;
	if (next) next->prev	= prev;
	if (LST==P)	LST			= next;
}

// insert node at the top of the head
void CEffect_Rain::p_insert(Particle* P, Particle* &LST)
{
	VERIFY		(P);
	P->prev					= 0;
	P->next					= LST;
	if (LST)	LST->prev	= P;
	LST						= P;
}

// determine size of _list_
int CEffect_Rain::p_size(Particle* P)
{
	if (0==P)	return 0;
	int cnt = 0;
	while (P)	{
		P	=	P->next;
		cnt +=	1;
	}
	return cnt;
}

// alloc node
CEffect_Rain::Particle*	CEffect_Rain::p_allocate()
{
	Particle*	P			= particle_idle;
	if (0==P)				return nullptr;
	p_remove	(P,particle_idle);
	p_insert	(P,particle_active);
	return		P;
}

// xr_free node
void CEffect_Rain::p_free(Particle* P)
{
	p_remove	(P,particle_active);
	p_insert	(P,particle_idle);
}
