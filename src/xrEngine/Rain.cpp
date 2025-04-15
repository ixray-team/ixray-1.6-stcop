#include "stdafx.h"
#pragma once

#include "Rain.h"
#include "IGame_Persistent.h"
#include "Environment.h"
#include "Editor/XrEditorSceneInterface.h"

#ifdef _EDITOR
    #include "ui_toolscustom.h"
#else
    #include "Render.h"
    #include "IGame_Level.h"
    #include "../xrCore/Collision/xr_area.h"
    #include "xr_object.h"
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CEffect_Rain::CEffect_Rain()
{
	state = stIdle;
	snd_Ambient.create("ambient\\rain", st_Effect, sg_Undefined);

	string_path RainSounds = {};
	FS.update_path(RainSounds, "$game_sounds$", "ambient\\rain_drops_roof.ogg");

	if (FS.exist(RainSounds))
	{
		snd_RoofDroplets.create("ambient\\rain_drops_roof", st_Effect, sg_Undefined);
		snd_RoofDropletsHard.create("ambient\\thunder_drops_roof", st_Effect, sg_Undefined);
	}

	m_rainVolume = 0.0f;
	p_create();
}

CEffect_Rain::~CEffect_Rain()
{
	snd_Ambient.destroy();
	snd_RoofDroplets.destroy();
	snd_RoofDropletsHard.destroy();
	m_rainVolume = 0.0f;

	// Cleanup
	p_destroy();
}

// Born
void CEffect_Rain::Born(Item& dest, float radius, shared_str& rainType)
{
	static shared_str st_drizzle = "drizzle";
	static shared_str st_dense = "dense";
	static shared_str st_spherical = "spherical";

	static auto computeDirection = [](float variation, Item& dest)
	{
		Fvector axis = {};
		axis.set(0.f, -1.f, 0.f);
		float k = g_pGamePersistent->Environment().CurrentEnv->rain_angle / g_pGamePersistent->Environment().drop_max_wind_vel;
		float pitch = g_pGamePersistent->Environment().drop_max_angle * k - PI_DIV_2;

		axis.setHP(g_pGamePersistent->Environment().CurrentEnv->rain_angle_rotation, pitch);

		dest.D.random_dir(axis, deg2rad(::Random.randF(0.f, variation)));
	};

	const Fvector& view = Device.vCameraPosition_saved;

	if (rainType.equal(st_drizzle))
	{
		float angle = ::Random.randF(0.f, PI_MUL_2);
		float dist = _sqrt(::Random.randF()) * radius;
		float x = dist * _cos(angle);
		float z = dist * _sin(angle);

		computeDirection(10.f, dest);

		dest.P.set(x + view.x - dest.D.x * g_pGamePersistent->Environment().source_offset,
			g_pGamePersistent->Environment().source_offset + view.y,
			z + view.z - dest.D.z * g_pGamePersistent->Environment().source_offset);

		dest.fSpeed = ::Random.randF(g_pGamePersistent->Environment().CurrentEnv->rain_speed_min * 0.5f,
			g_pGamePersistent->Environment().CurrentEnv->rain_speed_max * 0.7f);
	}
	else if (rainType.equal(st_dense))
	{
		float angle = ::Random.randF(0.f, PI_MUL_2);
		float dist = _sqrt(::Random.randF()) * (radius * 0.5f);
		float x = dist * _cos(angle);
		float z = dist * _sin(angle);

		computeDirection(5.f, dest);

		dest.P.set(x + view.x - dest.D.x * g_pGamePersistent->Environment().source_offset,
			g_pGamePersistent->Environment().source_offset + view.y,
			z + view.z - dest.D.z * g_pGamePersistent->Environment().source_offset);

		dest.fSpeed = ::Random.randF(g_pGamePersistent->Environment().CurrentEnv->rain_speed_min,
			g_pGamePersistent->Environment().CurrentEnv->rain_speed_max);
	}
	else if (rainType.equal(st_spherical))
	{
		float theta = ::Random.randF(0.f, PI_MUL_2);
		float phi = ::Random.randF(0.f, PI_DIV_2);
		float r = ::Random.randF() * radius;

		float x = r * sinf(phi) * cosf(theta);
		float y = r * cosf(phi);
		float z = r * sinf(phi) * sinf(theta);

		computeDirection(15.f, dest);

		dest.P.set(x + view.x, y + view.y, z + view.z);
		dest.fSpeed = ::Random.randF(g_pGamePersistent->Environment().CurrentEnv->rain_speed_min,
			g_pGamePersistent->Environment().CurrentEnv->rain_speed_max);
	}
	else//default
	{
		float angle = ::Random.randF(0.f, PI_MUL_2);
		float dist = _sqrt(::Random.randF()) * radius;
		float x = dist * _cos(angle);
		float z = dist * _sin(angle);

		computeDirection(10.f, dest);

		dest.P.set(x + view.x - dest.D.x * g_pGamePersistent->Environment().source_offset,
			g_pGamePersistent->Environment().source_offset + view.y,
			z + view.z - dest.D.z * g_pGamePersistent->Environment().source_offset);

		dest.fSpeed = ::Random.randF(g_pGamePersistent->Environment().CurrentEnv->rain_speed_min,
			g_pGamePersistent->Environment().CurrentEnv->rain_speed_max);
	}

	float height = 
		g_pGamePersistent->Environment().max_distance + g_pGamePersistent->Environment().add_const_dist_coefficient;

	RenewItem(dest, height, RayPick(dest.P, dest.D, height, collide::rqtBoth));
}

BOOL CEffect_Rain::RayPick(const Fvector& s, const Fvector& d, float& range, collide::rq_target tgt)
{
	BOOL bRes 			= TRUE;
	if (Device.IsEditorMode())
		return EditorScene->RayPick(s, d, range);
	else
	{
		if (!g_pGameLevel || !g_pGameLevel->bReady)
			return false;
	}
	collide::rq_result	RQ;
	CObject* E 			= g_pGameLevel->CurrentViewEntity();
	bRes 				= g_pGameLevel->ObjectSpace.RayPick( s,d,range,tgt,RQ,E);	
    if (bRes) range 	= RQ.range;
    return bRes;
}

void CEffect_Rain::RenewItem(Item& dest, float height, BOOL bHit)
{
	dest.uv_set			= Random.randI(2);
    if (bHit){
		dest.dwTime_Life= Device.dwTimeGlobal + iFloor(1000.f*height/dest.fSpeed) - Device.dwTimeDelta;
		dest.dwTime_Hit	= Device.dwTimeGlobal + iFloor(1000.f*height/dest.fSpeed) - Device.dwTimeDelta;
		dest.Phit.mad	(dest.P,dest.D,height);
	}else{
		dest.dwTime_Life= Device.dwTimeGlobal + iFloor(1000.f*height/dest.fSpeed) - Device.dwTimeDelta;
		dest.dwTime_Hit	= Device.dwTimeGlobal + iFloor(2*1000.f*height/dest.fSpeed)-Device.dwTimeDelta;
		dest.Phit.set	(dest.P);
	}
}

void CEffect_Rain::OnFrame()
{
	PROF_EVENT("CEffect_Rain::OnFrame");
#ifndef _EDITOR
    if (!g_pGameLevel && !Device.IsEditorMode())
        return;
#endif

    if (g_dedicated_server)
    {
        return;
    }

    // Parse states
    float factor = g_pGamePersistent->Environment().CurrentEnv->rain_density;
    static float hemi_factor = 0.f;
    CObject *E = g_pGameLevel ? g_pGameLevel->CurrentViewEntity() : nullptr;
    if (E && E->renderable_ROS())
    {
		float* hemi_cube = E->renderable_ROS()->get_luminocity_hemi_cube();
		float hemi_val = _max(hemi_cube[0], hemi_cube[1]);
		hemi_val = _max(hemi_val, hemi_cube[2]);
		hemi_val = _max(hemi_val, hemi_cube[3]);
		hemi_val = _max(hemi_val, hemi_cube[5]);

		//		float f					= 0.9f*hemi_factor + 0.1f*hemi_val;
		float f = hemi_val;
		float t = Device.fTimeDelta;
		clamp(t, 0.001f, 1.0f);
		hemi_factor = hemi_factor * (1.0f - t) + f * t;
	}

	ref_sound& CurDropSnd = factor < 0.7f ? snd_RoofDroplets : snd_RoofDropletsHard;
	switch (state)
	{
	case stIdle:
	{
		if (factor < EPS_L)
		{
			if (snd_Ambient._feedback())
				snd_Ambient.stop();
			return;
		}

		state = stWorking;
		snd_Ambient.play(nullptr, sm_Looped);
		CurDropSnd.play(nullptr, sm_Looped);
		snd_Ambient.set_position(Fvector().set(0, 0, 0));
		snd_Ambient.set_range(g_pGamePersistent->Environment().source_offset, g_pGamePersistent->Environment().source_offset * 2.f);
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
	if (snd_Ambient._feedback())
	{
		m_rainVolume = factor * hemi_factor;
		clamp(m_rainVolume, 0.1f, 1.0f);
		snd_Ambient.set_volume(m_rainVolume);
	}

	if (CurDropSnd._feedback())
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
	
	if (Device.IsEditorMode())
		UpdateItems();
}

void CEffect_Rain::UpdateItems()
{
	PROF_EVENT("CEffect_Rain::UpdateItems");
	xrCriticalSectionGuard guard(&rainCS);

	float	factor = g_pGamePersistent->Environment().CurrentEnv->rain_density;
	if (factor < EPS_L)			return;

	u32 desired_items = iFloor(0.5f * (1.f + factor) * float(g_pGamePersistent->Environment().max_desired_items));

	// born _new_ if needed
	// owner.items.reserve		(desired_items);
	while (items.size() < desired_items)
	{
		Born(items.emplace_back(), g_pGamePersistent->Environment().source_rain_radius_render +
			g_pGamePersistent->Environment().add_const_dist_coefficient_render, g_pGamePersistent->Environment().CurrentEnv->rain_type);
	}

	// build source plane
	float b_radius_wrap_sqr = _sqr(((g_pGamePersistent->Environment().source_rain_radius_render +
		g_pGamePersistent->Environment().add_const_dist_coefficient_render) + 0.5f));

	const Fvector& vEye = Device.vCameraPosition_saved;

	Fplane src_plane;
	Fvector norm = { 0.f,-1.f,0.f };
	Fvector upper; 	upper.set(vEye.x, vEye.y + g_pGamePersistent->Environment().source_offset, vEye.z);
	src_plane.build(upper, norm);

	// perform update

	for (CEffect_Rain::Item& one : items)
	{
		if (one.dwTime_Hit < Device.dwTimeGlobal)
			Hit(one.Phit);

		if (one.dwTime_Life < Device.dwTimeGlobal)
			Born(one, g_pGamePersistent->Environment().source_rain_radius_render +
				g_pGamePersistent->Environment().add_const_dist_coefficient_render, g_pGamePersistent->Environment().CurrentEnv->rain_type);

		float dt = Device.fTimeDelta;
		one.P.mad(one.D, one.fSpeed * dt);

		Fvector	wdir;	wdir.set(one.P.x - vEye.x, 0, one.P.z - vEye.z);
		float	wlen = wdir.square_magnitude();
		if (wlen > b_radius_wrap_sqr)
		{
			wlen = _sqrt(wlen);
			if ((one.P.y - vEye.y) < g_pGamePersistent->Environment().sink_offset)
			{
				// need born
				one.invalidate();
			}
			else
			{
				Fvector		inv_dir, src_p;
				inv_dir.invert(one.D);
				wdir.div(wlen);

				one.P.mad(one.P, wdir, -(wlen + g_pGamePersistent->Environment().source_rain_radius_render +
					g_pGamePersistent->Environment().add_const_dist_coefficient_render));

				if (src_plane.intersectRayPoint(one.P, inv_dir, src_p))
				{
					float dist_sqr = one.P.distance_to_sqr(src_p);
					float height = g_pGamePersistent->Environment().max_distance;
					if (RayPick(src_p, one.D, height, collide::rqtBoth))
					{
						if (_sqr(height) <= dist_sqr)
						{
							one.invalidate();								// need born
							//							Log("1");
						}
						else
						{
							RenewItem(one, height - _sqrt(dist_sqr), TRUE);		// fly to point
							//							Log("2",height-dist);
						}
					}
					else
					{
						RenewItem(one, g_pGamePersistent->Environment().max_distance - _sqrt(dist_sqr), FALSE);		// fly ...
						//						Log("3",1.5f*b_height-dist);
					}
				}
				else
				{
					// need born
					one.invalidate();
					//					Log("4");
				}
			}
		}
	}
}

void CEffect_Rain::InvalidateState()
{
	state = stIdle;
	m_rainVolume = 0.0f;
}


void CEffect_Rain::Render()
{
	PROF_EVENT("CEffect_Rain::Render")
#ifndef _EDITOR
    if (!g_pGameLevel && !Device.IsEditorMode())
        return;
#endif
	xrCriticalSectionGuard guard(&rainCS);
    m_pRender->Render(*this);
}

// startup _new_ particle system
void CEffect_Rain::Hit(Fvector& pos)
{
	if (0!=::Random.randI(2))
		return;

	Particle* P = p_allocate();
	if (0==P)
		return;

	const Fsphere &bv_sphere = m_pRender->GetDropBounds();

	P->time						= g_pGamePersistent->Environment().particles_time;
	P->mXForm.rotateY			(::Random.randF(PI_MUL_2));
	P->mXForm.translate_over	(pos);
	P->mXForm.transform_tiny	(P->bounds.P, bv_sphere.P);
	P->bounds.R					= bv_sphere.R;
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
	particle_idle	= &particle_pool.front();
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
