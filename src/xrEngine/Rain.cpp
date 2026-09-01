#include "stdafx.h"

#include "Rain.h"
#include "IGame_Persistent.h"
#include "Environment.h"
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
	rainCollideObjects.push_back(obj);
}

void CEffect_Rain::RemoveRainCollidableObject(CObject* obj)
{
	auto iter = std::find(rainCollideObjects.begin(), rainCollideObjects.end(), obj);
	if (iter != rainCollideObjects.end())
	{
		rainCollideObjects.erase(iter);
	}
}

ICF bool RayPick(const Fvector& s, const Fvector& d, float& range, collide::rq_target tgt)
{
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

ICF void RenewItem(CEffect_Rain::Item& dest, float height, bool bHit, u32 time_global, u32 dt)
{
	dest.uv_set = Random.randI(2);
	if (bHit)
	{
		dest.dwTime_Life= time_global + iFloor(1000.f*height/dest.fSpeed) - dt;
		dest.dwTime_Hit	= time_global + iFloor(1000.f*height/dest.fSpeed) - dt;
		dest.Phit.mad	(dest.P,dest.D,height);
	}
	else
	{
		dest.dwTime_Life= time_global + iFloor(1000.f*height/dest.fSpeed) - dt;
		dest.dwTime_Hit	= time_global + iFloor(2*1000.f*height/dest.fSpeed)-dt;
		dest.Phit.set	(dest.P);
	}
}

// Born
ICF void computeDirection(CEffect_Rain::Item& item, float variation, CEnvironment& env)
{
	Fvector axis = {};
	axis.set(0.f, -1.f, 0.f);
	float k = env.CurrentEnv->rain_angle / env.drop_max_wind_vel;
	float pitch = env.drop_max_angle * k - PI_DIV_2;

	axis.setHP(env.CurrentEnv->rain_angle_rotation, pitch);

	item.D.random_dir(axis, deg2rad(::Random.randF(0.f, variation)));
}

ICF void Born(CEffect_Rain::Item& dest)
{
	static shared_str st_drizzle = "drizzle";
	static shared_str st_dense = "dense";
	static shared_str st_spherical = "spherical";
	CEnvironment& env = g_pGamePersistent->Environment();

	const Fvector& view = Device.vCameraPosition_saved;
	float radius = env.source_rain_radius_render + env.add_const_dist_coefficient_render;
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

	float height = env.max_distance + env.add_const_dist_coefficient;

	RenewItem(dest, height, RayPick(dest.P, dest.D, height, collide::rqtBoth), Device.dwTimeGlobal, Device.dwTimeDelta);
}

void CEffect_Rain::Enable(bool status)
{
	IsEnabled = status;
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
			snd_Ambient.play(nullptr, sm_Looped);
			if (!Device.IsEditorMode() || (Device.IsEditorMode() && bIsSndOnRoof))
				CurDropSnd.play(nullptr, sm_Looped);
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
	
	if (Device.IsEditorMode())
		UpdateItems();
}

CEffect_Rain::Item::Item()
{
	Born(*this);
}

void CEffect_Rain::UpdateItems()
{
	PROF_EVENT("CEffect_Rain::UpdateItems");
	if (g_dedicated_server)
	{
		return;
	}

	CEnvironment& env = g_pGamePersistent->Environment();
	float factor = env.CurrentEnv->rain_density;
	if (factor < EPS_L) return;

	float dt = Device.fTimeDelta;
	u32 udt = Device.dwTimeDelta;
	u32 time_global = Device.dwTimeGlobal;
	const Fvector& vEye = Device.vCameraPosition_saved;

	float radius = env.source_rain_radius_render + env.add_const_dist_coefficient_render;
	float b_radius_wrap_sqr = _sqr(((radius)+0.5f));
	float sink_offset = env.sink_offset;
	float max_distance = env.max_distance;
	float rain_length = env.CurrentEnv->rain_length;
	float rain_width = env.CurrentEnv->rain_width;
	float factor_visual = factor / 2.f + .5f;
	Fvector& f_rain_color = g_pGamePersistent->Environment().CurrentEnv->rain_color;
	u32 u_rain_color = color_rgba_f(f_rain_color.x, f_rain_color.y, f_rain_color.z, factor_visual);
	shared_str& rain_type = env.CurrentEnv->rain_type;

	if (psDeviceFlags.test(rsR4) || psDeviceFlags.test(rsR2))
	{
		f_rain_color.mul(0.9f);
		factor_visual *= 0.8f;
	}

	Fvector2 Sprite_UV[2][4]
	{
		{{0.f,1.f},{0.f,0.f},{1.f,1.f},{1.f,0.f}},
		{{1.f,0.f},{1.f,1.f},{0.f,0.f},{0.f,1.f}}
	};

	u32 desired_items = iFloor(0.5f * (1.f + factor) * float(env.max_desired_items));
	items.resize(desired_items);
	m_sprites.clear(); m_sprites.reserve(items.size());

	// build source plane
	Fplane src_plane;
	Fvector norm = { 0.f,-1.f,0.f };
	Fvector upper; 	upper.set(vEye.x, vEye.y + env.source_offset, vEye.z);
	src_plane.build(upper, norm);

	for (CEffect_Rain::Item& one : items)
	{
		Fvector& pos_head = one.P;
		if (one.dwTime_Hit < time_global)
		{
			if (0 == ::Random.randI(2))
			{
				xrCriticalSectionGuard guard(&rainCS);
				if(CEffect_Rain::Particle* P = p_allocate())
				{
					const Fsphere& bv_sphere = m_pRender->GetDropBounds();

					P->time = g_pGamePersistent->Environment().particles_time;
					P->mXForm.rotateY(::Random.randF(PI_MUL_2));
					P->mXForm.translate_over(one.Phit);
					P->mXForm.transform_tiny(P->bounds.P, bv_sphere.P);
					P->bounds.R = bv_sphere.R;
				}
			}
		}

		if (one.dwTime_Life < time_global)
			Born(one);

		pos_head.mad(one.D, one.fSpeed * dt);

		Fvector	wdir; wdir.set(pos_head.x - vEye.x, 0, pos_head.z - vEye.z);
		float wlen = wdir.square_magnitude();
		if (wlen > b_radius_wrap_sqr)
		{
			wlen = _sqrt(wlen);
			if ((pos_head.y - vEye.y) < sink_offset)
				one.dwTime_Life = 0;
			else
			{
				Fvector inv_dir, src_p;
				inv_dir.invert(one.D);
				wdir.div(wlen);

				pos_head.mad(pos_head, wdir, -(wlen + radius));

				if (src_plane.intersectRayPoint(pos_head, inv_dir, src_p))
				{
					float dist_sqr = pos_head.distance_to_sqr(src_p);
					if (RayPick(src_p, one.D, max_distance, collide::rqtBoth))
					{
						if (_sqr(max_distance) <= dist_sqr)
							one.dwTime_Life = 0;
						else
							RenewItem(one, max_distance - _sqrt(dist_sqr), true, time_global, udt);
					}
					else
						RenewItem(one, max_distance - _sqrt(dist_sqr), false, time_global, udt);
				}
				else
					one.dwTime_Life = 0;
			}
		}
		
		Fvector pos_trail; pos_trail.mad(pos_head, one.D, -rain_length * factor_visual);

		// Culling
		Fvector sC, lineD;
		float sR;
		sC.sub(pos_head, pos_trail);
		lineD.normalize(sC);
		sC.mul(.5f);
		sR = sC.magnitude();
		sC.add(pos_trail);

		if (!::Render->ViewBase.testSphere_dirty(sC, sR))
			continue;

		Fvector	P, lineTop, camDir;
		camDir.sub(sC, vEye);
		camDir.normalize();
		lineTop.crossproduct(camDir, lineD);
		u32 s = one.uv_set;
		xrCriticalSectionGuard guard(&rainCS);
		m_sprites.push_back(
			{
				Fvector().mad(pos_trail,lineTop,-rain_width),u_rain_color,Sprite_UV[s][0].x,Sprite_UV[s][0].y,
				Fvector().mad(pos_trail,lineTop,rain_width)	,u_rain_color,Sprite_UV[s][1].x,Sprite_UV[s][1].y,
				Fvector().mad(pos_head,lineTop,-rain_width)	,u_rain_color,Sprite_UV[s][2].x,Sprite_UV[s][2].y,
				Fvector().mad(pos_head,lineTop,rain_width)	,u_rain_color,Sprite_UV[s][3].x,Sprite_UV[s][3].y
			});
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
	if (!g_pGameLevel && !Device.IsEditorMode())
		return;

	if (IsEnabled)
	{
		xrCriticalSectionGuard guard(&rainCS);
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
