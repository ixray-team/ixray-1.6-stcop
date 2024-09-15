#include "stdafx.h"
#pragma once

#include "Rain.h"
#include "igame_persistent.h"
#include "environment.h"
#include "Editor/XrEditorSceneInterface.h"

#ifdef _EDITOR
    #include "ui_toolscustom.h"
#else
    #include "render.h"
	#include "igame_level.h"
	#include "../xrcdb/xr_area.h"
	#include "xr_object.h"
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CEffect_Rain::CEffect_Rain()
{
	state							= stIdle;
	
	snd_Ambient.create				("ambient\\rain",st_Effect,sg_Undefined);

	p_create();
}

CEffect_Rain::~CEffect_Rain()
{
	snd_Ambient.destroy				();

	// Cleanup
	p_destroy						();
}

/*
	* Добавить моросящий дождь:
	* Для моросящего дождя характерны меньшие размеры капель и более низкая скорость. Это можно реализовать через изменение скорости и случайное смещение капель.

	void CEffect_Rain::Born(Item& dest, float radius)
	{
		Fvector axis;
		axis.set(0.f, -1.f, 0.f);

		// Вычисление направления дождя
		float k = g_pGamePersistent->Environment().CurrentEnv->rain_angle / g_pGamePersistent->Environment().drop_max_wind_vel;
		float pitch = g_pGamePersistent->Environment().drop_max_angle * k - PI_DIV_2;
		axis.setHP(g_pGamePersistent->Environment().CurrentEnv->rain_angle_rotation, pitch);

		Fvector& view = Device.vCameraPosition;
		float angle = ::Random.randF(0.f, PI_MUL_2);
		float dist = ::Random.randF();

		dist = _sqrt(dist) * radius;
		float x = dist * _cos(angle);
		float z = dist * _sin(angle);

		// Увеличение случайного угла для моросящего дождя (делает разброс больше)
		float drizzle_angle_variation = deg2rad(::Random.randF(0.f, 10.f));

		// Устанавливаем направление капли
		dest.D.random_dir(axis, drizzle_angle_variation);

		// Позиционирование с учетом источника и случайного разброса
		dest.P.set(x + view.x - dest.D.x * g_pGamePersistent->Environment().source_offset,
				   g_pGamePersistent->Environment().source_offset + view.y,
				   z + view.z - dest.D.z * g_pGamePersistent->Environment().source_offset);

		// Скорость капли, уменьшаем для моросящего дождя
		dest.fSpeed = ::Random.randF(g_pGamePersistent->Environment().CurrentEnv->rain_speed_min * 0.5f,
									 g_pGamePersistent->Environment().CurrentEnv->rain_speed_max * 0.7f);

		// Высота падения капель
		float height = g_pGamePersistent->Environment().max_distance + g_pGamePersistent->Environment().add_const_dist_coefficient;

		// Пересчитываем положение с учетом препятствий
		RenewItem(dest, height, RayPick(dest.P, dest.D, height, collide::rqtBoth));
	}

	* Добавить плотный дождь
	* Для создания эффекта плотного дождя можно уменьшить радиус разброса и увеличить количество капель.

	void CEffect_Rain::BornDenseRain(Item& dest, float radius)
	{
		Fvector axis;
		axis.set(0.f, -1.f, 0.f);

		// Направление дождя (без изменений)
		float k = g_pGamePersistent->Environment().CurrentEnv->rain_angle / g_pGamePersistent->Environment().drop_max_wind_vel;
		float pitch = g_pGamePersistent->Environment().drop_max_angle * k - PI_DIV_2;
		axis.setHP(g_pGamePersistent->Environment().CurrentEnv->rain_angle_rotation, pitch);

		Fvector& view = Device.vCameraPosition;
		float angle = ::Random.randF(0.f, PI_MUL_2);
		float dist = ::Random.randF();

		// Более плотный дождь – уменьшаем разброс
		dist = _sqrt(dist) * (radius * 0.5f);  // Уменьшаем радиус разброса
		float x = dist * _cos(angle);
		float z = dist * _sin(angle);

		// Устанавливаем направление капли (возможен небольшой случайный угол)
		float dense_angle_variation = deg2rad(::Random.randF(0.f, 5.f));  // Плотнее – меньший разброс
		dest.D.random_dir(axis, dense_angle_variation);

		// Позиционирование с учетом источника и уменьшенного разброса
		dest.P.set(x + view.x - dest.D.x * g_pGamePersistent->Environment().source_offset,
				   g_pGamePersistent->Environment().source_offset + view.y,
				   z + view.z - dest.D.z * g_pGamePersistent->Environment().source_offset);

		// Скорость капель для плотного дождя можно оставить стандартной
		dest.fSpeed = ::Random.randF(g_pGamePersistent->Environment().CurrentEnv->rain_speed_min,
									 g_pGamePersistent->Environment().CurrentEnv->rain_speed_max);

		// Высота падения капель
		float height = g_pGamePersistent->Environment().max_distance + g_pGamePersistent->Environment().add_const_dist_coefficient;

		// Пересчитываем положение с учетом препятствий
		RenewItem(dest, height, RayPick(dest.P, dest.D, height, collide::rqtBoth));
	}

	* Вариант со случайной сферой
	* Если необходимо создать дождь в форме более плотной сферы, можно сделать следующее:
	* Добавить случайное смещение координат капель в сферу (например, используя сферическое распределение).
	* Управлять этим параметром в зависимости от текущей погоды.
	
	void CEffect_Rain::BornSphericalRain(Item& dest, float radius)
	{
		Fvector axis;
		axis.set(0.f, -1.f, 0.f);

		// Направление дождя (без изменений)
		float k = g_pGamePersistent->Environment().CurrentEnv->rain_angle / g_pGamePersistent->Environment().drop_max_wind_vel;
		float pitch = g_pGamePersistent->Environment().drop_max_angle * k - PI_DIV_2;
		axis.setHP(g_pGamePersistent->Environment().CurrentEnv->rain_angle_rotation, pitch);

		Fvector& view = Device.vCameraPosition;

		// Генерация точки в сфере для плотного дождя
		float theta = ::Random.randF(0.f, PI_MUL_2);
		float phi = ::Random.randF(0.f, PI_DIV_2);
		float r = ::Random.randF() * radius;

		float x = r * sinf(phi) * cosf(theta);
		float y = r * cosf(phi);
		float z = r * sinf(phi) * sinf(theta);

		// Направление капли в случайной сфере
		dest.D.random_dir(axis, deg2rad(::Random.randF(0.f, 15.f)));  // Случайный разброс угла

		// Позиционирование
		dest.P.set(x + view.x, y + view.y, z + view.z);

		// Скорость капли
		dest.fSpeed = ::Random.randF(g_pGamePersistent->Environment().CurrentEnv->rain_speed_min,
									 g_pGamePersistent->Environment().CurrentEnv->rain_speed_max);

		// Высота падения капель
		float height = g_pGamePersistent->Environment().max_distance + g_pGamePersistent->Environment().add_const_dist_coefficient;

		// Пересчет с учетом препятствий
		RenewItem(dest, height, RayPick(dest.P, dest.D, height, collide::rqtBoth));
	}
*/


// Born
void CEffect_Rain::Born(Item& dest, float radius)
{
	Fvector axis;
	axis.set(0.f, -1.f, 0.f);

	float k = g_pGamePersistent->Environment().CurrentEnv->rain_angle / g_pGamePersistent->Environment().drop_max_wind_vel;
	float pitch = g_pGamePersistent->Environment().drop_max_angle * k - PI_DIV_2;
	axis.setHP(g_pGamePersistent->Environment().CurrentEnv->rain_angle_rotation, pitch);

	Fvector& view = Device.vCameraPosition;
	float angle = ::Random.randF(0.f, PI_MUL_2);
	float dist = ::Random.randF();

	dist = _sqrt(dist) * radius;
	float x = dist * _cos(angle);
	float z = dist * _sin(angle);

	dest.D.random_dir(axis, deg2rad(g_pGamePersistent->Environment().drop_angle));
	dest.P.set(x + view.x - dest.D.x * g_pGamePersistent->Environment().source_offset, g_pGamePersistent->Environment().source_offset + view.y, z + view.z - dest.D.z * g_pGamePersistent->Environment().source_offset);
	dest.fSpeed = ::Random.randF(g_pGamePersistent->Environment().CurrentEnv->rain_speed_min,
		g_pGamePersistent->Environment().CurrentEnv->rain_speed_max);

	float height = g_pGamePersistent->Environment().max_distance + g_pGamePersistent->Environment().add_const_dist_coefficient;
	RenewItem(dest, height, RayPick(dest.P, dest.D, height, collide::rqtBoth));
}

BOOL CEffect_Rain::RayPick(const Fvector& s, const Fvector& d, float& range, collide::rq_target tgt)
{
	BOOL bRes 			= TRUE;
	if (Device.IsEditorMode())
	{
#ifndef MASTER_GOLD
		EditorScene->RayPick(s, d, range);
			return true;
#endif
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

void	CEffect_Rain::OnFrame	()
{
#ifndef _EDITOR
	if (!g_pGameLevel&&!Device.IsEditorMode())			return;
#endif

	if (g_dedicated_server) {
		return;
	}

	// Parse states
	float	factor				= g_pGamePersistent->Environment().CurrentEnv->rain_density;
	static float hemi_factor	= 0.f;
#ifndef _EDITOR
	CObject* E 					= g_pGameLevel ? g_pGameLevel->CurrentViewEntity() : nullptr;
	if (E&&E->renderable_ROS())
	{
//		hemi_factor				= 1.f-2.0f*(0.3f-_min(_min(1.f,E->renderable_ROS()->get_luminocity_hemi()),0.3f));
		float* hemi_cube		= E->renderable_ROS()->get_luminocity_hemi_cube();
		float hemi_val			= _max(hemi_cube[0],hemi_cube[1]);
		hemi_val				= _max(hemi_val, hemi_cube[2]);
		hemi_val				= _max(hemi_val, hemi_cube[3]);
		hemi_val				= _max(hemi_val, hemi_cube[5]);
		
//		float f					= 0.9f*hemi_factor + 0.1f*hemi_val;
		float f					= hemi_val;
		float t					= Device.fTimeDelta;
		clamp					(t, 0.001f, 1.0f);
		hemi_factor				= hemi_factor*(1.0f-t) + f*t;
	}
#endif

	switch (state)
	{
	case stIdle:		
		if (factor < EPS_L) {
			if (snd_Ambient._feedback())
				snd_Ambient.stop();
			return;
		}
		state					= stWorking;
		snd_Ambient.play		(0,sm_Looped);
		snd_Ambient.set_position(Fvector().set(0,0,0));
		snd_Ambient.set_range	(g_pGamePersistent->Environment().source_offset, g_pGamePersistent->Environment().source_offset*2.f);
	break;
	case stWorking:
		if (factor<EPS_L){
			state				= stIdle;
			snd_Ambient.stop	();
			return;
		}
		break;
	}

	// ambient sound
	if (snd_Ambient._feedback())
	{
		snd_Ambient.set_volume(_max(0.1f, factor) * hemi_factor);
	}
}

//#include "xr_input.h"
void	CEffect_Rain::Render	()
{
#ifndef _EDITOR
	if (!g_pGameLevel&&!Device.IsEditorMode())			return;
#endif

	m_pRender->Render(*this);
}

// startup _new_ particle system
void	CEffect_Rain::Hit		(Fvector& pos)
{
	if (0!=::Random.randI(2))	return;
	Particle*	P	= p_allocate();
	if (0==P)	return;

	const Fsphere &bv_sphere = m_pRender->GetDropBounds();

	P->time						= g_pGamePersistent->Environment().particles_time;
	P->mXForm.rotateY			(::Random.randF(PI_MUL_2));
	P->mXForm.translate_over	(pos);
	P->mXForm.transform_tiny	(P->bounds.P, bv_sphere.P);
	P->bounds.R					= bv_sphere.R;

}

// initialize particles pool
void CEffect_Rain::p_create		()
{
	// pool
	particle_pool.resize	(g_pGamePersistent->Environment().max_particles);
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
void CEffect_Rain::p_destroy	()
{
	// active and idle lists
	particle_active	= 0;
	particle_idle	= 0;
	
	// pool
	particle_pool.clear	();
}

// _delete_ node from _list_
void CEffect_Rain::p_remove	(Particle* P, Particle* &LST)
{
	VERIFY		(P);
	Particle*	prev		= P->prev;	P->prev = nullptr;
	Particle*	next		= P->next;	P->next	= nullptr;
	if (prev) prev->next	= next;
	if (next) next->prev	= prev;
	if (LST==P)	LST			= next;
}

// insert node at the top of the head
void CEffect_Rain::p_insert	(Particle* P, Particle* &LST)
{
	VERIFY		(P);
	P->prev					= 0;
	P->next					= LST;
	if (LST)	LST->prev	= P;
	LST						= P;
}

// determine size of _list_
int CEffect_Rain::p_size	(Particle* P)
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
CEffect_Rain::Particle*	CEffect_Rain::p_allocate	()
{
	Particle*	P			= particle_idle;
	if (0==P)				return nullptr;
	p_remove	(P,particle_idle);
	p_insert	(P,particle_active);
	return		P;
}

// xr_free node
void	CEffect_Rain::p_free(Particle* P)
{
	p_remove	(P,particle_active);
	p_insert	(P,particle_idle);
}
