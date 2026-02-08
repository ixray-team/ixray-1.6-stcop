// Rain.h: interface for the CRain class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include "../xrCore/Collision/xr_collide_defs.h"

//refs
class ENGINE_API IRender_DetailModel;
class ENGINE_API IRender_ObjectSpecific;
class ENGINE_API CEnvironment;
#include "../Include/xrRender/FactoryPtr.h"
#include "../Include/xrRender/RainRender.h"
//
class ENGINE_API CEffect_Rain
{
	friend class dxRainRender;
public:
	struct	Item
	{
		Item();
		Fvector			P;
		Fvector			Phit;
		Fvector			D;
		float			fSpeed;
		u32				dwTime_Life;
		u32				dwTime_Hit;
		u32				uv_set;
	};
	struct rain_sprite
	{
		struct { Fvector p; u32 color; Fvector2 uv; } buff[4];
	};
	struct	Particle
	{
		Particle		*next,*prev;
		Fmatrix			mXForm;
		Fsphere			bounds;
		float			time;
	};
	enum	States
	{
		stIdle		= 0,
		stWorking
	};
	FactoryPtr<IRainRender>	m_pRender;

private:

	xr_vector<Item>					items;

	xr_vector<rain_sprite>			m_sprites;
	States							state;

	// Particles
	xr_vector<Particle>				particle_pool;
	Particle*						particle_active;
	Particle*						particle_idle;

	// Sounds
	ref_sound						snd_Ambient;
	ref_sound						snd_RoofDroplets;
	ref_sound						snd_RoofDropletsHard;
	xrCriticalSection				rainCS;
	float m_rainVolume = 0.0f;
public:
	// Utilities
	void							p_create		();
	void							p_destroy		();

	void							p_remove		(Particle* P, Particle* &LST);
	void							p_insert		(Particle* P, Particle* &LST);
	int								p_size			(Particle* LST);
	Particle*						p_allocate		();
	void							p_free			(Particle* P);

	// Some methods

									CEffect_Rain	();
									~CEffect_Rain	();

	void							Render			();
	void							OnFrame			();
	void							UpdateItems		();
	void InvalidateState();
	float GetRainVolume() const { return m_rainVolume; }

	IRender_ObjectSpecific* Rain_ROS = nullptr;
	bool IsEnabled = true;
	void Enable(bool Value);
};
