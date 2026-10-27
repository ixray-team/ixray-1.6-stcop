// Rain.h: interface for the CRain class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include <cstddef>
#include "../xrCore/Collision/xr_collide_defs.h"

//refs
class ENGINE_API IRender_DetailModel;
class ENGINE_API IRender_ObjectSpecific;
class ENGINE_API CEnvironment;
class ENGINE_API CGameFont;
#include "../Include/xrRender/FactoryPtr.h"
#include "../Include/xrRender/RainRender.h"
//
class ENGINE_API CEffect_Rain
{
	friend class dxRainRender;
public:
	struct	Item
	{
		Item() = default;
		Fvector			P;
		Fvector			Phit;
		Fvector			D;
		float			fSpeed;
		// Negative requests a new trajectory; zero retains its endpoint until the next update.
		float RemainingDistance = -1.f;
		u32 Seed = 0;
		Fvector HitNormal = {0.f, 1.f, 0.f};
		bool			bHit = false;
	};
	struct RainInstance
	{
		Fvector Position;
		u32 Seed;
		Fvector Direction;
		float Speed;
	};
	static_assert(sizeof(RainInstance) == 32);
	static_assert(offsetof(RainInstance, Seed) == 12 && offsetof(RainInstance, Direction) == 16 && offsetof(RainInstance, Speed) == 28);
	struct ImpactInstance
	{
		Fvector Position;
		float Age;
		Fvector Normal;
		float Lifetime;
		u32 Seed;
		float Scale;
		Fvector2 Padding;
	};
	static_assert(sizeof(ImpactInstance) == 48);
	static_assert(offsetof(ImpactInstance, Normal) == 16 && offsetof(ImpactInstance, Seed) == 32 && offsetof(ImpactInstance, Scale) == 36);
	struct	Particle
	{
		Particle		*next,*prev;
		ImpactInstance Instance;
	};
	enum	States
	{
		stIdle		= 0,
		stWorking
	};
	FactoryPtr<IRainRender>	m_pRender;

	xr_vector<CObject*> rainCollideObjects;

private:

	xr_vector<Item>					items;

	xr_vector<RainInstance> RainInstances;
	xr_vector<ImpactInstance> ImpactInstances;
	Fvector RainColor = {1.f, 1.f, 1.f};
	float RainLength = 0.f;
	float RainWidth = 0.f;
	float RainDropsOpacity = 0.f;
	float RainDropsNearDistance = 0.5f;
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
	void AddRainCollidableObject(CObject* obj);
	void RemoveRainCollidableObject(CObject* obj);
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
	void Statistics(CGameFont& Font);
	float GetRainVolume() const { return m_rainVolume; }

	IRender_ObjectSpecific* Rain_ROS = nullptr;
	bool IsEnabled = true;
	void Enable(bool Value);

	float HudDropsTime() const { return m_hudDropsTime; }
	float HudDropsAmount() const { return m_hudDropsAmount; }

private:
	void BuildInstances();
	float m_hudDropsTime = 0.f;
	float m_hudDropsAmount = 0.f;
	void UpdateHudDrops(float rain, float sky);
};
