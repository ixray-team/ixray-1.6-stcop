#pragma once

namespace PAPI
{
	enum PActionEnum;
	struct ParticleAction;
}

class CParticlesObject : public IRenderable
{
public:
	bool				m_destroy_on_game_load = false;
	bool				m_bLooped = false;
	bool				m_bPlaying = false;
	bool				m_bAutoStop = false;
	bool				m_bAutoRemove = false;
	xr_atomic_bool		m_NeedDestroy = false;
	u32					dwLastTime = 0U;
	int					m_iLifeTime = int_max;

						CParticlesObject	(const char* p_name, bool bAutoRemove, bool destroy_on_game_load);
	virtual				~CParticlesObject	();

	virtual void		renderable_Render	();
	virtual void		Update				(u32 dt, CFrustum& viewbase);
	virtual	IRenderable* dcast_Renderable	() { return this; }

	Fvector&			Position			();
	void				SetXFORM			(const Fmatrix& m);
	IC	Fmatrix&		XFORM				() {return renderable.xform;}
	void				UpdateParent		(const Fmatrix& m, const Fvector& vel);
	void				SetLiveUpdate		(bool b);
	bool				GetLiveUpdate		();
	u32					GetSpriteCount		();
	void				play_at_pos			(const Fvector& pos, bool xform=false);
	void				Play				(bool bHudMode);
	void				Stop				(bool bDefferedStop=true);
	bool				IsPlaying			();

	IC bool				IsLooped			() { return m_bLooped; }
	IC bool				IsAutoRemove		() { return m_bAutoRemove; }
	IC bool				IsAlive				() { return m_iLifeTime > 0; }
	IC void				Destroy				() { m_NeedDestroy = true; m_iLifeTime = 0; }

	const shared_str	Name				();

	PAPI::ParticleAction* FindAction		(shared_str PEName, PAPI::PActionEnum type);
};


namespace Particles::Details
{
	xr_shared_ptr<CParticlesObject> Create(const char* p_name, bool bAutoRemove = true, bool remove_on_game_load = true);

	template <class T>
	static IC void Destroy(T& p)
	{
		if (p)
		{
			p->Destroy();
			p = 0;
		}
	}
}