#ifndef ParticlesObjectH
#define ParticlesObjectH

#include "../xrCore/Collision/ISpatial.h"
#include "../xrEngine/IRenderable.h"

namespace PAPI
{
	enum PActionEnum;
	struct ParticleAction;
}

class CParticlesObject : public IRenderable
{
public:
	bool					m_destroy_on_game_load	;
	bool					m_bLooped				;//����, ��� ������� ���������
	bool					m_bStopping				;//������� ������� Stop()
	xr_atomic_bool			m_NeedDestroy = false	;
	u32						dwLastTime				;
	int						m_iLifeTime				;
	BOOL					m_bAutoRemove			;
	BOOL					m_bDead					;

						CParticlesObject	(LPCSTR p_name, BOOL bAutoRemove, bool destroy_on_game_load);
	virtual				~CParticlesObject	();

	virtual void		renderable_Render	();

	Fvector&			Position			();
	void				SetXFORM			(const Fmatrix& m);
	IC	Fmatrix&		XFORM				() {return renderable.xform;}
	virtual void		Update				(u32 dt);
	void				UpdateParent		(const Fmatrix& m, const Fvector& vel);
	void				SetLiveUpdate		(BOOL b);
	BOOL				GetLiveUpdate		();
	void				play_at_pos			(const Fvector& pos, BOOL xform=FALSE);
	virtual void		Play				(bool bHudMode);
	void				Stop				(BOOL bDefferedStop=TRUE);
	
	bool				IsLooped			() {return m_bLooped;}
	bool				IsAutoRemove		();
	bool				IsPlaying			();
	void				SetAutoRemove		(bool auto_remove);

	virtual void		PSI_destroy			();
	virtual void		PSI_internal_delete	();

	IC BOOL				PSI_alive			() { return m_iLifeTime > 0; }
	IC void				PSI_SetLifeTime		(float life_time) { m_iLifeTime = iFloor(life_time * 1000); }
	IC const bool&		destroy_on_game_load() const { return m_destroy_on_game_load; }

	const shared_str	Name				();

	PAPI::ParticleAction* FindAction		(shared_str PEName, PAPI::PActionEnum type);

	virtual	IRenderable* dcast_Renderable() { return this; }
};


namespace Particles::Details
{
	xr_shared_ptr<CParticlesObject> Create(LPCSTR p_name, BOOL bAutoRemove = TRUE, bool remove_on_game_load = true);

	template <class T>
	static void Destroy(T& p)
	{
		if (p)
		{
			p->PSI_destroy();
			p = 0;
		}
	}
}

#endif /*ParticlesObjectH*/
