#ifndef ParticlesObjectH
#define ParticlesObjectH

#include "../xrEngine/PS_instance.h"
#include "../xrParticles/stdafx.h"

extern PARTICLES_API const Fvector zero_vel;

class PARTICLES_API CParticlesObject :
	public CPS_Instance
{
	friend class CParticlesAsync;
	using inherited =  CPS_Instance;

	u32					dwLastTime;
	void				Init				(LPCSTR p_name, IRender_Sector* S, BOOL bAutoRemove);
	void				UpdateSpatial		();

protected:
	bool				m_bLooped;			//флаг, что система зациклена
	bool				m_bStopping;		//вызвана функция Stop()
	bool				NeedUpdate = false;

public:
	virtual				~CParticlesObject	();
						CParticlesObject	(LPCSTR p_name, BOOL bAutoRemove, bool destroy_on_game_load);

	virtual bool		shedule_Needed		()	{return true;};
	virtual float		shedule_Scale		()	;
	virtual void		renderable_Render	();
	void				PerformAllTheWork	();

	Fvector&			Position			();
	void				SetXFORM			(const Fmatrix& m);
	IC	Fmatrix&		XFORM				()	{return renderable.xform;}
	void				Update				(u32 dt);
	void				UpdateParent		(const Fmatrix& m, const Fvector& vel);
	void				SetLiveUpdate		(BOOL b);
	BOOL				GetLiveUpdate		();
	void				play_at_pos			(const Fvector& pos, BOOL xform=FALSE);
	virtual void		Play				(bool bHudMode);
	void				Stop				(BOOL bDefferedStop=TRUE);
	//virtual BOOL		Locked				()				{ return mt_dt; }
	
	bool				IsLooped			() {return m_bLooped;}
	bool				IsAutoRemove		();
	bool				IsPlaying			();
	void				SetAutoRemove		(bool auto_remove);

	const shared_str	Name				();
};


namespace Particles::Details
{
	PARTICLES_API xr_shared_ptr<CParticlesObject> Create(LPCSTR p_name, BOOL bAutoRemove = TRUE, bool remove_on_game_load = true);

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
