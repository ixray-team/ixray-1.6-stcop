//---------------------------------------------------------------------------
#ifndef ParticleEffectH
#define ParticleEffectH
//---------------------------------------------------------------------------

#include "ParticleEffectDef.h"
#include "particle_core/particle_holder.h"
#include "../../Include/xrRender/ParticleCustom.h"
#include "FBasicVisual.h"

namespace PS
{
	class ECORE_API CParticleEffect final: public dxRender_Visual, public IParticleCustom
	{
		friend class CPEDef;
	public:
		float				m_fElapsedLimit = 0.f;

		s32					m_MemDT = 0;

		Fvector				m_InitialPosition = {0.f, 0.f, 0.f};
		xrCriticalSection	onframe_lock, cache_lock;
		xr_atomic_u32 chache_frame = 0;
		CPEDef*				m_Def = nullptr;
		ref_geom			geom;
        Fmatrix				m_XFORM = Fidentity;

		PAPI::ParticleHolder Pholder;

    	DestroyCallback		m_DestroyCallback = nullptr;
        CollisionCallback	m_CollisionCallback = nullptr;

		enum{
			flRT_Playing		= (1<<0),
			flRT_DefferedStop	= (1<<1),
			flRT_XFORM			= (1<<2),
			flRT_HUDmode		= (1<<3),
			flRT_LiveUpdate		= (1<<4),
			flRT_RelatedChild	= (1<<5),
			flRT_FreeChild		= (1<<6),
		};
		Flags8				m_RT_Flags = {0u};

		virtual 			~CParticleEffect	();

		void	 			OnFrame				(u32 dt);
#ifndef _EDITOR
		virtual void	 	UpdateCache			();
#endif

		virtual void		Render				(float LOD);
		virtual void		Copy				(dxRender_Visual* pFrom);

		virtual void 		GeomCreate();
		virtual void 		GeomDestroy();

		virtual void		UpdateParent		(const Fmatrix& m, const Fvector& velocity, bool bXFORM);

		void				Compile				(CPEDef* def);

		ICF CPEDef*			GetDefinition		(){return m_Def;}

		virtual void		Play				();
		virtual void		Stop				(bool bDefferedStop=TRUE);
		virtual bool		IsPlaying			(){return m_RT_Flags.is(flRT_Playing);}
		
		virtual void		SetHudMode			(bool b){m_RT_Flags.set(flRT_HUDmode,b);}
		virtual bool		GetHudMode			()		{return m_RT_Flags.is(flRT_HUDmode);}

		virtual void		SetLiveUpdate		(bool b){m_RT_Flags.set(flRT_LiveUpdate,b);}
		virtual bool		GetLiveUpdate		()		{return m_RT_Flags.is(flRT_LiveUpdate);}

		virtual float		GetTimeLimit		(){VERIFY(m_Def); return m_Def->m_Flags.is(CPEDef::dfTimeLimit)?m_Def->m_fTimeLimit:-1.f;}

		virtual const shared_str	Name			(){VERIFY(m_Def); return m_Def->m_Name;}

        void				SetDestroyCB		(DestroyCallback 	destroy_cb)		{m_DestroyCallback 	= destroy_cb;}
        void				SetCollisionCB		(CollisionCallback	collision_cb)	{m_CollisionCallback= collision_cb;}
        void				SetBirthDeadCB		(PAPI::OnBirthParticleCB bc, PAPI::OnDeadParticleCB dc, void* owner, u32 p);		

	    virtual u32			SpriteCount		();
		PAPI::ParticleAction* FindPA(shared_str PEName, PAPI::PActionEnum Action) override;

		virtual IParticleCustom* dcast_ParticleCustom() { return this; }
	};
    void OnEffectParticleBirth	(void* owner, u32 param, PAPI::Particle& m, u32 idx);
    void OnEffectParticleDead	(void* owner, u32 param, PAPI::Particle& m, u32 idx);

    extern const u32		uDT_STEP;
	extern const float		fDT_STEP;
}
//---------------------------------------------------------------------------
#endif
