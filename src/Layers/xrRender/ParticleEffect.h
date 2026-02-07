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
	class ECORE_API CParticleEffect: public dxRender_Visual, public IParticleCustom
	{
		friend class CPEDef;
	protected:
		float				m_fElapsedLimit;

		s32					m_MemDT;

		Fvector				m_InitialPosition;
		xrCriticalSection	onframe_lock;
	public:
		CPEDef*				m_Def;
		ref_geom			geom;
        Fmatrix				m_XFORM;
		PAPI::ParticleHolder Pholder;
    protected:
    	DestroyCallback		m_DestroyCallback;
        CollisionCallback	m_CollisionCallback;
	public:
		enum{
			flRT_Playing		= (1<<0),
			flRT_DefferedStop	= (1<<1),
			flRT_XFORM			= (1<<2),
			flRT_HUDmode		= (1<<3),
			flRT_LiveUpdate		= (1<<4),
		};
		Flags8				m_RT_Flags;
	protected:

		void				RefreshShader		();
	public:
							CParticleEffect		();
		virtual 			~CParticleEffect	();

		void	 			OnFrame				(u32 dt);

		virtual void		Render				(float LOD);
		virtual void		Copy				(dxRender_Visual* pFrom);

		virtual void 		OnDeviceCreate		();
		virtual void 		OnDeviceDestroy		();

		virtual void		UpdateParent		(const Fmatrix& m, const Fvector& velocity, BOOL bXFORM);

		BOOL				Compile				(CPEDef* def);

		ICF CPEDef*			GetDefinition		(){return m_Def;}

		virtual void		Play				();
		virtual void		Stop				(BOOL bDefferedStop=TRUE);
		virtual BOOL		IsPlaying			(){return m_RT_Flags.is(flRT_Playing);}
		
		virtual void		SetHudMode			(BOOL b){m_RT_Flags.set(flRT_HUDmode,b);}
		virtual BOOL		GetHudMode			()		{return m_RT_Flags.is(flRT_HUDmode);}

		virtual void		SetLiveUpdate		(BOOL b){m_RT_Flags.set(flRT_LiveUpdate,b);}
		virtual BOOL		GetLiveUpdate		()		{return m_RT_Flags.is(flRT_LiveUpdate);}

		virtual float		GetTimeLimit		(){VERIFY(m_Def); return m_Def->m_Flags.is(CPEDef::dfTimeLimit)?m_Def->m_fTimeLimit:-1.f;}

		virtual const shared_str	Name			(){VERIFY(m_Def); return m_Def->m_Name;}

        void				SetDestroyCB		(DestroyCallback 	destroy_cb)		{m_DestroyCallback 	= destroy_cb;}
        void				SetCollisionCB		(CollisionCallback	collision_cb)	{m_CollisionCallback= collision_cb;}
        void				SetBirthDeadCB		(PAPI::OnBirthParticleCB bc, PAPI::OnDeadParticleCB dc, void* owner, u32 p);		

	    virtual u32			ParticlesCount		();
		PAPI::ParticleAction* FindPA(shared_str PEName, PAPI::PActionEnum Action) override;
	};
    void OnEffectParticleBirth	(void* owner, u32 param, PAPI::Particle& m, u32 idx);
    void OnEffectParticleDead	(void* owner, u32 param, PAPI::Particle& m, u32 idx);

    extern const u32		uDT_STEP;
	extern const float		fDT_STEP;
}
//---------------------------------------------------------------------------
#endif
