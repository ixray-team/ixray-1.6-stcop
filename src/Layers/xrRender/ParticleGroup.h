//---------------------------------------------------------------------------
#ifndef ParticleGroupH
#define ParticleGroupH
#include "../../Include/xrRender/ParticleCustom.h"
#include "FBasicVisual.h"

class ButtonValue;

namespace PS
{
	class CParticleEffect;

	class ECORE_API CPGDef
	{
	public:
		shared_str			m_Name;
		Flags32				m_Flags;
		float				m_fTimeLimit;
		struct ECORE_API SEffect{
			enum{
				flDefferedStop		= (1<<0),
				flOnPlayChild		= (1<<1),
				flEnabled			= (1<<2),
                flOnPlayChildRewind	= (1<<4),
                flOnBirthChild		= (1<<5),
				flOnDeadChild		= (1<<6),
			};
			Flags32			m_Flags;
			shared_str		m_EffectName;  
			shared_str		m_OnPlayChildName;
			shared_str		m_OnBirthChildName;
			shared_str		m_OnDeadChildName;
			float			m_Time0;
			float			m_Time1;
							SEffect				(){m_Flags.zero();/*set(flEnabled)*/m_Time0=0;m_Time1=0;}
#ifdef _EDITOR
			CPGDef* parent = nullptr;
            BOOL			Equal				(const SEffect&);
			void FillProp(LPCSTR pref, PropItemVec& items, u32 clr);
			void FillPropInit(PropItemVec& items, LPCSTR pref);
#endif
		};

		using EffectVec = xr_vector<SEffect*>;
		using EffectIt = EffectVec::iterator;

		EffectVec			m_Effects;
#ifdef _EDITOR
// change Equal if variables changed 
		void   	OnEffectsEditClick	(ButtonValue* sender, bool& bDataModified, bool& bSafe);
		void   	OnEffectTypeChange	(PropValue* sender);
		void   	OnEffectEditClick	(ButtonValue* sender, bool& bDataModified, bool& bSafe);
		bool RemoveEffect(int idx, bool& safe);
		bool RemoveEffect(SEffect* effect);
		void   	OnControlClick	(ButtonValue* sender, bool& bDataModified, bool& bSafe);
		void   	OnParamsChange	(PropValue* sender);
		void	OnParamsChangeInternal();
		void				FillProp	   	(LPCSTR pref, PropItemVec& items, void* owner);
		BOOL				Equal			(const CPGDef* pe);
		bool				Validate 			(bool bMsg);
		int FindEffect(SEffect* effect);
#endif
	public:
							CPGDef		  	();
							~CPGDef		  	();
		void				SetName		  	(LPCSTR name);

		void 				Save		  	(IWriter& F);
		BOOL 				LoadOriginal	(IReader& F);

		void 				Save2		  	(CInifile& ini);
		BOOL 				Load2		 	(CInifile& ini);
		BOOL				Load2Original	(CInifile& ini);
		BOOL 				Load2Extended	(CInifile& ini);

#ifdef _EDITOR
        void				Clone			(CPGDef* source);
#endif
	};

	class ECORE_API CParticleGroup: public dxRender_Visual, public IParticleCustom
	{
		const CPGDef*		m_Def;
		float				m_CurrentTime;
		Fvector				m_InitialPosition;
	public:
		xrCriticalSection	onframe_lock;
		using VisualVec = xr_vector<dxRender_Visual*>;
		using VisualVecIt = VisualVec::iterator;

		struct SItem
		{
			dxRender_Visual* _effect;
			VisualVec _children_related;
			VisualVec _children_free;
			xr_set<dxRender_Visual*> _children_destroy;

		public:
			~SItem();

			void Set(dxRender_Visual* e);
			void Clear();

			IC u32 GetVisuals(xr_vector<dxRender_Visual*>& visuals)
			{
				visuals.reserve(_children_related.size() + _children_free.size() + 1);
				if (_effect)
					visuals.push_back(_effect);

				visuals.insert(visuals.end(), _children_related.begin(), _children_related.end());
				visuals.insert(visuals.end(), _children_free.begin(), _children_free.end());
				return u32(visuals.size());
			}

			void OnDeviceCreate();
			void OnDeviceDestroy();

			void StartRelatedChild(CParticleEffect* emitter, LPCSTR eff_name, PAPI::Particle& m);
			void StopRelatedChild(u32 idx);
			void StartFreeChild(CParticleEffect* emitter, LPCSTR eff_name, PAPI::Particle& m);

			void UpdateParent(const Fmatrix& m, const Fvector& velocity, BOOL bXFORM);
			void OnFrame(u32 u_dt, const CPGDef::SEffect& def, Fbox& box, bool& bPlaying);
			void DelayDeleteChilds();

			u32 ParticlesCount();
			BOOL IsPlaying() const;
			void Play();
			void Stop(BOOL def_stop);
		};

		using SItemVec = xr_vector<SItem>;
		using SItemVecIt = SItemVec::iterator;

		SItemVec			items;
	public:
		enum{
			flRT_Playing		= (1<<0),
			flRT_DefferedStop	= (1<<1),
		};
		Flags8				m_RT_Flags;
	public:
		CParticleGroup	();
		virtual				~CParticleGroup	();
		virtual void	 	OnFrame			(u32 dt);

		virtual void		Copy			(dxRender_Visual* pFrom) {FATAL("Can't duplicate particle system - NOT IMPLEMENTED");}

		virtual void 		OnDeviceCreate	();
		virtual void 		OnDeviceDestroy	();

		virtual void		UpdateParent	(const Fmatrix& m, const Fvector& velocity, BOOL bXFORM);

		BOOL				Compile			(CPGDef* def);

		const CPGDef*		GetDefinition	(){return m_Def;}

		virtual void		Play			();
		virtual void		Stop			(BOOL bDefferedStop=TRUE);
		virtual BOOL		IsPlaying		(){return m_RT_Flags.is(flRT_Playing);}

		virtual void		SetHudMode			(BOOL b);
		virtual BOOL		GetHudMode			();

		virtual void		SetLiveUpdate		(BOOL b);
		virtual BOOL		GetLiveUpdate		();

		virtual float		GetTimeLimit	(){VERIFY(m_Def); return m_Def->m_fTimeLimit;}

		virtual const shared_str	Name		(){VERIFY(m_Def); return m_Def->m_Name;}

        virtual u32 		ParticlesCount	();
		PAPI::ParticleAction* FindPA(shared_str PEName, PAPI::PActionEnum Action) override;
	};

	namespace PG
	{
		enum class Version: u16
		{
			Original = 0x0003,
			Extended,
			MAX,
			Latest = MAX - 1,
		};

		enum class Chunks: u32
		{
			VERSION = 0x0001,
			NAME = 0x0002,
			FLAGS = 0x0003,
			EFFECTS = 0x0004, // obsolete
			TIME_LIMIT = 0x0005,
			EFFECTS2 = 0x0007,
		};
		
	}

}
//----------------------------------------------------

//---------------------------------------------------------------------------
#endif