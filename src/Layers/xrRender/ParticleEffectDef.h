//---------------------------------------------------------------------------
#ifndef ParticleEffectDefH
#define ParticleEffectDefH

#include "Shader.h"

namespace EPALegacy
{
	struct EParticleAction;
}

class ButtonValue;

namespace PAPI
{
	struct Particle;
	struct ParticleEffect;
	struct PAHeader;
	struct ParticleAction;

	using PAVec = xr_vector<ParticleAction*>;
	using PAVecIt = PAVec::iterator;
}
struct EParticleAction;        

namespace PS
{
	class CParticleEffect;

	typedef bool ( * CollisionCallback)(CParticleEffect* E, PAPI::Particle& P, const Fvector& pt, const Fvector& norm); // TRUE-continue collision exec
	typedef void ( * DestroyCallback)	(CParticleEffect* E, PAPI::Particle& P);

	class PFunction;
	struct SFrame
	{
		Fvector2			m_fTexSize;
		Fvector2			reserved; 
		int     			m_iFrameDimX;
		int 				m_iFrameCount;
		float				m_fSpeed;

		void 				InitDefault()
		{
			m_fTexSize.set	(32.f/256.f,64.f/128.f);
			m_iFrameDimX 	= 8;
			m_iFrameCount 	= 16;
			m_fSpeed		= 24.f;
		}
		IC void       		CalculateTC(int frame, Fvector2& lt, Fvector2& rb)
		{
			lt.x       	 	= (frame%m_iFrameDimX)*m_fTexSize.x;
			lt.y        	= (frame/m_iFrameDimX)*m_fTexSize.y;
			rb.x        	= lt.x+m_fTexSize.x;
			rb.y        	= lt.y+m_fTexSize.y;
		}
	};

	class ECORE_API CPEDef
	{
	public:
		enum{
			dfSprite		= (1<<0),
//			dfObject		= (1<<1),

			dfFramed		= (1<<10),
			dfAnimated		= (1<<11),
			dfRandomFrame   = (1<<12),
			dfRandomPlayback= (1<<13),
            
			dfTimeLimit		= (1<<14),

            dfAlignToPath	= (1<<15),
            dfCollision		= (1<<16),
            dfCollisionDel	= (1<<17),
            dfVelocityScale	= (1<<18),
            dfCollisionDyn	= (1<<19),
			dfWorldAlign	= (1<<20),
            dfFaceAlign		= (1<<21),
            dfCulling		= (1<<22),
            dfCullCCW		= (1<<23),
		};
		shared_str		  	m_Name;
		Flags32				m_Flags;
	// texture
		shared_str		  	m_ShaderName;
		shared_str		  	m_TextureName;
		ref_shader			m_CachedShader;
		SFrame				m_Frame;
	// compiled actions
        CMemoryWriter		m_Actions;
	// def        
		float				m_fTimeLimit;			// time limit
		int					m_MaxParticles;			// max particle count
	    Fvector				m_VelocityScale;		// velocity scale
	    Fvector				m_APDefaultRotation;	// align to path
    // collision
	    float 				m_fCollideOneMinusFriction;
        float 				m_fCollideResilience;
        float 				m_fCollideSqrCutoff; 
	public:
		bool 				SaveActionList		(IWriter& F);
		bool 				LoadActionList		(IReader& F);
	// execute
		void				ExecuteAnimate		(PAPI::Particle *particles, u32 p_cnt, float dt);
        void				ExecuteCollision	(PAPI::Particle *particles, u32 p_cnt, float dt, CParticleEffect* owner, CollisionCallback cb);
	public:
                            CPEDef				();
                            ~CPEDef				();
        
		void				SetName				(const char* name);
        IC const char*			Name				()const{return *m_Name;}
        void				CreateShader		();
        void				DestroyShader		();

		void 				Save				(IWriter& F);
		bool 				LoadOriginal		(IReader& F);
		bool 				LoadExtended		(IReader& F);

		void 				Save2				(CInifile& ini);
		bool 				Load2				(CInifile& ini);
		bool				Load2Original		(CInifile& ini);
		bool				Load2Entended		(CInifile& ini);

#ifndef _EDITOR
		using EPAVec = xr_vector<EPALegacy::EParticleAction*>;
		using EPAVecIt = EPAVec::iterator;
#else
// change Copy&Equal if variables changed
	public:
		using EPAVec = xr_vector<EParticleAction*>;
		using EPAVecIt = EPAVec::iterator;

		EPAVec m_EActionList;
	public:             
		void   	FindActionByName	(const char* new_name, bool& res);
		bool   	NameOnAfterEdit					(PropValue* sender, shared_str& edit_val);
		bool   	CollisionFrictionOnAfterEdit	(PropValue* sender, float& edit_val);
		void   	CollisionFrictionOnBeforeEdit	(PropValue* sender, float& edit_val);
		void   	CollisionFrictionOnDraw			(PropValue* sender, xr_string& draw_val);
		bool   	CollisionCutoffOnAfterEdit		(PropValue* sender, float& edit_val);
		void   	CollisionCutoffOnBeforeEdit		(PropValue* sender, float& edit_val);
		void   	CollisionCutoffOnDraw			(PropValue* sender, xr_string& draw_val);
		void   	OnActionEditClick	(ButtonValue* sender, bool& bDataModified, bool& bSafe);
		bool	RemoveAction(EParticleAction* action);
		bool	RemoveAction(int idx);
		bool	MoveUpAction(EParticleAction* action);
		bool	MoveDownAction(EParticleAction* action);
		bool	MoveUpAction(int idx);
		bool	MoveDownAction(int idx);
	    void   	OnFrameResize		(PropValue* sender);
	    void   	OnShaderChange		(PropValue* sender);
	    void   	OnFlagChange		(PropValue* sender);
		void   	OnControlClick		(ButtonValue* sender, bool& bDataModified, bool& bSafe);
		void   	OnActionsClick		(ButtonValue* sender, bool& bDataModified, bool& bSafe);
        bool   	OnAfterActionNameEdit(PropValue* sender, shared_str& edit_val);
		void				FillProp		   	(const char* pref, ::PropItemVec& items, void* owner);
		
		void OnSpriteFrameClicked(ButtonValue* value, bool& bModif, bool& bSafe);
		void OnSpriteAnimatedClicked(ButtonValue* value, bool& bModif, bool& bSafe);
		void OnTimeLimitClicked(ButtonValue* value, bool& bModif, bool& bSafe);
		void OnSpriteClicked(ButtonValue* value, bool& bModif, bool& bSafe);
		void OnSpriteCullingClicked(ButtonValue* value, bool& bModif, bool& bSafe);
		void OnMovementAlignClicked(ButtonValue* value, bool& bModif, bool& bSafe);
		void OnMovementVelocityClicked(ButtonValue* value, bool& bModif, bool& bSafe);
		void OnMovementCollisionClicked(ButtonValue* value, bool& bModif, bool& bSafe);
		
		void				Copy				(const CPEDef& src);
		bool				Equal				(const CPEDef* pe);
		void 				Render				(const Fmatrix& parent);
		static PFunction*	FindCommandPrototype(const char* src, const char*& dest);
		void   	FillActionList		(ChooseItemVec& items, void* param);
        bool 				Validate 			(bool bMsg);
		void				OnDrawUI			();
#endif
		void Compile(EPAVec& v);
	};

	namespace PE
	{
		enum class Version: u16
		{
			Original = 0x0001,
			Extended,
			MAX,
			Latest = MAX - 1,
		};

		enum class Chunks: u32
		{
			VERSION = 0x0001,
			NAME = 0x0002,
			EFFECTDATA = 0x0003,
			ACTIONLIST = 0x0004,
			FLAGS = 0x0005,
			FRAME = 0x0006,
			SPRITE = 0x0007,
			TIMELIMIT = 0x0008,
			TIMELIMIT2 = 0x0009,
			SOURCETEXT_ = 0x0020, // obsolete
			COLLISION = 0x0021,
			VEL_SCALE = 0x0022,
			EDATA = 0x0024,
			ALIGN_TO_PATH = 0x0025,
		};
	}
};
//---------------------------------------------------------------------------
#endif
