//---------------------------------------------------------------------------
#ifndef ParticleEffectDefH
#define ParticleEffectDefH
#include "RenderAPI.h"

#include "Shader.h"
#include "../../Include/xrRender/ParticleDefinition.h"

namespace EPALegacy
{
	struct EParticleAction;
}

class ButtonValue;

namespace PAPI
{
	struct Particles;
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

	typedef bool ( * CollisionCallback)(CParticleEffect* E, PAPI::Particles& P, size_t i, const Fvector& pt, const Fvector& norm); // true-continue collision exec
	typedef void ( * DestroyCallback)	(CParticleEffect* E, PAPI::Particles& P, size_t i);

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

	// Exported per member: the Particle Editor defines the editor-only members (declared under _EDITOR) in its own module.
	class CPEDef : public IParticleDefinition
	{
	public:
		enum
		{
			dfSprite = 1u << 0,
			dfFramed = 1u << 10,
			dfAnimated = 1u << 11,
			dfRandomFrame = 1u << 12,
			dfRandomPlayback = 1u << 13,
			dfTimeLimit = 1u << 14,
			dfAlignToPath = 1u << 15,
			dfCollision = 1u << 16,
			dfCollisionDel = 1u << 17,
			dfVelocityScale = 1u << 18,
			dfCollisionDyn = 1u << 19,
			dfWorldAlign = 1u << 20,
			dfFaceAlign = 1u << 21,
			dfCulling = 1u << 22,
			dfCullCCW = 1u << 23,
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
	// editor action list (owned by the Particle Editor through IParticleEditorActions; empty in the game)
		using EditorActionVec = xr_vector<::EParticleAction*>;
		using EditorActionVecIt = EditorActionVec::iterator;
		EditorActionVec		m_EActionList;
	public:
		RENDER_API bool 	SaveActionList		(IWriter& F);
		RENDER_API bool 	LoadActionList		(IReader& F);
	// execute
		RENDER_API void		ExecuteAnimate		(PAPI::Particles& particles, u32 p_cnt, float dt);
        RENDER_API void		ExecuteCollision	(PAPI::Particles& particles, u32 p_cnt, float dt, CParticleEffect* owner, CollisionCallback cb);
	public:
        RENDER_API          CPEDef				();
        RENDER_API          ~CPEDef				();
        
		RENDER_API void		SetName				(const char* name) override;
        IC const char*			Name				()const override {return *m_Name;}
        RENDER_API void		CreateShader		() override;
        RENDER_API void		DestroyShader		() override;
		int GetMaxParticles() const override { return m_MaxParticles; }
		void SetMaxParticles(int count) override { m_MaxParticles = count; }
		float GetTimeLimit() const override { return m_fTimeLimit; }
		bool HasTimeLimit() const override { return m_Flags.test(dfTimeLimit); }
		void SetTimeLimit(float seconds) override { m_Flags.set(dfTimeLimit, true); m_fTimeLimit = seconds; }
		const shared_str& GetShaderName() const override { return m_ShaderName; }
		const shared_str& GetTextureName() const override { return m_TextureName; }
		void SetSprite(const char* shader, const char* texture) override
		{
			m_ShaderName = shader;
			m_TextureName = texture;
			m_Flags.set(dfSprite, shader != nullptr && shader[0] != 0 && texture != nullptr && texture[0] != 0);
		}
		u32 GetCompiledActionBytes() const override { return m_Actions.size(); }
		bool GetSourceRate(float& rate) const override;
		bool SetSourceRate(float rate) override;

		RENDER_API void 	Save				(IWriter& F);
		RENDER_API bool 	LoadOriginal		(IReader& F);
		RENDER_API bool 	LoadExtended		(IReader& F);

		RENDER_API void 	Save2				(CInifile& ini);
		RENDER_API bool 	Load2				(CInifile& ini);
		RENDER_API bool		Load2Original		(CInifile& ini);
		RENDER_API bool		Load2Entended		(CInifile& ini);

		using EPAVec = xr_vector<EPALegacy::EParticleAction*>;
		using EPAVecIt = EPAVec::iterator;
#ifdef _EDITOR
// change Copy&Equal if variables changed
	public:
		void				CompileEditorActions();
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
		// Reloads the definition in place from a Save stream (undo)
		bool				Restore				(IReader& F);
		bool				Equal				(const CPEDef* pe);
		void 				Render				(const Fmatrix& parent);
		static PFunction*	FindCommandPrototype(const char* src, const char*& dest);
		void   	FillActionList		(ChooseItemVec& items, void* param);
        bool 				Validate 			(bool bMsg);
		void				OnDrawUI			();
#endif
		RENDER_API void Compile(EPAVec& v);
	};

	// Editor action list I/O. The Particle Editor installs an implementation; it is null in the game
	// and in the other editors, which use the compiled action streams only.
	class IParticleEditorActions
	{
	public:
		virtual ~IParticleEditorActions() = default;

		virtual void LoadActions(CPEDef& Def, IReader& F) = 0;
		virtual void SaveActions(CPEDef& Def, IWriter& F) = 0;
		virtual void LoadActions2(CPEDef& Def, CInifile& Ini) = 0;
		virtual void SaveActions2(CPEDef& Def, CInifile& Ini) = 0;
		virtual void DestroyActions(CPEDef& Def) = 0;
	};

	extern RENDER_API IParticleEditorActions* g_ParticleEditorActions;

};
//---------------------------------------------------------------------------
#endif
