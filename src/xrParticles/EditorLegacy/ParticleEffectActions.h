#pragma once
#include "luabind/detail/primitives.hpp"

namespace PS
{
	class CPEDef;
}

namespace EPALegacy
{
	using AnsiString = xr_string;
	using AStringVec = xr_vector<AnsiString>;
	using AStringIt = AStringVec::iterator;

	using LPAStringVec = xr_vector<AnsiString*>;
	using LPAStringIt = LPAStringVec::iterator;
	
	struct PBool final
	{
		bool		val;
		PBool		():val(false){}
		PBool		(BOOL _val):val(_val){}
		PBool		(bool _val):val(_val){}
		void 		set(bool v){val=v;}
	};

	struct PFloat final{
		float		val;
		float		mn;
		float		mx;
		PFloat		()	{val=0.f;mn=0.f;mx=0.f;}
		PFloat		(float _val, float _mn, float _mx):val(_val),mn(_mn),mx(_mx){}
		void 		set(float v){val=v;}
	};

	struct PInt final{
		int			val;
		int			mn;
		int			mx;
		PInt		()	{val=0;mn=0;mx=0;}
		PInt		(int _val, int _mn, int _mx):val(_val),mn(_mn),mx(_mx){}
		void 		set(int v){val=v;}
	};

	struct PVector final{
		Fvector		val;
		float		mn;
		float		mx;
		enum EType{
			vNum,
			vAngle,
			vColor,
			_force_u32 = u32(-1),
		};
		EType		type;
		PVector		(){val.set(0,0,0);mn=0.f;mx=0.f;}
		PVector		(EType t, Fvector _val, float _mn, float _mx):type(t),val(_val),mn(_mn),mx(_mx){}
		void 		set(const Fvector& v){val.set(v);}
		void 		set(float x, float y, float z){val.set(x,y,z);}
	};

	struct PString final
	{
		shared_str val;
		PString():val(""){}
		PString(shared_str _val):val(_val){}
		PString(LPCSTR _val):val(_val){}
		void set(shared_str v){val=v;}
		void set(LPCSTR v){val=v;}
	};

	struct PDomain final
	{
	public:
		PAPI::PDomainEnum	type;
		union{
			float	f[9];
			Fvector	v[3];  
		};
		enum EType{
			vNum,
			vAngle,
			vColor,
			_force_u32 = u32(-1),
		};
		enum {
			flRenderable = (1<<0)
		};
		EType		e_type;
		Flags32		flags;
		u32			clr;
	public:
		PDomain 	(){}
		PDomain		(EType et, BOOL renderable, u32 color=0x00000000, PAPI::PDomainEnum type = PAPI::PDPoint,	
														float inA0 = 0.0f,	float inA1 = 0.0f,	float inA2 = 0.0f,
														float inA3 = 0.0f,	float inA4 = 0.0f,	float inA5 = 0.0f,
														float inA6 = 0.0f,	float inA7 = 0.0f,	float inA8 = 0.0f	);
		~PDomain	();
		PDomain 	(const PDomain &in);

		void		MoveXYZ		(float x, float y, float z);
		void		RotateXYZ	(float x, float y, float z);
		void		ScaleXYZ	(float x, float y, float z);

		Fvector&	GetCenter();
	
		void 		Load		(IReader& F);
		void 		Save		(IWriter& F) const;

		void 		Load2		(CInifile& ini, const shared_str& sect);
		void 		Save2		(CInifile& ini, const shared_str& sect) const;
	};
	struct PARTICLES_API EParticleAction
	{	
		enum class EVersion : u32
		{
			Old,
			Original,
			Extended
		};
	
		PS::CPEDef*		parent = nullptr;
		shared_str 		actionName;
		shared_str 		actionType;
		shared_str		hint;
		enum{
			flEnabled	= (1<<0),
			flDraw		= (1<<1),
		};
		Flags32				flags;
		PAPI::PActionEnum	type;
		EVersion Version;

		xr_map<AnsiString, PDomain> domains;
		xr_map<AnsiString, PBool> bools;
		xr_map<AnsiString, PFloat> floats;
		xr_map<AnsiString, PInt> ints;
		xr_map<AnsiString, PVector> vectors;
		xr_map<AnsiString, PString> strings;

		enum EValType{
			tpDomain,
			tpVector,
			tpFloat,
			tpBool,
			tpInt,
			tpString,
		};
		struct SOrder{
			EValType	type;
			xr_string	name;
			EVersion min_version = EVersion::Original;
			SOrder(EValType _type, xr_string _name, EVersion _min_version = EVersion::Original);
		};
		using OrderVec = xr_vector<SOrder>;
		using OrderVecIt = OrderVec::iterator;

		OrderVec		orders;
    
		EParticleAction	(PAPI::PActionEnum	_type)
		{
			flags.assign(flEnabled);
			type		= _type;
		}
	public:
		virtual ~EParticleAction() = default;
		SOrder&	appendFloat	(LPCSTR name, float v, float mn, float mx);
		SOrder&	appendInt	(LPCSTR name, int v, int mn=-P_MAXINT, int mx=P_MAXINT);
		SOrder&	appendVector(LPCSTR name, PVector::EType type, float vx, float vy, float vz, float mn=-P_MAXFLOAT, float mx=P_MAXFLOAT);
		SOrder&	appendDomain(LPCSTR name, PDomain v);
		SOrder&	appendBool	(LPCSTR name, BOOL b);
		SOrder&	appendBool	(LPCSTR name, bool b);
		SOrder&	appendString(LPCSTR name, const shared_str& v);
		SOrder&	appendString(LPCSTR name, LPCSTR v);
		PFloat&			_float		(LPCSTR name){auto 	it=floats.find(name); 	R_ASSERT2(it!=floats.end(),name);	return it->second;}
		PInt&			_int		(LPCSTR name){auto 	it=ints.find(name); 	R_ASSERT2(it!=ints.end(),name);		return it->second;}
		PVector&		_vector		(LPCSTR name){auto 	it=vectors.find(name); 	R_ASSERT2(it!=vectors.end(),name);	return it->second;}
		PDomain&		_domain		(LPCSTR name){auto 	it=domains.find(name); 	R_ASSERT2(it!=domains.end(),name);	return it->second;}
		PBool&			_bool		(LPCSTR name){auto 	it=bools.find(name); 	R_ASSERT2(it!=bools.end(),name); 	return it->second;}
		PBool*			_bool_safe	(LPCSTR name){auto 	it=bools.find(name); 	return (it!=bools.end())?&it->second:0;}
		PString&		_string		(LPCSTR name){auto	it=strings.find(name);	R_ASSERT(it!=strings.end(),name);	return it->second;}
	public:
	
		virtual void	Compile		(IWriter& F)=0;

		virtual void 	Load		(IReader& F);
		virtual void 	Save		(IWriter& F);
		virtual void 	Load2		(CInifile& ini, const shared_str& sect);
		virtual void 	Save2		(CInifile& ini, const shared_str& sect);
	private:
	
		template <int count>
		LPCSTR GenerateKey_Extended(char (&buff)[count], LPCSTR type, LPCSTR sect_ref, LPCSTR id)
		{
			xr_string str_id = id;
			xr_strlwr(str_id);
			std::ranges::replace(str_id, ' ', '_');
			std::ranges::replace(str_id, '\\', '_');
			if (sect_ref)
			{
				xr_sprintf(buff, "%s_%s_%s", type, sect_ref, str_id.c_str());
			} else
			{
				xr_sprintf(buff, "%s_%s", type, str_id.c_str());
			}
			return buff;
		};
	};

	struct PARTICLES_API EPAAvoid final : public EParticleAction
	{
		EPAAvoid	();
		virtual void	Compile		(IWriter& F);
	};
 
	struct PARTICLES_API EPABounce final : public EParticleAction
	{
		EPABounce	();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPACopyVertexB final : public EParticleAction
	{
		EPACopyVertexB();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPADamping final : public EParticleAction
	{
		EPADamping	();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAExplosion final : public EParticleAction
	{
		EPAExplosion();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAFollow final : public EParticleAction
	{
		EPAFollow	();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAGravitate final : public EParticleAction
	{
		EPAGravitate();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAGravity final : public EParticleAction
	{
		EPAGravity	();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAJet final : public EParticleAction
	{
		EPAJet		();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAKillOld final : public EParticleAction
	{
		EPAKillOld	();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAMatchVelocity final : public EParticleAction
	{
		EPAMatchVelocity();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAMove final : public EParticleAction
	{
		EPAMove		();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAOrbitLine final : public EParticleAction
	{
		EPAOrbitLine();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAOrbitPoint final : public EParticleAction
	{
		EPAOrbitPoint();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPARandomAccel final : public EParticleAction
	{
		EPARandomAccel();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPARandomDisplace final : public EParticleAction
	{
		EPARandomDisplace();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPARandomVelocity final : public EParticleAction
	{
		EPARandomVelocity();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPARestore final : public EParticleAction
	{
		EPARestore	();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAScatter final : public EParticleAction
	{
		EPAScatter	();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPASink final : public EParticleAction
	{
		EPASink		();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPASinkVelocity final : public EParticleAction
	{
		EPASinkVelocity();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPASpeedLimit final : public EParticleAction
	{
		EPASpeedLimit();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPASource final : public EParticleAction
	{
		EPASource	();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPATargetColor final : public EParticleAction
	{
		EPATargetColor();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPATargetSize final : public EParticleAction
	{
		EPATargetSize();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPATargetRotate final : public EParticleAction
	{
		EPATargetRotate();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPATargetVelocity final : public EParticleAction
	{
		EPATargetVelocity();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPAVortex final : public EParticleAction
	{
		EPAVortex	();
		virtual void	Compile		(IWriter& F);
	};

	struct PARTICLES_API EPATurbulence final : public EParticleAction
	{
		float	***nval;
		float 	age;
	public:
		EPATurbulence();
	
		virtual void	Compile		(IWriter& F);
	};

	// Binders

	struct PARTICLES_API EPABindColorValue final: public EParticleAction
	{
		EPABindColorValue();
		virtual void Compile(IWriter& F);
	};

	struct PARTICLES_API EPABindColorAlpha final: public EParticleAction
	{
		EPABindColorAlpha();
		virtual void Compile(IWriter& F);
	};

	struct PARTICLES_API EPABindSizeValue final: public EParticleAction
	{
		EPABindSizeValue();
		virtual void Compile(IWriter& F);
	};

	struct PARTICLES_API EPABindRotateValue final: public EParticleAction
	{
		EPABindRotateValue();
		virtual void Compile(IWriter& F);
	};

	struct PARTICLES_API EPABindVelocityValue final: public EParticleAction
	{
		EPABindVelocityValue();
		virtual void Compile(IWriter& F);
	};

	// Animators

	struct PARTICLES_API EPAColorAnimator final : public EParticleAction
	{
		EPAColorAnimator();
		virtual void Compile(IWriter& F);
	};

	struct PARTICLES_API EPASizeAnimator final : public EParticleAction
	{
		EPASizeAnimator();
		virtual void Compile(IWriter& F);
	};

	struct PARTICLES_API EPAVelocityAnimator final : public EParticleAction
	{
		EPAVelocityAnimator();
		virtual void Compile(IWriter& F);
	};

	struct PARTICLES_API EPAVelocityRotationAnimator final : public EParticleAction
	{
		EPAVelocityRotationAnimator();
		virtual void Compile(IWriter& F);
	};

	struct PARTICLES_API EPACreator // the only reason I created this struct is to export this fucking function, because for some reason it didn't do that as normal function!
	{
		static EParticleAction* pCreateEActionImpl(PAPI::PActionEnum type);
	};
}
