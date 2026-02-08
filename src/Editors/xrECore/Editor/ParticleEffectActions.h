#pragma once
#include "../../Include/xrRender/ParticleCustom.h"
#include "../../Layers/xrRender/particle_core/particle_actions_collection.h"

struct PBool
{
	bool		val;
    PBool		():val(false){}
    PBool		(BOOL _val):val(_val){}
    PBool		(bool _val):val(_val){}
    void 		set(bool v){val=v;}
};

struct PFloat{
    float		val;
    float		mn;
    float		mx;
    PFloat		()	{val=0.f;mn=0.f;mx=0.f;}
    PFloat		(float _val, float _mn, float _mx):val(_val),mn(_mn),mx(_mx){}
    void 		set(float v){val=v;}
};

struct PInt{
    int			val;
    int			mn;
    int			mx;
    PInt		()	{val=0;mn=0;mx=0;}
    PInt		(int _val, int _mn, int _mx):val(_val),mn(_mn),mx(_mx){}
    void 		set(int v){val=v;}
};

struct PVector{
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

struct PString
{
	shared_str val;
	PString():val(""){}
	PString(shared_str _val):val(_val){}
	PString(LPCSTR _val):val(_val){}
	void set(shared_str v){val=v;}
	void set(LPCSTR v){val=v;}
};

struct PEnum
{
	xr_token* tokens = nullptr;
	u32 value = 0;
	u8 EnumSize = 0;
	PEnum() = default;
	PEnum(xr_token* _tokens, u8 _EnumSize):tokens(_tokens),EnumSize(_EnumSize){}
	PEnum(xr_token* _tokens, u8 _EnumSize, u32 _value):tokens(_tokens),EnumSize(_EnumSize),value(_value){}
	void set(u32 v){value=v;}
};

struct ECORE_API PDomain
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
protected:
	void  OnTypeChange(PropValue* sender);
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
    
	void 		Render		(u32 color, const Fmatrix& parent);
    void 		FillProp	(PropItemVec& items, LPCSTR pref, u32 clr);
};
struct EParticleAction
{		
	enum class EVersion : u32
	{
		Old,
		Original,
		Extended,
		SomeVasnyaBranch,
		MAX,
		Current = MAX - 1,
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
	xr_map<AnsiString, PEnum> enums;

    enum EValType{
    	tpDomain,
        tpVector,
        tpFloat,
        tpBool,
        tpInt,
    	tpString,
    	tpEnum,
    };
    struct SOrder{
    	EValType	type;
    	EChooseMode string_type = smCustom;
    	xr_string	name;
    	EVersion min_version = EVersion::Original;
    	SOrder(EValType _type, xr_string _name, EVersion _min_version = EVersion::Original);
        SOrder(EValType	_type, xr_string _name, EChooseMode _string_type, EVersion _min_version = EVersion::Original);
    };
    using OrderVec = xr_vector<SOrder>;
    using OrderVecIt = OrderVec::iterator;

    OrderVec		orders;
    
    EParticleAction	(PAPI::PActionEnum	_type)
    {
    	flags.assign(flEnabled);
        type		= _type;
    }

    virtual ~EParticleAction() = default;

public:
	SOrder&	appendFloat	(LPCSTR name, float v, float mn, float mx);
	SOrder&	appendInt	(LPCSTR name, int v, int mn=-P_MAXINT, int mx=P_MAXINT);
	SOrder&	appendVector(LPCSTR name, PVector::EType type, float vx, float vy, float vz, float mn=-P_MAXFLOAT, float mx=P_MAXFLOAT);
	SOrder&	appendDomain(LPCSTR name, PDomain v);
	SOrder&	appendBool	(LPCSTR name, BOOL b);
	SOrder&	appendBool	(LPCSTR name, bool b);
	SOrder&	appendString(LPCSTR name, const shared_str& v, EChooseMode _string_type = smCustom);
	SOrder&	appendString(LPCSTR name, LPCSTR v, EChooseMode _string_type = smCustom);
	SOrder& appendEnum(LPCSTR name, xr_token* variants, u8 EnumSize, u32 index);
	template<XRay::Concepts::Enum T>
	SOrder& appendEnum(LPCSTR name, xr_token* variants, T index){ return appendEnum(name, variants, sizeof(T), (u32)index); }
	PFloat& _float(LPCSTR name){auto it=floats.find(name); R_ASSERT2(it!=floats.end(),name);	return it->second;}
	PInt& _int(LPCSTR name){auto it=ints.find(name); R_ASSERT2(it!=ints.end(),name); return it->second;}
	PVector& _vector(LPCSTR name){auto it=vectors.find(name); R_ASSERT2(it!=vectors.end(),name); return it->second;}
	PDomain& _domain(LPCSTR name){auto it=domains.find(name); R_ASSERT2(it!=domains.end(),name); return it->second;}
	PBool& _bool(LPCSTR name){auto it=bools.find(name); R_ASSERT2(it!=bools.end(),name); return it->second;}
	PBool* _bool_safe(LPCSTR name){auto it=bools.find(name); return (it!=bools.end())?&it->second:0;}
	PString& _string(LPCSTR name){auto it=strings.find(name); R_ASSERT(it!=strings.end(),name); return it->second;}
	PEnum& _enum(LPCSTR name){auto it = enums.find(name); R_ASSERT(it!=enums.end(),name); return it->second;}
public:
	void FillPropInit(PropItemVec& items, LPCSTR pref);
	
    virtual void	Compile		(IWriter& F)=0;
    virtual void 	FillProp	(PropItemVec& items, LPCSTR pref, u32 clr);

    virtual void 	Load		(IReader& F);
    virtual void 	Save		(IWriter& F);
    virtual void 	Load2		(CInifile& ini, const shared_str& sect);
    virtual void 	Save2		(CInifile& ini, const shared_str& sect);
    virtual void 	Render		(const Fmatrix& parent);
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

struct EPAAvoid : public EParticleAction
{
					EPAAvoid	();
    virtual void	Compile		(IWriter& F);
};
 
struct EPABounce : public EParticleAction
{
					EPABounce	();
    virtual void	Compile		(IWriter& F);
};

struct EPACopyVertexB : public EParticleAction
{
					EPACopyVertexB();
    virtual void	Compile		(IWriter& F);
};

struct EPADamping : public EParticleAction
{
					EPADamping	();
    virtual void	Compile		(IWriter& F);
};

struct EPAExplosion : public EParticleAction
{
					EPAExplosion();
    virtual void	Compile		(IWriter& F);
};

struct EPAFollow : public EParticleAction
{
					EPAFollow	();
    virtual void	Compile		(IWriter& F);
};

struct EPAGravitate : public EParticleAction
{
					EPAGravitate();
    virtual void	Compile		(IWriter& F);
};

struct EPAGravity : public EParticleAction
{
					EPAGravity	();
    virtual void	Compile		(IWriter& F);
};

struct EPAJet : public EParticleAction
{
					EPAJet		();
    virtual void	Compile		(IWriter& F);
    virtual void 	Render		(const Fmatrix& parent);
};

struct EPAKillOld : public EParticleAction
{
					EPAKillOld	();
    virtual void	Compile		(IWriter& F);
};

struct EPAMatchVelocity : public EParticleAction
{
					EPAMatchVelocity();
    virtual void	Compile		(IWriter& F);
};

struct EPAMove : public EParticleAction
{
					EPAMove		();
    virtual void	Compile		(IWriter& F);
};

struct EPAOrbitLine : public EParticleAction
{
					EPAOrbitLine();
    virtual void	Compile		(IWriter& F);
    virtual void 	Render		(const Fmatrix& parent);
};

struct EPAOrbitPoint : public EParticleAction
{
					EPAOrbitPoint();
    virtual void	Compile		(IWriter& F);
    virtual void 	Render		(const Fmatrix& parent);
};

struct EPARandomAccel : public EParticleAction
{
					EPARandomAccel();
    virtual void	Compile		(IWriter& F);
};

struct EPARandomDisplace : public EParticleAction
{
					EPARandomDisplace();
    virtual void	Compile		(IWriter& F);
};

struct EPARandomVelocity : public EParticleAction
{
					EPARandomVelocity();
    virtual void	Compile		(IWriter& F);
};

struct EPARestore : public EParticleAction
{
					EPARestore	();
    virtual void	Compile		(IWriter& F);
};

struct EPAScatter : public EParticleAction
{
					EPAScatter	();
    virtual void	Compile		(IWriter& F);
    virtual void 	Render		(const Fmatrix& parent);
};

struct EPASink : public EParticleAction
{
					EPASink		();
    virtual void	Compile		(IWriter& F);
};

struct EPASinkVelocity : public EParticleAction
{
					EPASinkVelocity();
    virtual void	Compile		(IWriter& F);
};

struct EPASpeedLimit : public EParticleAction
{
					EPASpeedLimit();
    virtual void	Compile		(IWriter& F);
};

struct EPASource : public EParticleAction
{
					EPASource	();
    virtual void	Compile		(IWriter& F);
};

struct EPATargetColor : public EParticleAction
{
					EPATargetColor();
    virtual void	Compile		(IWriter& F);
};

struct EPATargetSize : public EParticleAction
{
					EPATargetSize();
    virtual void	Compile		(IWriter& F);
};

struct EPATargetRotate : public EParticleAction
{
					EPATargetRotate();
    virtual void	Compile		(IWriter& F);
};

struct EPATargetVelocity : public EParticleAction
{
					EPATargetVelocity();
    virtual void	Compile		(IWriter& F);
};

struct EPAVortex : public EParticleAction
{
					EPAVortex	();
    virtual void	Compile		(IWriter& F);
    virtual void 	Render		(const Fmatrix& parent);
};

struct EPATurbulence : public EParticleAction
{
	float	***nval;
    float 	age;
public:
					EPATurbulence();
	
    virtual void	Compile		(IWriter& F);
    virtual void 	Render		(const Fmatrix& parent);
};

// Binders

struct EPABindColorValue: public EParticleAction
{
	EPABindColorValue();
	virtual void Compile(IWriter& F);
};

struct EPABindColorAlpha: public EParticleAction
{
	EPABindColorAlpha();
	virtual void Compile(IWriter& F);
};

struct EPABindSizeValue: public EParticleAction
{
	EPABindSizeValue();
	virtual void Compile(IWriter& F);
};

struct EPABindRotateValue: public EParticleAction
{
	EPABindRotateValue();
	virtual void Compile(IWriter& F);
};

struct EPABindVelocityValue: public EParticleAction
{
	EPABindVelocityValue();
	virtual void Compile(IWriter& F);
};

// Animators

struct EPAColorAnimator : public EParticleAction
{
	EPAColorAnimator();
	virtual void Compile(IWriter& F);
};

struct EPASizeAnimator : public EParticleAction
{
	EPASizeAnimator();
	virtual void Compile(IWriter& F);
};

struct EPAVelocityAnimator : public EParticleAction
{
	EPAVelocityAnimator();
	virtual void Compile(IWriter& F);
};

struct EPAVelocityRotationAnimator : public EParticleAction
{
	EPAVelocityRotationAnimator();
	virtual void Compile(IWriter& F);
};

extern ECORE_API xr_token2* actions_token;

typedef EParticleAction* (*_CreateEAction)(PAPI::PActionEnum type);
extern ECORE_API _CreateEAction 	pCreateEAction;

enum EEditMode
{
    emNone,
    emEffect,
    emGroup,
	emAction,
	emEffectSlot,
	emAnimCurve,
};

namespace PEd
{
	using ListTypeBase = u8;
	enum class LisType: ListTypeBase
	{
		Groups = 1 << 0,
		Effects = 1 << 1,
		AnimCurve = 1 << 2,
		// add new here
		All = Groups | Effects | AnimCurve
	};

	ListTypeBase operator|(PEd::LisType lis, PEd::LisType rhs);
	
}
