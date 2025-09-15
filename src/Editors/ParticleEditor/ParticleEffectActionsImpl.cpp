//---------------------------------------------------------------------------
#include "stdafx.h"


#include "../xrECore/Editor/ParticleEffectActions.h"
#include "../xrEProps/FolderLib.h"
#include "../Public/PropertiesListHelper.h"
#include "../../xrParticles/particle_actions_collection.h"
#include "../../xrParticles/noise.h"

using namespace PAPI; 
#define PARTICLE_ACTION_VERSION_MIN 0x0000
#define PARTICLE_ACTION_VERSION_MAX 0x0001
//---------------------------------------------------------------------------
xr_token2 actions_token_impl [] = {
	{ "Avoid",				"Steer particles away from a domain of space.", 			                PAAvoidID				},        
	{ "Bounce",				"Bounce particles off a domain of space.",					                PABounceID				},        
	{ "Copy VertexB",		"Set the secondary position from current position.",		                PACopyVertexBID			},        
	{ "Damping",			"Simulate air by slowing down particle velocities.",		                PADampingID				},        
	{ "Explosion",			"An Explosion.", 											                PAExplosionID			},        
	{ "Follow",				"Accelerate toward the next particle in the group.",		                PAFollowID				},        
	{ "Gravitate",			"Accelerate each particle toward each other particle.",		                PAGravitateID			},        
	{ "Gravity",			"Accelerate particles in the given direction.", 			                PAGravityID				},        
	{ "Jet",				"Accelerate particles that are near the center of the jet.",                PAJetID					},        
	{ "Kill Old",			"Remove old particles.", 									                PAKillOldID				},        
	{ "Match Velocity",		"Modify each particle’s velocity to be similar to that of its neighbors.", 	PAMatchVelocityID		},        
	{ "Move",				"Move particle positions based on velocities.", 							PAMoveID				},        
	{ "Orbit Line",			"Accelerate toward the closest point on the given line.", 					PAOrbitLineID			},        
	{ "Orbit Point",		"Accelerate toward the given center point.", 								PAOrbitPointID			},        
	{ "Random Accel",		"Accelerate particles in random directions.", 								PARandomAccelID			},        
	{ "Random Displace",	"Immediately replace position with a position from the domain.", 			PARandomDisplaceID		},        
	{ "Random Velocity",	"Immediately replace velocity with a velocity from the domain.", 			PARandomVelocityID		},        
	{ "Restore",			"Over time, restore particles to their secondary positions.", 				PARestoreID				},        
	{ "Scatter",			"Scatter particles from center.", 											PAScatterID				},
	{ "Sink",				"Kill particles with positions on wrong side of the specified domain.", 	PASinkID				},        
	{ "Sink Velocity",		"Kill particles with velocities on wrong side of the specified domain.", 	PASinkVelocityID		},        
	{ "Source",				"Add particles in the specified domain.", 									PASourceID				},        
	{ "Speed Limit",		"Clamp each particle’s speed to the given min and max.", 					PASpeedLimitID			},        
	{ "Target Color",		"Change color of all particles toward the specified color.", 				PATargetColorID			},        
	{ "Target Size",		"Change sizes of all particles toward the specified size.", 				PATargetSizeID			},        
	{ "Target Rotate",		"Change rotate of all particles toward the specified rotation.", 			PATargetRotateID		},        
	{ "Target Velocity",	"Change velocity of all particles toward the specified velocity.", 			PATargetVelocityID		},        
	{ "Vortex",				"Swirl particles around a vortex.", 										PAVortexID				},        
{ "Turbulence",			"A Turbulence.",															PATurbulenceID			},
	// Binders
{"Bind Velocity",			"Bind particle Velocity variable for manual update from code.",			PABindVelocityValueID},
{"Bind Rotation",			"Bind particle Rotation variable for manual update from code.",			PABindRotationValueID},
{"Bind Size",				"Bind particle Size variable for manual update from code.",				PABindSizeValueID},
{"Bind Color (RGB)",		"Bind particle Color (RGB channels) variable for manual update from code.",	PABindColorValueID},
{"Bind Color (alpha)",		"Bind particle Color (alpha channel) variable for manual update from code.",	PABindColorAlphaID},
	// Animators
	{"Color Animator",		"Change color of all particles corresponding to specified animation curve.",	PAColorAnimatorID},
	{"Size Animator",		"Change size of all particles corresponding to specified animation curve.",	PASizeAnimatorID},
	{"Velocity Animator",	"Change velocity of all particles corresponding to specified animation curve.",PAVelocityAnimatorID},
	{"Velocity Rotation Animator", "Change direction of velocity of all particles corresponding to specified animation curve.", PAVelocityRotationAnimatorID},
	{ 0,					0				  	 	}
};


EParticleAction* pCreateEActionImpl(PAPI::PActionEnum type)
{
	EParticleAction* pa	= 0;
	switch(type){
	case PAPI::PAAvoidID:			pa = new EPAAvoid			();	break;
	case PAPI::PABounceID:    		pa = new EPABounce			();	break;
	case PAPI::PACopyVertexBID:    	pa = new EPACopyVertexB		();	break;
	case PAPI::PADampingID:    		pa = new EPADamping			();	break;
	case PAPI::PAExplosionID:    	pa = new EPAExplosion		();	break;
	case PAPI::PAFollowID:    		pa = new EPAFollow			();	break;
	case PAPI::PAGravitateID:    	pa = new EPAGravitate		();	break;
	case PAPI::PAGravityID:    		pa = new EPAGravity			();	break;
	case PAPI::PAJetID:    			pa = new EPAJet				();	break;
	case PAPI::PAKillOldID:    		pa = new EPAKillOld			();	break;
	case PAPI::PAMatchVelocityID:   pa = new EPAMatchVelocity	();	break;
	case PAPI::PAMoveID:    		pa = new EPAMove		   	();	break;
	case PAPI::PAOrbitLineID:    	pa = new EPAOrbitLine		();	break;
	case PAPI::PAOrbitPointID:    	pa = new EPAOrbitPoint		();	break;
	case PAPI::PARandomAccelID:    	pa = new EPARandomAccel		();	break;
	case PAPI::PARandomDisplaceID:  pa = new EPARandomDisplace	();	break;
	case PAPI::PARandomVelocityID:  pa = new EPARandomVelocity	();	break;
	case PAPI::PARestoreID:    		pa = new EPARestore			();	break;
	case PAPI::PAScatterID:			pa = new EPAScatter			();	break;
	case PAPI::PASinkID:    		pa = new EPASink		   	();	break;
	case PAPI::PASinkVelocityID:    pa = new EPASinkVelocity   	();	break;
	case PAPI::PASourceID:    		pa = new EPASource			();	break;
	case PAPI::PASpeedLimitID:    	pa = new EPASpeedLimit		();	break;
	case PAPI::PATargetColorID:    	pa = new EPATargetColor		();	break;
	case PAPI::PATargetSizeID:    	pa = new EPATargetSize		();	break;
	case PAPI::PATargetRotateID:    pa = new EPATargetRotate 	();	break;
	case PAPI::PATargetRotateDID:   pa = new EPATargetRotate 	();	break;
	case PAPI::PATargetVelocityID:	pa = new EPATargetVelocity	();	break;
	case PAPI::PATargetVelocityDID: pa = new EPATargetVelocity	();	break;
	case PAPI::PAVortexID:    		pa = new EPAVortex			();	break;
	case PAPI::PATurbulenceID: 		pa = new EPATurbulence		();	break;
		// Binders
	case PAPI::PABindVelocityValueID:	pa = new EPABindVelocityValue();	break;
	case PAPI::PABindRotationValueID:	pa = new EPABindRotateValue();	break;
	case PAPI::PABindSizeValueID:	pa = new EPABindSizeValue();	break;
	case PAPI::PABindColorValueID:	pa = new EPABindColorValue();	break;
	case PAPI::PABindColorAlphaID:	pa = new EPABindColorAlpha();	break;
		// Animators
	case PAPI::PAColorAnimatorID: pa = new EPAColorAnimator(); break;
	case PAPI::PASizeAnimatorID: pa = new EPASizeAnimator(); break;
	case PAPI::PAVelocityAnimatorID: pa = new EPAVelocityAnimator(); break;
	case PAPI::PAVelocityRotationAnimatorID: pa = new EPAVelocityRotationAnimator(); break;
	default: return nullptr;
	}
	pa->type						= type;
	return pa;
}
//---------------------------------------------------------------------------
void EParticleAction::Render(const Fmatrix& parent)
{
	for (PDomainMapIt it = domains.begin(); it != domains.end(); it++)
		it->second.Render(it->second.clr, parent);
}

void EParticleAction::Load(IReader& F)
{
	u32 vers = F.r_u32();
	R_ASSERT(vers <= PARTICLE_ACTION_VERSION_MAX && vers >= PARTICLE_ACTION_VERSION_MIN);

	F.r_stringZ(actionName);
	flags.assign(F.r_u32());

	for (PFloatMapIt f_it = floats.begin(); f_it != floats.end(); f_it++)
	{
		f_it->second.val = F.r_float();
	}

	for (PVectorMapIt v_it = vectors.begin(); v_it != vectors.end(); v_it++)
	{
		F.r_fvector3(v_it->second.val);
	}

	for (PDomainMapIt d_it = domains.begin(); d_it != domains.end(); d_it++)
	{
		d_it->second.Load(F);
	}

	for (PBoolMapIt b_it = bools.begin(); b_it != bools.end(); b_it++)
	{
		b_it->second.val = F.r_u8();
	}

	for (PIntMapIt i_it = ints.begin(); i_it != ints.end(); i_it++)
	{
		i_it->second.val = F.r_s32();
	}

	for (PStringMapIt s_it = strings.begin(); s_it != strings.end(); s_it++)
	{
		F.r_stringZ(s_it->second.val);
	}
}

void EParticleAction::Load2(CInifile& ini, const shared_str& sect)
{
	u32 ver 					= ini.r_u32(sect.c_str(), "version");
	actionName					= ini.r_string(sect.c_str(), "action_name");
	flags.assign				(ini.r_u32(sect.c_str(), "flags"));
	
	u32 counter					= 0;
	string256					buff;
	for (PFloatMapIt f_it=floats.begin(); f_it!=floats.end(); ++f_it,++counter)
	{
		xr_sprintf				(buff, sizeof(buff),"flt_%04d",counter);
		if(ver==0)
		{
			if(ini.line_exist(sect.c_str(), buff))
				f_it->second.val		= ini.r_float(sect.c_str(), buff);
		}else
		f_it->second.val		= ini.r_float(sect.c_str(), buff);
	}
	counter=0;
	for (PVectorMapIt v_it=vectors.begin(); v_it!=vectors.end(); ++v_it,++counter)
	{
		xr_sprintf				(buff, sizeof(buff),"vec_%04d",counter);
		v_it->second.val		= ini.r_fvector3	(sect.c_str(), buff);
	}

	counter=0;
	for (PDomainMapIt d_it=domains.begin();	d_it!=domains.end(); ++d_it,++counter)
	{
		xr_sprintf				(buff, sizeof(buff),"domain_%s_%04d", sect.c_str(), counter);
		d_it->second.Load2		(ini, buff);
	}

	counter=0;
	for (PBoolMapIt b_it=bools.begin(); b_it!=bools.end(); ++b_it,++counter)
	{
		xr_sprintf				(buff, sizeof(buff),"bool_%04d",counter);
		b_it->second.val		= ini.r_bool		(sect.c_str(), buff);
	}

	counter=0;
	for (PIntMapIt i_it=ints.begin(); i_it!=ints.end(); ++i_it,++counter)
	{
		xr_sprintf				(buff, sizeof(buff),"int_%04d",counter);
		i_it->second.val		= ini.r_s32		(sect.c_str(), buff);
	}

	counter=0;
	for (PStringMapIt s_it=strings.begin(); s_it!=strings.end(); ++s_it,++counter)
	{
		xr_sprintf				(buff, sizeof(buff),"string_%04d",counter);
		s_it->second.val		= ini.r_string		(sect.c_str(), buff);
	}

}
void 	EParticleAction::Save		(IWriter& F)
{
	F.w_u32			(PARTICLE_ACTION_VERSION_MAX);
	F.w_stringZ		(actionName);
	F.w_u32			(flags.get());
	for (PFloatMapIt 	f_it=floats.begin(); 	f_it!=floats.end(); 	f_it++)	F.w_float	(f_it->second.val);
	for (PVectorMapIt 	v_it=vectors.begin(); 	v_it!=vectors.end(); 	v_it++)	F.w_fvector3(v_it->second.val);
	for (PDomainMapIt 	d_it=domains.begin(); 	d_it!=domains.end(); 	d_it++)	d_it->second.Save	(F);
	for (PBoolMapIt 	b_it=bools.begin(); 	b_it!=bools.end(); 		b_it++)	F.w_u8		((u8)b_it->second.val);
	for (PIntMapIt 		i_it=ints.begin(); 		i_it!=ints.end(); 		i_it++)	F.w_s32		(i_it->second.val);
	for (PStringMapIt 	s_it=strings.begin(); 	s_it!=strings.end(); 	s_it++)	F.w_stringZ	(s_it->second.val);
}

void EParticleAction::Save2(CInifile& ini, const shared_str& sect)
{
	ini.w_u32			(sect.c_str(), "version", PARTICLE_ACTION_VERSION_MAX);
	ini.w_string		(sect.c_str(), "action_name",	actionName.c_str());
	ini.w_u32			(sect.c_str(), "flags",			flags.get());
	
	u32 counter			= 0;
	string256			buff;
	for (PFloatMapIt f_it=floats.begin(); f_it!=floats.end(); ++f_it,++counter)
	{
		xr_sprintf		(buff, sizeof(buff),"flt_%04d",counter);
		ini.w_float		(sect.c_str(), buff, f_it->second.val);
	}
	counter=0;
	for (PVectorMapIt v_it=vectors.begin(); v_it!=vectors.end(); ++v_it,++counter)
	{
		xr_sprintf		(buff, sizeof(buff),"vec_%04d",counter);
		ini.w_fvector3	(sect.c_str(), buff, v_it->second.val);
	}

	counter=0;
	for (PDomainMapIt d_it=domains.begin();	d_it!=domains.end(); ++d_it,++counter)
	{
		xr_sprintf		(buff, sizeof(buff),"domain_%s_%04d", sect.c_str(), counter);
		d_it->second.Save2(ini, buff);
	}

	counter=0;
	for (PBoolMapIt b_it=bools.begin(); b_it!=bools.end(); ++b_it,++counter)
	{
		xr_sprintf		(buff, sizeof(buff),"bool_%04d",counter);
		ini.w_bool		(sect.c_str(), buff, b_it->second.val);
	}

	counter=0;
	for (PIntMapIt i_it=ints.begin(); i_it!=ints.end(); ++i_it,++counter)
	{
		xr_sprintf		(buff, sizeof(buff),"int_%04d",counter);
		ini.w_s32		(sect.c_str(), buff, i_it->second.val);
	}

	counter=0;
	for (PStringMapIt s_it=strings.begin(); s_it!=strings.end(); ++s_it,++counter)
	{
		xr_sprintf		(buff, sizeof(buff),"string_%04d",counter);
		ini.w_string	(sect.c_str(), buff, s_it->second.val.c_str());
	}
}

void EParticleAction::FillPropInit(PropItemVec& items, LPCSTR pref)
{
	u32 clr				= flags.is(EParticleAction::flEnabled)?0xFF000000:0xFFC0C0C0;
	string128 buffer;
	sprintf(buffer, "%s (%s)", *actionType, *actionName);
	shared_str a_pref		= PrepareKey(pref,"Actions", buffer);

	//ButtonValue* B			= PHelper().CreateButton(items,a_pref,"Up,Down,Remove",ButtonValue::flFirstOnly); B->tag = (s_it-m_EActionList.begin());
	//B->Owner()->prop_color	= clr;
	//B->OnBtnClickEvent.bind	(this,&PS::CPEDef::OnActionEditClick);

	RTextValue* R;
	R=PHelper().CreateRText	(items,PrepareKey(a_pref.c_str(),"Name"),&actionName);
	R->OnAfterEditEvent.bind(parent,&PS::CPEDef::OnAfterActionNameEdit);
	R->Owner()->prop_color	= clr;
	FillProp	(items,a_pref.c_str(),clr);
}

void 	EParticleAction::FillProp	(PropItemVec& items, LPCSTR pref, u32 clr)
{
	PropValue* V=0;
	for (OrderVecIt o_it=orders.begin(); o_it!=orders.end(); o_it++)
	{
		LPCSTR name 				= o_it->name.c_str();
		switch (o_it->type){           
		case tpDomain:
			{
				domains[o_it->name].FillProp(items, PrepareKey(pref,name).c_str(),clr);
				break;
			}
		case tpVector:
			{ 
				PVector& vect = vectors[o_it->name];
				switch (vect.type){
				case PVector::vNum:
					{
						V=PHelper().CreateVector	(items,	PrepareKey(pref,name).c_str(), &vect.val, vect.mn, vect.mx, 0.001f, 3);            
						break;
					}
				case PVector::vAngle:
					{
						V=PHelper().CreateAngle3	(items,	PrepareKey(pref,name).c_str(), &vect.val, vect.mn, vect.mx, 0.001f, 3);            
						break;
					}
				case PVector::vColor:
					{
						V=PHelper().CreateVColor	(items,	PrepareKey(pref,name).c_str(), &vect.val);
						break;
					}
				}
				break;
			}
		case tpFloat:
			{
				PFloat& flt	= floats[o_it->name];
				V=PHelper().CreateFloat		(items,	PrepareKey(pref,name).c_str(), &flt.val, flt.mn, flt.mx, 0.001f, 3);
				break;
			}
		case tpInt:
			{
				PInt& el	= ints[o_it->name];
				V=PHelper().CreateS32			(items,	PrepareKey(pref,name).c_str(), &el.val, el.mn, el.mx);
				break;
			}
		case tpBool:
			{
				V=PHelper().CreateBool		(items,	PrepareKey(pref,name).c_str(), &bools[o_it->name].val);
				break;
			}
		case tpString:
			{
				if (o_it->string_type == smCustom)
				{
					V=PHelper().CreateRText(items, PrepareKey(pref,name).c_str(), &strings[o_it->name].val);
				}
				else
				{
					V=PHelper().CreateChoose(items, PrepareKey(pref,name).c_str(), &strings[o_it->name].val, o_it->string_type);
				}
			}
		}
		if (V) V->Owner()->prop_color	= clr;
	}
	V=PHelper().CreateFlag32			(items,	PrepareKey(pref,"Draw").c_str(), 			&flags, flDraw);
	V->Owner()->prop_color				= clr;
	V=PHelper().CreateFlag32			(items,	PrepareKey(pref,"Enabled").c_str(), 		&flags, flEnabled);
	V->Owner()->prop_color				= clr;
}
void EParticleAction::appendFloat	(LPCSTR name, float v, float mn, float mx)
{
	orders.push_back				(SOrder(tpFloat,name));
	floats[name]					= PFloat(v,mn,mx);
}
void EParticleAction::appendInt		(LPCSTR name, int v, int mn, int mx)
{
	orders.push_back				(SOrder(tpInt,name));
	ints[name]						= PInt(v,mn,mx);
}
void EParticleAction::appendVector	(LPCSTR name, PVector::EType type, float vx, float vy, float vz, float mn, float mx)
{
	orders.push_back				(SOrder(tpVector,name));
	vectors[name]					= PVector(type,Fvector().set(vx,vy,vz),mn,mx);
}
void EParticleAction::appendDomain	(LPCSTR name, PDomain v)
{
	orders.push_back				(SOrder(tpDomain,name));
	domains[name]					= v;
}

void EParticleAction::appendBool	(LPCSTR name, BOOL v)
{
	orders.push_back				(SOrder(tpBool,name));
	bools[name]						= PBool(v);
}

void EParticleAction::appendBool	(LPCSTR name, bool v)
{
	orders.push_back				(SOrder(tpBool,name));
	bools[name]						= PBool(v);
}

void EParticleAction::appendString(LPCSTR name, shared_str v, EChooseMode _string_type)
{
	orders.push_back				(SOrder(tpString,name, _string_type));
	strings[name]						= PString(v);
}

void EParticleAction::appendString(LPCSTR name, LPCSTR v, EChooseMode _string_type)
{
	orders.push_back				(SOrder(tpString,name, _string_type));
	strings[name]						= PString(v);
}

//------------------------------------------------------------------------------
#define EXPAND_DOMAIN(D)			D.type,\
									D.f[0], D.f[1], D.f[2],\
									D.f[3], D.f[4], D.f[5],\
									D.f[6], D.f[7], D.f[8]
									
EPAAvoid::EPAAvoid					():EParticleAction(PAPI::PAAvoidID)
{
	actionType						= "Avoid";
	actionName						= actionType;
	appendDomain					("Position",	PDomain(PDomain::vNum,TRUE,0x6096FF96));
	appendFloat						("Magnitude",	0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Epsilon",		0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Look Ahead",	0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool						("Allow Rotate",TRUE);
}
void	EPAAvoid::Compile			(IWriter& F)
{
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float look_ahead = _float("Look Ahead").val;
	pDomain D = pDomain(EXPAND_DOMAIN(_domain("Position")));
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PAAvoid 		S;
	S.type			= PAAvoidID;
	S.positionL		= D;
	S.position		= S.positionL;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.look_ahead	= look_ahead;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPABounce::EPABounce				():EParticleAction(PAPI::PABounceID)
{
	actionType						= "Bounce";
	actionName						= actionType;
	appendDomain					("Position",PDomain(PDomain::vNum,TRUE,0x6096FEEC));
	appendFloat						("Friction",0.5f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Resilience",0.1f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Cutoff",1.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool						("Allow Rotate",	TRUE);
}
void	EPABounce::Compile			(IWriter& F)
{
	float friction = _float("Friction").val;
	float resilience = _float("Resilience").val;
	float cutoff = _float("Cutoff").val;
	pDomain D = pDomain(EXPAND_DOMAIN(_domain("Position")));
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PABounce 		S;
	S.type			= PABounceID;
	S.positionL		= D;
	S.position		= S.positionL;
	S.oneMinusFriction = 1.0f - friction;
	S.resilience	= resilience;
	S.cutoffSqr		= _sqr(cutoff);
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPACopyVertexB::EPACopyVertexB  	():EParticleAction(PAPI::PACopyVertexBID)
{
	actionType						= "CopyVertexB";
	actionName						= actionType;
	appendBool						("Copy Position", TRUE);
}
void	EPACopyVertexB::Compile	   	(IWriter& F)
{
	BOOL copy_pos = _bool("Copy Position").val;
	PACopyVertexB 	S;
	S.type			= PACopyVertexBID;
	S.copy_pos		= copy_pos;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPADamping::EPADamping				():EParticleAction(PAPI::PADampingID)
{
	actionType						= "Damping";
	actionName						= actionType;
	appendVector					("Damping", PVector::vNum, 0.f,0.f,0.f);
	appendFloat						("V Low",0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("V High",P_MAXFLOAT, -P_MAXFLOAT, P_MAXFLOAT);
}
void	EPADamping::Compile			(IWriter& F)
{
	const Fvector& damping = _vector("Damping").val;
	float vlow = _float("V Low").val;
	float vhigh = _float("V High").val;
	PADamping 	S;
	S.type			= PADampingID;
	S.damping		= pVector(damping.x, damping.y, damping.z);
	S.vlowSqr		= _sqr(vlow);
	S.vhighSqr		= _sqr(vhigh);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPAExplosion::EPAExplosion			():EParticleAction(PAPI::PAExplosionID)
{
	actionType						= "Explosion";
	actionName						= actionType;
	appendVector					("Center",PVector::vNum, 0.f,0.f,0.f);
	appendFloat						("Velocity",		1.f, 	-P_MAXFLOAT, 	P_MAXFLOAT);
	appendFloat						("Magnitude",		2.f, 	-P_MAXFLOAT, 	P_MAXFLOAT);
	appendFloat						("Standart Dev",	3.f,  	EPS, 			P_MAXFLOAT);
	appendFloat						("Epsilon",			EPS_L, 	EPS, 			P_MAXFLOAT);
	appendFloat						("Age",				0.f, 	0.f, 			P_MAXFLOAT);
	appendBool						("Allow Rotate",	TRUE);
}
void	EPAExplosion::Compile	  	(IWriter& F)
{
	const Fvector& center = _vector("Center").val;
	float velocity = _float("Velocity").val;
	float magnitude = _float("Magnitude").val;
	float stdev = _float("Standart Dev").val;
	float epsilon = _float("Epsilon").val;
	float age = _float("Age").val;
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PAExplosion 	S;
	S.type			= PAExplosionID;
	S.centerL		= pVector(center.x, center.y, center.z);
	S.center		= S.centerL;
	S.velocity		= velocity;
	S.magnitude		= magnitude;
	S.stdev			= stdev;
	S.epsilon		= epsilon;
	S.age			= age;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	if(S.epsilon < 0.0f)
		S.epsilon 	= EPS_L;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPAFollow::EPAFollow				():EParticleAction(PAPI::PAFollowID)
{
	actionType						= "Follow";
	actionName						= actionType;
	appendFloat						("Magnitude",0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Epsilon",EPS_L, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Max Radius",P_MAXFLOAT, -P_MAXFLOAT, P_MAXFLOAT);
}
void	EPAFollow::Compile			(IWriter& F)
{
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	PAFollow 	S;
	S.type			= PAFollowID;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPAGravitate::EPAGravitate			():EParticleAction(PAPI::PAGravitateID)
{
	actionType						= "Gravitate";
	actionName						= actionType;
	appendFloat						("Magnitude",1.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Epsilon",0.001f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Max Radius",10.0f, -P_MAXFLOAT, P_MAXFLOAT);
}
void	EPAGravitate::Compile	   	(IWriter& F)
{
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	PAGravitate 	S;
	S.type			= PAGravitateID;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPAGravity::EPAGravity				():EParticleAction(PAPI::PAGravityID)
{
	actionType						= "Gravity";
	actionName						= actionType;
	appendVector					("Direction",		PVector::vNum, 0.f,-9.8f,0.f);
	appendBool						("Allow Rotate",	TRUE);
}
void	EPAGravity::Compile			(IWriter& F)
{
	const Fvector& dir = _vector("Direction").val;
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PAGravity 	S;
	S.type			= PAGravityID;
	S.directionL	= pVector(dir.x, dir.y, dir.z);
	S.direction		= S.directionL;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPAJet::EPAJet						():EParticleAction(PAPI::PAJetID)
{
	actionType						= "Jet";
	actionName						= actionType;
	appendDomain					("Accelerate",PDomain(PDomain::vNum,FALSE));
	appendVector					("Center",PVector::vNum, 0.f,0.f,0.f);
	appendFloat						("Magnitude",0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Epsilon",EPS_L, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Max Radius",P_MAXFLOAT, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool						("Allow Rotate",	TRUE);
}
void	EPAJet::Compile				(IWriter& F)
{
	pDomain acc = pDomain(EXPAND_DOMAIN(_domain("Accelerate")));
	const Fvector& center = _vector("Center").val;
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PAJet 	S;
	S.type			= PAJetID;
	S.centerL		= pVector(center.x, center.y, center.z);
	S.center		= S.centerL;
	S.accL			= acc;
	S.acc			= S.accL;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

void	EPAJet::Render				(const Fmatrix& parent)
{
	EParticleAction::Render			(parent);
	RCache.set_xform_world			(parent);
	EDevice->SetShader				(EDevice->m_WireShader);
	DU_impl.DrawCross				(_vector("Center").val, 0.05f,0.05f,0.05f, 0.05f,0.05f,0.05f, 0x600000ff);
}

EPAKillOld::EPAKillOld				():EParticleAction(PAPI::PAKillOldID)
{
	actionType						= "KillOld";
	actionName						= actionType;
	appendFloat						("Age Limit",		5.f, 0.0f, P_MAXFLOAT);
	appendBool						("Kill Less Than",	FALSE);
}
void	EPAKillOld::Compile			(IWriter& F)
{
	float age_limit = _float("Age Limit").val;
	BOOL kill_less_than = _bool("Kill Less Than").val;
	PAKillOld 	S;
	S.type			= PAKillOldID;
	S.age_limit		= age_limit;
	S.kill_less_than = kill_less_than;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPAMatchVelocity::EPAMatchVelocity	():EParticleAction(PAPI::PAMatchVelocityID)
{
	actionType						= "MatchVelocity";
	actionName						= actionType;
	appendFloat						("Magnitude",0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Epsilon",EPS_L, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Max Radius",P_MAXFLOAT, -P_MAXFLOAT, P_MAXFLOAT);
}
void	EPAMatchVelocity::Compile 	(IWriter& F)
{
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	PAMatchVelocity 	S;
	S.type			= PAMatchVelocityID;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPAMove::EPAMove					():EParticleAction(PAPI::PAMoveID)
{
	actionType						= "Move";
	actionName						= actionType;
}
void	EPAMove::Compile			(IWriter& F)
{
	PAMove 		S;
	S.type			= PAMoveID;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPAOrbitLine::EPAOrbitLine			():EParticleAction(PAPI::PAOrbitLineID)
{
	actionType						= "OrbitLine";
	actionName						= actionType;
	appendVector					("Position",		PVector::vNum, 0.f,0.f,0.f);
	appendVector					("Axis",			PVector::vNum, 0.f,0.f,0.f);
	appendFloat						("Magnitude",		1.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Epsilon",			EPS_L, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Max Radius",		P_MAXFLOAT, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool						("Allow Rotate",	TRUE);
}
void	EPAOrbitLine::Compile	 	(IWriter& F)
{
	const Fvector& p = _vector("Position").val;
	const Fvector& axis = _vector("Axis").val;
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PAOrbitLine 	S;
	S.type			= PAOrbitLineID;
	S.pL			= pVector(p.x, p.y, p.z);
	S.p			= S.pL;
	S.axisL		= pVector(axis.x, axis.y, axis.z);
	S.axisL.normalize_safe();
	S.axis			= S.axisL;
	S.magnitude	= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

void	EPAOrbitLine::Render		(const Fmatrix& parent)
{
	EParticleAction::Render			(parent);
	RCache.set_xform_world			(parent);
	EDevice->SetShader				(EDevice->m_WireShader);
	Fvector p0,p1;
	p0								= _vector("Position").val;
	p1.add							(p0,_vector("Axis").val);
	DU_impl.DrawCross					(p0, 0.05f,0.05f,0.05f, 0.05f,0.05f,0.05f, 0x6000ff00);
	DU_impl.DrawCross					(p1, 0.05f,0.05f,0.05f, 0.05f,0.05f,0.05f, 0x6000ff00);
	DU_impl.DrawLine 					(p0, p1, 0x6000ff00);
}

EPAOrbitPoint::EPAOrbitPoint		():EParticleAction(PAPI::PAOrbitPointID)
{
	actionType						= "OrbitPoint";
	actionName						= actionType;
	appendVector					("Center",			PVector::vNum, 0.f,0.f,0.f);
	appendFloat						("Magnitude",		400.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Epsilon",			0.1f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Max Radius",		100.0f, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool						("Allow Rotate",	TRUE);
}
void	EPAOrbitPoint::Compile	   	(IWriter& F)
{
	const Fvector& center = _vector("Center").val;
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PAOrbitPoint 	S;
	S.type			= PAOrbitPointID;
	S.centerL		= pVector(center.x, center.y, center.z);
	S.center		= S.centerL;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

void	EPAOrbitPoint::Render		(const Fmatrix& parent)
{
	EParticleAction::Render			(parent);
	RCache.set_xform_world			(parent);
	EDevice->SetShader				(EDevice->m_WireShader);
	DU_impl.DrawCross					(_vector("Center").val, 0.05f,0.05f,0.05f, 0.05f,0.05f,0.05f, 0x6000ff00);
}

EPARandomAccel::EPARandomAccel		():EParticleAction(PAPI::PARandomAccelID)
{
	actionType						= "RandomAccel";
	actionName						= actionType;
	appendDomain					("Accelerate",PDomain(PDomain::vNum,FALSE));
	appendBool						("Allow Rotate",	TRUE);
}
void	EPARandomAccel::Compile	   	(IWriter& F)
{
	pDomain D = pDomain(EXPAND_DOMAIN(_domain("Accelerate")));
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PARandomAccel 	S;
	S.type			= PARandomAccelID;
	S.gen_accL		= D;
	S.gen_acc		= S.gen_accL;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPARandomDisplace::EPARandomDisplace():EParticleAction(PAPI::PARandomDisplaceID)
{
	actionType						= "RandomDisplace";
	actionName						= actionType;
	appendDomain					("Displace",PDomain(PDomain::vNum,FALSE));
	appendBool						("Allow Rotate",	TRUE);
}
void	EPARandomDisplace::Compile 	(IWriter& F)
{
	pDomain D = pDomain(EXPAND_DOMAIN(_domain("Displace")));
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PARandomDisplace 	S;
	S.type			= PARandomDisplaceID;
	S.gen_dispL		= D;
	S.gen_disp		= S.gen_dispL;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPARandomVelocity::EPARandomVelocity():EParticleAction(PAPI::PARandomVelocityID)
{
	actionType						= "RandomVelocity";
	actionName						= actionType;
	appendDomain					("Velocity",PDomain(PDomain::vNum,FALSE));
	appendBool						("Allow Rotate",	TRUE);
}
void	EPARandomVelocity::Compile 	(IWriter& F)
{
	pDomain D = pDomain(EXPAND_DOMAIN(_domain("Velocity")));
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PARandomVelocity 	S;
	S.type			= PARandomVelocityID;
	S.gen_velL		= D;
	S.gen_vel		= S.gen_velL;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPARestore::EPARestore				():EParticleAction(PAPI::PARestoreID)
{
	actionType						= "Restore";
	actionName						= actionType;
	appendFloat						("Time",			0.f, 0.0f, P_MAXFLOAT);
}
void	EPARestore::Compile			(IWriter& F)
{
	float time_left = _float("Time").val;
	PARestore 	S;
	S.type			= PARestoreID;
	S.time_left		= time_left;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPAScatter::EPAScatter				():EParticleAction(PAPI::PAScatterID)
{
	actionType						= "Scatter";
	actionName						= actionType;
	appendVector					("Center",PVector::vNum, 0.f,0.f,0.f);
	appendFloat						("Magnitude",0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Epsilon",EPS_L, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Max Radius",P_MAXFLOAT, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool						("Allow Rotate",	TRUE);
}
void	EPAScatter::Compile	 		(IWriter& F)
{
	const Fvector& center = _vector("Center").val;
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PAScatter 		S;
	S.type			= PAScatterID;
	S.centerL		= pVector(center.x, center.y, center.z);
	S.center		= S.centerL;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

void	EPAScatter::Render	   		(const Fmatrix& parent)
{
	EParticleAction::Render			(parent);
	RCache.set_xform_world			(parent);
	EDevice->SetShader				(EDevice->m_WireShader);
	DU_impl.DrawCross					(_vector("Center").val, 0.05f,0.05f,0.05f, 0.05f,0.05f,0.05f, 0x600000ff);
}

EPASink::EPASink					():EParticleAction(PAPI::PASinkID)
{
	actionType						= "Sink";
	actionName						= actionType;
	appendBool						("Kill Inside",		TRUE);
	appendDomain					("Domain",			PDomain(PDomain::vNum,TRUE,0x60ff0000));
	appendBool						("Allow Rotate",	TRUE);
}
void	EPASink::Compile			(IWriter& F)
{
	BOOL kill_inside = _bool("Kill Inside").val;
	pDomain D = pDomain(EXPAND_DOMAIN(_domain("Domain")));
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PASink 	S;
	S.type			= PASinkID;
	S.kill_inside	= kill_inside;
	S.positionL		= D;
	S.position		= S.positionL;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPASinkVelocity::EPASinkVelocity	():EParticleAction(PAPI::PASinkVelocityID)
{
	actionType						= "SinkVelocity";
	actionName						= actionType;
	appendBool						("Kill Inside",		TRUE);
	appendDomain					("Domain",PDomain(PDomain::vNum,FALSE));
	appendBool						("Allow Rotate",	TRUE);
}
void	EPASinkVelocity::Compile   	(IWriter& F)
{
	BOOL kill_inside = _bool("Kill Inside").val;
	pDomain D = pDomain(EXPAND_DOMAIN(_domain("Domain")));
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PASinkVelocity 	S;
	S.type			= PASinkVelocityID;
	S.kill_inside	= kill_inside;
	S.velocityL		= D;
	S.velocity		= S.velocityL;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPASource::EPASource				():EParticleAction(PAPI::PASourceID)
{
	actionType						= "Source";
	actionName						= actionType;
	appendFloat						("Rate",			100.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendDomain					("Domain",			PDomain(PDomain::vNum,TRUE,0x60FFEBAA));
	appendDomain					("Velocity",		PDomain(PDomain::vNum,FALSE));
	appendDomain					("Rotation",		PDomain(PDomain::vAngle,FALSE));
	appendDomain					("Size",			PDomain(PDomain::vNum,FALSE));
	appendBool						("Single Size",		FALSE);
	appendDomain					("Color",			PDomain(PDomain::vColor, FALSE, 0x00000000, PAPI::PDPoint,1.f,1.f,1.f,1.f,1.f,1.f,1.f,1.f,1.f));
	appendFloat						("Color\\Alpha",	0.f, 0.f, 1.f);
	appendFloat						("Starting Age",	0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Age Sigma",		0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Parent Motion",	0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool						("Allow Rotate",	FALSE);
}
void	EPASource::Compile			(IWriter& F)
{
	float particle_rate = _float("Rate").val;
	pDomain pos = pDomain(EXPAND_DOMAIN(_domain("Domain")));
	pDomain vel = pDomain(EXPAND_DOMAIN(_domain("Velocity")));
	pDomain rot = pDomain(EXPAND_DOMAIN(_domain("Rotation")));
	pDomain size = pDomain(EXPAND_DOMAIN(_domain("Size")));
	BOOL single_size = _bool("Single Size").val;
	pDomain color = pDomain(EXPAND_DOMAIN(_domain("Color")));
	float alpha = _float("Color\\Alpha").val;
	float age = _float("Starting Age").val;
	float age_sigma = _float("Age Sigma").val;
	float parent_motion = _float("Parent Motion").val;
	BOOL allow_rotate = _bool("Allow Rotate").val;
	PASource 	S;
	S.type			= PASourceID;
	S.particle_rate = particle_rate;
	S.positionL		= pos;
	S.position		= S.positionL;
	S.velocityL		= vel;
	S.velocity		= S.velocityL;
	S.size			= size;
	S.rot			= rot;
	S.color			= color;
	S.alpha			= alpha;
	S.age			= age;
	S.age_sigma		= age_sigma;
	S.m_Flags.assign((single_size?PASource::flSingleSize:0)|PASource::flVertexB_tracks);
	S.parent_vel	= pVector(0,0,0);
	S.parent_motion	= parent_motion;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPASpeedLimit::EPASpeedLimit		():EParticleAction(PAPI::PASpeedLimitID)
{
	actionType						= "SpeedLimit";
	actionName						= actionType;
	appendFloat						("Min Speed",			-1.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Max Speed",			15.0f, -P_MAXFLOAT, P_MAXFLOAT);
}
void	EPASpeedLimit::Compile	 	(IWriter& F)
{
	float min_speed = _float("Min Speed").val;
	float max_speed = _float("Max Speed").val;
	PASpeedLimit 	S;
	S.type			= PASpeedLimitID;
	S.min_speed = min_speed;
	S.max_speed = max_speed;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPATargetColor::EPATargetColor		():EParticleAction(PAPI::PATargetColorID)
{
	actionType						= "TargetColor";
	actionName						= actionType;
	appendVector					("Color",			PVector::vColor, 1.f,1.f,1.f, 0.f,1.f);
	appendFloat						("Alpha",			1.f, 0.0f,1.0f);
	appendFloat						("Scale",			1.f, 0.01f, P_MAXFLOAT);     
	appendFloat						("TimeFrom",		0.0f, 0.0f, 1.0f);     
	appendFloat						("TimeTo",			1.0f, 0.0f, 1.0f);     
}

void EPATargetColor::Load(IReader& F)
{
	u32 vers = F.r_u32();
	R_ASSERT(vers <= PARTICLE_ACTION_VERSION_MAX && vers >= PARTICLE_ACTION_VERSION_MIN);

	F.r_stringZ(actionName);
	flags.assign(F.r_u32());

	if (vers == 0)
	{
		constexpr int Count = 2;
		int Iter = 0;
		for (PFloatMapIt f_it = floats.begin(); f_it != floats.end(); f_it++)
		{
			if (Iter >= Count)
				break;

			f_it->second.val = F.r_float();

			Iter++;
		}
	}
	else
	{
		for (PFloatMapIt f_it = floats.begin(); f_it != floats.end(); f_it++)
			f_it->second.val = F.r_float();
	}

	for (PVectorMapIt v_it = vectors.begin(); v_it != vectors.end(); v_it++)	F.r_fvector3(v_it->second.val);
	for (PDomainMapIt d_it = domains.begin(); d_it != domains.end(); d_it++)	d_it->second.Load(F);
	for (PBoolMapIt b_it = bools.begin(); b_it != bools.end(); b_it++)	b_it->second.val = F.r_u8();
	for (PIntMapIt i_it = ints.begin(); i_it != ints.end(); i_it++)	i_it->second.val = F.r_s32();
}

void EPATargetColor::Compile(IWriter& F)
{
	const Fvector& color = _vector("Color").val;
	float alpha = _float("Alpha").val;
	float scale = _float("Scale").val;
	float time_from = _float("TimeFrom").val;
	float time_to = _float("TimeTo").val;
	PATargetColor 	S;
	S.type			= PATargetColorID;
	S.color = pVector(color.x, color.y, color.z);
	S.alpha = alpha;
	S.scale = scale;
	S.timeFrom = time_from;
	S.timeTo = time_to;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPATargetSize::EPATargetSize		():EParticleAction(PAPI::PATargetSizeID)
{
	actionType						= "TargetSize";
	actionName						= actionType;
	appendVector					("Size",			PVector::vNum, 2.f,2.f,0.001f, EPS_L);
	appendVector					("Scale",			PVector::vNum, 1.f,1.f,0.f);
}
void	EPATargetSize::Compile	  	(IWriter& F)
{
	const Fvector& size = _vector("Size").val;
	const Fvector& scale = _vector("Scale").val;
	PATargetSize 	S;
	S.type			= PATargetSizeID;
	S.size = pVector(size.x, size.y, size.z);
	S.scale = pVector(scale.x, scale.y, scale.z);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPATargetRotate::EPATargetRotate	():EParticleAction(PAPI::PATargetRotateID)
{
	actionType						= "TargetRotate";
	actionName						= actionType;
	appendVector					("Rotation",		PVector::vAngle, 0.f,0.f,0.f);
	appendFloat						("Scale",			1.f, 0.0f, P_MAXFLOAT);
}
void	EPATargetRotate::Compile   	(IWriter& F)
{
	const Fvector& rot = _vector("Rotation").val;
	float scale = _float("Scale").val;
	PATargetRotate 	S;
	S.type			= PATargetRotateID;
	S.rot = pVector(rot.x, rot.y, rot.z);
	S.scale = scale;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPATargetVelocity::EPATargetVelocity():EParticleAction(PAPI::PATargetVelocityID)
{
	actionType						= "TargetVelocity";
	actionName						= actionType;
	appendVector					("Velocity",		PVector::vNum, 0.f,0.f,0.f);
	appendFloat						("Scale",			1.f, 0.0f, P_MAXFLOAT);
	appendBool						("Allow Rotate",	TRUE);
}
void	EPATargetVelocity::Compile	(IWriter& F)
{
	PATargetVelocity 	S;
	S.type			= PATargetVelocityID;
	
	S.velocityL		= _vector("Velocity").val;
	S.velocity		= S.velocityL;
	S.scale			= _float("Scale").val;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,_bool("Allow Rotate").val);
	
	F.w_u32			(S.type);
	S.Save			(F);
}

EPAVortex::EPAVortex				():EParticleAction(PAPI::PAVortexID)
{
	actionType						= "Vortex";
	actionName						= actionType;
	appendVector					("Center",			PVector::vNum, 0.f,0.f,0.f);
	appendVector					("Axis",			PVector::vNum, 0.f,1.f,0.f);
	appendFloat						("Magnitude",		1.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Epsilon",			EPS_L, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Max Radius",		1.0f, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool						("Allow Rotate",	TRUE);
}
void	EPAVortex::Compile			(IWriter& F)
{
	PAVortex 	S;
	S.type			= PAVortexID;
	
	S.centerL		= _vector("Center").val;
	S.center		= S.centerL;
	S.axisL			= _vector("Axis").val;
	S.axisL.normalize_safe();
	S.axis			= S.axisL;
	S.magnitude		= _float("Magnitude").val;
	S.epsilon		= _float("Epsilon").val;
	S.max_radius	= _float("Max Radius").val;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,_bool("Allow Rotate").val);
	
	F.w_u32			(S.type);
	S.Save			(F);
}
void	EPAVortex::Render			(const Fmatrix& parent)
{
	EParticleAction::Render			(parent);
}

EPATurbulence::EPATurbulence		():EParticleAction(PAPI::PATurbulenceID)
{
	actionType						= "Turbulence";
	actionName						= actionType;
	appendFloat						("Frequency",		2.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendInt						("Octaves",			1,1);
	appendFloat						("Magnitude",		10.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Delta",			0.01f, -P_MAXFLOAT, P_MAXFLOAT);
	appendVector					("Movement",		PVector::vNum, 1,1,1);
// -
	nval=0; 
	age								= 0.f;
}

static const int detail=16;

void	EPATurbulence::Compile		(IWriter& F)
{

	PATurbulence 	S;
	S.type			= PATurbulenceID;
	
	S.frequency		= _float("Frequency").val;
	S.octaves		= _int("Octaves").val;
	S.magnitude		= _float("Magnitude").val;
	S.epsilon		= _float("Delta").val;
	S.offset.set	(_vector("Movement").val);

	S.age			= 0.f;
	
	F.w_u32			(S.type);
	S.Save			(F);
	
	if(nval != 0){
		for(int i = 0; i < detail; i++){
			for(int j = 0; j < detail; j++)
				delete [] nval[i][j];
		}
		for(int i = 0; i < detail; i++)
			delete [] nval[i];
		delete [] nval;
		nval = 0;
	}
}

struct Stp
{
	Fvector p;
	Fcolor 	c;
	Stp(const Fvector &_p, const Fcolor &_c):p(_p),c(_c){}
};

using StpVec = xr_vector<Stp>;
using StpVecIt = StpVec::iterator;

static StpVec pts;
IC bool sort_tp_pred(const Stp& x, const Stp& y)
{	
	float a = EDevice->vCameraPosition.distance_to_sqr(x.p);
	float b = EDevice->vCameraPosition.distance_to_sqr(y.p);
	return a>b;
}

void EPATurbulence::Render(const Fmatrix& parent)
{
	EParticleAction::Render			(parent);
	Fvector	vec;
	int		i, j, k;
	int		kb;
	int		ke;
	Fcolor	clr;

	float 	draw_area 	= 1;
	float 	csz 		= ((draw_area*2.f)/detail)/2.f;
	bool 	draw_p=true,draw_n=true;
	pts.clear();
	
	age		+= EDevice->fTimeDelta;
	// fill 
	if (nval == 0){
		nval = new float**[detail];
		for(i = 0; i < detail; i++)
		{
			nval[i] = new float*[detail];
			for(j = 0; j < detail; j++)
				nval[i][j] = new float[detail];
		}
	}
	{
		for (i = 0; i < detail; i++)
		{
			for (j = 0; j < detail; j++)
			{
				for (k = 0; k <  detail; k++)
				{
					Fvector& offs	= _vector("Movement").val;
					vec[0] =	(((float)i/(float)detail)-0.5)*2.0*(float)draw_area + offs.x*age;
					vec[1] =	(((float)j/(float)detail)-0.5)*2.0*(float)draw_area + offs.y*age;
					vec[2] =	(((float)k/(float)detail)-0.5)*2.0*(float)draw_area + offs.z*age;
					nval[i][j][k] = fractalsum3(vec, _float("Frequency").val, _int("Octaves").val);
				}
			}
		}
	}
	
	for (i = 0; i < detail; i++){
		for (j = 0; j < detail; j++){
//			if(1){
				kb = 0;
				ke = detail;
//      	}else{
//				kb = detail/2;
//				ke = detail/2+1;
//			}
			for (k = kb; k < ke; k++){
				vec[0] = (((float)i/(float)detail)-0.5)*2.0*draw_area;
				vec[1] = (((float)j/(float)detail)-0.5)*2.0*draw_area;
				vec[2] = (((float)k/(float)detail)-0.5)*2.0*draw_area;
					
				clr.set(0,0,0,0);
				if(draw_p && draw_n){
					if(nval[i][j][k] > 0.0){
						clr.r = nval[i][j][k];
						clr.a = nval[i][j][k];
					}else{
						clr.b = fabs(nval[i][j][k]);
						clr.a = fabs(nval[i][j][k]);
					}
				}else if (draw_p){
					if(nval[i][j][k] > 0.0)
						clr.set(nval[i][j][k]);
				}else if (draw_n){
					if(nval[i][j][k] < 0.0)
						clr.set(fabs(nval[i][j][k]));
				}
				pts.push_back(Stp(vec,clr));
			}
		}
	}
	std::sort(pts.begin(),pts.end(),sort_tp_pred);
	EDevice->SetShader(EDevice->m_SelectionShader);
	RCache.set_xform_world(Fidentity);
	for (StpVecIt it=pts.begin(); it!=pts.end(); it++)
		DU_impl.DrawCross	(it->p, csz,csz,csz, csz,csz,csz, it->c.get(), false);
}

EPABindColorValue::EPABindColorValue(): EParticleAction(PAPI::PABindColorValueID)
{
	actionType = "BindColorRGB";
	actionName = actionType;
	appendVector("InitialValue", PVector::vColor, 1.0f, 1.0f, 1.0f, 0.0f, 1.0f);
}

void EPABindColorValue::Compile(IWriter& F)
{
	PABindColorValue S;
	S.type = PABindColorValueID;
	S.BindValue.set(_vector("InitialValue").val);

	F.w_u32(S.type);
	S.Save(F);
}

EPABindColorAlpha::EPABindColorAlpha(): EParticleAction(PAPI::PABindColorAlphaID)
{
	actionType = "BindColorAlpha";
	actionName = actionType;
	appendFloat("InitialValue", 1.0f, 0.0f, 1.0f);
}

void EPABindColorAlpha::Compile(IWriter& F)
{
	PABindColorAlpha S;
	S.type = PABindColorAlphaID;
	S.BindValue = _float("InitialValue").val;

	F.w_u32(S.type);
	S.Save(F);
}

EPABindSizeValue::EPABindSizeValue(): EParticleAction(PAPI::PABindSizeValueID)
{
	actionType = "BindSize";
	actionName = actionType;
	appendVector("InitialValue", PVector::vNum, 1.0f, 1.0f, 1.0f, 0.0f, FLT_MAX);
	appendVector("Pivot", PVector::vNum, 0.0f, 0.0f, 0.0f, FLT_MIN, FLT_MAX);
}

void EPABindSizeValue::Compile(IWriter& F)
{
	PABindSizeValue S;
	S.type = PABindSizeValueID;
	S.BindValue.set(_vector("InitialValue").val);
	S.Pivot.set(_vector("Pivot").val);

	F.w_u32(S.type);
	S.Save(F);
}

EPABindRotateValue::EPABindRotateValue(): EParticleAction(PAPI::PABindRotationValueID)
{
	actionType = "BindRotation";
	actionName = actionType;
	appendVector("InitialValue", PVector::vNum, 1.0f, 1.0f, 1.0f, 0.0f, FLT_MAX);
}

void EPABindRotateValue::Compile(IWriter& F)
{
	PABindRotationValue S;
	S.type = PABindRotationValueID;
	S.BindValue.set(_vector("InitialValue").val);

	F.w_u32(S.type);
	S.Save(F);
}

EPABindVelocityValue::EPABindVelocityValue(): EParticleAction(PAPI::PABindVelocityValueID)
{
	actionType = "BindVelocity";
	actionName = actionType;
	appendVector("InitialValue", PVector::vNum, 1.0f, 1.0f, 1.0f, 0.0f, FLT_MAX);
}

void EPABindVelocityValue::Compile(IWriter& F)
{
	PABindVelocityValue S;
	S.type      = PABindVelocityValueID;
	S.BindValue.set(_vector("InitialValue").val);

	F.w_u32(S.type);
	S.Save(F);
}

EPAColorAnimator::EPAColorAnimator(): EParticleAction(PAPI::PAColorAnimatorID)
{
	actionType = "ColorAnimator";
	actionName = actionType;
	appendString("Animator", "", smPAC);
	appendBool("Looped", false);
	appendBool("Reverse", false);     
}

void EPAColorAnimator::Compile(IWriter& F)
{
	PAColorAnimator S;
	S.type = PAColorAnimatorID;
	S.Animator = _string("Animator").val;
	S.Looped = _bool("Looped").val;
	S.Reverse = _bool("Reverse").val;

	F.w_u32(S.type);
	S.Save(F);
}

EPASizeAnimator::EPASizeAnimator(): EParticleAction(PAPI::PASizeAnimatorID)
{
	actionType = "SizeAnimator";
	actionName = actionType;
	appendString("Animator", "", smPAC);
	appendBool("Looped", false);
	appendBool("Reverse", false);    
}

void EPASizeAnimator::Compile(IWriter& F)
{
	PASizeAnimator S;
	S.type = PASizeAnimatorID;
	S.Animator = _string("Animator").val;
	S.Looped = _bool("Looped").val;
	S.Reverse = _bool("Reverse").val;

	F.w_u32(S.type);
	S.Save(F);
}

EPAVelocityAnimator::EPAVelocityAnimator(): EParticleAction(PAPI::PAVelocityAnimatorID)
{
	actionType = "VelocityAnimator";
	actionName = actionType;
	appendString("Animator", "", smPAC);
	appendBool("Looped", false);
	appendBool("Reverse", false);
}

void EPAVelocityAnimator::Compile(IWriter& F)
{
	PAVelocityAnimator S;
	S.type = PAVelocityAnimatorID;
	S.Animator = _string("Animator").val;
	S.Looped = _bool("Looped").val;
	S.Reverse = _bool("Reverse").val;

	F.w_u32(S.type);
	S.Save(F);
}

EPAVelocityRotationAnimator::EPAVelocityRotationAnimator(): EParticleAction(PAPI::PAVelocityRotationAnimatorID)
{
	actionType = "VelocityRotationAnimator";
	actionName = actionType;
	appendString("Animator", "", smPAC);
	appendBool("Looped", false);
	appendBool("Reverse", false);
}

void EPAVelocityRotationAnimator::Compile(IWriter& F)
{
	PAVelocityAnimator S;
	S.type = PAVelocityAnimatorID;
	S.Animator = _string("Animator").val;
	S.Looped = _bool("Looped").val;
	S.Reverse = _bool("Reverse").val;

	F.w_u32(S.type);
	S.Save(F);
}

PEd::ListTypeBase PEd::operator|(PEd::LisType lis, PEd::LisType rhs)
{
	return ListTypeBase(lis) | ListTypeBase(rhs);
}

