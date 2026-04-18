//---------------------------------------------------------------------------
#include "stdafx.h"

#include "ParticleEffectActions.h"
#include "../particle_actions_collection.h"
#include "../noise.h"

using namespace PAPI; 
using namespace EPALegacy;

EParticleAction* EPACreator::pCreateEActionImpl(PAPI::PActionEnum type)
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

void EParticleAction::Load(IReader& F)
{
	Version = F.r_enum<EVersion>();
	F.r_stringZ(actionName);
	flags.assign(F.r_u32());

	if (Version <= EVersion::Original)
	{
		// read non-chunked action from original

		xr_vector<AnsiString> TempFloatVec;
		xr_vector<AnsiString> TempVectorVec;
		xr_vector<AnsiString> TempDomainVec;
		xr_vector<AnsiString> TempBoolVec;
		xr_vector<AnsiString> TempIntVec;
		xr_vector<AnsiString> TempStringVec;
		
		for (auto& elem : orders)
		{
			if (elem.min_version > EVersion::Original)
			{
				continue;
			}
			switch (elem.type)
			{
			case tpDomain:
				{
					TempDomainVec.push_back(elem.name);
					break;
				}
			case tpVector:
				{
					TempVectorVec.push_back(elem.name);
					break;
				}
			case tpFloat:
				{
					TempFloatVec.push_back(elem.name);
					break;
				}
			case tpBool:
				{
					TempBoolVec.push_back(elem.name);
					break;
				}
			case tpInt:
				{
					TempIntVec.push_back(elem.name);
					break;
				}
			case tpString:
				{
					TempStringVec.push_back(elem.name);
					break;
				}
			}
		}

		std::ranges::sort(TempFloatVec);
		std::ranges::sort(TempVectorVec);
		std::ranges::sort(TempDomainVec);
		std::ranges::sort(TempBoolVec);
		std::ranges::sort(TempIntVec);
		std::ranges::sort(TempStringVec);

		for (auto& elem : TempFloatVec)
		{
			floats[elem].val = F.r_float();
		}
		for (auto& elem : TempVectorVec)
		{
			F.r_fvector3(vectors[elem].val);
		}
		for (auto& elem : TempDomainVec)
		{
			domains[elem].Load(F);
		}
		for (auto& elem : TempBoolVec)
		{
			bools[elem].val = F.r_u8();
		}
		for (auto& elem : TempIntVec)
		{
			ints[elem].val = F.r_s32();
		}
		for (auto& elem : TempStringVec)
		{
			F.r_stringZ(strings[elem].val);
		}
		
	} else
	{
		constexpr u32 DataChunksID = 0;
		R_ASSERT(F.r_u32() == DataChunksID);
		auto ChunkSize = F.r_u32();
		auto ActionsChunk = IReader(F.pointer(), ChunkSize);
		auto BoolStream = ActionsChunk.open_chunk(tpBool);
		auto DomainStream = ActionsChunk.open_chunk(tpDomain);
		auto VectorStream = ActionsChunk.open_chunk(tpVector);
		auto FloatStream = ActionsChunk.open_chunk(tpFloat);
		auto IntStream = ActionsChunk.open_chunk(tpInt);
		auto StringStream = ActionsChunk.open_chunk(tpString);
		auto EnumStream = ActionsChunk.open_chunk(tpEnum);

		for (auto& elem : orders)
		{
			switch (elem.type)
			{
			case tpDomain:
				{
					domains[elem.name].Load(*DomainStream);
					break;
				}
			case tpVector:
				{
					VectorStream->r_fvector3(vectors[elem.name].val);
					break;
				}
			case tpFloat:
				{
					floats[elem.name].val = FloatStream->r_float();
					break;
				}
			case tpBool:
				{
					bools[elem.name].val = BoolStream->r_u8();
					break;
				}
			case tpInt:
				{
					ints[elem.name].val = IntStream->r_s32();
					break;
				}
			case tpString:
				{
					StringStream->r_stringZ(strings[elem.name].val);
					break;
				}
			case tpEnum:
				{
					auto& CurEnum = enums[elem.name];
					switch (CurEnum.EnumSize)
					{
					case 1:
						{
							CurEnum.value = EnumStream->r_u8();
							break;
						}
					case 2:
						{
							CurEnum.value = EnumStream->r_u16();
							break;
						}
					case 4:
						{
							CurEnum.value = EnumStream->r_u32();
							break;
						}
					case 8:
						{
							CurEnum.value = EnumStream->r_u64();
							break;
						}
					default:
						{
							FATAL("Invalid enum size");
						}
					}
				}
			}
		}
	}
}

void EParticleAction::Load2(CInifile& ini, const shared_str& sect)
{
	Version = ini.r_enum<EVersion>(sect.c_str(), "version");
	actionName = ini.r_string(sect.c_str(), "action_name");
	flags.assign(ini.r_u32(sect.c_str(), "flags"));

	if (Version <= EVersion::Original)
	{
		u32 counter					= 0;
		string256					buff;
		for (auto& it : floats)
		{
			bool Skip = false;
			for (const auto& elem : orders)
			{
				if (it.first == elem.name && elem.min_version > Version)
				{
					Skip = true;
					break;
				}
			}
			if (Skip)
			{
				continue;
			}
			xr_sprintf				(buff, sizeof(buff),"flt_%04d",counter++);
			if(Version==EVersion::Old)
			{
				if(ini.line_exist(sect.c_str(), buff))
				{
					it.second.val		= ini.r_float(sect.c_str(), buff);
				}
			}else
			{
				it.second.val		= ini.r_float(sect.c_str(), buff);
			}
		}
		counter=0;
		for (auto& it : vectors)
		{
			bool Skip = false;
			for (const auto& elem : orders)
			{
				if (it.first == elem.name && elem.min_version > Version)
				{
					Skip = true;
					break;
				}
			}
			if (Skip)
			{
				continue;
			}
			xr_sprintf				(buff, sizeof(buff),"vec_%04d",counter++);
			it.second.val		= ini.r_fvector3	(sect.c_str(), buff);
		}

		counter=0;
		for (auto& it : domains)
		{
			bool Skip = false;
			for (const auto& elem : orders)
			{
				if (it.first == elem.name && elem.min_version > Version)
				{
					Skip = true;
					break;
				}
			}
			if (Skip)
			{
				continue;
			}
			xr_sprintf(buff, sizeof(buff),"domain_%s_%04d", sect.c_str(), counter++);
			it.second.Load2(ini, buff);
		}

		counter=0;
		for (auto& it : bools)
		{
			bool Skip = false;
			for (const auto& elem : orders)
			{
				if (it.first == elem.name && elem.min_version > Version)
				{
					Skip = true;
					break;
				}
			}
			if (Skip)
			{
				continue;
			}
			xr_sprintf				(buff, sizeof(buff),"bool_%04d",counter++);
			it.second.val		= ini.r_bool		(sect.c_str(), buff);
		}

		counter=0;
		for (auto& it : ints)
		{
			bool Skip = false;
			for (const auto& elem : orders)
			{
				if (it.first == elem.name && elem.min_version > Version)
				{
					Skip = true;
					break;
				}
			}
			if (Skip)
			{
				continue;
			}
			xr_sprintf				(buff, sizeof(buff),"int_%04d",counter++);
			it.second.val		= ini.r_s32		(sect.c_str(), buff);
		}

		counter=0;
		for (auto& it : strings)
		{
			bool Skip = false;
			for (const auto& elem : orders)
			{
				if (it.first == elem.name && elem.min_version > Version)
				{
					Skip = true;
					break;
				}
			}
			if (Skip)
			{
				continue;
			}
			xr_sprintf				(buff, sizeof(buff),"string_%04d",counter++);
			it.second.val		= ini.r_string		(sect.c_str(), buff);
		}
	} else
	{
		string256 buff;
		for (auto& elem : orders)
		{
			if (elem.min_version > Version)
			{
				continue;
			}
			switch (elem.type)
			{
			case tpDomain:
				{
					auto d_it = domains.find(elem.name);
					R_ASSERT(d_it != domains.end());
					d_it->second.Load2(ini, GenerateKey_Extended(buff, "domain", sect.c_str(), elem.name.c_str()));
					break;
				}
			case tpVector:
				{
					auto v_it = vectors.find(elem.name);
					R_ASSERT(v_it != vectors.end());
					v_it->second.val = ini.r_fvector3(
						sect.c_str(),
						GenerateKey_Extended(buff, "vec", nullptr, elem.name.c_str()));
					break;
				}
			case tpFloat:
				{
					auto f_it = floats.find(elem.name);
					R_ASSERT(f_it != floats.end());
					if(Version==EVersion::Old)
					{
						if(ini.line_exist(sect.c_str(), buff))
						{
							f_it->second.val = ini.r_float(
								sect.c_str(),
								GenerateKey_Extended(buff, "flt", nullptr, elem.name.c_str()));
						}
					}else
					{
						f_it->second.val = ini.r_float(
							sect.c_str(),
							GenerateKey_Extended(buff, "flt", nullptr, elem.name.c_str()));
					}
					break;
				}
			case tpBool:
				{
					auto b_it = bools.find(elem.name);
					R_ASSERT(b_it != bools.end());
					b_it->second.val = ini.r_bool(
						sect.c_str(),
						GenerateKey_Extended(buff, "bool", nullptr, elem.name.c_str()));
					break;
				}
			case tpInt:
				{
					auto i_it = ints.find(elem.name);
					R_ASSERT(i_it != ints.end());
					i_it->second.val = ini.r_s32(
						sect.c_str(),
						GenerateKey_Extended(buff, "int", nullptr, elem.name.c_str()));
					break;
				}
			case tpString:
				{
					auto s_it = strings.find(elem.name);
					R_ASSERT(s_it != strings.end());
					s_it->second.val = ini.r_string(
						sect.c_str(),
						GenerateKey_Extended(buff, "str", nullptr, elem.name.c_str()));
					break;
				}
			case tpEnum:
				{
					auto e_it = enums.find(elem.name);
					R_ASSERT(e_it != enums.end());
					e_it->second.value = ini.r_u64(
						sect.c_str(),
						GenerateKey_Extended(buff, "enum", nullptr, elem.name.c_str())
					);
					break;
				}
			}
		}
	}

}
void 	EParticleAction::Save		(IWriter& F)
{
	F.w_enum(EVersion::Current);
	F.w_stringZ(actionName);
	F.w_u32(flags.get());

	CMemoryWriter BoolStream;
	CMemoryWriter DomainStream;
	CMemoryWriter VectorStream;
	CMemoryWriter FloatStream;
	CMemoryWriter IntStream;
	CMemoryWriter StringStream;
	CMemoryWriter EnumStream;

	for (auto& elem : orders)
	{
		switch (elem.type)
		{
		case tpDomain:
			{
				domains[elem.name].Save(DomainStream);
				break;
			}
		case tpVector:
			{
				VectorStream.w_fvector3(vectors[elem.name].val);
				break;
			}
		case tpFloat:
			{
				FloatStream.w_float(floats[elem.name].val);
				break;
			}
		case tpBool:
			{
				BoolStream.w_u8(bools[elem.name].val);
				break;
			}
		case tpInt:
			{
				IntStream.w_s32(ints[elem.name].val);
				break;
			}
		case tpString:
			{
				StringStream.w_stringZ(strings[elem.name].val);
				break;
			}
		case tpEnum:
			{
				auto& CurEnum = enums[elem.name];
				switch (CurEnum.EnumSize)
				{
				case 1:
					{
						EnumStream.w_u8(CurEnum.value);
						break;
					}
				case 2:
					{
						EnumStream.w_u16(CurEnum.value);
						break;
					}
				case 4:
					{
						EnumStream.w_u32(CurEnum.value);
						break;
					}
				case 8:
					{
						EnumStream.w_u64(CurEnum.value);
						break;
					}
				}
				break;
			}
		}
	}
	
	F.open_chunk(0);
	{
		F.open_chunk(tpFloat);
		F.w(FloatStream.pointer(), FloatStream.size());
		F.close_chunk();
	}
	{
		F.open_chunk(tpVector);
		F.w(VectorStream.pointer(), VectorStream.size());
		F.close_chunk();
	}
	{
		F.open_chunk(tpDomain);
		F.w(DomainStream.pointer(), DomainStream.size());
		F.close_chunk();
	}
	{
		F.open_chunk(tpBool);
		F.w(BoolStream.pointer(), BoolStream.size());
		F.close_chunk();
	}
	{
		F.open_chunk(tpInt);
		F.w(IntStream.pointer(), IntStream.size());
		F.close_chunk();
	}
	{
		F.open_chunk(tpString);
		F.w(StringStream.pointer(), StringStream.size());
		F.close_chunk();
	}
	{
		F.open_chunk(tpEnum);
		F.w(EnumStream.pointer(), EnumStream.size());
		F.close_chunk();
	}
	F.close_chunk();
}

void EParticleAction::Save2(CInifile& ini, const shared_str& sect)
{
	ini.w_enum(sect.c_str(), "version", EVersion::Current);
	ini.w_string(sect.c_str(), "action_name", actionName.c_str());
	ini.w_u32(sect.c_str(), "flags", flags.get());
	
	string256 buff;
	for (auto& elem : orders)
	{
		switch (elem.type)
		{
		case tpDomain:
			{
				auto d_it = domains.find(elem.name);
				R_ASSERT(d_it != domains.end());
				d_it->second.Save2(ini, GenerateKey_Extended(buff, "domain", sect.c_str(), elem.name.c_str()));
				break;
			}
		case tpVector:
			{
				auto v_it = vectors.find(elem.name);
				R_ASSERT(v_it != vectors.end());
				ini.w_fvector3(
					sect.c_str(),
					GenerateKey_Extended(buff, "vec", nullptr, elem.name.c_str()),
					v_it->second.val);
				break;
			}
		case tpFloat:
			{
				auto f_it = floats.find(elem.name);
				R_ASSERT(f_it != floats.end());
				ini.w_float(
					sect.c_str(),
					GenerateKey_Extended(buff, "flt", nullptr, elem.name.c_str()),
					f_it->second.val);
				break;
			}
		case tpBool:
			{
				auto b_it = bools.find(elem.name);
				R_ASSERT(b_it != bools.end());
				ini.w_bool(
					sect.c_str(),
					GenerateKey_Extended(buff, "bool", nullptr, elem.name.c_str()),
					b_it->second.val);
				break;
			}
		case tpInt:
			{
				auto i_it = ints.find(elem.name);
				R_ASSERT(i_it != ints.end());
				ini.w_s32(
					sect.c_str(),
					GenerateKey_Extended(buff, "int", nullptr, elem.name.c_str()),
					i_it->second.val);
				break;
			}
		case tpString:
			{
				auto s_it = strings.find(elem.name);
				R_ASSERT(s_it != strings.end());
				ini.w_string(
					sect.c_str(),
					GenerateKey_Extended(buff, "str", nullptr, elem.name.c_str()),
					s_it->second.val.c_str());
				break;
			}
		case tpEnum:
			{
				auto e_it = enums.find(elem.name);
				R_ASSERT(e_it != enums.end());
				ini.w_u64(
					sect.c_str(),
					GenerateKey_Extended(buff, "enum", nullptr, elem.name.c_str()),
					e_it->second.value
					);
				break;
			}
		}
	}
}

EParticleAction::SOrder::SOrder(EValType _type, xr_string _name, EVersion _min_version):type(_type),name(_name),min_version(_min_version)
{
}

EParticleAction::SOrder& EParticleAction::appendFloat	(const char* name, float v, float mn, float mx)
{
	orders.push_back(SOrder(tpFloat,name));
	floats[name] = PFloat(v,mn,mx);
	return orders.back();
}
EParticleAction::SOrder& EParticleAction::appendInt		(const char* name, int v, int mn, int mx)
{
	orders.push_back(SOrder(tpInt,name));
	ints[name] = PInt(v,mn,mx);
	return orders.back();
}
EParticleAction::SOrder& EParticleAction::appendVector	(const char* name, PVector::EType type, float vx, float vy, float vz, float mn, float mx)
{
	orders.push_back(SOrder(tpVector,name));
	vectors[name] = PVector(type,Fvector().set(vx,vy,vz),mn,mx);
	return orders.back();
}
EParticleAction::SOrder& EParticleAction::appendDomain	(const char* name, PDomain v)
{
	orders.push_back(SOrder(tpDomain,name));
	domains[name] = v;
	return orders.back();
}
EParticleAction::SOrder& EParticleAction::appendBool	(const char* name, bool v)
{
	orders.push_back(SOrder(tpBool,name));
	bools[name] = PBool(v);
	return orders.back();
}
EParticleAction::SOrder& EParticleAction::appendString(const char* name, const shared_str& v)
{
	orders.push_back(SOrder(tpString,name));
	strings[name] = PString(v);
	return orders.back();
}
EParticleAction::SOrder& EParticleAction::appendString(const char* name, const char* v)
{
	orders.push_back(SOrder(tpString,name));
	strings[name] = PString(v);
	return orders.back();
}

EParticleAction::SOrder& EParticleAction::appendEnum(const char* name, xr_token* variants, u8 EnumSize, u32 index)
{
	orders.push_back(SOrder(tpEnum,name));
	enums[name] = PEnum(variants,EnumSize,index);
	return orders.back();
}

//------------------------------------------------------------------------------
pDomain ConvDomain(const PDomain& Source)
{
	return pDomain(Source.type, Source.f[0], Source.f[1], Source.f[2], Source.f[3], Source.f[4], Source.f[5], Source.f[6], Source.f[7], Source.f[8]);
}
									
EPAAvoid::EPAAvoid					():EParticleAction(PAPI::PAAvoidID)
{
	actionType						= "Avoid";
	actionName						= actionType;
	appendDomain					("Position",	PDomain(PDomain::vNum,TRUE,0x6096FF96));
	appendFloat						("Magnitude",	0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Epsilon",		0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Look Ahead",	0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool						("Allow Rotate",TRUE);
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPAAvoid::Compile			(IWriter& F)
{
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float look_ahead = _float("Look Ahead").val;
	bool allow_rotate = _bool("Allow Rotate").val;
	PAAvoid 		S;
	S.type			= PAAvoidID;
	S.positionL		= ConvDomain(_domain("Position"));
	S.position		= S.positionL;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.look_ahead	= look_ahead;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPABounce::Compile			(IWriter& F)
{
	float friction = _float("Friction").val;
	float resilience = _float("Resilience").val;
	float cutoff = _float("Cutoff").val;
	bool allow_rotate = _bool("Allow Rotate").val;
	PABounce 		S;
	S.type			= PABounceID;
	S.positionL		= ConvDomain(_domain("Position"));
	S.position		= S.positionL;
	S.oneMinusFriction = 1.0f - friction;
	S.resilience	= resilience;
	S.cutoffSqr		= _sqr(cutoff);
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
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
	bool copy_pos = _bool("Copy Position").val;
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPADamping::Compile			(IWriter& F)
{
	const Fvector& damping = _vector("Damping").val;
	float vlow = _float("V Low").val;
	float vhigh = _float("V High").val;
	PADamping 	S;
	S.type			= PADampingID;
	S.damping		= Fvector(damping.x, damping.y, damping.z);
	S.vlowSqr		= _sqr(vlow);
	S.vhighSqr		= _sqr(vhigh);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPAExplosion::Compile	  	(IWriter& F)
{
	const Fvector& center = _vector("Center").val;
	float velocity = _float("Velocity").val;
	float magnitude = _float("Magnitude").val;
	float stdev = _float("Standart Dev").val;
	float epsilon = _float("Epsilon").val;
	float age = _float("Age").val;
	bool allow_rotate = _bool("Allow Rotate").val;
	PAExplosion 	S;
	S.type			= PAExplosionID;
	S.centerL		= Fvector(center.x, center.y, center.z);
	S.center		= S.centerL;
	S.velocity		= velocity;
	S.magnitude		= magnitude;
	S.stdev			= stdev;
	S.epsilon		= epsilon;
	S.age			= age;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	if(S.epsilon < 0.0f)
		S.epsilon 	= EPS_L;
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
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
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
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
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
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
	bool allow_rotate = _bool("Allow Rotate").val;
	PAGravity 	S;
	S.type			= PAGravityID;
	S.directionL	= Fvector(dir.x, dir.y, dir.z);
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPAJet::Compile				(IWriter& F)
{
	const Fvector& center = _vector("Center").val;
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	bool allow_rotate = _bool("Allow Rotate").val;
	PAJet 	S;
	S.type			= PAJetID;
	S.centerL		= Fvector(center.x, center.y, center.z);
	S.center		= S.centerL;
	S.accL			= ConvDomain(_domain("Accelerate"));
	S.acc			= S.accL;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
	F.w_u32			(S.type);
	S.Save			(F);
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
	bool kill_less_than = _bool("Kill Less Than").val;
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
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
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPAOrbitLine::Compile	 	(IWriter& F)
{
	const Fvector& p = _vector("Position").val;
	const Fvector& axis = _vector("Axis").val;
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	bool allow_rotate = _bool("Allow Rotate").val;
	PAOrbitLine 	S;
	S.type			= PAOrbitLineID;
	S.pL			= Fvector(p.x, p.y, p.z);
	S.p			= S.pL;
	S.axisL		= Fvector(axis.x, axis.y, axis.z);
	S.axisL.normalize_safe();
	S.axis			= S.axisL;
	S.magnitude	= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
	F.w_u32			(S.type);
	S.Save			(F);
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPAOrbitPoint::Compile	   	(IWriter& F)
{
	const Fvector& center = _vector("Center").val;
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	bool allow_rotate = _bool("Allow Rotate").val;
	PAOrbitPoint 	S;
	S.type			= PAOrbitPointID;
	S.centerL		= Fvector(center.x, center.y, center.z);
	S.center		= S.centerL;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPARandomAccel::EPARandomAccel		():EParticleAction(PAPI::PARandomAccelID)
{
	actionType						= "RandomAccel";
	actionName						= actionType;
	appendDomain					("Accelerate",PDomain(PDomain::vNum,FALSE));
	appendBool						("Allow Rotate",	TRUE);
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPARandomAccel::Compile	   	(IWriter& F)
{
	bool allow_rotate = _bool("Allow Rotate").val;
	PARandomAccel 	S;
	S.type			= PARandomAccelID;
	S.gen_accL		= ConvDomain(_domain("Accelerate"));
	S.gen_acc		= S.gen_accL;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
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
	bool allow_rotate = _bool("Allow Rotate").val;
	PARandomDisplace 	S;
	S.type			= PARandomDisplaceID;
	S.gen_dispL		= ConvDomain(_domain("Displace"));
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPARandomVelocity::Compile 	(IWriter& F)
{
	bool allow_rotate = _bool("Allow Rotate").val;
	PARandomVelocity 	S;
	S.type			= PARandomVelocityID;
	S.gen_velL		= ConvDomain(_domain("Velocity"));
	S.gen_vel		= S.gen_velL;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
	F.w_u32			(S.type);
	S.Save			(F);
}

EPARestore::EPARestore				():EParticleAction(PAPI::PARestoreID)
{
	actionType						= "Restore";
	actionName						= actionType;
	appendFloat						("Time",			0.f, 0.0f, P_MAXFLOAT);
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPARestore::Compile			(IWriter& F)
{
	float time_left = _float("Time").val;
	PARestore 	S;
	S.type			= PARestoreID;
	S.time_left		= time_left;
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPAScatter::Compile	 		(IWriter& F)
{
	const Fvector& center = _vector("Center").val;
	float magnitude = _float("Magnitude").val;
	float epsilon = _float("Epsilon").val;
	float max_radius = _float("Max Radius").val;
	bool allow_rotate = _bool("Allow Rotate").val;
	PAScatter 		S;
	S.type			= PAScatterID;
	S.centerL		= Fvector(center.x, center.y, center.z);
	S.center		= S.centerL;
	S.magnitude		= magnitude;
	S.epsilon		= epsilon;
	S.max_radius	= max_radius;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
	F.w_u32			(S.type);
	S.Save			(F);
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
	bool kill_inside = _bool("Kill Inside").val;
	bool allow_rotate = _bool("Allow Rotate").val;
	PASink 	S;
	S.type			= PASinkID;
	S.kill_inside	= kill_inside;
	S.positionL		= ConvDomain(_domain("Domain"));
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
	bool kill_inside = _bool("Kill Inside").val;
	bool allow_rotate = _bool("Allow Rotate").val;
	PASinkVelocity 	S;
	S.type			= PASinkVelocityID;
	S.kill_inside	= kill_inside;
	S.velocityL		= ConvDomain(_domain("Domain"));
	S.velocity		= S.velocityL;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,allow_rotate);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPASource::EPASource				():EParticleAction(PAPI::PASourceID)
{
	actionType = "Source";
	actionName = actionType;
	appendFloat("Rate", 100.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendDomain("Domain", PDomain(PDomain::vNum,TRUE,0x60FFEBAA));
	appendDomain("Velocity", PDomain(PDomain::vNum,FALSE));
	appendDomain("Rotation", PDomain(PDomain::vAngle,FALSE));
	appendBool("Align Rotation Velocity to Rotation", true).min_version = EVersion::Extended;
	appendDomain("Rotation Velocity", PDomain(PDomain::vNum, false)).min_version = EVersion::Extended;
	appendDomain("Size", PDomain(PDomain::vNum,FALSE));
	appendBool("Single Size", FALSE);
	appendDomain("Color", PDomain(PDomain::vColor, FALSE, 0x00000000, PAPI::PDPoint,1.f,1.f,1.f,1.f,1.f,1.f,1.f,1.f,1.f));
	appendBool("Color\\Random Alpha", false).min_version = EVersion::SomeVasnyaBranch;
	appendFloat("Color\\Alpha", 0.f, 0.f, 1.f);
	appendFloat("Color\\Alpha 2", 0.f, 0.f, 1.f).min_version = EVersion::SomeVasnyaBranch;
	appendFloat("Starting Age", 0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat("Age Sigma", 0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat("Parent Motion", 0.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool("Allow Rotate",	FALSE);
}
void	EPASource::Compile			(IWriter& F)
{
	PASource 	S;
	S.type			= PASourceID;
	S.particle_rate = _float("Rate").val;
	S.positionL		= ConvDomain(_domain("Domain"));
	S.position		= S.positionL;
	S.velocityL		= ConvDomain(_domain("Velocity"));
	S.velocity		= S.velocityL;
	S.size			= ConvDomain(_domain("Size"));
	S.rot			= ConvDomain(_domain("Rotation"));
	S.color			= ConvDomain(_domain("Color"));
	S.random_alpha			= _bool("Color\\Random Alpha").val;
	S.alpha			= _float("Color\\Alpha").val;
	S.alpha2			= _float("Color\\Alpha 2").val;
	S.age			= _float("Starting Age").val;
	S.age_sigma		= _float("Age Sigma").val;
	S.m_Flags.assign(
		(_bool("Single Size").val ? PASource::flSingleSize : 0)
		| PASource::flVertexB_tracks);
	S.parent_vel	= Fvector(0,0,0);
	S.parent_motion	= _float("Parent Motion").val;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,_bool("Allow Rotate").val);
	F.w_u32			(S.type);
	S.Save			(F);
}

EPASpeedLimit::EPASpeedLimit		():EParticleAction(PAPI::PASpeedLimitID)
{
	actionType						= "SpeedLimit";
	actionName						= actionType;
	appendFloat						("Min Speed",			-1.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Max Speed",			15.0f, -P_MAXFLOAT, P_MAXFLOAT);
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPASpeedLimit::Compile	 	(IWriter& F)
{
	float min_speed = _float("Min Speed").val;
	float max_speed = _float("Max Speed").val;
	PASpeedLimit 	S;
	S.type			= PASpeedLimitID;
	S.min_speed = min_speed;
	S.max_speed = max_speed;
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
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

void EPATargetColor::Compile(IWriter& F)
{
	const Fvector& color = _vector("Color").val;
	float alpha = _float("Alpha").val;
	float scale = _float("Scale").val;
	float time_from = _float("TimeFrom").val;
	float time_to = _float("TimeTo").val;
	PATargetColor 	S;
	S.type			= PATargetColorID;
	S.color = Fvector(color.x, color.y, color.z);
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
	S.size = Fvector(size.x, size.y, size.z);
	S.scale = Fvector(scale.x, scale.y, scale.z);
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
	S.rot = Fvector(rot.x, rot.y, rot.z);
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}
void	EPATargetVelocity::Compile	(IWriter& F)
{
	PATargetVelocity 	S;
	S.type			= PATargetVelocityID;
	
	S.velocityL		= _vector("Velocity").val;
	S.velocity		= S.velocityL;
	S.scale			= _float("Scale").val;
	S.m_Flags.set	(ParticleAction::ALLOW_ROTATE,_bool("Allow Rotate").val);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
	
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

EPATurbulence::EPATurbulence		():EParticleAction(PAPI::PATurbulenceID)
{
	actionType						= "Turbulence";
	actionName						= actionType;
	appendFloat						("Frequency",		2.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendInt						("Octaves",			1,1);
	appendFloat						("Magnitude",		10.f, -P_MAXFLOAT, P_MAXFLOAT);
	appendFloat						("Delta",			0.01f, -P_MAXFLOAT, P_MAXFLOAT);
	appendVector					("Movement",		PVector::vNum, 1,1,1);
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
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
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;
	
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
	appendBool("Align Rotation Velocity to Velocity", true).min_version = EVersion::Extended;
}

void EPABindVelocityValue::Compile(IWriter& F)
{
	PABindVelocityValue S;
	S.type      = PABindVelocityValueID;
	S.BindValue.set(_vector("InitialValue").val);
	S.AlighRotVelocityToVelocity = _bool("Align Rotation Velocity to Velocity").val;

	F.w_u32(S.type);
	S.Save(F);
}

xr_token animators_types[] = {
	{"Replace", PAAnimatorType::Replace},
	{"Multiply", PAAnimatorType::Multiply},
	{0, 0}
};

EPAColorAnimator::EPAColorAnimator(): EParticleAction(PAPI::PAColorAnimatorID)
{
	actionType = "ColorAnimator";
	actionName = actionType;
	appendEnum<PAAnimatorType>("AnimatorType", animators_types, PAAnimatorType::Replace).min_version = EVersion::SomeVasnyaBranch;
	appendString("Animator", "");
	appendBool("Looped", false);
	appendBool("Reverse", false);
	appendBool("Wrap", false).min_version = EVersion::SomeVasnyaBranch;      
}

void EPAColorAnimator::Compile(IWriter& F)
{
	PAColorAnimator S;
	S.type = PAColorAnimatorID;
	S.AnimatorType = (PAAnimatorType)(_enum("AnimatorType").value);
	S.Animator = _string("Animator").val;
	S.Looped = _bool("Looped").val;
	S.Reverse = _bool("Reverse").val;
	S.Wrap = _bool("Wrap").val;

	F.w_u32(S.type);
	S.Save(F);
}

EPASizeAnimator::EPASizeAnimator(): EParticleAction(PAPI::PASizeAnimatorID)
{
	actionType = "SizeAnimator";
	actionName = actionType;
	appendEnum<PAAnimatorType>("AnimatorType", animators_types, PAAnimatorType::Replace).min_version = EVersion::SomeVasnyaBranch;
	appendString("Animator", "");
	appendBool("Looped", false);
	appendBool("Reverse", false);
	appendBool("Wrap", false).min_version = EVersion::SomeVasnyaBranch;     
}

void EPASizeAnimator::Compile(IWriter& F)
{
	PASizeAnimator S;
	S.type = PASizeAnimatorID;
	S.AnimatorType = (PAAnimatorType)(_enum("AnimatorType").value);
	S.Animator = _string("Animator").val;
	S.Looped = _bool("Looped").val;
	S.Reverse = _bool("Reverse").val;
	S.Wrap = _bool("Wrap").val;

	F.w_u32(S.type);
	S.Save(F);
}

EPAVelocityAnimator::EPAVelocityAnimator(): EParticleAction(PAPI::PAVelocityAnimatorID)
{
	actionType = "VelocityAnimator";
	actionName = actionType;
	appendEnum<PAAnimatorType>("AnimatorType", animators_types, PAAnimatorType::Replace).min_version = EVersion::SomeVasnyaBranch;
	appendString("Animator", "");
	appendBool("Looped", false);
	appendBool("Reverse", false);
	appendBool("Wrap", false).min_version = EVersion::SomeVasnyaBranch; 
}

void EPAVelocityAnimator::Compile(IWriter& F)
{
	PAVelocityAnimator S;
	S.type = PAVelocityAnimatorID;
	S.AnimatorType = (PAAnimatorType)(_enum("AnimatorType").value);
	S.Animator = _string("Animator").val;
	S.Looped = _bool("Looped").val;
	S.Reverse = _bool("Reverse").val;
	S.Wrap = _bool("Wrap").val;

	F.w_u32(S.type);
	S.Save(F);
}

EPAVelocityRotationAnimator::EPAVelocityRotationAnimator(): EParticleAction(PAPI::PAVelocityRotationAnimatorID)
{
	actionType = "VelocityRotationAnimator";
	actionName = actionType;
	appendEnum<PAAnimatorType>("AnimatorType", animators_types, PAAnimatorType::Replace).min_version = EVersion::SomeVasnyaBranch;
	appendString("Animator", "");
	appendBool("Looped", false);
	appendBool("Reverse", false);
	appendBool("Wrap", false).min_version = EVersion::SomeVasnyaBranch; 
}

void EPAVelocityRotationAnimator::Compile(IWriter& F)
{
	PAVelocityAnimator S;
	S.type = PAVelocityAnimatorID;
	S.AnimatorType = (PAAnimatorType)(_enum("AnimatorType").value);
	S.Animator = _string("Animator").val;
	S.Looped = _bool("Looped").val;
	S.Reverse = _bool("Reverse").val;
	S.Wrap = _bool("Wrap").val;

	F.w_u32(S.type);
	S.Save(F);
}
