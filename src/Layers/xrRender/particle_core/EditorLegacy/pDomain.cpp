#include "stdafx.h"

#include "ParticleEffectActions.h"

using namespace PAPI;
using namespace EPALegacy;

PDomain::PDomain(EType et, BOOL ra, u32 color, PDomainEnum t,	
										float inA0,	float inA1,	float inA2,	
								   		float inA3,	float inA4,	float inA5,
										float inA6,	float inA7,	float inA8	)
{
	flags.set(flRenderable,ra);
	e_type = et;
	type = t;
    clr	 = color;
	f[0] = inA0;
	f[1] = inA1;
	f[2] = inA2;    
	f[3] = inA3;
	f[4] = inA4;
	f[5] = inA5;
	f[6] = inA6;
	f[7] = inA7;
	f[8] = inA8;
}

PDomain::PDomain(const PDomain& inDomain)
{
	e_type 	= inDomain.e_type;
    flags	= inDomain.flags;
	type 	= inDomain.type;
    clr	 	= inDomain.clr;
	f[0]	= inDomain.f[0];
	f[1]	= inDomain.f[1];
	f[2]	= inDomain.f[2];
	f[3]	= inDomain.f[3];
	f[4]	= inDomain.f[4];
	f[5]	= inDomain.f[5];
	f[6]	= inDomain.f[6];
	f[7]	= inDomain.f[7];
	f[8]	= inDomain.f[8];
}

PDomain::~PDomain()
{
}

void PDomain::Load(IReader& F)
{
	type		= PDomainEnum(F.r_u32());
	F.r_fvector3(v[0]);
	F.r_fvector3(v[1]);
	F.r_fvector3(v[2]);
}

void PDomain::Load2(CInifile& ini, const shared_str& sect)
{
	type		= PDomainEnum(ini.r_u32(sect,"type"));
	v[0]		= ini.r_fvector3(sect,"v0");
	v[1]		= ini.r_fvector3(sect,"v1");
	v[2]		= ini.r_fvector3(sect,"v2");
}

void PDomain::Save(IWriter& F) const 
{
	F.w_u32		(type);
	F.w_fvector3(v[0]);
	F.w_fvector3(v[1]);
	F.w_fvector3(v[2]);
}

void PDomain::Save2(CInifile& ini, const shared_str& sect) const
{
	ini.w_u32		(sect.c_str(), "type", type);
	ini.w_fvector3	(sect.c_str(), "v0", v[0]);
	ini.w_fvector3	(sect.c_str(), "v1", v[1]);
	ini.w_fvector3	(sect.c_str(), "v2", v[2]);
}
