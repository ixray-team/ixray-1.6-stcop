#pragma once

class CPHContact;

#include "MathUtils.h"

struct SGameMtl;
namespace	CDB{	class TRI;	}
class		CBoneInstance;

class IPhysicsShellHolder;

// Contact of a dynamic geom with a static level triangle, as seen by shot-mark callbacks.
struct SPHShotMarkContact
{
	Fvector Position;
	Fvector Normal;
	IPhysicsShellHolder* Object = nullptr;
	u16 Material = 0;
	float VelCret = 0.f;
	bool HasEffect = false; // a dynamic body takes part, VelCret is valid
	bool HasUserData = false;
	bool IsCharacter = false;
	bool InvertNormal = false;
};

typedef void 	ContactCallbackFun(const CDB::TRI& T, Fvector* verts,const SPHShotMarkContact& c);
typedef	void	ObjectContactCallbackFun(bool& do_colide,bool bo1,CPHContact& c,SGameMtl* material_1,SGameMtl* material_2);


typedef void	_BCL	BoneCallbackFun(CBoneInstance* B);





typedef			void	PhysicsStepTimeCallback		(u32	step_start,u32	step_end);
//extern			PhysicsStepTimeCallback		*physics_step_time_callback;

template <typename geom_type>
void	t_get_box( const geom_type*	shell, const	Fmatrix& form,	Fvector&	sz, Fvector&	c )
{
	c.set(0,0,0);
	VERIFY( sizeof( form.i ) + sizeof( form._14_ ) == 4* sizeof( float ) );
	for(int i=0;3>i;++i)
	{	
		float lo,hi;
		const	Fvector &ax=cast_fv(((const	float*)&form+i*4));
		shell->get_Extensions(ax,0,lo,hi);
		sz[i]=hi-lo;c.add(Fvector().mul(ax,(lo+hi)/2));
	}
}

enum ERestrictionType : u8
{
	rtStalker = 0,
	rtStalkerSmall,
	rtMonsterMedium,
	rtNone,
	rtActor
};
