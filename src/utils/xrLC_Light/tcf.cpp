#include "stdafx.h"
#include "tcf.h"

void	_TCF::barycentric	(Fvector2 &P, float &u, float &v, float &w)
{
	Fvector2 	kV02; kV02.sub(uv[0],uv[2]);
	Fvector2 	kV12; kV12.sub(uv[1],uv[2]);
	Fvector2 	kPV2; kPV2.sub(P,    uv[2]);

	float		fM00 = kV02.dot(kV02);
	float		fM01 = kV02.dot(kV12);
	float		fM11 = kV12.dot(kV12);
	float		fR0  = kV02.dot(kPV2);
	float		fR1  = kV12.dot(kPV2);
	float		fDet = fM00*fM11 - fM01*fM01;

	u			= (fM11*fR0 - fM01*fR1)/fDet;
	v			= (fM00*fR1 - fM01*fR0)/fDet;
	w			= 1.0f - u - v;
}
 

bool _TCF::similar(    const _TCF &_tc, float eps /*= EPS*/  ) const
{
	return	uv[0].similar( _tc.uv[0], eps )&&
			uv[1].similar( _tc.uv[1], eps )&&
			uv[2].similar( _tc.uv[2], eps );
}
