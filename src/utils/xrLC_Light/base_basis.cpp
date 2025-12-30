#include "stdafx.h"

#include "base_basis.h"

bool	base_basis::similar			(const base_basis& o)
{
	s32 dx	= std::abs(s32(x)-s32(o.x));
	s32 dy	= std::abs(s32(y)-s32(o.y));
	s32 dz	= std::abs(s32(z)-s32(o.z));
	return (dx<=1 && dy<=1 && dz<=1);
}