#include "stdafx.h"
#include "base_color.h"

bool	base_color::				similar	( const base_color &c, float eps /*=EPS*/ ) const
{
	bool ret = 
		r._value == c.r._value  &&
		g._value == c.g._value  &&	 
		b._value == c.b._value  &&
		h._value == c.h._value  &&
		s._value == c.s._value  &&
		t._value == c.t._value;
	if(!ret)
	{
		EngineLog( "base color diff : dr = {} , dg = {}, db = {}, dh = {}, ds = {}, dt = {}",
			r._value - c.r._value,g._value - c.g._value,b._value - c.b._value,h._value - c.h._value,
			s._value - c.s._value , t._value - c.t._value 
		);
		EngineLog( "base color  : r = {},{} , g = {},{}, b = {},{}, h = {},{}, s = {},{}, t = {},{} ",
			r._value ,c.r._value,g._value ,c.g._value,b._value ,c.b._value,h._value ,c.h._value,
			s._value ,c.s._value , t._value ,c.t._value 
		);
	}
	return ret;
}