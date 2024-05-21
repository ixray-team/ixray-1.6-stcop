#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_CYLINDER_H__
#define __XR_CYLINDER_H__

#include "xr_vector3.h"

namespace xray_re {

template<typename T> struct _cylinder {
	void		reset();

	_vector3<T>	center;
	_vector3<T>	direction;	// align axis
	T		height;
	T		radius;
};

typedef _cylinder<float> fcylinder;
typedef _cylinder<double> dcylinder;

template<typename T> void _cylinder<T>::reset()
{
	center.set();
	direction.set();
	height = 0;
	radius = 0;
}

} // end of namespace xray_re

#endif
