#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_OBB_H__
#define __XR_OBB_H__

#include "xr_vector3.h"
#include "xr_matrix.h"

namespace xray_re {

template<typename T> struct _obb {
	void		get_xform(_matrix<T>& m);
	_obb<T>&	set_xform(const _matrix<T>& m);
	void		reset();

	_matrix33<T>	rotate;		// align axis
	_vector3<T>	translate;
	_vector3<T>	halfsize;
};

typedef _obb<float> fobb;

template<typename T> void _obb<T>::reset()
{
	rotate.identity();
	translate.set();
	halfsize.set();
}

template<typename T> inline void _obb<T>::get_xform(_matrix<T>& m)
{
	m.i.set(rotate.i); m._14 = 0;
	m.j.set(rotate.j); m._24 = 0;
	m.k.set(rotate.k); m._34 = 0;
	m.c.set(translate); m._44 = 1.f;
}

template<typename T> inline _obb<T>& _obb<T>::set_xform(const _matrix<T>& m)
{
	rotate.i.set(m.i);
	rotate.j.set(m.j);
	rotate.k.set(m.k);
	translate.set(m.c);
	return *this;
}

} // end of namespace xray_re

#endif
