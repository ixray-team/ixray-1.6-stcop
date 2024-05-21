#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_BUILD_ERR_H__
#define __XR_BUILD_ERR_H__

#include <vector>
#include "xr_vector3.h"

namespace xray_re {

class xr_writer;
class xr_memory_writer;

class xr_build_err {
public:
		xr_build_err();
		~xr_build_err();

	void	save(xr_writer& w) const;
	bool	save(const char* path, const char* name) const;

	void	zero_area_face(const fvector3& p0, const fvector3& p1, const fvector3& p2);
	void	zero_uv_area_face(const fvector3& p0, const fvector3& p1, const fvector3& p2);

	bool	empty() const;

private:
	xr_memory_writer*	m_invalid;
};

} // end of namespace xray_re

#endif
