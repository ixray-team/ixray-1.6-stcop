#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_OBJ_MOTION_H__
#define __XR_OBJ_MOTION_H__

#include "xr_motion.h"
#include "xr_vector3.h"

namespace xray_re {

class xr_obj_motion: public xr_motion {
public:
			xr_obj_motion();
	virtual 	~xr_obj_motion();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;
	bool		load_anm(const char* path);
	bool		save_anm(const char* path) const;

	void		evaluate(float time, fvector3& t, fvector3& r) const;

	void		create_envelopes();
	void		delete_envelopes();

	const xr_envelope* const*	envelopes() const;
	xr_envelope* const*		envelopes();

private:
	enum {
		OMOTION_VERSION_3	= 3,
		OMOTION_VERSION_4	= 4,
		OMOTION_VERSION_5	= 5,
	};
	xr_envelope*	m_envelopes[6];
};

inline xr_envelope* const* xr_obj_motion::envelopes() { return m_envelopes; }
inline const xr_envelope* const* xr_obj_motion::envelopes() const { return m_envelopes; }

} // end of namespace xray_re

#endif
