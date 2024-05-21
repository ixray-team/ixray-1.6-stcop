#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_BLENDER_H__
#define __XR_BLENDER_H__

#include "xr_types.h"

namespace xray_re {

class xr_reader;
class xr_writer;

struct blender_desc {
	uint64_t	clsid;
	char		name[128];
	char		owner[32];
	uint32_t	time;
	uint16_t	version;
	uint16_t	__unused;
};

class xr_blender {
public:
	virtual			~xr_blender();

	virtual void		load(xr_reader& r);
	virtual void		save(xr_writer& w) const;

	virtual const char*	comment() const = 0;
	virtual bool		can_be_detailed() const;
	virtual bool		can_be_lmapped() const;

protected:
	blender_desc		m_desc;
};

class xr_blender_accum_direct: public xr_blender {
public:
};

class xr_blender_accum_direct_mask: public xr_blender {
public:
};

class xr_blender_accum_point: public xr_blender {
public:
};

class xr_blender_accum_reflected: public xr_blender {
public:
};

class xr_blender_accum_spot: public xr_blender {
public:
};

class xr_blender_bloom_build: public xr_blender {
public:
};

class xr_blender_blur: public xr_blender {
public:
	virtual const char*	comment() const;
};

class xr_blender_bmmd: public xr_blender {
public:
	virtual const char*	comment() const;
	virtual bool		can_be_detailed() const;
	virtual bool		can_be_lmapped() const;
};

class xr_blender_combine: public xr_blender {
public:
};

class xr_blender_default: public xr_blender {
public:
	virtual const char*	comment() const;
	virtual bool		can_be_detailed() const;
	virtual bool		can_be_lmapped() const;
};

class xr_blender_default_aref: public xr_blender {
public:
	virtual const char*	comment() const;
	virtual bool		can_be_detailed() const;
	virtual bool		can_be_lmapped() const;
};

class xr_blender_deffer_aref: public xr_blender {
public:
};

class xr_blender_deffer_flat: public xr_blender {
public:
};

class xr_blender_deffer_model: public xr_blender {
public:
};

class xr_blender_detail_still: public xr_blender {
public:
	virtual const char*	comment() const;
};

class xr_blender_editor_selection: public xr_blender {
public:
	virtual const char*	comment() const;
};

class xr_blender_editor_wire: public xr_blender {
public:
	virtual const char*	comment() const;
};

class xr_blender_la_em_b: public xr_blender {
public:
	virtual const char*	comment() const;
	virtual bool		can_be_lmapped() const;
};

class xr_blender_light_occq: public xr_blender {
public:
};

class xr_blender_lm_eb_b: public xr_blender {
public:
	virtual const char*	comment() const;
	virtual bool		can_be_lmapped() const;
};

class xr_blender_luminance: public xr_blender {
public:
};

class xr_blender_model: public xr_blender {
public:
	virtual const char*	comment() const;
};

class xr_blender_model_eb_b: public xr_blender {
public:
	virtual const char*	comment() const;
};

class xr_blender_particle: public xr_blender {
public:
	virtual const char*	comment() const;
};

class xr_blender_screen_gray: public xr_blender {
public:
	virtual const char*	comment() const;
};

class xr_blender_screen_set: public xr_blender {
public:
	virtual const char*	comment() const;
};

class xr_blender_sh_world: public xr_blender {
public:
	virtual const char*	comment() const;
};

class xr_blender_tree: public xr_blender {
public:
	virtual const char*	comment() const;
	virtual bool		can_be_detailed() const;
};

class xr_blender_vertex: public xr_blender {
public:
	virtual const char*	comment() const;
	virtual bool		can_be_detailed() const;
};

class xr_blender_vertex_aref: public xr_blender {
public:
	virtual const char*	comment() const;
};

xr_blender* create_blender(uint64_t clsid);

} // end of namespace xray_re

#endif
