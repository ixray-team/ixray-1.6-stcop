#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SKL_MOTION_H__
#define __XR_SKL_MOTION_H__

#include "xr_motion.h"
#include "xr_vector3.h"

namespace xray_re {

class xr_envelope;

class xr_bone_motion {
public:
			xr_bone_motion();
			xr_bone_motion(const char* name);
	virtual		~xr_bone_motion();

	void		load_0(xr_reader& r);
	void		load_1(xr_reader& r);
	void		load_2(xr_reader& r);
	void		save(xr_writer& w) const;

	void		evaluate(float time, fvector3& t, fvector3& r) const;

	void		create_envelopes();
	void		delete_envelopes();

	std::string&			name();
	const std::string&		name() const;
	const xr_envelope* const*	envelopes() const;
	xr_envelope* const*		envelopes();
	uint8_t&			flags();
	uint8_t				flags() const;

	enum bone_motion_flag {
		BMF_WORLD_ORIENT	= 0x1,
	};

protected:
	std::string	m_name;
	xr_envelope*	m_envelopes[6];
	uint8_t		m_flags;
};

TYPEDEF_STD_VECTOR_PTR(xr_bone_motion)

struct motion_mark {
	float		t0, t1;
};

class xr_motion_marks: public std::vector<motion_mark> {
public:
	void		load(xr_reader& r);
	void		save(xr_writer& w) const;

protected:
	std::string	m_name;
};

TYPEDEF_STD_VECTOR_PTR(xr_motion_marks)

const uint16_t ALL_PARTITIONS = UINT16_MAX;

class xr_skl_motion: public xr_motion {
public:
			xr_skl_motion();
	virtual 	~xr_skl_motion();

	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;
	bool		load_skl(const char* path);
	bool		save_skl(const char* path) const;

	void		evaluate(uint16_t bone_id, float time, fvector3& t, fvector3& r) const;

	const xr_bone_motion_vec&	bone_motions() const;
	xr_bone_motion_vec&		bone_motions();

		int		marks_size();

	uint8_t		bone_motion_flags(uint16_t bone_id) const;

	enum motion_flag {
		SMF_FX		= 0x1,
		SMF_STOP_AT_END	= 0x2,
		SMF_NO_MIX	= 0x4,
		SMF_SYNC_PART	= 0x8,
	};

protected:
	enum {
		SMOTION_VERSION_4	= 4,
		SMOTION_VERSION_5	= 5,
		SMOTION_VERSION_6	= 6,
		SMOTION_VERSION_7	= 7,	// guessed 3120 (Clear Sky)
	};

	xr_bone_motion_vec		m_bone_motions;
	uint16_t			m_bone_or_part;
	float				m_speed;
	float				m_accrue;
	float				m_falloff;
	float				m_power;
	unsigned			m_flags;

	xr_motion_marks_vec		m_marks;
};

TYPEDEF_STD_VECTOR_PTR(xr_skl_motion)

inline std::string& xr_bone_motion::name() { return m_name; }
inline const std::string& xr_bone_motion::name() const { return m_name; }
inline xr_envelope* const* xr_bone_motion::envelopes() { return m_envelopes; }
inline const xr_envelope* const* xr_bone_motion::envelopes() const { return m_envelopes; }
inline uint8_t& xr_bone_motion::flags() { return m_flags; }
inline uint8_t xr_bone_motion::flags() const { return m_flags; }

inline uint8_t xr_skl_motion::bone_motion_flags(uint16_t bone_id) const
{
	return m_bone_motions.at(bone_id)->flags();
}
inline const xr_bone_motion_vec& xr_skl_motion::bone_motions() const
{
	return m_bone_motions;
}
inline xr_bone_motion_vec& xr_skl_motion::bone_motions()
{
	return m_bone_motions;
}
inline int xr_skl_motion::marks_size()
{
	return m_marks.size();
}

} // end of namespace xray_re

#endif
