#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_MOTION_H__
#define __XR_MOTION_H__

#include <string>
#include <vector>
#include "xr_types.h"

namespace xray_re {

enum {
	EOBJ_CHUNK_OMOTION	= 0x1100,	// .anm
	EOBJ_CHUNK_SMOTION	= 0x1200,	// .skl
};

class xr_reader;
class xr_writer;
class xr_envelope;

class xr_motion {
public:
	virtual 	~xr_motion();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	int32_t&		frame_start();
	int32_t			frame_start() const;
	int32_t&		frame_end();
	int32_t			frame_end() const;
	float&			fps();
	float			fps() const;
	std::string&		name();
	const std::string&	name() const;

	void			set_frame_range(int32_t start, int32_t end);

	enum motion_type {
		MT_OBJECT,
		MT_SKELETON,
	};

protected:
			xr_motion(motion_type type);

	int32_t		m_frame_start;
	int32_t		m_frame_end;
	float		m_fps;
	std::string	m_name;

private:
	uint32_t	m_type;
};

inline std::string& xr_motion::name() { return m_name; }
inline const std::string& xr_motion::name() const { return m_name; }
inline int32_t& xr_motion::frame_start() { return m_frame_start; }
inline int32_t xr_motion::frame_start() const { return m_frame_start; }
inline int32_t& xr_motion::frame_end() { return m_frame_end; }
inline int32_t xr_motion::frame_end() const { return m_frame_end; }
inline float& xr_motion::fps() { return m_fps; }
inline float xr_motion::fps() const { return m_fps; }

inline void xr_motion::set_frame_range(int32_t start, int32_t end)
{
	m_frame_start = start;
	m_frame_end = end;
}

} // end of namespace xray_re

#endif
