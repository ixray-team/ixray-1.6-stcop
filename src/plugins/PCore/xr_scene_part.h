#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_PART_H__
#define __XR_SCENE_PART_H__

#include <vector>
#include "xr_types.h"
#include "xr_scene_common.h"
#include "xr_scene_revision.h"
#include "xr_writer.h"

namespace xray_re {

class xr_reader;
class xr_writer;
class xr_ini_writer;
class xr_scene;

class xr_scene_part {
public:
	virtual			~xr_scene_part() = 0;
	virtual void		load(xr_reader& r) = 0;
	virtual void		save(xr_writer& w) const = 0;
	
	virtual void		save_v12(xr_ini_writer* w) const = 0;

	xr_scene&		scene() const;
	const char*		file_name() const;
	scene_chunk_id		chunk_id() const;
	xr_scene_revision&	revision();
	const xr_scene_revision&revision() const;

protected:
				xr_scene_part(xr_scene& scene, const char* file_name,
						scene_chunk_id chunk_id);

private:
	xr_scene&		m_scene;
	const char*		m_file_name;
	scene_chunk_id		m_chunk_id;
	xr_scene_revision	m_revision;
};

TYPEDEF_STD_VECTOR_PTR(xr_scene_part)

inline xr_scene_part::xr_scene_part(xr_scene& scene, const char* file_name, scene_chunk_id chunk_id):
	m_scene(scene), m_file_name(file_name), m_chunk_id(chunk_id) {}

inline xr_scene& xr_scene_part::scene() const { return m_scene; }
inline const char* xr_scene_part::file_name() const { return m_file_name; }
inline scene_chunk_id xr_scene_part::chunk_id() const { return m_chunk_id; }
inline xr_scene_revision& xr_scene_part::revision() { return m_revision; }
inline const xr_scene_revision& xr_scene_part::revision() const { return m_revision; }

} // end of namespace xray_re

#endif
