#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_BONE_H__
#define __XR_BONE_H__

#include <vector>
#include <string>
#include "xr_matrix.h"
#include "xr_skeleton.h"

namespace xray_re {

class xr_reader;
class xr_writer;
class xr_skl_motion;
class xr_object;

class xr_bone;

TYPEDEF_STD_VECTOR_PTR(xr_bone)

class xr_bone {
public:
			xr_bone();
	virtual		~xr_bone();

	void		load_0(xr_reader& r);
	void		load_1(xr_reader& r);
	void		load_data(xr_reader& r);
	void		save(xr_writer& w) const;
	void		save_data(xr_writer& w) const;

	void		calculate_bind(const fmatrix& parent_xform);
	void		calculate_motion(xr_skl_motion* sm, const fmatrix& parent_xform);
	void		update_motion(const fvector3& offset, const fvector3& rotate);
	void		setup(uint16_t id, xr_object& object);

	bool			is_root() const;

	uint16_t		id() const;
	const xr_bone*		parent() const;

	xr_bone_vec&		children();
	const xr_bone_vec&	children() const;

	std::string&		name();
	const std::string&	name() const;
	std::string&		parent_name();
	const std::string&	parent_name() const;
	std::string&		vmap_name();
	const std::string&	vmap_name() const;
	const fmatrix&		bind_xform() const;
	const fmatrix&		bind_i_xform() const;
	const fmatrix&		motion_xform() const;
	const fmatrix&		motion_i_xform() const;
	const fmatrix&		last_xform() const;
	fvector3&		bind_offset();
	const fvector3&		bind_offset() const;
	fvector3&		bind_rotate();
	const fvector3&		bind_rotate() const;
	std::string&		gamemtl();
	const std::string&	gamemtl() const;

protected:
	uint16_t		m_id;
	xr_bone*		m_parent;
	xr_bone_vec		m_children;

	fvector3		m_mot_offset;
	fvector3		m_mot_rotate;
	float			m_mot_length;

	fmatrix			m_mot_xform;
	fmatrix			m_mot_i_xform;
	fmatrix			m_bind_xform;
	fmatrix			m_bind_i_xform;
	fmatrix			m_last_xform;
	fmatrix			m_render_xform;

	std::string		m_name;			// BONE_CHUNK_DEF (there are two flavours of it!)
	std::string		m_parent_name;
	std::string		m_vmap_name;

	fvector3		m_bind_rotate;		// BONE_CHUNK_BIND_POSE
	fvector3		m_bind_offset;
	float			m_bind_length;

	std::string		m_gamemtl;		// BONE_CHUNK_MATERIAL
	s_bone_shape		m_shape;		// BONE_CHUNK_SHAPE

	s_joint_ik_data		m_joint_ik_data;	// BONE_CHUNK_IK_JOINT, BONE_CHUNK_IK_FLAGS,
							// BONE_CHUNK_BREAK_PARAMS, BONE_CHUNK_FRICTION
	float			m_mass;			// BONE_CHUNK_MASS_PARAMS
	fvector3		m_center_of_mass;
};

class xr_partition {
public:
				xr_partition();
				xr_partition(const xr_bone_vec& bones);
	virtual			~xr_partition();

	void			load_0(xr_reader& r, const xr_bone_vec& all_bones);
	void			load_1(xr_reader& r);
	void			save(xr_writer& w) const;
	void			setup(uint16_t id);

	uint16_t			id() const;
	const std::string&		name() const;
	std::vector<std::string>&	bones();

protected:
	uint16_t			m_id;
	std::string			m_name;
	std::vector<std::string>	m_bones;
};

TYPEDEF_STD_VECTOR_PTR(xr_partition)

inline bool xr_bone::is_root() const { return m_parent == 0; }
inline uint16_t xr_bone::id() const { return m_id; }
inline const xr_bone* xr_bone::parent() const { return m_parent; }
inline xr_bone_vec& xr_bone::children() { return m_children; }
inline const xr_bone_vec& xr_bone::children() const { return m_children; }
inline std::string& xr_bone::name() { return m_name; }
inline const std::string& xr_bone::name() const { return m_name; }
inline std::string& xr_bone::parent_name() { return m_parent_name; }
inline const std::string& xr_bone::parent_name() const { return m_parent_name; }
inline std::string& xr_bone::vmap_name() { return m_vmap_name; }
inline const std::string& xr_bone::vmap_name() const { return m_vmap_name; }
inline const fmatrix& xr_bone::bind_xform() const { return m_bind_xform; }
inline const fmatrix& xr_bone::bind_i_xform() const { return m_bind_i_xform; }
inline fvector3& xr_bone::bind_offset() { return m_bind_offset; }
inline const fvector3& xr_bone::bind_offset() const { return m_bind_offset; }
inline fvector3& xr_bone::bind_rotate() { return m_bind_rotate; }
inline const fvector3& xr_bone::bind_rotate() const { return m_bind_rotate; }
inline std::string& xr_bone::gamemtl() { return m_gamemtl; }
inline const std::string& xr_bone::gamemtl() const { return m_gamemtl; }

inline uint16_t xr_partition::id() const { return m_id; }
inline const std::string& xr_partition::name() const { return m_name; }
inline std::vector<std::string>& xr_partition::bones() { return m_bones; }

} // end of namespace xray_re

#endif
