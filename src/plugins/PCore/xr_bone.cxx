#include <utility>
#include "xr_object_format.h"
#include "xr_bone.h"
#include "xr_motion.h"
#include "xr_object.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_bone::xr_bone(): m_parent(0),
	m_bind_length(.5f), m_gamemtl("default_object"), m_mass(10.f)
{
	m_bind_offset.set();
	m_bind_rotate.set();
	m_center_of_mass.set();
}

xr_bone::~xr_bone() {}

void xr_bone::load_1(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk<uint16_t>(BONE_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == BONE_VERSION_1 || version == BONE_VERSION);

	if (!r.find_chunk(BONE_CHUNK_DEF))
		xr_not_expected();
	r.r_sz(m_name);
	r.r_sz(m_parent_name);
	r.r_sz(m_vmap_name);
	r.debug_find_chunk();

	if (!r.find_chunk(BONE_CHUNK_BIND_POSE))
		xr_not_expected();
	r.r_fvector3(m_bind_offset);
	r.r_fvector3(m_bind_rotate);
	m_bind_length = r.r_float();
	r.debug_find_chunk();
	if (version == BONE_VERSION_1)
		std::swap(m_bind_offset.x, m_bind_offset.y);

	load_data(r);
}

void xr_bone::load_0(xr_reader& r)
{
	r.r_sz(m_name);
	r.r_sz(m_parent_name);
	r.r_sz(m_vmap_name);
	r.r_fvector3(m_bind_offset);
	r.r_fvector3(m_bind_rotate);
	m_bind_length = r.r_float();
	std::swap(m_bind_offset.x, m_bind_offset.y);

	// FIXME: reset other fields
	xr_not_implemented();
}

void xr_bone::load_data(xr_reader& r)
{
	if (!r.find_chunk(BONE_CHUNK_DEF, 0, false))
		xr_not_expected();
	r.r_sz(m_name);
	r.debug_find_chunk();

	if (!r.find_chunk(BONE_CHUNK_MATERIAL))
		xr_not_expected();
	r.r_sz(m_gamemtl);
	r.debug_find_chunk();

	if (!r.find_chunk(BONE_CHUNK_SHAPE))
		xr_not_expected();
	r.r<s_bone_shape>(m_shape);
	r.debug_find_chunk();

	if (r.find_chunk(BONE_CHUNK_IK_FLAGS)) {
		m_joint_ik_data.ik_flags = r.r_u32();
		r.debug_find_chunk();
	}

	if (!r.find_chunk(BONE_CHUNK_IK_JOINT))
		xr_not_expected();
	m_joint_ik_data.type = r.r_u32();
	r.r_cseq<s_joint_limit>(3, m_joint_ik_data.limits);
	m_joint_ik_data.spring_factor = r.r_float();
	m_joint_ik_data.damping_factor = r.r_float();
	r.debug_find_chunk();

	if (r.find_chunk(BONE_CHUNK_BREAK_PARAMS)) {
		m_joint_ik_data.break_force = r.r_float();
		m_joint_ik_data.break_torque = r.r_float();
		r.debug_find_chunk();
	}

	if (r.find_chunk(BONE_CHUNK_FRICTION)) {
		m_joint_ik_data.friction = r.r_float();
		r.debug_find_chunk();
	}

	if (r.find_chunk(BONE_CHUNK_MASS_PARAMS)) {
		m_mass = r.r_float();
		r.r_fvector3(m_center_of_mass);
		r.debug_find_chunk();
	}
}

void xr_bone::save(xr_writer& w) const
{
	w.w_chunk<uint16_t>(BONE_CHUNK_VERSION, BONE_VERSION);

	w.open_chunk(BONE_CHUNK_DEF);	// first type!
	w.w_sz(m_name);
	w.w_sz(m_parent_name);
	w.w_sz(m_vmap_name);
	w.close_chunk();

	w.open_chunk(BONE_CHUNK_BIND_POSE);
	w.w_fvector3(m_bind_offset);
	w.w_fvector3(m_bind_rotate);
	w.w_float(m_bind_length);
	w.close_chunk();

	save_data(w);
}

void xr_bone::save_data(xr_writer& w) const
{
	w.w_chunk(BONE_CHUNK_DEF, m_name);	// second type!!!
	w.w_chunk(BONE_CHUNK_MATERIAL, m_gamemtl);
	w.w_chunk<s_bone_shape>(BONE_CHUNK_SHAPE, m_shape);
	w.w_chunk<uint32_t>(BONE_CHUNK_IK_FLAGS, m_joint_ik_data.ik_flags);

	w.open_chunk(BONE_CHUNK_IK_JOINT);
	w.w_u32(m_joint_ik_data.type);

	//w.w_cseq<s_joint_limit>(3, m_joint_ik_data.limits);
	for (int i = 0; i < 3; ++i) //invert limits for AE
	{
		fvector2 vec = m_joint_ik_data.limits[i].limit;
		float tmp = vec.x;
		vec.x = -vec.y;
		vec.y = -tmp;
		w.w_fvector2(vec);
		w.w_float(m_joint_ik_data.limits[i].spring_factor);
		w.w_float(m_joint_ik_data.limits[i].damping_factor);
	}

	w.w_float(m_joint_ik_data.spring_factor);
	w.w_float(m_joint_ik_data.damping_factor);
	w.close_chunk();

	w.open_chunk(BONE_CHUNK_BREAK_PARAMS);
	w.w_float(m_joint_ik_data.break_force);
	w.w_float(m_joint_ik_data.break_torque);
	w.close_chunk();

	w.w_chunk<float>(BONE_CHUNK_FRICTION, m_joint_ik_data.friction);

	w.open_chunk(BONE_CHUNK_MASS_PARAMS);
	w.w_float(m_mass);
	w.w_fvector3(m_center_of_mass);
	w.close_chunk();
}

void xr_bone::calculate_bind(const fmatrix& parent_xform)
{
	m_bind_xform.set_xyz_i(m_bind_rotate);
	m_bind_xform.c.set(m_bind_offset);
	m_bind_xform.mul_a_43(parent_xform);
	m_bind_i_xform.invert_43(m_bind_xform);

	for (xr_bone_vec_it it = m_children.begin(), end = m_children.end(); it != end; ++it)
		(*it)->calculate_bind(m_bind_xform);
}

void xr_bone::calculate_motion(xr_skl_motion* sm, const fmatrix& parent_xform)
{
	uint8_t flags = sm->bone_motion_flags(m_id);
	if (flags & xr_bone_motion::BMF_WORLD_ORIENT) {
		m_mot_xform.set_xyz_i(m_mot_rotate);
		m_mot_xform.c.set(m_mot_offset);
		m_last_xform.mul(parent_xform, m_mot_xform);

		m_last_xform.i.set(m_mot_xform.i);
		m_last_xform.j.set(m_mot_xform.j);
		m_last_xform.k.set(m_mot_xform.k);

		m_mot_xform.mul_a_43(fmatrix().invert_43(parent_xform));
	} else {
		m_mot_xform.set_xyz_i(m_mot_rotate);
		m_mot_xform.c.set(m_mot_offset);
		m_last_xform.mul(parent_xform, m_mot_xform);
	}
	m_render_xform.mul_43(m_last_xform, m_bind_i_xform);
	for (xr_bone_vec_it it = m_children.begin(), end = m_children.end(); it != end; ++it)
		(*it)->calculate_motion(sm, m_last_xform);
}

void xr_bone::update_motion(const fvector3& offset, const fvector3& rotate)
{
	m_mot_offset.set(offset);
	m_mot_rotate.set(rotate);
}

void xr_bone::setup(uint16_t id, xr_object& object)
{
	m_id = id;
	if (!m_parent_name.empty()) {
		m_parent = object.find_bone(m_parent_name);
		
		if (m_parent)
			m_parent->m_children.push_back(this);
		else
			m_parent_name.clear();
	}
}

////////////////////////////////////////////////////////////////////////////////

xr_partition::xr_partition(): m_id(0), m_name("default") {}

xr_partition::xr_partition(const xr_bone_vec& bones): m_id(0), m_name("default")
{
	m_bones.reserve(bones.size());
	for (xr_bone_vec_cit it = bones.begin(), end = bones.end(); it != end; ++it)
		m_bones.push_back((*it)->name());
}

xr_partition::~xr_partition() {}

void xr_partition::load_1(xr_reader& r)
{
	r.r_sz(m_name);
	r.r_seq(r.r_u32(), m_bones, xr_reader::f_r_sz());
}

void xr_partition::load_0(xr_reader& r, const xr_bone_vec& all_bones)
{
	r.r_sz(m_name);
	for (uint_fast32_t n = r.r_u32(); n; --n)
		m_bones.push_back(all_bones.at(r.r_u32())->name());
}

void xr_partition::save(xr_writer& w) const
{
	w.w_sz(m_name);
	w.w_size_u32(m_bones.size());
	w.w_seq(m_bones, xr_writer::f_w_sz());
}

void xr_partition::setup(uint16_t id) { m_id = id; }
