#include <algorithm>
#include "xr_ogf_v4.h"
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

struct xr_ogf_v4::bone_io: public xr_bone {
	void	import(xr_reader& r);
	void	import_ikdata(xr_reader& r);
	void	define(uint16_t id, const std::string& name);
};

struct xr_ogf_v4::partition_io: public xr_partition {
	void	import(xr_reader& r, xr_bone_vec& all_bones);
};

struct xr_ogf_v4::bone_motion_io: public xr_ogf::bone_motion_io {
	void	import(xr_reader& r, uint_fast32_t num_keys);
};

struct xr_ogf_v4::motion_io: public xr_skl_motion {
			motion_io();
	uint16_t	import_params(xr_reader& r, unsigned version);
	void		import_bone_motions(xr_reader& r, xr_bone_vec& all_bones);
};

inline xr_ogf_v4::motion_io::motion_io() { m_fps = OGF4_MOTION_FPS; }

////////////////////////////////////////////////////////////////////////////////

xr_ogf_v4::xr_ogf_v4(): xr_ogf(OGF4_VERSION), m_fast(0),
	m_ext_vb_index(0), m_ext_vb_offset(0), m_ext_vb_size(0),
	m_ext_ib_index(0), m_ext_ib_offset(0), m_ext_ib_size(0),
	m_ext_swib_index(0) {}

xr_ogf_v4::~xr_ogf_v4()
{
	delete m_fast;
}

void xr_ogf_v4::clear()
{
	xr_ogf::clear();
	m_swib.clear();
	m_source.clear();
	m_export_tool.clear();
	delete m_fast;
	m_fast = 0;
}

bool xr_ogf_v4::hierarchical() const
{
	switch (m_model_type) {
	case MT4_HIERRARHY:
	case MT4_SKELETON_ANIM:
	case MT4_SKELETON_RIGID:
	case MT4_LOD:
		return true;
	default:
		return false;
	}
}

bool xr_ogf_v4::skeletal() const
{
	switch (m_model_type) {
	case MT4_SKELETON_RIGID:
	case MT4_SKELETON_ANIM:
		return true;
	default:
		return false;
	}
}

bool xr_ogf_v4::animated() const
{
	return m_model_type == MT4_SKELETON_ANIM;
}

bool xr_ogf_v4::progressive() const
{
	switch (m_model_type) {
	case MT4_PROGRESSIVE:
	case MT4_SKELETON_GEOMDEF_PM:
	case MT4_TREE_PM:
		return true;
	default:
		return false;
	}
}

bool xr_ogf_v4::versioned() const { return is_chunk_loaded(OGF4_S_DESC); }

void xr_ogf_v4::setup_ib0()
{
	m_ib0.proxy(m_ib, m_swib[0].offset, m_swib[0].num_tris*3);
}

void xr_ogf_v4::set_ext_geom(const xr_vbuf_vec& ext_vbufs,
		const xr_ibuf_vec& ext_ibufs, const xr_swibuf_vec& ext_swibufs)
{
	if (is_chunk_loaded(OGF4_VCONTAINER) || is_chunk_loaded(OGF4_GCONTAINER))
		m_vb.proxy(ext_vbufs.at(m_ext_vb_index), m_ext_vb_offset, m_ext_vb_size);
	if (is_chunk_loaded(OGF4_ICONTAINER) || is_chunk_loaded(OGF4_GCONTAINER))
		m_ib.proxy(ext_ibufs.at(m_ext_ib_index), m_ext_ib_offset, m_ext_ib_size);
	if (is_chunk_loaded(OGF4_SWICONTAINER)) {
		m_swib.proxy(ext_swibufs.at(m_ext_swib_index));
		setup_ib0();
	} else if (is_chunk_loaded(OGF4_SWIDATA)) {
		setup_ib0();
	}
}

////////////////////////////////////////////////////////////////////////////////

void xr_ogf_v4::load_header(xr_reader& r)
{
	m_version = static_cast<ogf_version>(r.r_u8());
	m_model_type = static_cast<ogf_model_type>(r.r_u8());
	m_shader_id = r.r_u16();
	r.r(m_bbox);
	r.r(m_bsphere);
	set_chunk_loaded(OGF_HEADER);
}

inline void xr_ogf_v4::load_texture(xr_reader& r)
{
	xr_ogf::load_texture(r);
	set_chunk_loaded(OGF4_TEXTURE);
}

inline void xr_ogf_v4::load_vertices(xr_reader& r)
{
	ogf_vertex_format fmt = static_cast<ogf_vertex_format>(r.r_u32());
	size_t n = r.r_u32();
	m_vb.load_ogf4(r, n, fmt);
	r.debug_find_chunk();
	set_chunk_loaded(OGF4_VERTICES);
}

inline void xr_ogf_v4::load_indices(xr_reader& r)
{
	size_t n = r.r_u32();
	xr_assert(n && (n % 3) == 0);
	m_ib.load(r, n);
	r.debug_find_chunk();
	set_chunk_loaded(OGF4_INDICES);
}

inline void xr_ogf_v4::load_p_map(xr_reader& r)
{
	xr_not_implemented();
	set_chunk_loaded(OGF4_P_MAP);
}

inline void xr_ogf_v4::load_swidata(xr_reader& r)
{
	m_swib.load(r);
	// setup ib0 only if this is not level subdivision
	if (is_chunk_loaded(OGF4_INDICES))
		setup_ib0();
	set_chunk_loaded(OGF4_SWIDATA);
}

inline void xr_ogf_v4::load_vcontainer(xr_reader& r)
{
	m_ext_vb_index = r.r_u32();
	m_ext_vb_offset = r.r_u32();
	m_ext_vb_size = r.r_u32();
	set_chunk_loaded(OGF4_VCONTAINER);
}

inline void xr_ogf_v4::load_icontainer(xr_reader& r)
{
	m_ext_ib_index = r.r_u32();
	m_ext_ib_offset = r.r_u32();
	m_ext_ib_size = r.r_u32();
	set_chunk_loaded(OGF4_ICONTAINER);
}

void xr_ogf_v4::load_children(xr_reader& r)
{
	assert(m_children.empty());
	xr_reader* s;
	for (uint32_t id = 0; (s = r.open_chunk(id)); ++id) {
		xr_ogf_v4* ogf = new xr_ogf_v4;
		ogf->load_ogf(*s);
		m_children.push_back(ogf);
		r.close_chunk(s);
	}
	set_chunk_loaded(OGF4_CHILDREN);
}

void xr_ogf_v4::load_children_l(xr_reader& r)
{
	r.r_seq(r.r_u32(), m_children_l);
	set_chunk_loaded(OGF4_CHILDREN_L);
}

inline void xr_ogf_v4::load_loddef2(xr_reader& r)
{
	r.r_cseq<ogf4_lod_face>(8, m_lod_faces);
	set_chunk_loaded(OGF4_LODDEF2);
}

inline void xr_ogf_v4::load_treedef2(xr_reader& r)
{
	r.r(m_tree_xform);
	r.r(m_c_scale);
	r.r(m_c_bias);
	set_chunk_loaded(OGF4_TREEDEF2);
}

inline void xr_ogf_v4::load_s_userdata(xr_reader& r)
{
	r.r_sz(m_userdata);
	set_chunk_loaded(OGF4_S_USERDATA);
}

inline void xr_ogf_v4::load_s_desc(xr_reader& r)
{
	r.r_sz(m_source);
	r.r_sz(m_export_tool);
	m_export_time = r.r_u32();
	r.r_sz(m_owner_name);
	m_creation_time = r.r_u32();
	r.r_sz(m_modif_name);
	m_modified_time = r.r_u32();
	set_chunk_loaded(OGF4_S_DESC);
}

inline void xr_ogf_v4::load_s_motion_refs_0(xr_reader& r)
{
	r.r_sz(m_motion_refs);
	set_chunk_loaded(OGF4_S_MOTION_REFS_0);
}

inline void xr_ogf_v4::load_s_motion_refs_1(xr_reader& r)
{
	for (uint_fast32_t n = r.r_u32(); n > 0;) {
		m_motion_refs += r.skip_sz();
		if (--n)
			m_motion_refs += ',';
	}
	set_chunk_loaded(OGF4_S_MOTION_REFS_1);
}

inline void xr_ogf_v4::load_swicontainer(xr_reader& r)
{
	m_ext_swib_index = r.r_u32();
	set_chunk_loaded(OGF4_SWICONTAINER);
}

void xr_ogf_v4::load_gcontainer(xr_reader& r)
{
	m_ext_vb_index = r.r_u32();
	m_ext_vb_offset = r.r_u32();
	m_ext_vb_size = r.r_u32();
	m_ext_ib_index = r.r_u32();
	m_ext_ib_offset = r.r_u32();
	m_ext_ib_size = r.r_u32();
	set_chunk_loaded(OGF4_GCONTAINER);
}

void xr_ogf_v4::load_fastpath(xr_reader& r)
{
	// FIXME: ignoring for now
	set_chunk_loaded(OGF4_FASTPATH);
}

void xr_ogf_v4::load_s_lods(xr_reader& r)
{
	// 2945 uses embedded LOD model while 3120, 3456+ reference external file.
	// try to distinguish by size.
	if (r.size() <= 0x100) {
		r.r_sz(m_lod_ref);
	} else {
		assert(m_lods.empty());
		xr_reader* s;
		for (uint32_t id = 0; (s = r.open_chunk(id)); ++id) {
			xr_ogf* ogf = new xr_ogf_v4;
			m_lods.push_back(ogf);
			ogf->load_ogf(*s);
			r.close_chunk(s);
		}
	}
	set_chunk_loaded(OGF4_S_LODS);
}

void xr_ogf_v4::bone_motion_io::import(xr_reader& r, uint_fast32_t num_keys)
{
	create_envelopes();

	unsigned flags = r.r_u8();

	if (flags & KPF_R_ABSENT) {
		insert_key(0, r.skip<ogf_key_qr>());
	} else {
		r.r_u32();
		for (size_t i = 0; i != num_keys; ++i)
			insert_key(i/OGF4_MOTION_FPS, r.skip<ogf_key_qr>());
	}
	if (flags & KPF_T_PRESENT) {
		r.r_u32();
		fvector3 t_init, t_size, value;
		if (flags & KPF_T_HQ) {
			const ogf4_key_qt_hq* keys_qt = r.skip<ogf4_key_qt_hq>(num_keys);
			r.r_fvector3(t_size);
			r.r_fvector3(t_init);
			for (uint_fast32_t i = 0; i != num_keys; ++i) {
				keys_qt[i].dequantize(value, t_size);
				value.add(t_init);
				insert_key(float(i)/OGF4_MOTION_FPS, &value);
			}
		} else {
			const ogf4_key_qt* keys_qt = r.skip<ogf4_key_qt>(num_keys);
			r.r_fvector3(t_size);
			r.r_fvector3(t_init);
			for (uint_fast32_t i = 0; i != num_keys; ++i) {
				keys_qt[i].dequantize(value, t_size);
				value.add(t_init);
				insert_key(float(i)/OGF4_MOTION_FPS, &value);
			}
		}
	} else {
		insert_key(0, r.skip<fvector3>());
	}
}

inline void xr_ogf_v4::motion_io::import_bone_motions(xr_reader& r, xr_bone_vec& all_bones)
{
	uint_fast32_t num_keys = r.r_u32();
	m_frame_start = 0;
	m_frame_end = int32_t(num_keys & INT32_MAX);

	assert(m_bone_motions.empty());
	m_bone_motions.reserve(all_bones.size());
	for (xr_bone_vec_it it = all_bones.begin(), end = all_bones.end(); it != end; ++it) {
		xr_ogf_v4::bone_motion_io* bm = new xr_ogf_v4::bone_motion_io;
		bm->name() = (*it)->name();
		bm->import(r, num_keys);
		m_bone_motions.push_back(bm);
	}
}

void xr_ogf_v4::load_s_motions(xr_reader& r)
{
	if (!r.find_chunk(0))
		xr_not_expected();
	size_t num_motions = r.r_u32();
	xr_assert(m_motions.size() == num_motions);
	for (uint32_t id = 1; id <= num_motions; ++id) {
		if (!r.find_chunk(id))
			xr_not_expected();

		const char* name = r.skip_sz();
		motion_io* smotion = static_cast<motion_io*>(find_motion(name));
		if (smotion == 0) {
			msg("unknown motion %s", name);
			throw xr_error();
		}
		smotion->import_bone_motions(r, m_bones);
		r.debug_find_chunk();
	}
	set_chunk_loaded(OGF4_S_MOTIONS);
}

inline void xr_ogf_v4::partition_io::import(xr_reader& r, xr_bone_vec& all_bones)
{
	r.r_sz(m_name);
	std::string name;
	for (uint_fast32_t n = r.r_u16(); n; --n) {
		r.r_sz(name);
		uint_fast32_t id = r.r_u32();
		xr_assert(id < MAX_BONES);
		if (all_bones.size() <= id)
			all_bones.resize(id + 1);
		if (all_bones[id] == 0) {
			xr_ogf_v4::bone_io* bone = new xr_ogf_v4::bone_io;
			bone->define(uint16_t(id), name);
			all_bones[id] = bone;
		} else {
			xr_assert(all_bones.at(id)->name() == name);
		}
		m_bones.push_back(name);
	}
}

void xr_ogf_v4::bone_io::define(uint16_t id, const std::string& name)
{
	m_id = id;
	m_name = name;
}

inline uint16_t xr_ogf_v4::motion_io::import_params(xr_reader& r, unsigned version)
{
	r.r_sz(m_name);
	m_flags = r.r_u32();
	m_bone_or_part = r.r_u16();
	uint16_t motion_id = r.r_u16();
	m_speed = r.r_float();
	m_power = r.r_float();
	m_accrue = r.r_float();
	m_falloff = r.r_float();
	if (version == OGF4_S_SMPARAMS_VERSION_4) {
		m_marks.resize(r.r_u32());
		for (xr_motion_marks_vec_it it = m_marks.begin(), end = m_marks.end(); it != end; ++it) {
			xr_motion_marks* mm = new xr_motion_marks;
			mm->load(r);
			*it = mm;
		}
	}
	return motion_id;
}

struct read_partition_v4 {
	xr_bone_vec& all_bones;
	read_partition_v4(xr_bone_vec& _all_bones): all_bones(_all_bones) {}
	void operator()(xr_partition*& _part, xr_reader& r) {
		xr_ogf_v4::partition_io* part = new xr_ogf_v4::partition_io;
		_part = part;
		part->import(r, all_bones);
	}
};

void xr_ogf_v4::load_s_smparams(xr_reader& r)
{
	uint16_t version = r.r_u16();
	xr_assert(version == OGF4_S_SMPARAMS_VERSION_3 || version == OGF4_S_SMPARAMS_VERSION_4);

	assert(m_partitions.empty());
	r.r_seq(r.r_u16(), m_partitions, read_partition_v4(m_bones));
	setup_partitions();

	assert(m_motions.empty());
	size_t num_motions = r.r_u16();
	m_motions.resize(num_motions);
	for (; num_motions; --num_motions) {
		motion_io* smotion = new xr_ogf_v4::motion_io;
		m_motions.at(smotion->import_params(r, version)) = smotion;
	}
	assert(std::find(m_motions.begin(), m_motions.end(), static_cast<xr_skl_motion*>(0)) == m_motions.end());

	set_chunk_loaded(OGF4_S_SMPARAMS);
}

inline void xr_ogf_v4::bone_io::import_ikdata(xr_reader& r)
{
	uint32_t version = r.r_u32();
	xr_assert(version == OGF4_S_JOINT_IK_DATA_VERSION);
	r.r_sz(m_gamemtl);
	r.r(m_shape);
	r.r(m_joint_ik_data);
	r.r_fvector3(m_bind_rotate);
	r.r_fvector3(m_bind_offset);
	m_bind_length = 0.5f;
	m_mass = r.r_float();
	r.r_fvector3(m_center_of_mass);
}

void xr_ogf_v4::load_s_ikdata(xr_reader& r)
{
	for (xr_bone_vec_it it = m_bones.begin(), end = m_bones.end(); it != end; ++it)
		static_cast<bone_io*>(*it)->import_ikdata(r);
	set_chunk_loaded(OGF4_S_IKDATA);
}

inline void xr_ogf_v4::bone_io::import(xr_reader& r)
{
	r.r_sz(m_name);
	r.r_sz(m_parent_name);
	m_vmap_name = m_name;
	r.advance(sizeof(fobb));
}

struct read_bone_v4 {
	xr_ogf_v4& ogf;
	read_bone_v4(xr_ogf_v4& _ogf): ogf(_ogf) {}
	void operator()(xr_bone*& bone, xr_reader& r) {
		xr_ogf_v4::bone_io* bone_v4 = new xr_ogf_v4::bone_io;
		bone_v4->import(r);
		bone = bone_v4;
	}
};

void xr_ogf_v4::load_s_bone_names(xr_reader& r)
{
	assert(m_bones.empty());
	r.r_seq(r.r_u32(), m_bones, read_bone_v4(*this));
	setup_bones();
	set_chunk_loaded(OGF4_S_BONE_NAMES);
}

////////////////////////////////////////////////////////////////////////////////

void xr_ogf_v4::load_render_visual(xr_reader& r)
{
	// header is already loaded

	// should it be here?
	if (r.find_chunk(OGF4_S_DESC)) {
		load_s_desc(r);
		r.debug_find_chunk();
	}

	if (r.find_chunk(OGF4_TEXTURE)) {
		load_texture(r);
		r.debug_find_chunk();
	}
}

void xr_ogf_v4::load_visual(xr_reader& r)
{
	load_render_visual(r);
	if (r.find_chunk(OGF4_GCONTAINER)) {
		assert(m_path.empty());
		load_gcontainer(r);
		r.debug_find_chunk();

		xr_reader* s = r.open_chunk(OGF4_FASTPATH);
		if (s) {
			load_fastpath(*s);
			r.close_chunk(s);
		}
	} else {
		if (r.find_chunk(OGF4_VCONTAINER)) {
			load_vcontainer(r);
			r.debug_find_chunk();
		} else {
			if (!r.find_chunk(OGF4_VERTICES))
				xr_not_expected();
			load_vertices(r);
			r.debug_find_chunk();
		}
		if (r.find_chunk(OGF4_ICONTAINER)) {
			load_icontainer(r);
			r.debug_find_chunk();
		} else {
			if (!r.find_chunk(OGF4_INDICES))
				xr_not_expected();
			load_indices(r);
			r.debug_find_chunk();
		}
	}
}

void xr_ogf_v4::load_hierrarhy_visual(xr_reader& r)
{
	load_render_visual(r);

	if (r.find_chunk(OGF4_CHILDREN_L)) {
		load_children_l(r);
		r.debug_find_chunk();
	} else {
		xr_reader* s = r.open_chunk(OGF4_CHILDREN);
		assert(s);
		load_children(*s);
		r.close_chunk(s);
	}
}

void xr_ogf_v4::load_progressive(xr_reader& r)
{
	load_visual(r);
	xr_reader* s = r.open_chunk(OGF4_SWIDATA);
	xr_assert(s);
	load_swidata(*s);
	r.close_chunk(s);
}

void xr_ogf_v4::load_kinematics_animated(xr_reader& r)
{
	load_kinematics(r);
	if (r.find_chunk(OGF4_S_MOTION_REFS_0)) {
		load_s_motion_refs_0(r);
		r.debug_find_chunk();
	} else if (r.find_chunk(OGF4_S_MOTION_REFS_1)) {
		load_s_motion_refs_1(r);
		r.debug_find_chunk();
	} else {
		xr_reader* s = r.open_chunk(OGF4_S_SMPARAMS);
		xr_assert(s);
		load_s_smparams(*s);
		assert(s->eof());
		r.close_chunk(s);

		s = r.open_chunk(OGF4_S_MOTIONS);
		xr_assert(s);
		load_s_motions(*s);
		assert(s->eof());
		r.close_chunk(s);
	}
}

void xr_ogf_v4::load_kinematics(xr_reader& r)
{
	load_hierrarhy_visual(r);
	xr_reader* s = r.open_chunk(OGF4_S_LODS);
	if (s) {
		load_s_lods(*s);
		r.close_chunk(s);
	}
	s = r.open_chunk(OGF4_S_USERDATA);
	if (s) {
		load_s_userdata(*s);
		assert(s->eof());
		r.close_chunk(s);
	}
	if (!r.find_chunk(OGF4_S_BONE_NAMES))
		xr_not_expected();
	load_s_bone_names(r);
	r.debug_find_chunk();

	if (r.find_chunk(OGF4_S_IKDATA)) {
		load_s_ikdata(r);
		r.debug_find_chunk();
	}
}

inline void xr_ogf_v4::load_skeletonx(xr_reader& r)
{
	assert(r.find_chunk(OGF4_VERTICES));
}

void xr_ogf_v4::load_skeletonx_pm(xr_reader& r)
{
	load_skeletonx(r);
	load_progressive(r);
}

inline void xr_ogf_v4::load_skeletonx_st(xr_reader& r)
{
	load_skeletonx(r);
	load_visual(r);
}

void xr_ogf_v4::load_lod(xr_reader& r)
{
	load_hierrarhy_visual(r);
	if (!r.find_chunk(OGF4_LODDEF2))
		xr_not_expected();
	load_loddef2(r);
	r.debug_find_chunk();
}

void xr_ogf_v4::load_tree_visual(xr_reader& r)
{
	load_visual(r);
	if (!r.find_chunk(OGF4_TREEDEF2))
		xr_not_expected();
	load_treedef2(r);
	r.debug_find_chunk();
}

inline void xr_ogf_v4::load_tree_visual_st(xr_reader& r)
{
	load_tree_visual(r);
}

void xr_ogf_v4::load_particle_effect(xr_reader& r)
{
	xr_not_implemented();
}

void xr_ogf_v4::load_particle_group(xr_reader& r)
{
	xr_not_implemented();
}

void xr_ogf_v4::load_tree_visual_pm(xr_reader& r)
{
	load_tree_visual(r);
	if (!r.find_chunk(OGF4_SWICONTAINER))
		xr_not_expected();
	load_swicontainer(r);
	r.debug_find_chunk();
}

////////////////////////////////////////////////////////////////////////////////

void xr_ogf_v4::load_ogf(xr_reader& r)
{
	if (!r.find_chunk(OGF_HEADER))
		xr_not_expected();
	load_header(r);
	r.debug_find_chunk();

	m_flags = EOF_STATIC;
	switch (m_model_type) {
	case MT4_NORMAL:
		load_visual(r);
		break;
	case MT4_HIERRARHY:
		load_hierrarhy_visual(r);
		break;
	case MT4_PROGRESSIVE:
		load_progressive(r);
		m_flags = EOF_PROGRESSIVE;
		break;
	case MT4_SKELETON_ANIM:
		load_kinematics_animated(r);
		m_flags = EOF_DYNAMIC;
		break;
	case MT4_SKELETON_GEOMDEF_PM:
		load_skeletonx_pm(r);
		m_flags = EOF_PROGRESSIVE;
		break;
	case MT4_SKELETON_GEOMDEF_ST:
		load_skeletonx_st(r);
		break;
	case MT4_LOD:
		load_lod(r);
		m_flags = EOF_MULTIPLE_USAGE;
		break;
	case MT4_TREE_ST:
		load_tree_visual_st(r);
		m_flags = EOF_STATIC;
		break;
	case MT4_PARTICLE_EFFECT:
		load_particle_effect(r);
		break;
	case MT4_PARTICLE_GROUP:
		load_particle_group(r);
		break;
	case MT4_SKELETON_RIGID:
		load_kinematics(r);
		m_flags = EOF_DYNAMIC;
		break;
	case MT4_TREE_PM:
		load_tree_visual_pm(r);
		m_flags = EOF_PROGRESSIVE;
		break;
	default:
		xr_not_expected();
		break;
	}
	check_unhandled_chunks(r);
}

void xr_ogf_v4::load_omf(xr_reader& r)
{
	m_model_type = MT4_OMF;

	xr_reader* s = r.open_chunk(OGF4_S_SMPARAMS);
	xr_assert(s != 0);
	load_s_smparams(*s);
	assert(s->eof());
	r.close_chunk(s);

	s = r.open_chunk(OGF4_S_MOTIONS);
	xr_assert(s != 0);
	load_s_motions(*s);
	assert(s->eof());
	r.close_chunk(s);

	check_unhandled_chunks(r);
}

bool xr_ogf_v4::load_omf(const char* path)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path);
	if (r == 0)
		return false;

	bool status = true;
	try {
		load_omf(*r);
	} catch (xr_error) {
		clear();
		status = false;
	}
	fs.r_close(r);
	return status;
}
