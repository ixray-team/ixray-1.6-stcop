#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_OGF_V4_H__
#define __XR_OGF_V4_H__

#include <vector>
#include "xr_ogf.h"

namespace xray_re {

class xr_ogf_v4: public xr_ogf {
public:
				xr_ogf_v4();
	virtual			~xr_ogf_v4();

	virtual void		clear();
	virtual void		load_ogf(xr_reader& r);
	virtual bool		load_omf(const char* path);

	void			set_ext_geom(const xr_vbuf_vec& ext_vbufs,
						const xr_ibuf_vec& ext_ibufs,
						const xr_swibuf_vec& ext_swibufs);

	virtual bool		hierarchical() const;
	virtual bool		skeletal() const;
	virtual bool		animated() const;
	virtual bool		progressive() const;
	virtual bool		versioned() const;

	virtual const xr_ibuf&	ib() const;
	virtual uint32_t	texture_l() const;
	virtual uint32_t	shader_l() const;
	const fmatrix&		xform() const;
//	const ogf4_lod_face*	lod_faces() const;

	uint32_t		ext_vb_index() const;
	uint32_t		ext_vb_offset() const;
	uint32_t		ext_vb_size() const;

	uint32_t		ext_ib_index() const;
	uint32_t		ext_ib_offset() const;
	uint32_t		ext_ib_size() const;

	uint32_t		ext_swib_index() const;

public:
	struct bone_io;
	struct partition_io;
	struct motion_io;
	struct bone_motion_io;

protected:
	void	load_omf(xr_reader& r);
	void	load_header(xr_reader& r);
	void	load_texture(xr_reader& r);
	void	load_vertices(xr_reader& r);
	void	load_p_map(xr_reader& r);
	void	load_indices(xr_reader& r);
	void	load_swidata(xr_reader& r);
	void	load_vcontainer(xr_reader& r);
	void	load_icontainer(xr_reader& r);
	void	load_children(xr_reader& r);
	void	load_children_l(xr_reader& r);
	void	load_loddef2(xr_reader& r);
	void	load_treedef2(xr_reader& r);
	void	load_s_bone_names(xr_reader& r);
	void	load_s_smparams(xr_reader& r);
	void	load_s_motions(xr_reader& r);
	void	load_s_ikdata(xr_reader& r);
	void	load_s_userdata(xr_reader& r);
	void	load_s_desc(xr_reader& r);
	void	load_s_motion_refs_0(xr_reader& r);
	void	load_s_motion_refs_1(xr_reader& r);
	void	load_swicontainer(xr_reader& r);
	void	load_gcontainer(xr_reader& r);
	void	load_fastpath(xr_reader& r);
	void	load_s_lods(xr_reader& r);

private:
	void	load_render_visual(xr_reader& r);
	void	load_visual(xr_reader& r);
	void	load_hierrarhy_visual(xr_reader& r);
	void	load_progressive(xr_reader& r);
	void	load_kinematics_animated(xr_reader& r);
	void	load_kinematics(xr_reader& r);
	void	load_skeletonx(xr_reader& r);
	void	load_skeletonx_pm(xr_reader& r);
	void	load_skeletonx_st(xr_reader& r);
	void	load_lod(xr_reader& r);
	void	load_tree_visual(xr_reader& r);
	void	load_tree_visual_st(xr_reader& r);
	void	load_particle_effect(xr_reader& r);
	void	load_particle_group(xr_reader& r);
	void	load_tree_visual_pm(xr_reader& r);

	void	setup_ib0();

private:
	uint32_t	m_shader_id;	// OGF_HEADER

	xr_ibuf		m_ib0;		// proxy ib according to swi0
	xr_swibuf	m_swib;		// OGF_SWIDATA or OGF_SWICONTAINER

	//ogf4_lod_face	m_lod_faces[8];	// OGF_LODDEF2

	ogf4_5color	m_c_scale;	// OGF_TREEDEF2
	ogf4_5color	m_c_bias;
	fmatrix		m_tree_xform;

	int		m_fixme;	// OGF_P_MAP chunk

	std::string	m_source;	// OGF_S_DESC fields which do not fit in xr_object
	std::string	m_export_tool;
	uint32_t	m_export_time;

	xr_ogf*		m_fast;		// OGF_FASTPATH chunk;

	std::string	m_s_lods;	// Clear Sky version of OGF_S_LODS

	uint32_t	m_ext_vb_index;		// OGF_VCONTAINER
	uint32_t	m_ext_vb_offset;
	uint32_t	m_ext_vb_size;

	uint32_t	m_ext_ib_index;		// OGF_ICONTAINER
	uint32_t	m_ext_ib_offset;
	uint32_t	m_ext_ib_size;

	uint32_t	m_ext_swib_index;	// OGF_SWICONTAINER
};

TYPEDEF_STD_VECTOR_PTR(xr_ogf_v4)

inline const xr_ibuf& xr_ogf_v4::ib() const { return progressive() ? m_ib0 : m_ib; }
inline uint32_t xr_ogf_v4::texture_l() const { return m_shader_id; }
inline uint32_t xr_ogf_v4::shader_l() const { return m_shader_id; }
inline const fmatrix& xr_ogf_v4::xform() const { return m_tree_xform; }
//inline const ogf4_lod_face* xr_ogf_v4::lod_faces() const { return m_lod_faces; }

inline uint32_t xr_ogf_v4::ext_vb_index() const { return m_ext_vb_index; }
inline uint32_t xr_ogf_v4::ext_vb_offset() const { return m_ext_vb_offset; }
inline uint32_t xr_ogf_v4::ext_vb_size() const { return m_ext_vb_size; }
inline uint32_t xr_ogf_v4::ext_ib_index() const { return m_ext_ib_index; }
inline uint32_t xr_ogf_v4::ext_ib_offset() const { return m_ext_ib_offset; }
inline uint32_t xr_ogf_v4::ext_ib_size() const { return m_ext_ib_size; }
inline uint32_t xr_ogf_v4::ext_swib_index() const { return m_ext_swib_index; }

} // end of namespace xray_re

#endif
