#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_OGF_H__
#define __XR_OGF_H__

#include <vector>
#include <string>
#include "xr_ogf_format.h"
#include "xr_geom_buf.h"
#include "xr_object.h"

namespace xray_re {

class xr_reader;

class xr_ogf: public xr_object {
public:
			xr_ogf(ogf_version version);
	virtual		~xr_ogf();

	// auto-detect version and load
	static xr_ogf*	load_ogf(const std::string& path);

	virtual void	clear();
	virtual bool	load_ogf(const char* path, const std::string& name);
	virtual void	load_ogf(xr_reader& r) = 0;
	virtual void	to_object();

	class ogf_error: public xr_error {};

	virtual bool	hierarchical() const = 0;
	virtual bool	skeletal() const = 0;
	virtual bool	animated() const = 0;
	virtual bool	progressive() const = 0;
	virtual bool	versioned() const = 0;

	const std::string&		path() const;
	ogf_version			version() const;
	ogf_model_type			model_type() const;
	const fbox&			bbox() const;
	const fsphere&			bsphere() const;
	const std::vector<xr_ogf*>&	children() const;
	const std::vector<uint32_t>&	children_l() const;
	const std::vector<xr_ogf*>&	lods() const;
	virtual const std::string&	texture() const;
	virtual const std::string&	shader() const;
	virtual uint32_t		texture_l() const = 0;
	virtual uint32_t		shader_l() const = 0;
	virtual const xr_vbuf&		vb() const;
	virtual const xr_ibuf&		ib() const;
	const ogf4_lod_face*	lod_faces() const;

	struct bone_motion_io: public xr_bone_motion {
		void	insert_key(float time, const ogf_key_qr* value);
		void	insert_key(float time, const fvector3* value);
	};

protected:
	virtual xr_surface*	create_surface(const xr_raw_surface& raw_surface) const;

	bool	is_chunk_loaded(uint32_t id) const;
	void	set_chunk_loaded(uint32_t id);
	void	check_unhandled_chunks(xr_reader& r) const;

	void	load_texture(xr_reader& r);

protected:
	uint32_t		m_loaded;	// mask of loaded chunks (mostly for debugging)
	std::string		m_path;		// empty if embedded

	ogf_version		m_version;	// OGF_HEADER
	ogf_model_type		m_model_type;
	ogf_bbox		m_bbox;		// OGF_HEADER or OGF_BBOX (v3)
	ogf_bsphere		m_bsphere;	// OGF_HEADER or OGF_BSPHERE (v3)

	std::string		m_texture;	// OGF_TEXTURE
	std::string		m_shader;

	xr_vbuf			m_vb;		// OGF_VERTICES/OGF_GCONTAINER/OGF_VCONTAINER
	xr_ibuf			m_ib;		// OGF_INDICES/OGF_GCONTAINER/OGF_ICONTAINER

	std::vector<xr_ogf*>	m_children;	// OGF_CHILDREN/OGF_CHILD_REFS
	std::vector<uint32_t>	m_children_l;	// OGF_CHILDREN_L

	std::vector<xr_ogf*>	m_lods;		// OGF_LODS and OGF_S_LODS

	ogf4_lod_face	m_lod_faces[8];	// OGF_LODDEF2
};

TYPEDEF_STD_VECTOR_PTR(xr_ogf)

inline bool xr_ogf::is_chunk_loaded(uint32_t id) const { return (m_loaded & (1 << id)) != 0; }
inline void xr_ogf::set_chunk_loaded(uint32_t id) { m_loaded |= 1 << id; }

inline const std::string& xr_ogf::path() const { return m_path; }
inline ogf_version xr_ogf::version() const { return m_version; }
inline ogf_model_type xr_ogf::model_type() const { return m_model_type; }
inline const fbox& xr_ogf::bbox() const { return m_bbox; }
inline const fsphere& xr_ogf::bsphere() const { return m_bsphere; }
inline const xr_ogf_vec& xr_ogf::children() const { return m_children; }
inline const std::vector<uint32_t>& xr_ogf::children_l() const { return m_children_l; }
inline const xr_ogf_vec& xr_ogf::lods() const { return m_lods; }
inline const std::string& xr_ogf::texture() const { return m_texture; }
inline const std::string& xr_ogf::shader() const { return m_shader; }
inline const xr_vbuf& xr_ogf::vb() const { return m_vb; }
inline const xr_ibuf& xr_ogf::ib() const { return m_ib; }
inline const ogf4_lod_face* xr_ogf::lod_faces() const { return m_lod_faces; }

} // end of namespace xray_re

#endif
