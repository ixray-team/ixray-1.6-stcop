#include <algorithm>
#include "xr_math.h"
#include "xr_ogf.h"
#include "xr_ogf_v3.h"
#include "xr_ogf_v4.h"
#include "xr_mesh_builder.h"
#include "xr_utils.h"
#include "xr_envelope.h"
#include "xr_file_system.h"

using namespace xray_re;

void xr_ogf::bone_motion_io::insert_key(float time, const ogf_key_qr* value)
{
	dquaternion q;
	value->dequantize(q);
	dmatrix xform;
	xform.rotation(q);
	dvector3 r;
	xform.get_xyz_i(r);
	m_envelopes[4]->insert_key(time, float(r.x));
	m_envelopes[3]->insert_key(time, float(r.y));
	m_envelopes[5]->insert_key(time, float(r.z));
}

void xr_ogf::bone_motion_io::insert_key(float time, const fvector3* value)
{
	m_envelopes[0]->insert_key(time, value->x);
	m_envelopes[1]->insert_key(time, value->y);
	m_envelopes[2]->insert_key(time, value->z);
}

////////////////////////////////////////////////////////////////////////////////

xr_ogf::xr_ogf(ogf_version version): m_loaded(0), m_version(version) {}

xr_ogf::~xr_ogf()
{
	delete_elements(m_children);
	delete_elements(m_lods);
}

void xr_ogf::clear()
{
	xr_object::clear();
	m_loaded = 0;
	m_path.clear();
	m_bbox.null();
	m_bsphere.reset();
	m_texture.clear();
	m_shader.clear();
	m_vb.clear();
	m_ib.clear();
	trim_container(m_children);
	trim_container(m_lods);
	m_children_l.clear();
}

xr_ogf* xr_ogf::load_ogf(const std::string& path)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path);
	if (r == 0)
		return 0;

	xr_ogf* ogf = 0;
	if (!r->find_chunk(OGF_HEADER))
		xr_not_expected();

	unsigned version = r->r_u8();
	if (version == OGF3_VERSION) {
		ogf = new xr_ogf_v3();
	} else if (version == OGF4_VERSION) {
		ogf = new xr_ogf_v4();
	} else {
		msg("load_ogf: unknown version %u", version);
	}
	if (ogf) {
		try {
			ogf->m_path = path;
			ogf->load_ogf(*r);
		} catch (xr_error) {
			delete ogf;
			ogf = 0;
		}
	}
	fs.r_close(r);
	return ogf;
}

bool xr_ogf::load_ogf(const char* path, const std::string& name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load_ogf(*r);
	fs.r_close(r);
	return true;
}

void xr_ogf::check_unhandled_chunks(xr_reader& r) const
{
	// FIXME: there is no need to "open" chunks
	uint32_t id;
	for (xr_reader* s = 0; (s = r.open_chunk_next(id, s)) != 0;) {
		if (!is_chunk_loaded(id))
			msg("unhandled chunk %u", id);
	}
}

void xr_ogf::load_texture(xr_reader& r)
{
	r.r_sz(m_texture);
	r.r_sz(m_shader);
}

xr_surface* xr_ogf::create_surface(const xr_raw_surface& raw_surface) const
{
	xr_surface* surface = new xr_surface(true);
	if (hierarchical()) {
		surface->texture() = m_children.at(raw_surface.texture)->texture();
		surface->eshader() = m_children.at(raw_surface.eshader)->shader();
	} else {
		surface->texture() = texture();
		surface->eshader() = shader();
	}
	if (raw_surface.two_sided())
		surface->set_two_sided();
	return surface;
}

void xr_ogf::to_object()
{
	xr_mesh_builder* mesh = new xr_mesh_builder;
	if (hierarchical()) {
		size_t vb_reserve = 0, ib_reserve = 0;
		unsigned vb_signature = 0;
		for (xr_ogf_vec_it it = m_children.begin(), end = m_children.end(); it != end; ++it) {
			xr_ogf* ogf = *it;
			vb_signature |= ogf->vb().signature();
			vb_reserve += ogf->vb().size();
			ib_reserve += ogf->ib().size();
			if (ogf->progressive())
				m_flags |= EOF_PROGRESSIVE;
		}
		mesh->prepare(vb_signature, vb_reserve, ib_reserve);
		uint16_t shader_id = 0;
		for (xr_ogf_vec_it it = m_children.begin(), end = m_children.end(); it != end; ++it) {
			xr_ogf* ogf = *it;
			mesh->push(ogf->vb(), ogf->ib(), shader_id, shader_id);
			++shader_id;
		}
	} else {
		mesh->prepare(vb().signature());
		mesh->push(vb(), ib(), 0, 0);
	}
	mesh->compact_geometry();
	mesh->remove_duplicate_faces();
	mesh->remove_back_faces();
	mesh->commit(*this);
	if (m_path.empty()) {
		mesh->name() = "ogfShape";
	} else {
		xr_file_system::split_path(m_path, 0, &mesh->name());
		mesh->name() += "Shape";
	}
	denominate_surfaces();
	trim_container(m_children);
	trim_container(m_lods);
}
