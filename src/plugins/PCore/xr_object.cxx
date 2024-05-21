#include <algorithm>
#include "xr_object.h"
#include "xr_object_format.h"
#include "xr_motion.h"
#include "xr_bone.h"
#include "xr_mesh.h"
#include "xr_surface.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_file_system.h"
#include "xr_string_utils.h"
#include "xr_utils.h"
#include "xr_name_gen.h"

using namespace xray_re;

xr_object::xr_object(): m_flags(EOF_STATIC),
	m_owner_name("unknown"), m_creation_time(0),
	m_modif_name("unknown"), m_modified_time(0),
	m_surface_factory(0)
{
	m_position.set();
	m_rotation.set();
}

xr_object::xr_object(const xr_surface_factory* surface_factory):
	m_flags(EOF_STATIC),
	m_owner_name("unknown"), m_creation_time(0),
	m_modif_name("unknown"), m_modified_time(0),
	m_surface_factory(surface_factory)
{
	m_position.set();
	m_rotation.set();
}

xr_object::~xr_object()
{
	delete_elements(m_surfaces);
	delete_elements(m_meshes);
	delete_elements(m_bones);
	delete_elements(m_motions);
	delete_elements(m_partitions);
}

void xr_object::clear()
{
	m_flags = EOF_STATIC;
	m_owner_name = "unknown";
	m_creation_time = 0;
	m_modif_name = "unknown";
	m_modified_time = 0;
	clear_container(m_surfaces);
	clear_container(m_meshes);
	clear_container(m_bones);
	clear_container(m_motions);
	clear_container(m_partitions);
}

void xr_object::setup_bones()
{
	uint16_t bone_id = 0;
	const xr_bone* root = NULL;
	for (xr_bone_vec_it it = m_bones.begin(), end = m_bones.end();
			it != end; ++it, ++bone_id) {
		(*it)->setup(bone_id, *this);
		if ((*it)->parent() == NULL) {
			if (root != NULL)
				xr_assert(!"Object contains more than one skeleton");
			root = *it;
		}
	}
}

void xr_object::calculate_bind()
{
	for (xr_bone_vec_it it = m_bones.begin(), end = m_bones.end(); it != end; ++it) {
		if ((*it)->is_root()) {
			(*it)->calculate_bind(fmatrix().identity());
			break;
		}
	}
}

const xr_bone* xr_object::root_bone() const
{
	for (xr_bone_vec_cit it = m_bones.begin(), end = m_bones.end(); it != end; ++it) {
		if ((*it)->is_root())
			return *it;
	}
	return 0;
}

void xr_object::setup_partitions()
{
	uint16_t part_id = 0;
	for (xr_partition_vec_it it = m_partitions.begin(),
			end = m_partitions.end(); it != end; ++it) {
		(*it)->setup(part_id++);
	}
}

struct read_mesh {
	xr_object& object;
	read_mesh(xr_object& _object): object(_object) {}
	void operator()(xr_mesh*& mesh, xr_reader& r) {
		mesh = new xr_mesh;
		mesh->load(r, object);
	}
};

struct read_partition_0 {
	xr_bone_vec& all_bones;
	read_partition_0(xr_bone_vec& _all_bones): all_bones(_all_bones) {}
	void operator()(xr_partition*& part, xr_reader& r) {
		part = new xr_partition;
		part->load_0(r, all_bones);
	}
};

void xr_object::load_object(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk<uint16_t>(EOBJ_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == EOBJ_VERSION);

	if (!r.r_chunk<uint32_t>(EOBJ_CHUNK_FLAGS, m_flags))
		xr_not_expected();

	if (r.find_chunk(EOBJ_CHUNK_USERDATA)) {
		r.r_sz(m_userdata);
		r.debug_find_chunk();
	}

	if (r.find_chunk(EOBJ_CHUNK_LOD_REF)) {
		r.r_sz(m_lod_ref);
		r.debug_find_chunk();
	}

	if (r.find_chunk(EOBJ_CHUNK_SURFACES_2)) {
		r.r_seq(r.r_u32(), m_surfaces, xr_reader::f_r_new<xr_surface>(&xr_surface::load_2));
		r.debug_find_chunk();
	} else if (r.find_chunk(EOBJ_CHUNK_SURFACES_1)) {
		r.r_seq(r.r_u32(), m_surfaces, xr_reader::f_r_new<xr_surface>(&xr_surface::load_1));
		r.debug_find_chunk();
	} else if (r.find_chunk(EOBJ_CHUNK_SURFACES_0)) {
		r.r_seq(r.r_u32(), m_surfaces, xr_reader::f_r_new<xr_surface>(&xr_surface::load_0));
		r.debug_find_chunk();
		if (r.find_chunk(EOBJ_CHUNK_SHADERS_0)) {
			for (xr_surface_vec_it it = m_surfaces.begin(), end = m_surfaces.end();
					it != end; ++it) {
				(*it)->cshader() = r.skip_sz();
			}
			r.debug_find_chunk();
		}
	} else {
		xr_not_expected();
	}

	xr_reader* s = r.open_chunk(EOBJ_CHUNK_MESHES);
	if (s) {
		s->r_chunks(m_meshes, read_mesh(*this));
		r.close_chunk(s);
	}

	s = r.open_chunk(EOBJ_CHUNK_BONES_1);
	if (s) {
		s->r_chunks(m_bones, xr_reader::f_r_new<xr_bone>(&xr_bone::load_1));
		r.close_chunk(s);
	} else if (r.find_chunk(EOBJ_CHUNK_BONES_0)) {
		s->r_seq(r.r_u32(), m_bones, xr_reader::f_r_new<xr_bone>(&xr_bone::load_0));
		r.debug_find_chunk();
	}
	setup_bones();
	if (r.find_chunk(EOBJ_CHUNK_MOTIONS)) {
		r.r_seq(r.r_u32(), m_motions, xr_reader::f_r_new<xr_skl_motion>(&xr_skl_motion::load));
		r.debug_find_chunk();
	}
	if (r.find_chunk(EOBJ_CHUNK_MOTION_REFS)) {
		r.r_sz(m_motion_refs);
		r.debug_find_chunk();
	}
	if (r.find_chunk(EOBJ_CHUNK_PARTITIONS_0)) {
		r.r_seq(r.r_u32(), m_partitions, read_partition_0(m_bones));
		r.debug_find_chunk();
	} else if (r.find_chunk(EOBJ_CHUNK_PARTITIONS_1)) {
		r.r_seq(r.r_u32(), m_partitions, xr_reader::f_r_new<xr_partition>(&xr_partition::load_1));
		r.debug_find_chunk();
	}
	if (r.find_chunk(EOBJ_CHUNK_TRANSFORM)) {
		r.r_fvector3(m_position);
		r.r_fvector3(m_rotation);
		r.debug_find_chunk();
	}
	if (r.find_chunk(EOBJ_CHUNK_REVISION)) {
		r.r_sz(m_owner_name);
		m_creation_time = r.r_u32();
		r.r_sz(m_modif_name);
		m_modified_time = r.r_u32();
		r.debug_find_chunk();
	}
}

bool xr_object::load_object(const char* path)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path);
	if (r == 0)
		return false;
	bool status = false;
	xr_reader* s = r->open_chunk(EOBJ_CHUNK_MAIN);
	if (s) {
		try {
			load_object(*s);
			status = true;
		} catch (xr_error) {
			clear();
		}
		r->close_chunk(s);
	}
	fs.r_close(r);
	return status;
}

void xr_object::save_object(xr_writer& w) const
{
	w.w_chunk<uint16_t>(EOBJ_CHUNK_VERSION, EOBJ_VERSION);

	w.w_chunk(EOBJ_CHUNK_USERDATA, m_userdata);

	w.w_chunk(EOBJ_CHUNK_LOD_REF, m_lod_ref);

	w.w_chunk<uint32_t>(EOBJ_CHUNK_FLAGS, m_flags);

	w.open_chunk(EOBJ_CHUNK_MESHES);
	w.w_chunks(m_meshes, xr_writer::f_w_const<xr_mesh>(&xr_mesh::save));
	w.close_chunk();

	w.open_chunk(EOBJ_CHUNK_SURFACES_2);
	w.w_size_u32(m_surfaces.size());
	w.w_seq(m_surfaces, xr_writer::f_w_const<xr_surface>(&xr_surface::save));
	w.close_chunk();

	if (!m_bones.empty()) {
		w.open_chunk(EOBJ_CHUNK_BONES_1);
		w.w_chunks(m_bones, xr_writer::f_w_const<xr_bone>(&xr_bone::save));
		w.close_chunk();
	}
	if (!m_motions.empty()) {
		w.open_chunk(EOBJ_CHUNK_MOTIONS);
		w.w_size_u32(m_motions.size());
		w.w_seq(m_motions, xr_writer::f_w_const<xr_skl_motion>(&xr_skl_motion::save));
		w.close_chunk();
	}
	if (!m_motion_refs.empty()) {
		w.w_chunk(EOBJ_CHUNK_MOTION_REFS, m_motion_refs);
	}
	if (!m_partitions.empty()) {
		w.open_chunk(EOBJ_CHUNK_PARTITIONS_1);
		w.w_size_u32(m_partitions.size());
		w.w_seq(m_partitions, xr_writer::f_w_const<xr_partition>(&xr_partition::save));
		w.close_chunk();
	}
	if (m_flags & EOF_DYNAMIC) {
		w.open_chunk(EOBJ_CHUNK_TRANSFORM);
		w.w_fvector3(m_position);
		w.w_fvector3(m_rotation);
		w.close_chunk();
	}

	w.open_chunk(EOBJ_CHUNK_REVISION);
	w.w_sz(m_owner_name);
	w.w_u32(m_creation_time);
	w.w_sz(m_modif_name);
	w.w_u32(m_modified_time);
	w.close_chunk();
}

bool xr_object::save_object(const char* path, compress_options compress) const
{
	xr_memory_writer w;
	save_object(w);

	xr_memory_writer file;
	file.w_raw_chunk(EOBJ_CHUNK_MAIN, w.data(), w.tell(), compress != compress_options::none);

	bool status = file.save_to(path);
	return status;
}

bool xr_object::save_object(const char* path, const std::string& name, compress_options compress) const
{
	xr_memory_writer w;
	save_object(w);

	xr_memory_writer file;
	file.w_raw_chunk(EOBJ_CHUNK_MAIN, w.data(), w.tell(), compress != compress_options::none);

	bool status = file.save_to(path, name);
	return status;
}

void xr_object::load_bones(xr_reader& r)
{
	r.r_chunks(m_bones, xr_reader::f_r_new<xr_bone>(&xr_bone::load_data));
	if (r.find_chunk(EOBJ_CHUNK_PARTITIONS_1)) {
		r.r_seq(r.r_u32(), m_partitions, xr_reader::f_r_new<xr_partition>(&xr_partition::load_1));
		r.debug_find_chunk();
	}
}

bool xr_object::load_bones(const char* path)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path);
	if (r == 0)
		return false;
	bool status = false;
	try {
		load_bones(*r);
		status = true;
	} catch (xr_error) {
		clear();
	}
	fs.r_close(r);
	return status;
}

void xr_object::save_bones(xr_writer& w) const
{
	w.w_chunks(m_bones, xr_writer::f_w_const<xr_bone>(&xr_bone::save_data));
	w.open_chunk(EOBJ_CHUNK_PARTITIONS_1);
	w.w_size_u32(m_partitions.size());
	w.w_seq(m_partitions, xr_writer::f_w_const<xr_partition>(&xr_partition::save));
	w.close_chunk();
}

bool xr_object::save_bones(const char* path) const
{
	xr_memory_writer* w = new xr_memory_writer();
	save_bones(*w);
	bool status = w->save_to(path);
	delete w;
	return status;
}

void xr_object::load_skls(xr_reader& r)
{
	r.r_seq(r.r_u32(), m_motions, xr_reader::f_r_new<xr_skl_motion>(&xr_skl_motion::load));
}

bool xr_object::load_skls(const char* path)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path);
	if (r == 0)
		return false;
	bool status = false;
	try {
		load_skls(*r);
		status = true;
	} catch (xr_error) {
		clear();
	}
	fs.r_close(r);
	return status;
}

void xr_object::save_skls(xr_writer& w) const
{
	w.w_size_u32(m_motions.size());
	w.w_seq(m_motions, xr_writer::f_w_const<xr_skl_motion>(&xr_skl_motion::save));
}

bool xr_object::save_skls(const char* path) const
{
	xr_memory_writer* w = new xr_memory_writer();
	save_skls(*w);
	bool status = w->save_to(path);
	delete w;
	return status;
}

struct surface_name_pred { bool operator()(const xr_surface* l, const xr_surface* r) const {
	return l->name() < r->name();
}};

void xr_object::denominate_surfaces()
{
	for (xr_surface_vec_it it = m_surfaces.begin(), end = m_surfaces.end(); it != end; ++it) {
		xr_surface* surface = *it;
		const std::string& texture = surface->texture();
		std::string::size_type pos = texture.rfind('\\');
		if (pos == std::string::npos)
			pos = 0;
		else
			++pos;
		std::string& name = surface->name();
		name.reserve(texture.size() - pos + 3);
		for (std::string::const_iterator it1 = texture.begin() + pos,
				end1 = texture.end(); it1 != end1; ++it1) {
			int c = *it1;
			name.push_back(std::isalnum(c) ? char(std::tolower(c)) : '_');
		}
		name += "_S";
	}
	std::sort(m_surfaces.begin(), m_surfaces.end(), surface_name_pred());
	char suffix[8];
	for (xr_surface_vec_it it = m_surfaces.begin(), end = m_surfaces.end(); it != end;) {
		const std::string& name = (*it)->name();
		for (unsigned i = 1; ++it != end && (*it)->name() == name; ++i) {
			xr_snprintf(suffix, sizeof(suffix), "%u", i);
			(*it)->name() += suffix;
		}
	}
}

void xr_object::denominate()
{
	denominate_surfaces();
	xr_name_gen name("mesh", false);
	for (xr_mesh_vec_it it = m_meshes.begin(), end = m_meshes.end(); it != end; ++it, name.next())
		(*it)->name().assign(name.get()).append("Shape");
}

void xr_object::to_object() {}

xr_surface* xr_object::attach(const xr_raw_surface& raw_surface)
{
	xr_surface*& surface = m_raw_surfaces[raw_surface];
	if (surface == 0) {
		surface = m_surface_factory ?
				m_surface_factory->create_surface(raw_surface) :
				create_surface(raw_surface);
		xr_assert(surface);
		m_surfaces.push_back(surface);
	}
	return surface;
}

xr_surface* xr_object::create_surface(const xr_raw_surface& raw_surface) const { return 0; }

xr_surface* xr_object::find_surface(const std::string& name) { return find_by_name(m_surfaces, name); }
xr_mesh* xr_object::find_mesh(const std::string& name) { return find_by_name(m_meshes, name); }
xr_bone* xr_object::find_bone(const std::string& name) { return find_by_name(m_bones, name); }
xr_partition* xr_object::find_partition(const std::string& name) { return find_by_name(m_partitions, name); }
xr_skl_motion* xr_object::find_motion(const std::string& name) { return find_by_name(m_motions, name); }
