#include <algorithm>
#include "xr_gamemtls_lib.h"
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

xr_gamemtls_lib::~xr_gamemtls_lib()
{
	delete_elements(m_materials);
	delete_elements(m_material_pairs);
}

void xr_gamemtl::load(xr_reader& r)
{
	if (!r.find_chunk(GAMEMTL_CHUNK_MAIN))
		xr_not_expected();
	id = r.r_u32();
	r.r_sz(name);
	r.debug_find_chunk();

	if (r.find_chunk(GAMEMTL_CHUNK_DESC)) {
		r.r_sz(desc);
		r.debug_find_chunk();
	}

	if (!r.find_chunk(GAMEMTL_CHUNK_FLAGS))
		xr_not_expected();
	flags = r.r_u32();
	r.debug_find_chunk();

	if (!r.find_chunk(GAMEMTL_CHUNK_PHYSICS))
		xr_not_expected();
	friction = r.r_float();
	damping = r.r_float();
	spring = r.r_float();
	bouncing_start_velocity = r.r_float();
	bounce = r.r_float();
	r.debug_find_chunk();

	if (!r.find_chunk(GAMEMTL_CHUNK_FACTORS))
		xr_not_expected();
	shoot_factor = r.r_float();
	bounce_damage_factor = r.r_float();
	vis_transparency_factor = r.r_float();
	snd_occlusion_factor = r.r_float();
	r.debug_find_chunk();

	//in build 1580 not present
	if (!r.r_chunk<float>(GAMEMTL_CHUNK_FLOTATION, flotation_factor))
		flotation_factor = 1.0f;

	//in build 1865 not present
	if (!r.r_chunk<float>(GAMEMTL_CHUNK_INJURY, injurious_speed))
		injurious_speed = 0.0f;
}

void xr_gamemtlpair::load(xr_reader& r)
{
	if (!r.find_chunk(GAMEMTLPAIR_CHUNK_PAIR))
		xr_not_expected();
	mtl0 = r.r_u32();
	mtl1 = r.r_u32();
	id = r.r_u32();
	id_parent = r.r_u32();
	own_props = r.r_u32();
	r.debug_find_chunk();

	if (!r.find_chunk(GAMEMTLPAIR_CHUNK_BREAKING))
		xr_not_expected();
	r.r_sz(breaking_sounds);
	r.debug_find_chunk();

	if (!r.find_chunk(GAMEMTLPAIR_CHUNK_STEP))
		xr_not_expected();
	r.r_sz(step_sounds);
	r.debug_find_chunk();

	if (!r.find_chunk(GAMEMTLPAIR_CHUNK_COLLIDE))
		xr_not_expected();
	r.r_sz(collide_sounds);
	r.r_sz(collide_particles);
	r.r_sz(collide_marks);
	r.debug_find_chunk();
}

void xr_gamemtls_lib::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk<uint16_t>(GAMEMTLS_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == GAMEMTLS_VERSION);

	if (!r.find_chunk(GAMEMTLS_CHUNK_AUTOINC))
		xr_not_expected();
	m_material_index = r.r_u32();
	m_material_pair_index = r.r_u32();
	r.debug_find_chunk();

	xr_reader* s = r.open_chunk(GAMEMTLS_CHUNK_MATERIALS);
	if (s) {
		xr_reader* f;
		for (uint32_t id = 0; (f = s->open_chunk(id)); ++id) {
			xr_gamemtl* material = new xr_gamemtl;
			material->load(*f);
			m_materials.push_back(material);
			s->close_chunk(f);
		}
		r.close_chunk(s);
	}
	s = r.open_chunk(GAMEMTLS_CHUNK_MATERIAL_PAIRS);
	if (s) {
		xr_reader* f;
		for (uint32_t id = 0; (f = s->open_chunk(id)); ++id) {
			xr_gamemtlpair* gamemtlpair = new xr_gamemtlpair;
			gamemtlpair->load(*f);
			m_material_pairs.push_back(gamemtlpair);
			s->close_chunk(f);
		}
		r.close_chunk(s);
	}
}

bool xr_gamemtls_lib::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}

bool xr_gamemtls_lib::load(const std::string& path)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}

struct id_pred {
#if 1
	bool operator()(const xr_gamemtl* l, const xr_gamemtl* r) const { return l->id < r->id; }
#else
	uint32_t id;
	explicit id_pred(uint32_t _id): id(_id) {}
	bool operator()(const xr_gamemtl* l) const { return l->id < id; }
#endif
};

const xr_gamemtl* xr_gamemtls_lib::get_material(uint32_t id) const
{
#if 1
	xr_gamemtl pattern;
	pattern.id = id;
	xr_gamemtl_vec_cit it = std::lower_bound(m_materials.begin(), m_materials.end(), &pattern, id_pred());
#else
	xr_gamemtl_vec_cit it = lower_bound_if(m_materials.begin(), m_materials.end(), id_pred(id));
#endif
	return (it == m_materials.end() || (*it)->id != id) ? 0 : *it;
}

const xr_gamemtl* xr_gamemtls_lib::get_material(const char* name) const
{
	for (xr_gamemtl_vec_cit it = m_materials.begin(), end = m_materials.end(); it != end; ++it) {
		if ((*it)->name == name)
			return *it;
	}
	return 0;
}
