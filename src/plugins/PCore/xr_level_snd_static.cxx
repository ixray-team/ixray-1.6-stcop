#include "xr_level_snd_static.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_ini_file.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_snd_static::~xr_level_snd_static()
{
	delete_elements(m_sound_srcs);
}

struct read_sound_src { void operator()(sound_src_data*& _sound_src, xr_reader& r) const {
	sound_src_data* sound_src = new sound_src_data;
	_sound_src = sound_src;
	bool compressed;
	size_t size = r.find_chunk(0, &compressed);
	xr_assert(size && !compressed);
	r.r_sz(sound_src->name);
	r.r_fvector3(sound_src->position);
	sound_src->volume = r.r_float();
	sound_src->freq = r.r_float();
	r.r_i32vector2(sound_src->active_time);
	r.r_i32vector2(sound_src->play_time);
	r.r_i32vector2(sound_src->pause_time);
}};

void xr_level_snd_static::load(xr_reader& r)
{
	r.r_chunks(m_sound_srcs, read_sound_src());
}

void xr_level_snd_static::load(const xr_ini_file& ini)
{
	if (!ini.section_exist("static_sounds"))
		return;
	m_sound_srcs.reserve(ini.line_count("static_sounds"));
	const char* value;
	for (size_t i = 0; ini.r_line("static_sounds", i, 0, &value); ++i) {
		char snd_name[128];
		float x, y, z;
#if defined(_MSC_VER) && _MSC_VER >= 1400
		if (4 == sscanf_s(value, "%s,%f,%f,%f", snd_name, sizeof(snd_name), &x, &y, &z)) {
#else
		// FIXME!!!
		if (4 == sscanf(value, "%s,%f,%f,%f", snd_name, &x, &y, &z)) {
#endif
			sound_src_data* sound_src = new sound_src_data;
			sound_src->name = snd_name;
			sound_src->position.set(x, y, z);
			sound_src->volume = 1.f;
			sound_src->freq = 1.f;
			sound_src->active_time.set(0, 0);
			sound_src->play_time.set(0, 0);
			sound_src->pause_time.set(0, 0);
			m_sound_srcs.push_back(sound_src);
		}
	}
}

struct write_sound_src { void operator()(const sound_src_data* sound_src, xr_writer& w) const {
	w.open_chunk(0);
	w.w_sz(sound_src->name);
	w.w_fvector3(sound_src->position);
	w.w_float(sound_src->volume);
	w.w_float(sound_src->freq);
	w.w_i32vector2(sound_src->active_time);
	w.w_i32vector2(sound_src->play_time);
	w.w_i32vector2(sound_src->pause_time);
}};

void xr_level_snd_static::save(xr_writer& w) const
{
	w.w_chunks(m_sound_srcs, write_sound_src());
}
