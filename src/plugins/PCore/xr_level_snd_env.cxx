#include "xr_level_snd_env.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_level_snd_env::~xr_level_snd_env() {}

void xr_level_snd_env::load(xr_reader& r)
{
	xr_reader* s = r.open_chunk(FSL_SND_ENV_IDS);
	xr_assert(s);
	while (!s->eof()) {
		m_env_ids.push_back(s->skip_sz());
	};
	r.close_chunk(s);

	s = r.open_chunk(FSL_SND_ENV_GEOMETRY);
	xr_assert(s);
	xr_cform::load(*s);
	r.close_chunk(s);

	xr_cform::generate_vertex_faces();
}

void xr_level_snd_env::save(xr_writer& w) const
{
	w.open_chunk(FSL_SND_ENV_IDS);
	w.w_seq(m_env_ids, xr_writer::f_w_sz());
	w.close_chunk();

	w.open_chunk(FSL_SND_ENV_GEOMETRY);
	xr_cform::save(w);
	w.close_chunk();
}
