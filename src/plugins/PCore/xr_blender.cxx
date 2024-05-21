#include "xr_blender.h"

using namespace xray_re;

#define MKCLSID(w1, w2)		((uint64_t(w1) << 32) | (w2))

xr_blender* create_blender(uint64_t clsid)
{
	switch (clsid) {
	case MKCLSID('BmmD', 'old '):
		return new xr_blender_bmmd;
	case MKCLSID('BLUR', '    '):
		return new xr_blender_blur;
	case MKCLSID('D_ST', 'ILL '):
		return new xr_blender_detail_still;
	case MKCLSID('D_TR', 'EE  '):
		return new xr_blender_tree;
	case MKCLSID('E_SE', 'L   '):
		return new xr_blender_editor_selection;
	case MKCLSID('LM  ', '    '):
		return new xr_blender_default;
	case MKCLSID('E_WI', 'RE  '):
		return new xr_blender_editor_wire;
	case MKCLSID('LM_A', 'REF '):
		return new xr_blender_default_aref;
	case MKCLSID('LaEm', 'B   '):
		return new xr_blender_la_em_b;
	case MKCLSID('LmEb', 'B   '):
		return new xr_blender_lm_eb_b;
	case MKCLSID('MODE', 'LEbB'):
		return new xr_blender_model_eb_b;
	case MKCLSID('MODE', 'L   '):
		return new xr_blender_model;
	case MKCLSID('PART', 'ICLE'):
		return new xr_blender_particle;
	case MKCLSID('SH_W', 'ORLD'):
		return new xr_blender_sh_world;
	case MKCLSID('S_GR', 'AY  '):
		return new xr_blender_screen_gray;
	case MKCLSID('V_AR', 'EF  '):
		return new xr_blender_vertex_aref;
	case MKCLSID('V   ', '    '):
		return new xr_blender_vertex;
	case MKCLSID('S_SE', 'T   '):
		return new xr_blender_screen_set;
#if 0
	case MKCLSID('', ''):
		return new xr_blender_;
	case MKCLSID('', ''):
		return new xr_blender_;
	case MKCLSID('', ''):
		return new xr_blender_;
#endif
	};
	return 0;
}

////////////////////////////////////////////////////////////////////////////////

xr_blender::~xr_blender() {}

void xr_blender::load(xr_reader& r)
{
}

void xr_blender::save(xr_writer& w) const
{
}

bool xr_blender::can_be_detailed() const { return false; }
bool xr_blender::can_be_lmapped() const { return false; }

const char* xr_blender_bmmd::comment() const { return "LEVEL: implicit**detail"; }
bool xr_blender_bmmd::can_be_detailed() const { return true; }
bool xr_blender_bmmd::can_be_lmapped() const { return true; }

const char* xr_blender_blur::comment() const { return "INTERNAL: blur"; }

const char* xr_blender_detail_still::comment() const { return "LEVEL: detail objects"; }

const char* xr_blender_tree::comment() const { return "LEVEL: trees/bushes"; }
bool xr_blender_tree::can_be_detailed() const { return true; }

const char* xr_blender_editor_selection::comment() const { return "EDITOR: selection"; }

const char* xr_blender_default::comment() const { return "LEVEL: lmap*base (default)"; }
bool xr_blender_default::can_be_detailed() const { return true; }
bool xr_blender_default::can_be_lmapped() const { return true; }

const char* xr_blender_editor_wire::comment() const { return "EDITOR: wire"; }

const char* xr_blender_default_aref::comment() const { return "LEVEL: lmap*base.aref"; }
bool xr_blender_default_aref::can_be_detailed() const { return true; }
bool xr_blender_default_aref::can_be_lmapped() const { return true; }

const char* xr_blender_la_em_b::comment() const { return "LEVEL: (lmap+env*const)*base"; }
bool xr_blender_la_em_b::can_be_lmapped() const { return true; }

const char* xr_blender_lm_eb_b::comment() const { return "LEVEL: lmap*(env^base)"; }
bool xr_blender_lm_eb_b::can_be_lmapped() const { return true; }

const char* xr_blender_model_eb_b::comment() const { return "MODEL: env^base"; }

const char* xr_blender_model::comment() const { return "MODEL: default"; }

const char* xr_blender_particle::comment() const { return "particles"; }

const char* xr_blender_sh_world::comment() const { return "INTERNAL: shadow projecting"; }

const char* xr_blender_screen_gray::comment() const { return "INTERNAL: gray-scale effect"; }

const char* xr_blender_vertex_aref::comment() const { return "LEVEL: diffuse*base.aref"; }

const char* xr_blender_vertex::comment() const { return "LEVEL: diffuse*base"; }
bool xr_blender_vertex::can_be_detailed() const { return true; }

const char* xr_blender_screen_set::comment() const { return "basic (simple)"; }
