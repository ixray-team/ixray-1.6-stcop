#include "stdafx.h"
#include "DetailManager.h"

// Precomputed detail-layers bake: persists the FMB noise field and the cluster/perlin
// asset field to $level$\detail_layers\ so a reload with unchanged detail settings skips
// the (CPU-heavy) noise regeneration.
//
// File set (engine writes 1-3; the IMGui paint tool (step 3) adds 4-5 later):
//   last_detail_settings.txt                    - human readable settings + settings crc
//   precomputed_detail_fmb_noise.bin            - fmb_field: float[(sx+1)*(sz+1)]
//   precomputed_detail_perlin_scale.bin         - cluster_field u8[sx*sz] + cluster_rnd_field u8[sx*sz]
//   user_detail_fmb_noise_mask.bin              - sparse override list for fmb_field (u32 idx, float)
//   user_detail_perlin_scale_mask.bin          - sparse override list for cluster fields (u32 idx, u8)
//
// All I/O is best-effort like the rest of the engine: a read-only $level$ folder simply
// regenerates the fields in memory every load (CFileWriter logs and no-ops on failure).

#ifndef _EDITOR

// Console vars live in the render DLL (xrRender_console.cpp). Same flat-extern pattern
// as DetailManager_Decompress.cpp. Declared at file scope (NOT inside the anonymous
// namespace below): an extern inside an anonymous namespace would get internal linkage.
extern bool		ps_r__detail_use_alternative_tree_assets;
extern bool		ps_r__detail_use_cluster_mix_tree_assets;
extern float	ps_r__detail_cluster_seed;
extern float	ps_r__detail_cluster_patch_size_min;
extern float	ps_r__detail_cluster_patch_size_max;
extern float	ps_r__detail_cluster_sharpness;
extern float	ps_r__detail_cluster_warp_min;
extern float	ps_r__detail_cluster_warp_max;
extern bool		ps_r__detail_fmb_use_layer_1;
extern bool		ps_r__detail_fmb_use_layer_2;
extern bool		ps_r__detail_fmb_use_layer_3;
extern float	ps_r__detail_fmb_layer_1_frequency;
extern float	ps_r__detail_fmb_layer_1_amplitude;
extern float	ps_r__detail_fmb_layer_1_seed;
extern float	ps_r__detail_fmb_layer_1_power;
extern float	ps_r__detail_fmb_layer_2_frequency;
extern float	ps_r__detail_fmb_layer_2_amplitude;
extern float	ps_r__detail_fmb_layer_2_seed;
extern float	ps_r__detail_fmb_layer_2_power;
extern float	ps_r__detail_fmb_layer_3_frequency;
extern float	ps_r__detail_fmb_layer_3_amplitude;
extern float	ps_r__detail_fmb_layer_3_seed;
extern float	ps_r__detail_fmb_layer_3_power;

namespace
{
constexpr u32 DL_MK_FMB  = u32('D') | (u32('F') << 8) | (u32('M') << 16) | (u32('B') << 24);
constexpr u32 DL_MK_PER  = u32('D') | (u32('P') << 8) | (u32('E') << 16) | (u32('R') << 24);
constexpr u32 DL_VERSION = 1;

constexpr const char* DL_SETTINGS_FILE = "detail_layers\\last_detail_settings.txt";
constexpr const char* DL_FMB_FILE      = "detail_layers\\precomputed_detail_fmb_noise.bin";
constexpr const char* DL_PERLIN_FILE   = "detail_layers\\precomputed_detail_perlin_scale.bin";

#pragma pack(push, 1)
struct DL_Header
{
	u32 magic;
	u32 version;
	u32 size_x;
	u32 size_z;
	s32 offs_x;
	s32 offs_z;
	u32 settings_crc;
	u32 data_size;
};
#pragma pack(pop)

static_assert(sizeof(DL_Header) == 32, "DL_Header size");

// Deterministic fingerprint of every parameter that shapes the precomputed fields.
// rnd_scale_min/max are intentionally excluded: they are applied at unpack time on top
// of the baked t factor / cluster index, not when the fields are generated.
struct DL_Settings
{
	u32 size_x;
	u32 size_z;
	s32 offs_x;
	s32 offs_z;
	u32 alt_models_count;
	u32 vanilla_grass_count;
	u8  use_alternative_tree_assets;
	u8  use_cluster_mix_tree_assets;
	float cluster_seed;
	float cluster_patch_size_min;
	float cluster_patch_size_max;
	float cluster_sharpness;
	float cluster_warp_min;
	float cluster_warp_max;
	u8  fmb_use_layer_1;
	float fmb_layer_1_frequency;
	float fmb_layer_1_amplitude;
	float fmb_layer_1_seed;
	float fmb_layer_1_power;
	u8  fmb_use_layer_2;
	float fmb_layer_2_frequency;
	float fmb_layer_2_amplitude;
	float fmb_layer_2_seed;
	float fmb_layer_2_power;
	u8  fmb_use_layer_3;
	float fmb_layer_3_frequency;
	float fmb_layer_3_amplitude;
	float fmb_layer_3_seed;
	float fmb_layer_3_power;
};

bool DetailLayers_FeaturesActive()
{
	return ps_r__detail_use_cluster_mix_tree_assets
		|| ps_r__detail_fmb_use_layer_1
		|| ps_r__detail_fmb_use_layer_2
		|| ps_r__detail_fmb_use_layer_3;
}

u32 DetailLayers_SettingsCRC(CDetailManager* mgr)
{
	DL_Settings st = {};
	st.size_x = mgr->dtH.size_x;
	st.size_z = mgr->dtH.size_z;
	st.offs_x = mgr->dtH.offs_x;
	st.offs_z = mgr->dtH.offs_z;
	st.alt_models_count = mgr->alt_models_count;
	st.vanilla_grass_count = mgr->vanilla_grass_count;

	st.use_alternative_tree_assets = ps_r__detail_use_alternative_tree_assets ? 1 : 0;
	st.use_cluster_mix_tree_assets = ps_r__detail_use_cluster_mix_tree_assets ? 1 : 0;
	st.cluster_seed = ps_r__detail_cluster_seed;
	st.cluster_patch_size_min = ps_r__detail_cluster_patch_size_min;
	st.cluster_patch_size_max = ps_r__detail_cluster_patch_size_max;
	st.cluster_sharpness = ps_r__detail_cluster_sharpness;
	st.cluster_warp_min = ps_r__detail_cluster_warp_min;
	st.cluster_warp_max = ps_r__detail_cluster_warp_max;

	st.fmb_use_layer_1 = ps_r__detail_fmb_use_layer_1 ? 1 : 0;
	st.fmb_layer_1_frequency = ps_r__detail_fmb_layer_1_frequency;
	st.fmb_layer_1_amplitude = ps_r__detail_fmb_layer_1_amplitude;
	st.fmb_layer_1_seed = ps_r__detail_fmb_layer_1_seed;
	st.fmb_layer_1_power = ps_r__detail_fmb_layer_1_power;
	st.fmb_use_layer_2 = ps_r__detail_fmb_use_layer_2 ? 1 : 0;
	st.fmb_layer_2_frequency = ps_r__detail_fmb_layer_2_frequency;
	st.fmb_layer_2_amplitude = ps_r__detail_fmb_layer_2_amplitude;
	st.fmb_layer_2_seed = ps_r__detail_fmb_layer_2_seed;
	st.fmb_layer_2_power = ps_r__detail_fmb_layer_2_power;
	st.fmb_use_layer_3 = ps_r__detail_fmb_use_layer_3 ? 1 : 0;
	st.fmb_layer_3_frequency = ps_r__detail_fmb_layer_3_frequency;
	st.fmb_layer_3_amplitude = ps_r__detail_fmb_layer_3_amplitude;
	st.fmb_layer_3_seed = ps_r__detail_fmb_layer_3_seed;
	st.fmb_layer_3_power = ps_r__detail_fmb_layer_3_power;

	return crc32(&st, sizeof(st));
}

// Writes a setting as "name = value" and appends the running crc into a second buffer.
// The file is human-friendly; the machine check uses the crc line.
void DetailLayers_WriteSettingsFile(CDetailManager* mgr, u32 crc)
{
	auto line = [](const char* fmt, ...) {
		char buf[256];
		va_list args;
		va_start(args, fmt);
		int n = vsprintf(buf, fmt, args);
		va_end(args);
		return xr_string(buf, n > 0 ? n : 0);
	};

	CMemoryWriter W;
	W.w_string(line("; IX-Ray precomputed detail layers settings").c_str());
	W.w_string(line("; Regenerate the fields by bumping any value below or deleting the bins.").c_str());
	W.w_string(line("grid_size_x = %u", mgr->dtH.size_x).c_str());
	W.w_string(line("grid_size_z = %u", mgr->dtH.size_z).c_str());
	W.w_string(line("grid_offs_x = %d", mgr->dtH.offs_x).c_str());
	W.w_string(line("grid_offs_z = %d", mgr->dtH.offs_z).c_str());
	W.w_string(line("alt_models_count = %u", mgr->alt_models_count).c_str());
	W.w_string(line("vanilla_grass_count = %u", mgr->vanilla_grass_count).c_str());
	W.w_string(line("use_alternative_tree_assets = %s", ps_r__detail_use_alternative_tree_assets ? "true" : "false").c_str());
	W.w_string(line("use_cluster_mix_tree_assets = %s", ps_r__detail_use_cluster_mix_tree_assets ? "true" : "false").c_str());
	W.w_string(line("cluster_seed = %.9g", ps_r__detail_cluster_seed).c_str());
	W.w_string(line("cluster_patch_size_min = %.9g", ps_r__detail_cluster_patch_size_min).c_str());
	W.w_string(line("cluster_patch_size_max = %.9g", ps_r__detail_cluster_patch_size_max).c_str());
	W.w_string(line("cluster_sharpness = %.9g", ps_r__detail_cluster_sharpness).c_str());
	W.w_string(line("cluster_warp_min = %.9g", ps_r__detail_cluster_warp_min).c_str());
	W.w_string(line("cluster_warp_max = %.9g", ps_r__detail_cluster_warp_max).c_str());
	W.w_string(line("fmb_use_layer_1 = %s", ps_r__detail_fmb_use_layer_1 ? "true" : "false").c_str());
	W.w_string(line("fmb_layer_1_frequency = %.9g", ps_r__detail_fmb_layer_1_frequency).c_str());
	W.w_string(line("fmb_layer_1_amplitude = %.9g", ps_r__detail_fmb_layer_1_amplitude).c_str());
	W.w_string(line("fmb_layer_1_seed = %.9g", ps_r__detail_fmb_layer_1_seed).c_str());
	W.w_string(line("fmb_layer_1_power = %.9g", ps_r__detail_fmb_layer_1_power).c_str());
	W.w_string(line("fmb_use_layer_2 = %s", ps_r__detail_fmb_use_layer_2 ? "true" : "false").c_str());
	W.w_string(line("fmb_layer_2_frequency = %.9g", ps_r__detail_fmb_layer_2_frequency).c_str());
	W.w_string(line("fmb_layer_2_amplitude = %.9g", ps_r__detail_fmb_layer_2_amplitude).c_str());
	W.w_string(line("fmb_layer_2_seed = %.9g", ps_r__detail_fmb_layer_2_seed).c_str());
	W.w_string(line("fmb_layer_2_power = %.9g", ps_r__detail_fmb_layer_2_power).c_str());
	W.w_string(line("fmb_use_layer_3 = %s", ps_r__detail_fmb_use_layer_3 ? "true" : "false").c_str());
	W.w_string(line("fmb_layer_3_frequency = %.9g", ps_r__detail_fmb_layer_3_frequency).c_str());
	W.w_string(line("fmb_layer_3_amplitude = %.9g", ps_r__detail_fmb_layer_3_amplitude).c_str());
	W.w_string(line("fmb_layer_3_seed = %.9g", ps_r__detail_fmb_layer_3_seed).c_str());
	W.w_string(line("fmb_layer_3_power = %.9g", ps_r__detail_fmb_layer_3_power).c_str());
	W.w_string(line("crc = 0x%08X", crc).c_str());

	IWriter* FS_W = FS.w_open("$level$", DL_SETTINGS_FILE);
	if (FS_W)
	{
		FS_W->w(W.pointer(), W.size());
		FS.w_close(FS_W);
	}
}
} // namespace

bool CDetailManager::DetailLayers_LoadFromBake()
{
	u32 sx = dtH.size_x;
	u32 sz = dtH.size_z;
	if (sx == 0 || sz == 0)
		return false; // fallback: Build* fills sentinels

	if (!DetailLayers_FeaturesActive())
		return false; // feature disabled: vanilla fields (255/0.0f), never reuse

	u32 crc = DetailLayers_SettingsCRC(this);

	// 1) settings crc sanity (also stored in each bin header)
	IReader* settings = FS.r_open("$level$", DL_SETTINGS_FILE);
	if (!settings)
		return false;
	xr_string text;
	text.resize(settings->length());
	if (settings->length())
		settings->r(text.data(), settings->length());
	FS.r_close(settings);

	// parse "crc = 0x" line
	u32 file_crc = 0xffffffff;
	size_t pos = text.find("crc = 0x");
	if (pos != xr_string::npos)
	{
		sscanf(text.c_str() + pos + 8, "%x", &file_crc);
	}
	if (file_crc != crc)
		return false;

	// 2) FMB field
	IReader* fmb = FS.r_open("$level$", DL_FMB_FILE);
	if (!fmb)
		return false;
	bool fmb_ok = false;
	xr_vector<float> fmb_data;
	if (fmb->length() >= (intptr_t)sizeof(DL_Header))
	{
		DL_Header hf;
		fmb->r(&hf, sizeof(DL_Header));
		u32 corner_count = (sx + 1) * (sz + 1);
		if (hf.magic == DL_MK_FMB && hf.version == DL_VERSION
			&& hf.size_x == sx && hf.size_z == sz
			&& hf.offs_x == dtH.offs_x && hf.offs_z == dtH.offs_z
			&& hf.settings_crc == crc
			&& hf.data_size == corner_count * sizeof(float)
			&& fmb->length() >= (intptr_t)(sizeof(DL_Header) + hf.data_size))
		{
			fmb_data.resize(corner_count);
			fmb->r(fmb_data.data(), hf.data_size);
			fmb_ok = true;
		}
	}
	FS.r_close(fmb);
	if (!fmb_ok)
		return false;

	// 3) Cluster/perlin field
	IReader* per = FS.r_open("$level$", DL_PERLIN_FILE);
	if (!per)
		return false;
	bool per_ok = false;
	xr_vector<u8> cl_data, rnd_data;
	if (per->length() >= (intptr_t)sizeof(DL_Header))
	{
		DL_Header hp;
		per->r(&hp, sizeof(DL_Header));
		u32 slot_count = sx * sz;
		if (hp.magic == DL_MK_PER && hp.version == DL_VERSION
			&& hp.size_x == sx && hp.size_z == sz
			&& hp.offs_x == dtH.offs_x && hp.offs_z == dtH.offs_z
			&& hp.settings_crc == crc
			&& hp.data_size == slot_count * 2
			&& per->length() >= (intptr_t)(sizeof(DL_Header) + hp.data_size))
		{
			cl_data.resize(slot_count);
			rnd_data.resize(slot_count);
			per->r(cl_data.data(), slot_count);
			per->r(rnd_data.data(), slot_count);
			per_ok = true;
		}
	}
	FS.r_close(per);
	if (!per_ok)
		return false;

	// everything matched: commit into the manager fields
	fmb_field.swap(fmb_data);
	cluster_field.swap(cl_data);
	cluster_rnd_field.swap(rnd_data);
	Msg("* detail_layers: loaded precomputed fields (%ux%u, crc 0x%08X)", sx, sz, crc);
	return true;
}

void CDetailManager::DetailLayers_SaveToBake()
{
	u32 sx = dtH.size_x;
	u32 sz = dtH.size_z;
	if (sx == 0 || sz == 0)
		return;

	if (!DetailLayers_FeaturesActive())
		return; // don't persist a "vanilla" bake

	u32 crc = DetailLayers_SettingsCRC(this);

	// Validate both fields up front so we write all-or-nothing: a mismatched vector
	// would otherwise make the bake partially updated on disk.
	u32 corner_count = (sx + 1) * (sz + 1);
	u32 slot_count = sx * sz;
	if (fmb_field.size() != corner_count
		|| cluster_field.size() != slot_count
		|| cluster_rnd_field.size() != slot_count)
		return;

	// FMB
	{
		DL_Header h = {};
		h.magic = DL_MK_FMB;
		h.version = DL_VERSION;
		h.size_x = sx;
		h.size_z = sz;
		h.offs_x = dtH.offs_x;
		h.offs_z = dtH.offs_z;
		h.settings_crc = crc;
		h.data_size = (u32)(fmb_field.size() * sizeof(float));

		IWriter* W = FS.w_open("$level$", DL_FMB_FILE);
		if (W)
		{
			W->w(&h, sizeof(h));
			W->w(fmb_field.data(), h.data_size);
			FS.w_close(W);
		}
	}

	// Cluster/perlin
	{
		DL_Header h = {};
		h.magic = DL_MK_PER;
		h.version = DL_VERSION;
		h.size_x = sx;
		h.size_z = sz;
		h.offs_x = dtH.offs_x;
		h.offs_z = dtH.offs_z;
		h.settings_crc = crc;
		h.data_size = slot_count * 2;

		IWriter* W = FS.w_open("$level$", DL_PERLIN_FILE);
		if (W)
		{
			W->w(&h, sizeof(h));
			W->w(cluster_field.data(), slot_count);
			W->w(cluster_rnd_field.data(), slot_count);
			FS.w_close(W);
		}
	}

	DetailLayers_WriteSettingsFile(this, crc);
	Msg("* detail_layers: saved precomputed fields (%ux%u, crc 0x%08X)", sx, sz, crc);
}

// Revert the r__detail_* console state (seeds, frequencies, layer toggles, ...) to the
// values that were baked into the last settings.txt, so a forced bake-load cannot
// desync: any console tweak or a later level restart would otherwise re-derive a
// different crc and regenerate/overwrite the bake.
void CDetailManager::DetailLayers_ApplySettingsFromBake()
{
	IReader* settings = FS.r_open("$level$", DL_SETTINGS_FILE);
	if (!settings)
	{
		Msg("! detail_layers: no %s, cannot restore settings", DL_SETTINGS_FILE);
		return;
	}
	xr_string text;
	text.resize(settings->length());
	if (settings->length())
		settings->r(text.data(), settings->length());
	FS.r_close(settings);

	struct Setting
	{
		const char* key;
		bool is_bool;
		void* p;
	};
	Setting table[] = {
		{"use_alternative_tree_assets", true, &ps_r__detail_use_alternative_tree_assets},
		{"use_cluster_mix_tree_assets", true, &ps_r__detail_use_cluster_mix_tree_assets},
		{"cluster_seed", false, &ps_r__detail_cluster_seed},
		{"cluster_patch_size_min", false, &ps_r__detail_cluster_patch_size_min},
		{"cluster_patch_size_max", false, &ps_r__detail_cluster_patch_size_max},
		{"cluster_sharpness", false, &ps_r__detail_cluster_sharpness},
		{"cluster_warp_min", false, &ps_r__detail_cluster_warp_min},
		{"cluster_warp_max", false, &ps_r__detail_cluster_warp_max},
		{"fmb_use_layer_1", true, &ps_r__detail_fmb_use_layer_1},
		{"fmb_layer_1_frequency", false, &ps_r__detail_fmb_layer_1_frequency},
		{"fmb_layer_1_amplitude", false, &ps_r__detail_fmb_layer_1_amplitude},
		{"fmb_layer_1_seed", false, &ps_r__detail_fmb_layer_1_seed},
		{"fmb_layer_1_power", false, &ps_r__detail_fmb_layer_1_power},
		{"fmb_use_layer_2", true, &ps_r__detail_fmb_use_layer_2},
		{"fmb_layer_2_frequency", false, &ps_r__detail_fmb_layer_2_frequency},
		{"fmb_layer_2_amplitude", false, &ps_r__detail_fmb_layer_2_amplitude},
		{"fmb_layer_2_seed", false, &ps_r__detail_fmb_layer_2_seed},
		{"fmb_layer_2_power", false, &ps_r__detail_fmb_layer_2_power},
		{"fmb_use_layer_3", true, &ps_r__detail_fmb_use_layer_3},
		{"fmb_layer_3_frequency", false, &ps_r__detail_fmb_layer_3_frequency},
		{"fmb_layer_3_amplitude", false, &ps_r__detail_fmb_layer_3_amplitude},
		{"fmb_layer_3_seed", false, &ps_r__detail_fmb_layer_3_seed},
		{"fmb_layer_3_power", false, &ps_r__detail_fmb_layer_3_power},
	};

	u32 restored = 0;
	for (const Setting& s : table)
	{
		xr_string find = xr_string(s.key) + " = ";
		size_t pos = text.find(find);
		if (pos == xr_string::npos)
			continue;

		// value runs to end-of-line (settings.txt is written by w_string => CRLF)
		size_t v = pos + find.size();
		size_t eol = text.find('\n', v);
		xr_string val = text.substr(v, eol == xr_string::npos ? xr_string::npos : eol - v);
		val.erase(val.find_last_not_of(" \t\r\n") + 1);

		if (s.is_bool)
			*(bool*)s.p = (val == "true");
		else
			*(float*)s.p = (float)atof(val.c_str());

		restored++;
	}
	Msg("* detail_layers: restored %u r__detail_* settings from bake", restored);
}
#endif // !_EDITOR