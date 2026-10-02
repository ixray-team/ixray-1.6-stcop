#include "stdafx.h"
#include "../../xrEngine/xr_ioc_cmd.h"

void fix_texture_name(LPSTR fn)
{
	auto _ext = strext(fn);
	if (_ext && (0 == _stricmp(_ext, ".tga") ||
		0 == _stricmp(_ext, ".dds") ||
		0 == _stricmp(_ext, ".bmp") ||
		0 == _stricmp(_ext, ".ogm")))
	{
		*_ext = 0;
	}
}

int get_texture_load_lod(const char* fn, size_t& w, size_t& h)
{
#ifdef _EDITOR
	return psTextureLOD;
#else
	static auto& target_size = CCC_Integer::FastCommand("render.experemental.target_res", 8192, 256, RHI_REQ_TEXTURE2D_U_OR_V_DIMENSION);
	auto min_size = (int)std::min(w, h);

	int target_lod = 0;

	while (min_size > target_size)
	{
		min_size /= 2;
		target_lod ++;
	}

	return std::max(target_lod, psTextureLOD);
#endif
}

u32 calc_texture_size(int lod, u32 mip_cnt, u32 orig_size)
{
	if (1 == mip_cnt)
	{
		return orig_size;
	}
	int _lod = lod;
	float res = float(orig_size);

	while (_lod > 0)
	{
		--_lod;
		res -= res / 1.333f;
	}
	return iFloor(res);
}

IRHISurface* CRender::load_texture(const char* fname, u32& msize, bool bStaging)
{
	return texture_load(fname, msize, bStaging);
}

bool CRender::get_texture_metadata(const char* absolute_path, RHITextureMetadata* out_data)
{
	if (!absolute_path || !out_data || !FS.exist(absolute_path)) {
		return false;
	}
	IReader* reader = FS.r_open(absolute_path);
	if (!reader) {
		return false;
	}
	HRESULT result = GRHI->GetDDSMetadata(reader->pointer(), reader->length(), *out_data);
	FS.r_close(reader);
	return SUCCEEDED(result);
}

IRHISurface* CRender::texture_load(const char* name, u32& out_size, bool staging)
{
	R_ASSERT(name && name[0]);
	static bool allow_staging = !ps_r__common_flags.test(RFLAG_NO_RAM_TEXTURES);
	staging &= allow_staging;
	string_path filename = {};
	string_path path = {};
	xr_strcpy(filename, name);
	fix_texture_name(filename);
	if (!FS.exist(path, _game_textures_, filename, ".dds") && strstr(filename, "_bump")) {
		const char* fallback = strstr(filename, "_bump#") ? "ed\\ed_dummy_bump#" : "ed\\ed_dummy_bump";
		Msg("! Fallback to default bump map: %s", filename);
		R_ASSERT2(FS.exist(path, _game_textures_, fallback, ".dds"), fallback);
	} else if (!FS.exist(path, "$level$", filename, ".dds") &&
		!FS.exist(path, "$game_saves$", filename, ".dds") &&
		!FS.exist(path, _game_textures_, filename, ".dds")) {
		Msg("! Can't find texture '%s'", filename);
		R_ASSERT(FS.exist(path, _game_textures_, "ed\\ed_not_existing_texture", ".dds"));
	}
	IReader* reader = FS.r_open(path);
	R_ASSERT2(reader, path);
	RHITextureMetadata metadata;
	HRESULT result = GRHI->GetDDSMetadata(reader->pointer(), reader->length(), metadata);
	if (FAILED(result)) {
		string512 error;
		xr_sprintf(error, "Failed to get DDS metadata for '%s': %s (0x%08X)", filename, Debug.dxerror2string(result), result);
		VERIFY2(false, error);
		Msg("! DDS METADATA ERROR: %s (0x%08X)", filename, result);
		FS.r_close(reader);
		return nullptr;
	}
	u32 support = 0;
	bool fallback = !GRHI->SupportsTextureSampling(static_cast<ERHI_FORMAT>(metadata.format), support);
	if (fallback) {
		Msg("! TEXTURE FORMAT ERROR: %s, format %d, supported flags 0x%08X", filename, metadata.format, support);
	}
	int lod = 0;
	if (!metadata.cubemap && !metadata.volumemap) {
		_strlwr(path);
		size_t width = metadata.width;
		size_t height = metadata.height;
		lod = get_texture_load_lod(path, width, height);
	}
	int initial_lod = lod;
	IRHISurface* surface = nullptr;
	result = GRHI->LoadDDS(reader->pointer(), reader->length(), staging ? ERHI_USAGE::USAGE_STAGING : ERHI_USAGE::USAGE_DEFAULT,
		staging ? 0 : u32(ERHI_BIND_FLAG::SHADER_RESOURCE), staging ? ERHI_CPU_ACCESS_FLAG_WRITE : ERHI_CPU_ACCESS_FLAG_NONE, lod, fallback, &surface);
	out_size = calc_texture_size(lod, metadata.mipmap_count - (initial_lod - lod), reader->length());
	FS.r_close(reader);
	if (FAILED(result)) {
		Msg("! TEXTURE CREATION ERROR: %s (0x%08X)", filename, result);
	}
	return surface;
}
