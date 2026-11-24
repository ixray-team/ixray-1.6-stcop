#include "stdafx.h"
#include "GpuResource.h"
#include "../../xrEngine/xr_ioc_cmd.h"

void fix_texture_name(char* fn)
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

int get_texture_load_lod(const char* fn, size_t& w, size_t& h, int Floor)
{
	if (Device.IsEditorMode())
	{
		return Floor;
	}

	auto min_size = (int)std::min(w, h);

	int target_lod = 0;

	while (min_size > g_TextureTargetResolution)
	{
		min_size /= 2;
		target_lod ++;
	}

	return std::max(target_lod, Floor);
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

namespace
{
	bool FindTextureFile(string_path& FileName, const char* Name)
	{
		return FS.exist(FileName, "$level$", Name, ".dds") ||
			FS.exist(FileName, "$game_saves$", Name, ".dds") ||
			FS.exist(FileName, _game_textures_, Name, ".dds");
	}
}

void ResolveTextureFile(const char* name, string_path& filename, string_path& path)
{
	filename[0] = 0;
	path[0] = 0;
	xr_strcpy(filename, name);
	fix_texture_name(filename);
	// Save thumbnails arrive as a path inside $game_saves$; resolving them against a
	// texture root would prepend another directory and never match.
	string_path direct = {};
	xr_strconcat(direct, name, ".dds");
	if (!FS.exist(path, _game_textures_, filename, ".dds") && strstr(filename, "_bump")) {
		const char* fallback = strstr(filename, "_bump#") ? "ed\\ed_dummy_bump#" : "ed\\ed_dummy_bump";
		Msg("! Fallback to default bump map: %s", filename);
		R_ASSERT2(FS.exist(path, _game_textures_, fallback, ".dds"), fallback);
	} else if (FS.exist(direct)) {
		xr_strcpy(path, direct);
	} else if (!FindTextureFile(path, filename)) {
		Msg("! Can't find texture '%s'", filename);
		R_ASSERT(FS.exist(path, _game_textures_, "ed\\ed_not_existing_texture", ".dds"));
	}
}

// What a caller outside the textures holds alone: NRI's own handle goes, and the surface keeps the texture
IRHISurface* CRender::texture_load(const char* name, u32& out_size, bool)
{
	R_ASSERT(name && name[0]);
	GpuResource Made;
	return Made.Load(name, out_size) && Made.Ready() ? Made.Adopt() : nullptr;
}
