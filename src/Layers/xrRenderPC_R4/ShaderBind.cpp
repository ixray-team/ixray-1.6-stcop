#include "stdafx.h"
#include "ShaderBind.h"

static ShaderBindSlot g_binds[32];
static u32 g_bind_count = 0;

void ShaderBind_Set(const ShaderBindSlot* items, u32 count)
{
	g_bind_count = std::min(count, u32(std::size(g_binds)));
	if (g_bind_count)
		CopyMemory(g_binds, items, g_bind_count * sizeof(ShaderBindSlot));
}

void ShaderBind_Clear()
{
	g_bind_count = 0;
}

xr_vector<ShaderBindSlot> ShaderBind_Snapshot()
{
	return xr_vector<ShaderBindSlot>(g_binds, g_binds + g_bind_count);
}

u32 ShaderBind_CacheKey()
{
	u32 hash = (2166136261u ^ 2u) * 16777619u;
	for (u32 i = 0; i < g_bind_count; ++i)
	{
		for (const char* s = g_binds[i].name; *s; ++s)
			hash = (hash ^ u32(u8(*s))) * 16777619u;
		hash = (hash ^ g_binds[i].slot) * 16777619u;
		hash = (hash ^ u32(u8(g_binds[i].space))) * 16777619u;
	}
	return g_bind_count ? hash : 0;
}

static bool token_at(const xr_string& line, u32 pos, u32 len)
{
	const bool left = pos == 0 || !std::isalnum(u8(line[pos - 1])) && line[pos - 1] != '_';
	const u32 end = pos + len;
	const bool right = end >= line.size() || !std::isalnum(u8(line[end])) && line[end] != '_';
	return left && right;
}

static bool starts_with_token(const xr_string& line, u32 i, const char* word)
{
	const u32 len = xr_strlen(word);
	if (i + len > line.size() || line.compare(i, len, word) != 0)
		return false;
	return i + len == line.size() || (!std::isalnum(u8(line[i + len])) && line[i + len] != '_');
}

static char decl_space(const xr_string& line)
{
	u32 i = 0;
	while (i < line.size() && (line[i] == ' ' || line[i] == '\t' || line[i] == '\r'))
		++i;
	if (starts_with_token(line, i, "static"))
		return 0;
	if (starts_with_token(line, i, "uniform"))
	{
		i += 7;
		while (i < line.size() && (line[i] == ' ' || line[i] == '\t'))
			++i;
	}
	static const struct { const char* kind; char space; } kinds[] = {
		{ "Texture1DArray", 't' }, { "Texture2DArray", 't' }, { "Texture2DMS", 't' }, { "TextureCubeArray", 't' },
		{ "Texture1D", 't' }, { "Texture2D", 't' }, { "Texture3D", 't' }, { "TextureCube", 't' },
		{ "RWTexture1D", 'u' }, { "RWTexture2D", 'u' }, { "RWTexture3D", 'u' },
		{ "SamplerComparisonState", 's' }, { "SamplerState", 's' }, { "sampler", 's' },
		{ "AppendStructuredBuffer", 'u' }, { "ConsumeStructuredBuffer", 'u' },
		{ "RWStructuredBuffer", 'u' }, { "StructuredBuffer", 't' },
		{ "RWByteAddressBuffer", 'u' }, { "ByteAddressBuffer", 't' },
		{ "RWBuffer", 'u' }, { "Buffer", 't' },
	};
	for (const auto& kind : kinds)
	{
		if (starts_with_token(line, i, kind.kind))
			return kind.space;
	}
	return 0;
}

bool ShaderBind_Rewrite(const u8* in_data, u32 in_size, u8*& out_data, u32& out_size)
{
	if (!g_bind_count || !in_data || !in_size)
		return false;

	xr_string text(reinterpret_cast<const char*>(in_data), in_size);
	xr_string rebuilt;
	rebuilt.reserve(text.size() + 64);
	u32 line_start = 0;
	bool changed = false;
	while (line_start < text.size())
	{
		u32 line_end = line_start;
		while (line_end < text.size() && text[line_end] != '\n')
			++line_end;
		xr_string line = text.substr(line_start, line_end - line_start);
		const char space = decl_space(line);
		if (space)
		{
			for (u32 i = 0; i < g_bind_count; ++i)
			{
				const u32 len = u32(xr_strlen(g_binds[i].name));
				const u32 pos = u32(line.find(g_binds[i].name));
				if (pos == u32(xr_string::npos) || !token_at(line, pos, len))
					continue;
				const u32 semi = u32(line.find(';', pos));
				if (semi == u32(xr_string::npos))
					continue;
				char reg[32];
				xr_sprintf(reg, "register(%c%u)", g_binds[i].space, g_binds[i].slot);
				const u32 existing = u32(line.find("register(", pos));
				if (existing != u32(xr_string::npos) && existing < semi)
				{
					const u32 close = u32(line.find(')', existing));
					if (close == u32(xr_string::npos) || close > semi)
						continue;
					line.replace(existing, close - existing + 1, reg);
				}
				else
					line.insert(semi, xr_string(" : ") + reg);
				changed = true;
				break;
			}
		}
		rebuilt += line;
		if (line_end < text.size())
			rebuilt += '\n';
		line_start = line_end < text.size() ? line_end + 1 : text.size();
	}
	if (!changed)
		return false;
	out_size = u32(rebuilt.size());
	out_data = xr_alloc<u8>(out_size + 1);
	CopyMemory(out_data, rebuilt.data(), out_size);
	out_data[out_size] = 0;
	return true;
}
