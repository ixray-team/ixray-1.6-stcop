#include "stdafx.h"
#include <cstdio>
#include <thread>
#include <atomic>
#include "../../xrCore/Platform/Atomic.h"
#include "../../xrCore/Platform/MacOS/CommandLine.h"
#include "../../xrCore/Platform/MacOS/TextEncoding.h"
#include "../../xrRHI/Platform/RHIInputLayout.h"
#include "../../xrUI/stdafx.h"
#include "../../xrUI/UITextureMaster.h"
#include "../../Layers/xrRender/TextureDescriptionName.h"

int main(int ArgumentCount, char** Arguments)
{
	Memory._initialize();
	// Filesystem listings use native separators; compiled assets retain backslashes.
	const char* MetadataFiles[] =
		{
			"terrain/terrain_escape.thm",
			"terrain\\terrain_escape.THM",
			"nested/terrain\\terrain_escape.DDS",
			"ground/soil.tga",
			"ground\\soil.BMP",
			"movies/ambient.ogm",
			"terrain/terrain_escape",
			"terrain\\terrain_escape",
			"nested/terrain\\terrain_escape",
			"terrain/terrain_escape.ltx",
			"terrain/terrain_escape.diffuse.dds",
			"folder.with.dots/terrain_escape",
			"terrain/terrain_escape.dds.backup"
		};
	const char* RendererNames[] =
		{
			"terrain\\terrain_escape",
			"terrain\\terrain_escape",
			"nested\\terrain\\terrain_escape",
			"ground\\soil",
			"ground\\soil",
			"movies\\ambient",
			"terrain\\terrain_escape",
			"terrain\\terrain_escape",
			"nested\\terrain\\terrain_escape",
			"terrain\\terrain_escape.ltx",
			"terrain\\terrain_escape.diffuse",
			"folder.with.dots\\terrain_escape",
			"terrain\\terrain_escape.dds.backup"
		};
	static_assert(std::size(MetadataFiles) == std::size(RendererNames));
	for (u32 Index = 0; Index < std::size(MetadataFiles); ++Index)
	{
		xr_string MetadataName = MetadataFiles[Index];
		fix_texture_thm_name(MetadataName.data());
		xr_map<shared_str, float> DetailScales;
		DetailScales.emplace(shared_str(MetadataName.c_str()), 144.0f);
		const auto Found = DetailScales.find(shared_str(RendererNames[Index]));
		if (Found == DetailScales.end() || Found->second != 144.0f)
		{
			printf("FAIL: production THM name '%s' became '%s', expected '%s'\n", MetadataFiles[Index], MetadataName.c_str(), RendererNames[Index]);
			return 17;
		}
	}
	const shared_str Textures[] = {"ui/cache_texture_a", "ui/cache_texture_b"};
	const shared_str Shaders[] = {"hud/cache_shader_a", "hud/cache_shader_b"};
	xr_vector<sh_pair> Keys;
	for (const auto& Texture : Textures)
	{
		for (const auto& Shader : Shaders)
		{
			Keys.push_back({Texture, Shader});
		}
	}
	xr_map<sh_pair, u32> ShaderCache;
	for (u32 Index = 0; Index < Keys.size(); ++Index)
	{
		ShaderCache.emplace(Keys[Index], Index);
		for (const auto& Other : Keys)
		{
			if ((Keys[Index] < Other) && (Other < Keys[Index]))
			{
				puts("FAIL: UI shader cache key ordering violates asymmetry");
				return 15;
			}
		}
	}
	for (u32 Index = 0; Index < Keys.size(); ++Index)
	{
		const auto Found = ShaderCache.find(Keys[Index]);
		if (Found == ShaderCache.end() || Found->second != Index)
		{
			puts("FAIL: UI shader cache lookup lost a texture/shader pair");
			return 16;
		}
	}
	char Action[32] = {};
	if (strncpy_s(Action, "UI_DOWN$$suffix", 7) != 0 || strcmp(Action, "UI_DOWN") != 0)
	{
		printf("FAIL: expected UI_DOWN, received '%s'\n", Action);
		return 1;
	}
	if (strncpy_s(Action, "ignored", 0) != 0 || Action[0] != '\0')
	{
		return 2;
	}
	if (_strnicmp("UI_down", "ui_DOWN", 7) != 0)
	{
		return 3;
	}
	char Path[4096] = {};
	char Empty[] = "";
	Platform::GetAbsolutePath(Path, Empty, sizeof(Path));
	if (!std::filesystem::path(Path).is_absolute())
	{
		return 4;
	}
	char Missing[] = "ixray-nonexistent-path/child";
	Platform::GetAbsolutePath(Path, Missing, sizeof(Path));
	if (!std::filesystem::path(Path).is_absolute() || std::filesystem::path(Path).filename() != "child")
	{
		return 5;
	}
	char Tiny[1] = {'x'};
	Platform::GetAbsolutePath(Tiny, Empty, sizeof(Tiny));
	if (Tiny[0] != '\0')
	{
		return 6;
	}
	const RHIInputElementDesc Layout[] =
		{
			{"POSITION", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0},
			{"TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT, 1, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0},
			{"COLOR", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, RHI_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0},
			{"TEXCOORD", 1, ERHI_FORMAT::R32G32_FLOAT, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0}};
	if (RHIPlatform::ComputeInputLayoutStride(Layout, 3) != 16 || RHIPlatform::ComputeInputLayoutStride(Layout, 4) != 32)
	{
		return 7;
	}
	const xr_string Legacy("\xCF\xF0\xE8\xE2\xE5\xF2");
	const xr_string UTF8("\xD0\x9F\xD1\x80\xD0\xB8\xD0\xB2\xD0\xB5\xD1\x82");
	const auto Codepoints = Platform::DecodeText(Legacy.c_str());
	if (Platform::ANSI_TO_UTF8(Legacy) != UTF8 || Platform::UTF8_to_CP1251(UTF8) != Legacy || Codepoints.size() != 6 || Codepoints[0] != 0x41F || Codepoints[5] != 0x442)
	{
		return 8;
	}
	const auto Icon = Platform::DecodeText("\xEE\x80\x81");
	if (Icon.size() != 1 || Icon[0] != 0xE001)
	{
		return 9;
	}
	if (ArgumentCount > 1 && strstr(Platform::GetProcessCommandLine(), Arguments[1]) == nullptr)
	{
		return 10;
	}
	struct AtomicState
	{
		u32 Value = 7;
		u32 Adjacent = 0xA5A5A5A5;
	} Atomic;
	if (Platform::AtomicCompareExchange(&Atomic.Value, 7u, 9u) != 7 || Atomic.Value != 9 || Atomic.Adjacent != 0xA5A5A5A5 || Platform::AtomicCompareExchange(&Atomic.Value, 7u, 11u) != 9 || Atomic.Value != 9)
	{
		return 11;
	}
	const char* Normalized = Platform::ValidPath("/alias\\path");
	if (xr_string(Platform::ValidPath(Normalized)) != "/alias/path")
	{
		return 12;
	}
	xr_atomic_s32 Ready = 0;
	xr_atomic_s32 Mismatches = 0;
	auto CheckPaths = [&](const char* Input, const char* Expected)
	{
		++Ready;
		while (Ready.load() != 2)
		{
			std::this_thread::yield();
		}
		for (int Index = 0; Index < 10000; ++Index)
		{
			if (xr_string(Platform::ValidPath(Input)) != Expected)
			{
				++Mismatches;
			}
		}
	};
	std::thread First(CheckPaths, "/alpha\\alpha\\alpha\\alpha\\alpha\\alpha", "/alpha/alpha/alpha/alpha/alpha/alpha");
	std::thread Second(CheckPaths, "/bravo\\bravo\\bravo\\bravo\\bravo\\bravo", "/bravo/bravo/bravo/bravo/bravo/bravo");
	First.join();
	Second.join();
	if (Mismatches.load() != 0)
	{
		return 13;
	}
	puts("PASS: macOS bounded strings, absolute paths input layout strides CP1251/Unicode text 32-bit atomics UI cache ordering texture detail metadata lookup and concurrent/repeated path normalization");
	return 0;
}

