#include "../../xrUI/stdafx.h"
#include "../../xrUI/UITextureMaster.h"
#include <cstdio>

int main()
{
	Memory._initialize();
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
	xr_map<sh_pair, u32> Cache;
	for (u32 Index = 0; Index < Keys.size(); ++Index)
	{
		Cache.emplace(Keys[Index], Index);
		for (const auto& Other : Keys)
		{
			if ((Keys[Index] < Other) && (Other < Keys[Index]))
			{
				puts("FAIL: shader cache ordering violates asymmetry");
				return 1;
			}
		}
	}
	for (u32 Index = 0; Index < Keys.size(); ++Index)
	{
		const auto Found = Cache.find(Keys[Index]);
		if (Found == Cache.end() || Found->second != Index)
		{
			puts("FAIL: shader cache lost a texture/shader pair");
			return 2;
		}
	}
	puts("PASS: UI shader cache ordering and lookup");
	return 0;
}
