#pragma once

namespace ShaderBindings
{
	struct Binding
	{
		std::string_view Name;
		u32 Slot;
	};

#define BIND_TEXTURE(Name, Slot) Binding{ #Name, Slot },
#define BIND_SAMPLER(Name, Slot)
	inline constexpr Binding Textures[] =
	{
#include "../../../gamedata/shaders/d3d11/shared/bindings.hlsli"
	};
#undef BIND_TEXTURE
#undef BIND_SAMPLER

#define BIND_TEXTURE(Name, Slot)
#define BIND_SAMPLER(Name, Slot) Binding{ #Name, Slot },
	inline constexpr Binding Samplers[] =
	{
#include "../../../gamedata/shaders/d3d11/shared/bindings.hlsli"
	};
#undef BIND_TEXTURE
#undef BIND_SAMPLER

	template<size_t Count>
	constexpr u32 Find(const Binding (&Table)[Count], std::string_view Name)
	{
		for (const Binding& Item : Table)
		{
			if (Item.Name == Name)
			{
				return Item.Slot;
			}
		}
		return u32(-1);
	}

	constexpr u32 Texture(std::string_view Name)
	{
		return Find(Textures, Name);
	}

	constexpr u32 Sampler(std::string_view Name)
	{
		return Find(Samplers, Name);
	}

	inline constexpr u32 SamplerCount = u32(std::size(Samplers));
}
