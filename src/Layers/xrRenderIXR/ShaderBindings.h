/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
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
#include "../../../shaders/common/bindings.hlsli"
	};
#undef BIND_TEXTURE
#undef BIND_SAMPLER

#define BIND_TEXTURE(Name, Slot)
#define BIND_SAMPLER(Name, Slot) Binding{ #Name, Slot },
	inline constexpr Binding Samplers[] =
	{
#include "../../../shaders/common/bindings.hlsli"
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
