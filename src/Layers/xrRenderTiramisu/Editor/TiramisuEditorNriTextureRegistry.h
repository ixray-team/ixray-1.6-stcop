#pragma once

#include "../../../xrCore/xrCore.h"

#include <cstdint>
#include <unordered_set>

// Render-thread реестр editor textures и их ImTextureID.
class TiramisuEditorNriTextureRegistry final
{
public:
	void Register(void* ShaderResourceDescriptor)
	{
		if (ShaderResourceDescriptor)
			Descriptors.insert(reinterpret_cast<std::uintptr_t>(ShaderResourceDescriptor));
	}

	void Unregister(void* ShaderResourceDescriptor)
	{
		if (ShaderResourceDescriptor)
			Descriptors.erase(reinterpret_cast<std::uintptr_t>(ShaderResourceDescriptor));
	}

	[[nodiscard]] bool Contains(const std::uintptr_t DescriptorId) const noexcept
	{
		return DescriptorId != 0 && Descriptors.contains(DescriptorId);
	}

	void Clear() noexcept
	{
		Descriptors.clear();
	}

private:
	xr_hash_set<std::uintptr_t> Descriptors;
};
