#pragma once
#include "../GPU/GpuRayTracer.h"

class VulkanContext;

// IGpuRayTracer on top of Vulkan compute + VK_KHR_ray_query (VkTrace.comp)
class VulkanRayTracer final : public IGpuRayTracer
{
	VulkanContext* Context = nullptr;

public:
	VulkanRayTracer();
	~VulkanRayTracer() override;

	VulkanRayTracer(const VulkanRayTracer&) = delete;
	VulkanRayTracer& operator=(const VulkanRayTracer&) = delete;

	const char* GetName() const override { return "Vulkan"; }

	void Initialize() override;

	void LoadScene(const GpuScene& Scene) override;
	void UnloadScene() override;

	void Trace(u8 Flags, const xr_vector<RayRecvestIndex>& Rays, xr_vector<base_color_c>& Colors) override;
	void ReleaseThreadData() override {}
};
