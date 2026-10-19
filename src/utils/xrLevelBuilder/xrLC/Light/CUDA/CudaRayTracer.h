#pragma once
#include "../GPU/GpuRayTracer.h"

// IGpuRayTracer on top of CUDA + OptiX (CuTrace.cu)
class CudaRayTracer final : public IGpuRayTracer
{
public:
	const char* GetName() const override { return "CUDA"; }

	void Initialize() override;

	void LoadScene(const GpuScene& Scene) override;
	void UnloadScene() override;

	void Trace(u8 Flags, const xr_vector<RayRecvestIndex>& Rays, xr_vector<base_color_c>& Colors) override;
	void ReleaseThreadData() override;
};
