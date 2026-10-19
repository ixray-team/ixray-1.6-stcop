#pragma once
#include "../LightingBackend.h"

class IGpuRayTracer;

// GPU backend: batches LightPoint requests for an IGpuRayTracer (CUDA, Vulkan)
class GpuLightingBackend final :
	public ILightingBackend
{
	IGpuRayTracer& Tracer;

public:
	explicit GpuLightingBackend(IGpuRayTracer& InTracer) : Tracer(InTracer) {}

	const char* GetName() const override;

	void InitializeLevel() override;
	void ReleaseLevel() override;

	void LightAdaptiveHT(CBuild& Build) override;
	void LightImplicit(ImplicitDeflector& Deflector) override;
	void LightDeflectors() override;
	void LightVertices(u32 Flags) override;
	void LightMURefs() override;

	void OnStageFinished(ELightingStage Stage) override;
	void OnLevelFinished() override;

	void BeginDetails() override;
	void LightDetailSlot(DetailSlot& Slot, xr_vector<DetailsTask>& Samples, base_lighting& Lights, u32 Count) override;
	void FinishDetailsThread() override;
	void EndDetails() override;
};
