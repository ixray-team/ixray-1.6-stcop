#pragma once
#include "../LightingBackend.h"

// CPU backend: Intel Embree
class EmbreeLightingBackend final : 
	public ILightingBackend
{
public:
	const char* GetName() const override { return "Embree"; }

	void InitializeLevel() override;
	void ReleaseLevel() override {}

	void LightAdaptiveHT(CBuild& Build) override;
	void LightImplicit(ImplicitDeflector& Deflector) override;
	void LightDeflectors() override;
	void LightVertices(u32 Flags) override;
	void LightMURefs() override;

	void BeginDetails() override;
	void LightDetailSlot(DetailSlot& Slot, xr_vector<DetailsTask>& Samples, base_lighting& Lights, u32 Count) override;
	void EndDetails() override {}
};
