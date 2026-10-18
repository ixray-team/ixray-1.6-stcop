#pragma once

class CBuild;
class ImplicitDeflector;
class base_lighting;
struct DetailSlot;
struct DetailsTask;

// Points where a backend may capture intermediate results (e.g. for the GPU preview)
enum class ELightingStage
{
	Deflectors,
	Vertices,
	MUModels
};

// Ray tracing implementation used to bake level lighting (xrLC) and detail slots (xrDO)
class ILightingBackend
{
public:
	virtual ~ILightingBackend() = default;

	virtual const char* GetName() const = 0;

	// xrLC
	virtual void InitializeLevel() = 0;
	virtual void ReleaseLevel() = 0;

	virtual void LightAdaptiveHT(CBuild& Build) = 0;
	virtual void LightImplicit(ImplicitDeflector& Deflector) = 0;
	virtual void LightDeflectors() = 0;
	virtual void LightVertices(u32 Flags) = 0;
	virtual void LightMURefs() = 0;

	virtual void OnStageFinished(ELightingStage Stage) {}
	virtual void OnLevelFinished() {}

	// xrDO
	virtual void BeginDetails() = 0;
	virtual void LightDetailSlot(DetailSlot& Slot, xr_vector<DetailsTask>& Samples, base_lighting& Lights, u32 Count) = 0;
	virtual void FinishDetailsThread() {}
	virtual void EndDetails() = 0;
};

// Backend selected by the current compiler mode
ILightingBackend& GetLightingBackend();
