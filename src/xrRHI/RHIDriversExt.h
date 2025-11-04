#pragma once

enum class ERHI_GPU
{
	NVIDIA,
	AMD,
	INTELL
};

class CNvReader;
class CAMDReader;

class IRHIGPU
{
public:
	ERHI_GPU GPUID = ERHI_GPU::INTELL;

public:
	virtual ~IRHIGPU() = default;
	virtual void Initialize() = 0;
	virtual void Destroy() {}

	virtual u32 GetPercentActive() = 0;
	virtual u32 GetGPUCount() = 0;

	virtual CNvReader* GetNV()   { return nullptr; }
	virtual CAMDReader* GetAMD() { return nullptr; }
	virtual bool SetDepthBounds(bool, float zMin, float zMax) = 0;
};