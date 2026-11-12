#pragma once

namespace nri
{
struct Device;
struct Queue;
} // namespace nri

// Owns the graphics device. The native backends borrow its device and queue until they are ported.
class RHI_API NriDevice
{
public:
	explicit NriDevice(ERHI_API_LAYER Api);
	~NriDevice();

	NriDevice(const NriDevice&) = delete;
	NriDevice& operator=(const NriDevice&) = delete;

	void* NativeDevice() const;
	void* NativeQueue() const;

	nri::Device* Device = nullptr;
	nri::Queue* Queue = nullptr;
};
