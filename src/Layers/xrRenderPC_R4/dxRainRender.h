#pragma once

#include "../../Include/xrRender/RainRender.h"

class dxRainRender : public IRainRender
{
public:
	dxRainRender();
	~dxRainRender() override;
	void Copy(IRainRender& Source) override;
	void Render(CEffect_Rain& Owner) override;
	const Fsphere& GetDropBounds() const override;

private:
	class InstanceStream
	{
	public:
		InstanceStream() = default;
		InstanceStream(const InstanceStream&) = delete;
		InstanceStream& operator=(const InstanceStream&) = delete;
		~InstanceStream();
		bool Upload(const void* Data, u32 Count, u32 Stride);
		IRHIShaderResourceView* View = nullptr;

	private:
		IRHIBuffer* Buffer = nullptr;
		u32 Capacity = 0;
	};

	ref_shader AirShader;
	ref_shader ImpactShader;
	ref_geom Geometry;
	InstanceStream AirStream;
	InstanceStream ImpactStream;
	Fsphere DropBounds;
};
