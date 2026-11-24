#pragma once

#include "../../Include/xrRender/RainRender.h"
#include "../../xrEngine/Rain.h"

class dxRainRender : public IRainRender
{
public:
	dxRainRender();
	~dxRainRender() override;
	void Copy(IRainRender& Source) override;
	void Render(CEffect_Rain& Owner) override;
	const Fsphere& GetDropBounds() const override;

	// What a frame draws of the rain, taken from the game's with the frame's collect, after the view's frustum is made
	// (the drops are culled by it); Draw reads nothing else
	static void Take(CEffect_Rain& Owner);
	static void Draw();

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

	struct FrameState
	{
		xr_vector<CEffect_Rain::RainInstance> RainInstances;
		xr_vector<CEffect_Rain::ImpactInstance> ImpactInstances;
		Fvector RainColor;
		float RainLength, RainWidth, RainDropsOpacity;
	};

	void DrawState();

	static dxRainRender* Frame;
	FrameState State;
	ref_shader AirShader;
	ref_shader ImpactShader;
	ref_geom Geometry;
	InstanceStream AirStream;
	InstanceStream ImpactStream;
	Fsphere DropBounds;
};
