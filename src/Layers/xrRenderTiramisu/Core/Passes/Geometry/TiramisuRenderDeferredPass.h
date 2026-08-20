#pragma once

#include "TiramisuRenderTypes.h"

#include <NRI.h>

// Geometry pass пишет opaque/masked mesh batches в versioned MRT G-buffer.
// Lighting, transparency и world features остаются отдельными passes renderer.
class TiramisuRenderDeferredPass
{
public:
	TiramisuRenderDeferredPass();
	~TiramisuRenderDeferredPass();
	// Разрешает material proxies и формирует стабильный список draws до upload.
	void Prepare_RenderThread();
	void Render(nri::CommandBuffer& CurrentCommandBuffer);

private:
	struct FPreparedDraw
	{
		nri::Pipeline* Pipeline = nullptr;
		nri::Buffer* IndexBuffer = nullptr;
		u64 IndexBufferOffset = 0;
		nri::IndexType IndexType = nri::IndexType::UINT16;
		nri::Buffer* VertexBuffer = nullptr;
		u64 VertexBufferOffset = 0;
		u32 VertexStride = 0;
		nri::DrawIndexedDesc Draw = {};
	};

	xr_vector<FPreparedDraw> PreparedDraws;
};
