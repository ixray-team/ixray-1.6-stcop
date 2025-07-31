#pragma once
#define VLOAD_SWI 0x10
#include "XRayRenderVisual.h"
class XRayFVisual :public XRayRenderVisual
{
public:
	XRayFVisual();
	virtual ~XRayFVisual();
	virtual void Load(const char* N, IReader* data, u32 dwFlags);
	virtual void Copy(XRayRenderVisual* from);
	//virtual bool Render(float LOD,EShaderElement SEType, XRayObjectRender& Item);
	size_t CountIndex;
	size_t OffsetIndex;
	size_t CountVertex;
	size_t OffsetVertex;
	u32 FVF;
	// BearFactoryPointer<BearRHI::BearRHIIndexBuffer> IndexBuffer;
	// BearFactoryPointer<BearRHI::BearRHIVertexBuffer> VertexBuffer;
	//
	// BearFactoryPointer<BearRHI::BearRHIStructuredBuffer> VertexBufferAsStructured;
	// BearFactoryPointer<BearRHI::BearRHIStructuredBuffer> MeshBuffer;
	//xr_vector<XRaySubset> MeshletSubsets;
	size_t CountMeshlet;
	size_t OffsetUniqueVertexIndices;
	size_t OffsetPrimitiveIndices;
protected:
	FSlideWindowItem	nSWI;

};