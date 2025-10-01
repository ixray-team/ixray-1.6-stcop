#pragma once

struct RHIViewport
{
	float TopLeftX = 0.0f;
	float TopLeftY = 0.0f;
	float Width = 0.0f;
	float Height = 0.0f;
	float MinDepth = 0.0f;
	float MaxDepth = 1.0f;

	RHIViewport() = default;

	RHIViewport(float x, float y, float w, float h, float minDepth = 0.0f, float maxDepth = 1.0f)
		: TopLeftX(x), TopLeftY(y), Width(w), Height(h), MinDepth(minDepth), MaxDepth(maxDepth)
	{
	}
};

struct RHIInputElementDesc
{
	const char* SemanticName;
	u32 SemanticIndex;
	ERHI_FORMAT Format;
	u32 InputSlot;
	u32 AlignedByteOffset;
	ERHI_INPUT_CLASSIFICATION InputSlotClass;
	u32 InstanceDataStepRate;
};