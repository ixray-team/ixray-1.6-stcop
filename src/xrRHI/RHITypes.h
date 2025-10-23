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

struct RHI_API RHISampleDesc
{
	ERHI_FILTER Filter;
	ERHI_TEXTURE_ADDRESS_MODE AddressU;
	ERHI_TEXTURE_ADDRESS_MODE AddressV;
	ERHI_TEXTURE_ADDRESS_MODE AddressW;
	float MipLODBias;
	u32 MaxAnisotropy;
	ERHI_COMPARISON_FUNC ComparisonFunc;
	float BorderColor[4];
	float MinLOD;
	float MaxLOD;

	bool operator==(const RHISampleDesc& desc2) const
	{
		if (Filter != desc2.Filter) return false;
		if (AddressU != desc2.AddressU) return false;
		if (AddressV != desc2.AddressV) return false;
		if (AddressW != desc2.AddressW) return false;
		if (ComparisonFunc != desc2.ComparisonFunc) return false;
		if (BorderColor[0] != desc2.BorderColor[0]) return false;
		if (BorderColor[1] != desc2.BorderColor[1]) return false;
		if (BorderColor[2] != desc2.BorderColor[2]) return false;
		if (BorderColor[3] != desc2.BorderColor[3]) return false;
		if (MinLOD != desc2.MinLOD) return false;
		if (MaxLOD != desc2.MaxLOD) return false;

		return true;
	}
};