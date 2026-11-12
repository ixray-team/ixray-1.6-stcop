#pragma once
#include "../RenderAPI.h"

class RENDER_API dx10SamplerStateCache
{
public:
	void Create();
	void Destroy();
	void SetMaxAnisotropy(u32 MaxAniso);
	void SetMipLodBias(float Bias);
	RHIObject* GetState(u32 Register) const { return States[ActiveSet][Register]; }

private:
	void CreateSet(u32 Set);
	void Bind() const;

	RHIObject* States[2][16] = {};
	u32 MaxAnisotropy = 1;
	u32 ActiveSet = 0;
	float MipLodBias = 0.0f;
};

extern RENDER_API dx10SamplerStateCache SSManager;
