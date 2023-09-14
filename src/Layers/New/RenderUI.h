#pragma once

namespace Rendering
{
	class UIRender : public IUIRender
	{
	public:
		void CreateUIGeom() override;
		void DestroyUIGeom() override;

		void SetShader(IUIShader& shader) override;
		void SetAlphaRef(int aref) override;
		void SetScissor(Irect* rect = NULL) override;
		void GetActiveTextureResolution(Fvector2& res) override;

		void PushPoint(float x, float y, float z, u32 C, float u, float v) override;

		void StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType) override;
		void FlushPrimitive() override;

		LPCSTR UpdateShaderName(LPCSTR tex_name, LPCSTR sh_name) override;

		void CacheSetXformWorld(const Fmatrix& M) override;
		void CacheSetCullMode(CullMode) override;
	};
}