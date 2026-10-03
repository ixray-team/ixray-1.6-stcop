#pragma once
#include "RenderAPI.h"
#include "dx10ConstantBuffer.h"
#include "../../../gamedata/shaders/d3d11/shared/fixed_cb.hlsli"

namespace FixedConstants
{
	// b0..b4: cb_frame, cb_view, cb_object, cb_material, cb_light
	constexpr u32 kSlots = 6;

	RENDER_API void Create();
	RENDER_API void Destroy();
	RENDER_API void UpdateFrame();
	RENDER_API void UpdateView();
	RENDER_API void UpdateObject(const Fmatrix& mW);
	RENDER_API void UpdateMaterial();
	RENDER_API void BindFrame();
	RENDER_API void BindView();
	RENDER_API void BindObject();
	RENDER_API void BindMaterial();
	RENDER_API void BindLight();
	RENDER_API void BindAll();
	RENDER_API void InvalidateBindings();
	RENDER_API bool IsFixedName(const char* n);
	RENDER_API int  FixedClass(const char* n);
	RENDER_API void Flush();

	RENDER_API void SetHemiMaterial(float x,float y,float z,float w);
	RENDER_API void SetHemiPosFaces(float x,float y,float z);
	RENDER_API void SetHemiNegFaces(float x,float y,float z);
	RENDER_API void SetHemiTfactor(const Fvector4& v);
	RENDER_API void SetHemiTfactor(float x,float y,float z,float w);
	RENDER_API void SetLitColor(const Fvector& c, const Fvector& dir);
	RENDER_API void SetDtParams(float x,float y,float z,float w);
	RENDER_API void SetDtParamsScale(float s);
	RENDER_API void SetParallax(float h);
	RENDER_API void SetAlphaRef(float a);
	RENDER_API void SetLModelLight(const Fvector& c, const Fvector& dir);
	RENDER_API void SetTriLOD(float lod);
	RENDER_API void SetTfactor(const Fvector4& v);
	RENDER_API void SetTreeXform(const Fmatrix& m);
	RENDER_API void SetTreeXformV(const Fmatrix& m);
	RENDER_API void SetTreeConsts(float x,float y,float z,float w);
	RENDER_API void SetTreeWave(const Fvector4& v);
	RENDER_API void SetTreeWind(const Fvector4& v);
	RENDER_API void SetTreeConstsOld(float x,float y,float z,float w);
	RENDER_API void SetTreeWaveOld(const Fvector4& v);
	RENDER_API void SetTreeWindOld(const Fvector4& v);
	RENDER_API void SetTreeCScale(float x,float y,float z,float w);
	RENDER_API void SetTreeCBias(float x,float y,float z,float w);
	RENDER_API void SetTreeCSun(float x,float y,float z,float w);
	RENDER_API void SetLMap(const Fmatrix& m);
	RENDER_API void SetShadow(const Fmatrix& m);
	RENDER_API void SetShadowSun(int idx, const Fmatrix& m);
	RENDER_API void SetLdynamic(const Fvector4& c, const Fvector4& p, const Fvector4& d);

	RENDER_API u32 NameHash(const char* n);

	RENDER_API bool OnSet(u32 h, const Fmatrix& A);
	RENDER_API bool OnSet(u32 h, const Fvector4& A);
	RENDER_API bool OnSet(u32 h, float A);
	RENDER_API bool OnSet(u32 h, int A);
	RENDER_API bool OnSetA(u32 h, u32 e, const Fmatrix& A);
	RENDER_API bool OnSetA(u32 h, u32 e, const Fvector4& A);

	// A constant's name either belongs to a fixed layout or it never will, so match it once
	// and cache the verdict: the long tail of blender/post-process constants then costs one
	// compare instead of walking the whole chain on every write.
	template<typename T>
	IC void OnSetCached(RHIShaderConstant* C, const T& A)
	{
		if (!C || C->fixed_id == 0) return;
		const bool hit = OnSet(C->name_hash, A);
		if (C->fixed_id < 0) C->fixed_id = hit ? 1 : 0;
	}
	template<typename T>
	IC void OnSetACached(RHIShaderConstant* C, u32 e, const T& A)
	{
		if (!C || C->fixed_id == 0) return;
		const bool hit = OnSetA(C->name_hash, e, A);
		if (C->fixed_id < 0) C->fixed_id = hit ? 1 : 0;
	}

	IC void OnSet(RHIShaderConstant* C, const Fmatrix& A) { OnSetCached(C, A); }
	IC void OnSet(RHIShaderConstant* C, const Fvector4& A) { OnSetCached(C, A); }
	IC void OnSet(RHIShaderConstant* C, float A) { OnSetCached(C, A); }
	IC void OnSet(RHIShaderConstant* C, int A) { OnSetCached(C, A); }
	IC void OnSetA(RHIShaderConstant* C, u32 e, const Fmatrix& A) { OnSetACached(C, e, A); }
	IC void OnSetA(RHIShaderConstant* C, u32 e, const Fvector4& A) { OnSetACached(C, e, A); }
}
