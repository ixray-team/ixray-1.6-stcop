#pragma once
#include "dx10ConstantBuffer.h"
#include "../../../gamedata/shaders/d3d11/shared/fixed_cb.hlsli"

namespace FixedConstants
{
	// b0..b4: cb_frame, cb_view, cb_object, cb_material, cb_light
	constexpr u32 kSlots = 6;

	ECORE_API void Create();
	ECORE_API void Destroy();
	ECORE_API void UpdateFrame();
	ECORE_API void UpdateView();
	ECORE_API void UpdateObject(const Fmatrix& mW);
	ECORE_API void UpdateMaterial();
	ECORE_API void SetReflectionCapture(const Fmatrix& view, float radius, bool isValid);
	ECORE_API void SetReflectionHistory(const Fvector& jitter, bool isValid);
	ECORE_API void BindFrame();
	ECORE_API void BindView();
	ECORE_API void BindObject();
	ECORE_API void BindMaterial();
	ECORE_API void BindLight();
	ECORE_API void BindAll();
	ECORE_API void InvalidateBindings();
	ECORE_API bool IsFixedName(const char* n);
	ECORE_API int  FixedClass(const char* n);
	ECORE_API void Flush();

	ECORE_API void SetHemiMaterial(float x,float y,float z,float w);
	ECORE_API void SetHemiPosFaces(float x,float y,float z);
	ECORE_API void SetHemiNegFaces(float x,float y,float z);
	ECORE_API void SetHemiTfactor(const Fvector4& v);
	ECORE_API void SetHemiTfactor(float x,float y,float z,float w);
	ECORE_API void SetLitColor(const Fvector& c, const Fvector& dir);
	ECORE_API void SetDtParams(float x,float y,float z,float w);
	ECORE_API void SetDtParamsScale(float s);
	ECORE_API void SetParallax(float h);
	ECORE_API void SetAlphaRef(float a);
	ECORE_API void SetLModelLight(const Fvector& c, const Fvector& dir);
	ECORE_API void SetTriLOD(float lod);
	ECORE_API void SetTfactor(const Fvector4& v);
	ECORE_API void SetTreeXform(const Fmatrix& m);
	ECORE_API void SetTreeXformV(const Fmatrix& m);
	ECORE_API void SetTreeConsts(float x,float y,float z,float w);
	ECORE_API void SetTreeWave(const Fvector4& v);
	ECORE_API void SetTreeWind(const Fvector4& v);
	ECORE_API void SetTreeConstsOld(float x,float y,float z,float w);
	ECORE_API void SetTreeWaveOld(const Fvector4& v);
	ECORE_API void SetTreeWindOld(const Fvector4& v);
	ECORE_API void SetTreeCScale(float x,float y,float z,float w);
	ECORE_API void SetTreeCBias(float x,float y,float z,float w);
	ECORE_API void SetTreeCSun(float x,float y,float z,float w);
	ECORE_API void SetLMap(const Fmatrix& m);
	ECORE_API void SetShadow(const Fmatrix& m);
	ECORE_API void SetShadowSun(int idx, const Fmatrix& m);
	ECORE_API void SetLdynamic(const Fvector4& c, const Fvector4& p, const Fvector4& d);

	ECORE_API u32 NameHash(const char* n);

	ECORE_API bool OnSet(u32 h, const Fmatrix& A);
	ECORE_API bool OnSet(u32 h, const Fvector4& A);
	ECORE_API bool OnSet(u32 h, float A);
	ECORE_API bool OnSet(u32 h, int A);
	ECORE_API bool OnSetA(u32 h, u32 e, const Fmatrix& A);
	ECORE_API bool OnSetA(u32 h, u32 e, const Fvector4& A);

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
