#pragma once
#include "../../../Include/xrRender/RenderVisual.h"
class XRayRenderVisual :public IRenderVisual
{
public:
	virtual ~XRayRenderVisual();
	virtual void				Load(const char* N, IReader* data, u32 dwFlags);
	virtual void				Copy(XRayRenderVisual* from);
	virtual void				Depart() {};
	virtual void				Spawn() {};
	virtual void				Release() {}
	virtual u32					getType();
	virtual vis_data& 	getVisData();
	//virtual bool Render(float LOD, EShaderElement SEType, XRayObjectRender& Item) {  return false; }
	virtual shared_str		getDebugName() ;
	//virtual void UpdateUniform(XRayUniformAllocator::EUniformType Type, void* ptr) {}
public:
	//XRayShader Shader;
	vis_data					Vis;
	u32 Type;
	shared_str DebugName;
};