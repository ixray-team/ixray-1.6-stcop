#include "stdafx.h"
#undef RENDER_FACTORY_INTERFACE
#define RENDER_FACTORY_INTERFACE(Class)\
I ## Class* XRayRenderFactory::Create ## Class()\
{\
	return static_cast<I ## Class*>(new XRay ## Class());\
}\
void XRayRenderFactory::Destroy##Class(I ## Class *pObject)\
{\
	xr_delete( static_cast< XRay ## Class*>(pObject));\
}

#ifndef _EDITOR
RENDER_FACTORY_INTERFACE(UISequenceVideoItem)
RENDER_FACTORY_INTERFACE(UIShader)
RENDER_FACTORY_INTERFACE(StatGraphRender)
RENDER_FACTORY_INTERFACE(RenderDeviceRender)
#ifdef DEBUG
RENDER_FACTORY_INTERFACE(ObjectSpaceRender)
#endif
RENDER_FACTORY_INTERFACE(WallMarkArray)
RENDER_FACTORY_INTERFACE(StatsRender)
#endif // _EDITOR

#ifndef _EDITOR
RENDER_FACTORY_INTERFACE(EnvironmentRender)
RENDER_FACTORY_INTERFACE(EnvDescriptorMixerRender)
RENDER_FACTORY_INTERFACE(EnvDescriptorRender)
RENDER_FACTORY_INTERFACE(RainRender)
RENDER_FACTORY_INTERFACE(LensFlareRender)
RENDER_FACTORY_INTERFACE(ThunderboltRender)
RENDER_FACTORY_INTERFACE(ThunderboltDescRender)
RENDER_FACTORY_INTERFACE(FlareRender)
#endif // _EDITOR
RENDER_FACTORY_INTERFACE(FontRender)

#define FACTORY_PTR_INSTANCIATE(Class) \
template<> void FactoryPtr<I##Class>::CreateObject() \
{ \
m_pObject = RenderFactory->Create##Class(); \
} \
\
template<> void FactoryPtr<I##Class>::DestroyObject() \
{ \
RenderFactory->Destroy##Class(m_pObject); \
m_pObject = nullptr; \
}

FACTORY_PTR_INSTANCIATE(UIShader)
