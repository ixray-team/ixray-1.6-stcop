#include "stdafx.h"

#include "../Include/xrRender/FactoryPtr.h"

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

FACTORY_PTR_INSTANCIATE(StatsRender)

FACTORY_PTR_INSTANCIATE(ThunderboltRender)
FACTORY_PTR_INSTANCIATE(ThunderboltDescRender)
FACTORY_PTR_INSTANCIATE(EnvDescriptorRender)
FACTORY_PTR_INSTANCIATE(EnvDescriptorMixerRender)

FACTORY_PTR_INSTANCIATE(FlareRender)
FACTORY_PTR_INSTANCIATE(LensFlareRender)
FACTORY_PTR_INSTANCIATE(RainRender)
FACTORY_PTR_INSTANCIATE(EnvironmentRender)
FACTORY_PTR_INSTANCIATE(ApplicationRender)
FACTORY_PTR_INSTANCIATE(WallMarkArray)
FACTORY_PTR_INSTANCIATE(UIShader)
FACTORY_PTR_INSTANCIATE(StatGraphRender)
