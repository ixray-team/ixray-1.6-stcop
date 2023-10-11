#include "StdAfx.h"

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

#ifdef DEBUG
FACTORY_PTR_INSTANCIATE(ObjectSpaceRender)
#endif
