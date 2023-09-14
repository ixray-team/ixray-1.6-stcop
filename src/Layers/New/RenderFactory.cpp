#include "pch.h"

//# include "dx##Class.h" \

#define RENDER_FACTORY_IMPLEMENT(Class) \
	I##Class* Rendering::RenderFactory::Create##Class() \
{ \
	return xr_new<Rendering::##Class>(); \
} \
	void Rendering::RenderFactory::Destroy##Class(I##Class *pObject)\
{ \
	xr_delete((Rendering::##Class*&)pObject); \
} \

#ifndef _EDITOR
RENDER_FACTORY_IMPLEMENT(UISequenceVideoItem)
RENDER_FACTORY_IMPLEMENT(UIShader)
RENDER_FACTORY_IMPLEMENT(StatGraphRender)
RENDER_FACTORY_IMPLEMENT(ConsoleRender)
RENDER_FACTORY_IMPLEMENT(RenderDeviceRender)
#	ifdef DEBUG
RENDER_FACTORY_IMPLEMENT(ObjectSpaceRender)
#	endif // DEBUG
RENDER_FACTORY_IMPLEMENT(ApplicationRender)
RENDER_FACTORY_IMPLEMENT(WallMarkArray)
RENDER_FACTORY_IMPLEMENT(StatsRender)
#endif // _EDITOR

#ifndef _EDITOR
RENDER_FACTORY_IMPLEMENT(ThunderboltRender)
RENDER_FACTORY_IMPLEMENT(ThunderboltDescRender)
RENDER_FACTORY_IMPLEMENT(RainRender)
RENDER_FACTORY_IMPLEMENT(LensFlareRender)
RENDER_FACTORY_IMPLEMENT(EnvironmentRender)
RENDER_FACTORY_IMPLEMENT(EnvDescriptorMixerRender)
RENDER_FACTORY_IMPLEMENT(EnvDescriptorRender)
RENDER_FACTORY_IMPLEMENT(FlareRender)
#endif
RENDER_FACTORY_IMPLEMENT(FontRender)

Rendering::RenderFactory RenderFactoryInterface;