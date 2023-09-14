#pragma once
#include "../../Include/xrRender/RenderFactory.h"

#include "Renders/StatGraphRender.h"
#include "Renders/LensFlareRender.h"
#include "Renders/ConsoleRender.h"
#include "Renders/ThunderboltRender.h"
#include "Renders/ThunderboltDescRender.h"
#include "Renders/RainRender.h"
#include "Renders/EnvironmentRender.h"
#include "Renders/RenderDeviceRender.h"
#include "Renders/ObjectSpaceRender.h"
#include "Renders/FontRender.h"
#include "Renders/ApplicationRender.h"
#include "Renders/WallMarkArray.h"
#include "Renders/StatsRender.h"
#include "Renders/UISequenceVideoItem.h"
#include "Renders/UIShader.h"

#define RENDER_FACTORY_DECLARE(Class) \
	virtual I##Class* Create##Class(); \
	virtual void Destroy##Class(I##Class *pObject);

namespace Rendering
{
	class RenderFactory : public IRenderFactory
	{
		RENDER_FACTORY_DECLARE(UISequenceVideoItem)
		RENDER_FACTORY_DECLARE(UIShader)
		RENDER_FACTORY_DECLARE(StatGraphRender)
		RENDER_FACTORY_DECLARE(ConsoleRender)
		RENDER_FACTORY_DECLARE(RenderDeviceRender)
#	ifdef DEBUG
		RENDER_FACTORY_DECLARE(ObjectSpaceRender)
#	endif
		RENDER_FACTORY_DECLARE(ApplicationRender)
		RENDER_FACTORY_DECLARE(WallMarkArray)
		RENDER_FACTORY_DECLARE(StatsRender)

		RENDER_FACTORY_DECLARE(FlareRender)
		RENDER_FACTORY_DECLARE(ThunderboltRender)
		RENDER_FACTORY_DECLARE(ThunderboltDescRender)
		RENDER_FACTORY_DECLARE(RainRender)
		RENDER_FACTORY_DECLARE(LensFlareRender)
		RENDER_FACTORY_DECLARE(EnvironmentRender)
		RENDER_FACTORY_DECLARE(EnvDescriptorMixerRender)
		RENDER_FACTORY_DECLARE(EnvDescriptorRender)
		RENDER_FACTORY_DECLARE(FontRender)
	};
}