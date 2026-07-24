#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuRenderTexture.h"

namespace RedImageTool
{
class RedImage;
}

// Обычная двумерная texture с загрузкой через resource manager.
class TiramisuRenderTexture2D :
	public TiramisuRenderTexture
{
public:
	TiramisuRenderTexture2D(const shared_str& InName = "None");
	bool LoadFromFile(const char* FilePath, bool bSrgb = false);
	bool LoadFromImage(const RedImageTool::RedImage& FromImage, bool bSrgb = false);
};
