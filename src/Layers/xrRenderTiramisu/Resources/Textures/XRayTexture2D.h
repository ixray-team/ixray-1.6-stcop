#pragma once
#include "XRayTexture.h"

namespace RedImageTool
{
    class RedImage;
}

class XRayTexture2D:public XRayTexture
{
public:
            XRayTexture2D   (const shared_str& InName = "None");
    bool    LoadFromFile    (const char* FilePath, bool bSrgb = false);
    bool    LoadFromImage   (const RedImageTool::RedImage& FromImage, bool bSrgb = false);
};
