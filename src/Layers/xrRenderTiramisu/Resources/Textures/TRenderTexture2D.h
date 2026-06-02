#pragma once
#include "TRenderTexture.h"

namespace RedImageTool
{
    class RedImage;
}

class TRenderTexture2D :
    public TRenderTexture
{
public:
            TRenderTexture2D   (const shared_str& InName = "None");
    bool    LoadFromFile    (const char* FilePath, bool bSrgb = false);
    bool    LoadFromImage   (const RedImageTool::RedImage& FromImage, bool bSrgb = false);
};
