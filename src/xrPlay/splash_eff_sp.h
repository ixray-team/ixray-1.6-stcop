#pragma once
namespace splash
{
    namespace spooky
    {
        int getBackgroundID(EEngineExternalPlatform platform)
        {
            if (platform == EEngineExternalPlatform::ShadowOfChernobyl)
                return IDB_SOC_SPLASH_BG_HW;

            if (platform == EEngineExternalPlatform::ClearSky)
                return IDB_CS_SPLASH_BG_HW;

            return IDB_COP_SPLASH_BG_HW;
        }
    }
}