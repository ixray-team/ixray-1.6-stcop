#pragma once
// This file includes icons derived from Microsoft Fluent UI System Icons.
// Microsoft Fluent UI icons are Microsoft Corporation.
// All rights belong to their respective owners.
// Used under applicable license terms.
#include <lunasvg.h>
#include "../../../xrEngine/EngineIcons.h"

namespace chezze_svg_temporary
{
    CTexture* RasterizeSvg(const char* svgText, int width, int height)
    {
        auto doc = lunasvg::Document::loadFromData(svgText);
        if (!doc)
            throw std::runtime_error("SVG parse failed");

		auto bitmap = doc->renderToBitmap(width * GUIManager->GetScaleDpi(), height * GUIManager->GetScaleDpi());
        bitmap.convertToRGBA();

        CTexture* TempTexture = new CTexture();

        RHITextureDesc Desc;
        Desc.Width = bitmap.width();
        Desc.Height = bitmap.height();
        Desc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
        //Desc.MipLevels = 1;
        //Desc.ArraySize = 1;
        Desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
        Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;

        RHISubResource SubResource{};
        SubResource.Width = bitmap.width();
        SubResource.Height = bitmap.height();
        SubResource.TextureFormat = Desc.Format;
        SubResource.RowPitch = bitmap.width() * 4;
        SubResource.Data = bitmap.data();
        SubResource.DataSize = bitmap.width() * 4;

        IRHISurface* Surf = GRHI->CreateTexture2D(Desc, SubResource);

        TempTexture->surface_set(Surf);
        Surf->Release();

        return TempTexture;
    }
}
