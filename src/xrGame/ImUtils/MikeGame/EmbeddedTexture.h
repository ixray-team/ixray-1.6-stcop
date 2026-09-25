#pragma once

#include <wrl/client.h>
namespace v_obj
{
    struct Texture
    {
		IRHIShaderResourceView* srv;

        int width = 0;
        int height = 0;

        bool IsValid() const
        {
            return srv != nullptr;
        }
    };

    Texture LoadTexture(
        const unsigned char* data,
        size_t size);

}