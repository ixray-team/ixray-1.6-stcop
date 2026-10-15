#pragma once

namespace Platform
{
#ifdef IXR_WINDOWS
    intptr_t GetOpenedFileSize(int descriptor);
#else
    inline intptr_t GetOpenedFileSize(int descriptor)
    {
        return _filelengthi64(descriptor);
    }
#endif
}
