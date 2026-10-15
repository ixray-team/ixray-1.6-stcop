#pragma once

namespace Platform
{
#ifdef IXR_WINDOWS
intptr_t GetOpenedFileSize(int Descriptor);
#else
inline intptr_t GetOpenedFileSize(int Descriptor)
{
	return _filelengthi64(Descriptor);
}
#endif
}  
