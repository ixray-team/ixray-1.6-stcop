#include "Application.h"

#ifdef __cplusplus
extern "C" {
#endif

	__declspec(dllexport) extern const uint32_t D3D12SDKVersion;
	const uint32_t D3D12SDKVersion = 619;

	__declspec(dllexport) extern const char* D3D12SDKPath;
	const char* D3D12SDKPath = "AgilitySDK/";

#ifdef __cplusplus
}
#endif

int ENTRY_FUNCTION(ENTRY_ARGS)
{
	CApplication App(ENTRY_ARGS_PUSH);
	return App.Run();
}
