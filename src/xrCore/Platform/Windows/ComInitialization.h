#pragma once

#include <objbase.h>

namespace Platform
{
class ComInitialization
{
public:
	ComInitialization() : Result(CoInitializeEx(nullptr, COINIT_MULTITHREADED)) {}
	~ComInitialization()
	{
		if (SUCCEEDED(Result))
		{
			CoUninitialize();
		}
	}
	ComInitialization(const ComInitialization&) = delete;
	ComInitialization& operator=(const ComInitialization&) = delete;
	bool IsReady() const { return SUCCEEDED(Result) || Result == RPC_E_CHANGED_MODE; }

private:
	HRESULT Result;
};
} // namespace Platform
