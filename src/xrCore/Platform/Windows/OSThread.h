#pragma once
using ThreadID = HANDLE;

namespace Platform
{
	unsigned int GetCoresCount();
	ThreadID GetCurrentThread();
}