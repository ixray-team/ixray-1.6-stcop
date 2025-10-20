#pragma once

namespace DedicatedConsoleInput
{
	void Start();
	void Stop();
	void HandleLogLine(const xr_string& utf8Text, u32 originalLength);
}

