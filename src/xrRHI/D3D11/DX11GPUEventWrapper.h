#pragma once

class InternalDX11GPUEventWrapper
{
private:
	int _index = -1;

public:
	InternalDX11GPUEventWrapper(const char* name, const wchar_t* wname);
	~InternalDX11GPUEventWrapper();
};