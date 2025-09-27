#pragma once

class InternalDX11GPUEventWrapper
{
private:
	int _index;

public:
	InternalDX11GPUEventWrapper(const char* name, const wchar_t* wname);
	~InternalDX11GPUEventWrapper();
};