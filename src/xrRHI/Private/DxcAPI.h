#pragma once
#include <stddef.h>

// This interface deliberately avoids both DXC and engine platform headers:
// DXC's native COM types differ from the ones provided by DXVK and DXMT.
class DxcObject final
{
public:
	DxcObject() = default;
	~DxcObject();
	DxcObject(const DxcObject&) = delete;
	DxcObject& operator=(const DxcObject&) = delete;

	void* Get() const { return Object; }
	void** Put();
	const void* Data() const;
	size_t Size() const;
	void Reset();

private:
	void* Object = nullptr;
};

namespace DxcAPI
{
int Create(void (*CreateFunction)(), DxcObject& Utils, DxcObject& Compiler);
int Compile(const DxcObject& Compiler, const void* Source, size_t Size, const wchar_t** Arguments, size_t ArgumentCount, DxcObject& Code, DxcObject& Errors);
int Reflect(const DxcObject& Utils, const void* Code, size_t Size, const void* InterfaceId, void** Reflection);
} // namespace DxcAPI
