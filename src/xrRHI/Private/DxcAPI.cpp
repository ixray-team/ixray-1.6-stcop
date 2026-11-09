// Do not include RHI.h here: native DXC supplies its own COM definitions.
#include "DxcAPI.h"
#ifdef _WIN32
#include <unknwn.h>
#endif
#include <dxcapi.h>

DxcObject::~DxcObject()
{
	Reset();
}

void** DxcObject::Put()
{
	Reset();
	return &Object;
}

const void* DxcObject::Data() const
{
	return Object ? static_cast<IDxcBlob*>(Object)->GetBufferPointer() : nullptr;
}

size_t DxcObject::Size() const
{
	return Object ? static_cast<IDxcBlob*>(Object)->GetBufferSize() : 0;
}

void DxcObject::Reset()
{
	if (Object)
	{
		static_cast<IUnknown*>(Object)->Release();
		Object = nullptr;
	}
}

int DxcAPI::Create(void (*CreateFunction)(), DxcObject& Utils, DxcObject& Compiler)
{
	const auto CreateInstance = reinterpret_cast<DxcCreateInstanceProc>(CreateFunction);
	if (!CreateInstance)
	{
		return E_FAIL;
	}
	int Result = CreateInstance(CLSID_DxcUtils, __uuidof(IDxcUtils), Utils.Put());
	if (SUCCEEDED(Result))
	{
		Result = CreateInstance(CLSID_DxcCompiler, __uuidof(IDxcCompiler3), Compiler.Put());
	}
	if (FAILED(Result))
	{
		Compiler.Reset();
		Utils.Reset();
	}
	return Result;
}

int DxcAPI::Compile(const DxcObject& Compiler, const void* Source, size_t Size, const wchar_t** Arguments, size_t ArgumentCount, DxcObject& Code, DxcObject& Errors)
{
	DxcBuffer Buffer = {Source, Size, DXC_CP_ACP};
	DxcObject ResultObject;
	auto Status = static_cast<IDxcCompiler3*>(Compiler.Get())->Compile(&Buffer, Arguments, static_cast<unsigned int>(ArgumentCount), nullptr, __uuidof(IDxcResult), ResultObject.Put());
	auto Result = static_cast<IDxcResult*>(ResultObject.Get());
	if (SUCCEEDED(Status))
	{
		if (!Result)
		{
			return E_FAIL;
		}
		const auto QueryStatus = Result->GetStatus(&Status);
		if (FAILED(QueryStatus))
		{
			return QueryStatus;
		}
	}
	if (Result && FAILED(Status))
	{
		Result->GetOutput(DXC_OUT_ERRORS, __uuidof(IDxcBlobUtf8), Errors.Put(), nullptr);
	}
	if (SUCCEEDED(Status))
	{
		Status = Result->GetOutput(DXC_OUT_OBJECT, __uuidof(IDxcBlob), Code.Put(), nullptr);
		if (SUCCEEDED(Status) && !Code.Get())
		{
			Status = E_FAIL;
		}
	}
	return Status;
}

int DxcAPI::Reflect(const DxcObject& Utils, const void* Code, size_t Size, const void* InterfaceId, void** Reflection)
{
	DxcBuffer Buffer = {Code, Size, 0};
	return static_cast<IDxcUtils*>(Utils.Get())->CreateReflection(&Buffer, *static_cast<const GUID*>(InterfaceId), Reflection);
}
