#include "RHI.h"
#include "RHIDXC.h"
#include <dxcapi.h>
#include <algorithm>
#include <string>

struct RHIDxc
{
	IDxcUtils* Utils = nullptr;
	IDxcCompiler3* Compiler = nullptr;
};

static RHIDxc& Dxc()
{
	static RHIDxc dxc = []
	{
		RHIDxc result;
		// DXC loads the external validator when its DLL is initialized.
		// Keep it loaded so compiled containers receive a runtime-valid signature.
		static const HMODULE validator = LoadLibraryA("dxil.dll");
		if (!validator)
		{
			Msg("! dxil.dll is missing; using DXBC shaders instead of unsigned DXIL");
			return result;
		}
		HMODULE module = LoadLibraryA("dxcompiler.dll");
		auto create = module ? (DxcCreateInstanceProc)GetProcAddress(module, "DxcCreateInstance") : nullptr;
		if (!create || FAILED(create(CLSID_DxcUtils, IID_PPV_ARGS(&result.Utils))) ||
			FAILED(create(CLSID_DxcCompiler, IID_PPV_ARGS(&result.Compiler))))
		{
			Msg("! dxcompiler.dll is missing or incompatible; DXIL shaders cannot be compiled");
			result = {};
		}
		return result;
	}();
	return dxc;
}

static void ExpandIncludes(IRHIShaderInclude* include, const char* text, size_t size, const std::string& name, std::string& out, xr_vector<std::string>& stack)
{
	stack.push_back(name);
	std::string label = name;
	std::replace(label.begin(), label.end(), '\\', '/');
	out += "#line 1 \"" + label + "\"\n";
	u32 line = 0;
	const char* const last = text + size;
	for (const char* cursor = text; cursor < last;)
	{
		const char* end = static_cast<const char*>(memchr(cursor, '\n', last - cursor));
		end = end ? end + 1 : last;
		++line;
		const char* word = cursor;
		while (word < end && (*word == ' ' || *word == '\t'))
		{
			++word;
		}
		const char* quote = nullptr;
		if (word < end && *word == '#')
		{
			for (++word; word < end && (*word == ' ' || *word == '\t'); ++word);
			if (end - word > 7 && !memcmp(word, "include", 7))
			{
				quote = static_cast<const char*>(memchr(word + 7, '"', end - word - 7));
			}
		}
		const char* close = quote ? static_cast<const char*>(memchr(quote + 1, '"', end - quote - 1)) : nullptr;
		const void* data = nullptr;
		u32 bytes = 0;
		if (close && include)
		{
			const std::string file(quote + 1, close);
			if (std::find(stack.begin(), stack.end(), file) != stack.end())
			{
				cursor = end;
				continue;
			}
			if (SUCCEEDED(include->Open(0, file.c_str(), nullptr, &data, &bytes)))
			{
				ExpandIncludes(include, static_cast<const char*>(data), bytes, file, out, stack);
				include->Close(data);
				out += "#line " + std::to_string(line + 1) + " \"" + label + "\"\n";
				cursor = end;
				continue;
			}
			Msg("! Shader include '%s' not found (from '%s')", file.c_str(), name.c_str());
		}
		out.append(cursor, end);
		cursor = end;
	}
	if (out.back() != '\n')
	{
		out += '\n';
	}
	stack.pop_back();
}

static RHIBlob* MakeBlob(const void* data, size_t size)
{
	auto blob = new RHIBlob(size);
	memcpy(blob->GetBufferPointer(), data, size);
	return blob;
}

bool RHI_IsDXIL(const void* code, size_t size)
{
	struct Header { u32 Magic; u8 Hash[16]; u16 Major, Minor; u32 Size, Parts; };
	auto bytes = static_cast<const u8*>(code);
	auto header = static_cast<const Header*>(code);
	if (size < sizeof(Header) || header->Magic != 'CBXD' || header->Parts > (size - sizeof(Header)) / sizeof(u32))
	{
		return false;
	}
	for (u32 part_idx = 0; part_idx < header->Parts; ++part_idx)
	{
		const u32 offset = reinterpret_cast<const u32*>(bytes + sizeof(Header))[part_idx];
		if (offset <= size - sizeof(u32) && *reinterpret_cast<const u32*>(bytes + offset) == 'LIXD')
		{
			return true;
		}
	}
	return false;
}

bool RHI_IsUnsignedDXIL(const void* code, size_t size)
{
	if (!RHI_IsDXIL(code, size))
	{
		return false;
	}
	// The container digest follows its four-byte magic; zero means unsigned.
	const auto bytes = static_cast<const u8*>(code);
	for (u32 index = 4; index < 20; ++index)
	{
		if (bytes[index])
		{
			return false;
		}
	}
	return true;
}

HRESULT RHI_DxcCompile(const void* source, size_t size, const char* name, const RHIShaderMacro* macros, IRHIShaderInclude* include,
	const char* entry, const char* target, u32 flags, RHIBlob** out_code, RHIBlob** out_errors)
{
	auto& dxc = Dxc();
	if (!dxc.Compiler)
	{
		return E_FAIL;
	}
	const std::wstring shaderTarget = std::wstring(1, wchar_t(target[0])) + L"s_6_0";
	std::vector<std::wstring> arguments = { L"-E", std::wstring(entry, entry + strlen(entry)), L"-T", shaderTarget, L"-HV", L"2018", L"-flegacy-resource-reservation" };
	if (flags & RHI_SHADER_PACK_MATRIX_ROW_MAJOR)
	{
		arguments.push_back(L"-Zpr");
	}
	if (flags & RHI_SHADER_SKIP_OPTIMIZATION)
	{
		arguments.push_back(L"-Od");
	}
	else if (flags & RHI_SHADER_OPTIMIZATION_LEVEL3)
	{
		arguments.push_back(L"-O3");
	}
	if (flags & RHI_SHADER_DEBUG)
	{
		arguments.push_back(L"-Zi");
		arguments.push_back(L"-Qembed_debug");
	}
	for (u32 macro_idx = 0; macros && macros[macro_idx].Name; ++macro_idx)
	{
		const std::string define = std::string("-D") + macros[macro_idx].Name + "=" + (macros[macro_idx].Definition ? macros[macro_idx].Definition : "1");
		arguments.emplace_back(define.begin(), define.end());
	}
	std::vector<LPCWSTR> pointers;
	for (const auto& argument : arguments)
	{
		pointers.push_back(argument.c_str());
	}
	std::string expanded;
	xr_vector<std::string> stack;
	ExpandIncludes(include, static_cast<const char*>(source), size, name, expanded, stack);
	DxcBuffer buffer = { expanded.data(), expanded.size(), DXC_CP_ACP };
	IDxcResult* result = nullptr;
	HRESULT status = dxc.Compiler->Compile(&buffer, pointers.data(), UINT32(pointers.size()), nullptr, IID_PPV_ARGS(&result));
	if (SUCCEEDED(status))
	{
		result->GetStatus(&status);
	}
	IDxcBlobUtf8* errors = nullptr;
	if (result && FAILED(status) && SUCCEEDED(result->GetOutput(DXC_OUT_ERRORS, IID_PPV_ARGS(&errors), nullptr)) && errors && errors->GetStringLength())
	{
		*out_errors = MakeBlob(errors->GetStringPointer(), errors->GetStringLength() + 1);
	}
	IDxcBlob* object = nullptr;
	if (SUCCEEDED(status) && SUCCEEDED(result->GetOutput(DXC_OUT_OBJECT, IID_PPV_ARGS(&object), nullptr)) && object)
	{
		if (RHI_IsUnsignedDXIL(object->GetBufferPointer(), object->GetBufferSize()))
		{
			Msg("! DXC produced unsigned DXIL for '%s'; using DXBC instead. Check dxil.dll compatibility", name);
			status = E_FAIL;
		}
		else
		{
			*out_code = MakeBlob(object->GetBufferPointer(), object->GetBufferSize());
		}
	}
	else if (SUCCEEDED(status))
	{
		status = E_FAIL;
	}
	if (object) object->Release();
	if (errors) errors->Release();
	if (result) result->Release();
	return status;
}

HRESULT RHI_DxcReflect(const void* code, size_t size, ID3D12ShaderReflection** out_reflection)
{
	auto& dxc = Dxc();
	if (!dxc.Utils)
	{
		return E_FAIL;
	}
	DxcBuffer buffer = { code, size, 0 };
	return dxc.Utils->CreateReflection(&buffer, IID_PPV_ARGS(out_reflection));
}
