#include "XRayShaderCompilerDesktop.h"

#include "Resources/Shaders/XRayShaderType.h"
#include "Resources/Shaders/Defines/XRayShaderDefinesContainer.h"

#if IXR_ENABLE_SHADER_COMPILER
#include <wrl/client.h>
#include "dxcapi.h"

#define DX_CHK(expr) R_ASSERT(SUCCEEDED(expr));
#include "ShaderIncluder.h"

std::wstring Utf8ToWide(const xr_string& str)
{
	if (str.empty())
		return {};

	int size = MultiByteToWideChar(CP_UTF8, 0, str.data(), (int)str.size(), nullptr, 0);

	std::wstring result(size, L'\0');
	MultiByteToWideChar(CP_UTF8, 0, str.data(), (int)str.size(), result.data(), size);

	return result;
}

bool XRayShaderCompilerDesktop::CompileDX12(const XRayShaderDefinesContainer& Defines, EXRayShaderType ShaderType, const xr_vector<xr_string>& IncludePaths, const char* SourceFileName, const char* ResultFileName, xr_string& OutMessage)
{
	DXCInluder LIncluder(DxcLibrary.Get());

	xr_vector<const wchar_t*> Arguments;
	xr_vector<std::wstring> UnicodeDefines;

	Microsoft::WRL::ComPtr<IDxcResult> Result;

	bool bHLSLFileIsUTF8;
	xr_vector<char> HLSLFile;
	ReadTextFile(SourceFileName, HLSLFile, bHLSLFileIsUTF8);
	UnicodeDefines.reserve(Defines.GetDefines().size() + IncludePaths.size() + 4);

	if (bDebugShader)
	{
		Arguments.push_back(L"-Od");
	}
	else
	{
		Arguments.push_back(L"-O3");
	}

	switch (ShaderType)
	{
	case EXRayShaderType::Pixel:
		Arguments.push_back(L"-Tps_6_6");
		break;
	case EXRayShaderType::Hull:
		Arguments.push_back(L"-Ths_6_6");
		break;
	case EXRayShaderType::Domain:
		Arguments.push_back(L"-Tds_6_6");
		break;
	case EXRayShaderType::Geometry:
		Arguments.push_back(L"-Tgs_6_6");
		break;
	case EXRayShaderType::Vertex:
		Arguments.push_back(L"-Tvs_6_6");

		break;
	case EXRayShaderType::Compute:
		Arguments.push_back(L"-Tcs_6_6");
		break;
	default:
		NODEFAULT;
		break;
	}
	Arguments.push_back(L"-E");
	Arguments.push_back(L"Main");
	// Store defines in managed buffers

	for (const shared_str& define : Defines.GetDefines())
	{
		xr_string Define = define.c_str();
		Define.append("=1");
		std::wstring& UniDefine = UnicodeDefines.emplace_back(Utf8ToWide(Define));

		Arguments.push_back(L"-D");
		Arguments.push_back(UniDefine.c_str());
	}
	
	Arguments.push_back(L"-D");
	Arguments.push_back(L"NRI_ENABLE_DRAW_PARAMETERS_EMULATION=1");
	
	if (bNeedCreateShaderPDB)
	{
		Arguments.push_back(L"-Zi");
		string_path PDBFileName;
		xr_strconcat(PDBFileName, ResultFileName, ".pdb");
		Arguments.push_back(L"-Fd");
		std::wstring& UniDefine = UnicodeDefines.emplace_back(Utf8ToWide(PDBFileName));
		Arguments.push_back(UniDefine.c_str());
	}

	for (const xr_string& path : IncludePaths)
	{
		std::wstring& UniDefine = UnicodeDefines.emplace_back(Utf8ToWide(path));
		Arguments.push_back(L"-I");
		Arguments.push_back(UniDefine.c_str());
	}

	{
		std::wstring& UniDefine = UnicodeDefines.emplace_back(Utf8ToWide(SourceFileName));
		Arguments.push_back(UniDefine.c_str());
	}

	DxcBuffer Buffer;
	Buffer.Ptr = HLSLFile.data();
	Buffer.Size = HLSLFile.size();
	Buffer.Encoding = bHLSLFileIsUTF8 ? DXC_CP_UTF8 : DXC_CP_ACP;

	DX_CHK(DxcCompiler->Compile(&Buffer, (LPCWSTR*)Arguments.data(), static_cast<UINT32>(Arguments.size()), &LIncluder, IID_PPV_ARGS(&Result)));

	HRESULT ResultCode;
	DX_CHK(Result->GetStatus(&ResultCode));
	if (FAILED(ResultCode))
	{
		IDxcBlobEncoding* PError = nullptr;
		DX_CHK(Result->GetErrorBuffer(&PError));

		xr_string InfoLog((LPCSTR)PError->GetBufferPointer(), PError->GetBufferSize());
		OutMessage.assign(InfoLog.c_str());
		return false;
	}
	else
	{
		IDxcBlobEncoding* PError = nullptr;
		DX_CHK(Result->GetErrorBuffer(&PError));
		if (PError->GetBufferSize())
		{
			xr_string InfoLog((LPCSTR)PError->GetBufferPointer(), PError->GetBufferSize());
			OutMessage.assign(InfoLog.c_str());
		}
	}

	Microsoft::WRL::ComPtr<IDxcBlobUtf16> pShaderName = nullptr;
	IDxcBlob* pShader = nullptr;
	IDxcBlob* pPDBShader = nullptr;

	DX_CHK(Result->GetOutput(DXC_OUT_OBJECT, IID_PPV_ARGS(&pShader), &pShaderName));
	if (bNeedCreateShaderPDB)
	{
		DX_CHK(Result->GetOutput(DXC_OUT_PDB, IID_PPV_ARGS(&pPDBShader), &pShaderName));
		string_path FileName;
		xr_strconcat(FileName, ResultFileName, ".pdb");

		CMemoryWriter File;
		File.w(pPDBShader->GetBufferPointer(), pPDBShader->GetBufferSize());
		File.save_to(FileName);
		pPDBShader->Release();
	}
	{

		CMemoryWriter File;
		string_path FileName;
		xr_strconcat(FileName, ResultFileName, ".checksum");
		File.w_u8(bDebugShader);
		File.w_u8(bNeedCreateShaderPDB);
		File.w_u32(HLSLFile.size());
		File.w_u32(crc32(HLSLFile.data(), HLSLFile.size()));
		File.w_u32(Defines.GetDefines().size());
		File.w_u32(Defines.GetCRC32());
		File.w_u32(GetIncludeCrc32());
		File.w_u32(GetIncludeCount());
		File.w_u32(GetIncludeSize());
		File.save_to(FileName);
	}

	CMemoryWriter File;
	File.w(pShader->GetBufferPointer(), pShader->GetBufferSize());
	File.save_to(ResultFileName);

	pShader->Release();
	return true;
}

bool XRayShaderCompilerDesktop::CompileDX11(const XRayShaderDefinesContainer& Defines, EXRayShaderType ShaderType, const xr_vector<xr_string>& IncludePaths, const char* SourceFileName, const char* ResultFileName, xr_string& OutMessage)
{
	NODEFAULT;
	return false;
}

bool XRayShaderCompilerDesktop::Compile(const XRayShaderDefinesContainer& Defines, EXRayShaderType ShaderType, const xr_vector<xr_string>& IncludePaths, const char* SourceFileName, const char* ResultFileName, xr_string& OutMessage)
{
	switch (GraphicsAPI)
	{
		case nri::GraphicsAPI::D3D11:
		{
			return CompileDX11(Defines, ShaderType, IncludePaths, SourceFileName, ResultFileName, OutMessage);
		}
		case nri::GraphicsAPI::D3D12:
		{
			return CompileDX12(Defines, ShaderType, IncludePaths, SourceFileName, ResultFileName, OutMessage);
		}
		case nri::GraphicsAPI::VK:
		{
			return CompileVK(Defines, ShaderType, IncludePaths, SourceFileName, ResultFileName, OutMessage);
		}
		default:
		{
			NODEFAULT;
			break;
		}
	}
	return false;
}

void XRayShaderCompilerDesktop::ReadFile(const char* Name, xr_vector<char>& Data)
{
	FILE* File = fopen(Name, "rb");
	R_ASSERT(File);
	fseek(File, 0, SEEK_END);
	size_t Size = _ftelli64(File);
	fseek(File, 0, SEEK_SET);
	Data.resize(Size);
	fread(Data.data(), 1, Size, File);
	fclose(File);
}

bool XRayShaderCompilerDesktop::FileExists(const char* name)
{
	return std::filesystem::exists(name);
}

XRayShaderCompilerDesktop::XRayShaderCompilerDesktop(nri::GraphicsAPI InGraphicsAPI, bool NeedCreateShaderPDB, bool DebugShader):XRayShaderCompilerBase(NeedCreateShaderPDB, DebugShader)
{
	GraphicsAPI = InGraphicsAPI;
	DX_CHK(DxcCreateInstance(CLSID_DxcCompiler, IID_PPV_ARGS(DxcCompiler.ReleaseAndGetAddressOf())));
	DX_CHK(DxcCreateInstance(CLSID_DxcLibrary, IID_PPV_ARGS(DxcLibrary.ReleaseAndGetAddressOf())));
}

const char* XRayShaderCompilerDesktop::GetDirectionName()
{
	switch (GraphicsAPI)
	{
		case nri::GraphicsAPI::D3D11: return "desktop\\dxbc\\";
		case nri::GraphicsAPI::D3D12: return "desktop\\dxil\\";
		case nri::GraphicsAPI::VK:    return "desktop\\spirv\\";
	}
	return "desktop\\";
}

bool XRayShaderCompilerDesktop::CompileVK(const XRayShaderDefinesContainer& Defines, EXRayShaderType ShaderType, const xr_vector<xr_string>& IncludePaths, const char* SourceFileName, const char* ResultFileName, xr_string& OutMessage)
{
	DXCInluder LIncluder(DxcLibrary.Get());

	xr_vector<const xr_special_char*> Arguments;
	xr_vector<std::wstring> UnicodeDefines;

	Microsoft::WRL::ComPtr<IDxcResult> Result;

	bool bHLSLFileIsUTF8;
	xr_vector<char> HLSLFile;
	ReadTextFile(SourceFileName, HLSLFile, bHLSLFileIsUTF8);
	UnicodeDefines.reserve(Defines.GetDefines().size() + IncludePaths.size() + 4);

	Arguments.push_back(L"-spirv");
	Arguments.push_back(L"-fspv-target-env=vulkan1.3");
	//Arguments.push_back(L"-fvk-support-nonzero-base-instance");

	Arguments.push_back(L"-fvk-s-shift");
	Arguments.push_back(L"0");
	Arguments.push_back(L"0");

	Arguments.push_back(L"-fvk-t-shift");
	Arguments.push_back(L"128");
	Arguments.push_back(L"0");

	Arguments.push_back(L"-fvk-b-shift");
	Arguments.push_back(L"32");
	Arguments.push_back(L"2");

	Arguments.push_back(L"-fvk-u-shift");
	Arguments.push_back(L"64");
	Arguments.push_back(L"0");

	Arguments.push_back(L"-fvk-bind-resource-heap");
	Arguments.push_back(L"0");
	Arguments.push_back(L"0");

	Arguments.push_back(L"-fvk-bind-sampler-heap");
	Arguments.push_back(L"1");
	Arguments.push_back(L"1");


	if (bDebugShader)
	{
		Arguments.push_back(L"-Od");
	}
	else
	{
		Arguments.push_back(L"-O3");
	}

	switch (ShaderType)
	{
	case EXRayShaderType::Pixel:
		Arguments.push_back(L"-Tps_6_6");
		break;
	case EXRayShaderType::Hull:
		Arguments.push_back(L"-Ths_6_6");
		break;
	case EXRayShaderType::Domain:
		Arguments.push_back(L"-Tds_6_6");
		break;
	case EXRayShaderType::Geometry:
		Arguments.push_back(L"-Tgs_6_6");
		break;
	case EXRayShaderType::Vertex:
		Arguments.push_back(L"-Tvs_6_6");

		break;
	case EXRayShaderType::Compute:
		Arguments.push_back(L"-Tcs_6_6");
		break;
	default:
		NODEFAULT;
		break;
	}
	Arguments.push_back(L"-E");
	Arguments.push_back(L"Main");
	// Store defines in managed buffers

	for (const shared_str& define : Defines.GetDefines())
	{
		xr_string Define = define.c_str();
		Define.append("=1");
		std::wstring& UniDefine = UnicodeDefines.emplace_back(Utf8ToWide(Define));

		Arguments.push_back(L"-D");
		Arguments.push_back(UniDefine.c_str());
	}
	

	if (bNeedCreateShaderPDB)
	{
		Arguments.push_back(L"-Zi");
	}

	for (const xr_string& path : IncludePaths)
	{
		std::wstring& UniDefine = UnicodeDefines.emplace_back(Utf8ToWide(path));
		Arguments.push_back(L"-I");
		Arguments.push_back(UniDefine.c_str());
	}

	{
		std::wstring& UniDefine = UnicodeDefines.emplace_back(Utf8ToWide(SourceFileName));
		Arguments.push_back(UniDefine.c_str());
	}

	DxcBuffer Buffer;
	Buffer.Ptr = HLSLFile.data();
	Buffer.Size = HLSLFile.size();
	Buffer.Encoding = bHLSLFileIsUTF8 ? DXC_CP_UTF8 : DXC_CP_ACP;

	DX_CHK(DxcCompiler->Compile(&Buffer, (LPCWSTR*)Arguments.data(), static_cast<UINT32>(Arguments.size()), &LIncluder, IID_PPV_ARGS(&Result)));

	HRESULT ResultCode;
	DX_CHK(Result->GetStatus(&ResultCode));
	if (FAILED(ResultCode))
	{
		IDxcBlobEncoding* PError = nullptr;
		DX_CHK(Result->GetErrorBuffer(&PError));

		xr_string InfoLog((LPCSTR)PError->GetBufferPointer(), PError->GetBufferSize());
		OutMessage.assign(InfoLog.c_str());
		return false;
	}
	else
	{
		IDxcBlobEncoding* PError = nullptr;
		DX_CHK(Result->GetErrorBuffer(&PError));
		if (PError->GetBufferSize())
		{
			xr_string InfoLog((LPCSTR)PError->GetBufferPointer(), PError->GetBufferSize());
			OutMessage.assign(InfoLog.c_str());
		}
	}

	Microsoft::WRL::ComPtr<IDxcBlobUtf16> pShaderName = nullptr;
	IDxcBlob* pShader = nullptr;
	IDxcBlob* pPDBShader = nullptr;

	DX_CHK(Result->GetOutput(DXC_OUT_OBJECT, IID_PPV_ARGS(&pShader), &pShaderName));

	{
		CMemoryWriter File;
		string_path FileName;
		xr_strconcat(FileName, ResultFileName, ".checksum");
		File.w_u8(bDebugShader);
		File.w_u8(bNeedCreateShaderPDB);
		File.w_u32(HLSLFile.size());
		File.w_u32(crc32(HLSLFile.data(), HLSLFile.size()));
		File.w_u32(Defines.GetDefines().size());
		File.w_u32(Defines.GetCRC32());
		File.w_u32(GetIncludeCrc32());
		File.w_u32(GetIncludeCount());
		File.w_u32(GetIncludeSize());
		File.save_to(FileName);
	}

	CMemoryWriter File;
	File.w(pShader->GetBufferPointer(), pShader->GetBufferSize());
	File.save_to(ResultFileName);

	pShader->Release();
	return true;
}

#endif
