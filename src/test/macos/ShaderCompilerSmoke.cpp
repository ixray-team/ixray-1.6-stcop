#include <d3dcompiler.h>
#include <cstdio>
#include <cstring>
#include <memory>

template <typename T>
struct ComRelease
{
	void operator()(T* Value) const
	{
		if (Value)
		{
			Value->Release();
		}
	}
};

template <typename T>
using ComPtr = std::unique_ptr<T, ComRelease<T>>;

class ShaderInclude final : public ID3DInclude
{
public:
	HRESULT Open(D3D_INCLUDE_TYPE, const char* Name, const void*, const void** Data, UINT* Bytes) override
	{
		if (strcmp(Name, "position.h") != 0)
		{
			return E_FAIL;
		}
		static constexpr char Source[] = "float4 Position(float4 V) { return V * FACTOR; }";
		*Data = Source;
		*Bytes = sizeof(Source) - 1;
		Opened = true;
		return S_OK;
	}

	HRESULT Close(const void*) override
	{
		return S_OK;
	}

	bool Opened = false;
};

int main()
{
	constexpr char Source[] = "#include \"position.h\"\ncbuffer Constants : register(b0) { float4 Offset; }; float4 main(float4 V : POSITION) : SV_Position { return Position(V) + Offset; }";
	D3D_SHADER_MACRO Defines[] = {{"FACTOR", "2.0"}, {nullptr, nullptr}};
	ShaderInclude Include;
	ID3DBlob* RawCode = nullptr;
	ID3DBlob* RawErrors = nullptr;
	HRESULT Result = D3DCompile(Source, sizeof(Source) - 1, "native-smoke.hlsl", Defines, &Include, "main", "vs_5_0", D3DCOMPILE_PACK_MATRIX_ROW_MAJOR, 0, &RawCode, &RawErrors);
	ComPtr<ID3DBlob> Code(RawCode);
	ComPtr<ID3DBlob> Errors(RawErrors);
	if (FAILED(Result) || !Code || !Include.Opened)
	{
		if (Errors)
		{
			fwrite(Errors->GetBufferPointer(), 1, Errors->GetBufferSize(), stderr);
		}
		return 1;
	}
	constexpr GUID ReflectionID = {0x8d536ca1, 0x0cca, 0x4956, {0xa8, 0x37, 0x78, 0x69, 0x63, 0x75, 0x55, 0x84}};
	ID3D11ShaderReflection* RawReflection = nullptr;
	Result = D3DReflect(Code->GetBufferPointer(), Code->GetBufferSize(), ReflectionID, reinterpret_cast<void**>(&RawReflection));
	ComPtr<ID3D11ShaderReflection> Reflection(RawReflection);
	if (FAILED(Result) || !Reflection)
	{
		return 2;
	}
	D3D11_SHADER_DESC Description = {};
	if (FAILED(Reflection->GetDesc(&Description)) || Description.ConstantBuffers != 1 || Description.BoundResources != 1)
	{
		return 3;
	}
	ID3DBlob* RawSignature = nullptr;
	Result = D3DGetInputSignatureBlob(Code->GetBufferPointer(), Code->GetBufferSize(), &RawSignature);
	ComPtr<ID3DBlob> Signature(RawSignature);
	if (FAILED(Result) || !Signature || Signature->GetBufferSize() == 0)
	{
		return 4;
	}
	constexpr char InvalidSource[] = "this is not HLSL";
	RawCode = nullptr;
	RawErrors = nullptr;
	Result = D3DCompile(InvalidSource, sizeof(InvalidSource) - 1, "invalid.hlsl", nullptr, nullptr, "main", "ps_5_0", 0, 0, &RawCode, &RawErrors);
	ComPtr<ID3DBlob> InvalidCode(RawCode);
	ComPtr<ID3DBlob> InvalidErrors(RawErrors);
	if (SUCCEEDED(Result) || !InvalidErrors || InvalidErrors->GetBufferSize() == 0)
	{
		return 5;
	}
	puts("PASS: native HLSL defines, include callback, DXBC, reflection, signature, and error reporting");
	return 0;
}

