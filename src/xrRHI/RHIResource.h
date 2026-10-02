#pragma once
#include <atomic>

class RHI_API RHIObject final
{
public:
	void AddRef();
	u32 Release();
	void GetDesc(RHISampleDesc* out_desc) const;

private:
	friend class CRHI;
    friend class InternalDevice11;
    friend class InternalDevice12;
	RHIObject(void* resource, void (*release)(void*), void (*describe)(void*, RHISampleDesc*) = nullptr);
	RHIObject(const RHIObject&) = delete;
	RHIObject& operator=(const RHIObject&) = delete;
	void* resource;
    void (*_release)(void*);
    void (*_describe)(void*, RHISampleDesc*);
	std::atomic<u32> references = 1;
};

class RHI_API RHIBlob final
{
public:
	explicit RHIBlob(size_t size);
	void AddRef();
	u32 Release();
	void* GetBufferPointer();
	size_t GetBufferSize() const;

private:
	RHIBlob(const RHIBlob&) = delete;
	RHIBlob& operator=(const RHIBlob&) = delete;
	xr_vector<u8> data;
	std::atomic<u32> references = 1;
};

struct RHIShaderMacro
{
	const char* Name;
	const char* Definition;
};

class IRHIShaderInclude
{
public:
	virtual HRESULT Open(u32 type, const char* name, const void* parent, const void** out_data, u32* out_size) = 0;
	virtual HRESULT Close(const void* data) = 0;
	virtual ~IRHIShaderInclude() = default;
};

enum class ERHI_SHADER_VARIABLE_TYPE : u32
{
	BOOL = 1,
	INT = 2,
	FLOAT = 3
};

enum class ERHI_SHADER_VARIABLE_CLASS : u32
{
	SCALAR,
	VECTOR,
	MATRIX_ROWS,
	MATRIX_COLUMNS,
	OBJECT,
	STRUCT
};

enum class ERHI_SHADER_RESOURCE_TYPE : u32
{
	CBUFFER,
	TBUFFER,
	TEXTURE,
	SAMPLER,
	UAV_RWTYPED
};

enum class ERHI_SHADER_COMPONENT_TYPE : u32
{
	UNKNOWN,
	UINT32,
	SINT32,
	FLOAT32
};

struct RHIShaderTypeDesc
{
	ERHI_SHADER_VARIABLE_CLASS Class;
	ERHI_SHADER_VARIABLE_TYPE Type;
	u32 Rows;
	u32 Columns;
	u32 Elements;
	u32 Members;
	u32 Offset;
	shared_str Name;

	bool operator==(const RHIShaderTypeDesc& other) const
	{
		return Class == other.Class && Type == other.Type && Rows == other.Rows && Columns == other.Columns &&
			Elements == other.Elements && Members == other.Members && Offset == other.Offset && Name.equal(other.Name);
	}
};

struct RHIShaderVariableDesc
{
	shared_str Name;
	u32 StartOffset;
	u32 Size;
	RHIShaderTypeDesc Type;
};

struct RHIShaderBufferDesc
{
	shared_str Name;
	u32 Type;
	u32 Size;
	u32 BindPoint;
	xr_vector<RHIShaderVariableDesc> Variables;
};

struct RHIShaderResourceDesc
{
	shared_str Name;
	ERHI_SHADER_RESOURCE_TYPE Type;
	u32 BindPoint;
	u32 BindCount;
};

struct RHIShaderInputDesc
{
	shared_str SemanticName;
	u32 SemanticIndex;
	u32 Register;
	u32 SystemValueType;
	ERHI_SHADER_COMPONENT_TYPE ComponentType;
	u8 Mask;
};

struct RHIShaderReflection
{
	xr_vector<RHIShaderBufferDesc> Buffers;
	xr_vector<RHIShaderResourceDesc> Resources;
	xr_vector<RHIShaderInputDesc> Inputs;
};

constexpr u32 RHI_SHADER_DEBUG = 1 << 0;
constexpr u32 RHI_SHADER_SKIP_OPTIMIZATION = 1 << 2;
constexpr u32 RHI_SHADER_PACK_MATRIX_ROW_MAJOR = 1 << 3;
constexpr u32 RHI_SHADER_OPTIMIZATION_LEVEL3 = 1 << 15;
constexpr u32 RHI_SHADER_DEBUG_NAME_FOR_SOURCE = 1 << 22;
constexpr u32 RHI_FEATURE_LEVEL_10_0 = 0xa000;
constexpr u32 RHI_FEATURE_LEVEL_10_1 = 0xa100;
constexpr u32 RHI_FEATURE_LEVEL_11_0 = 0xb000;
constexpr u32 RHI_FEATURE_LEVEL_11_1 = 0xb100;
