#ifndef r_rhiH
#define r_rhiH

// https://wickedengine.net/2021/05/06/graphics-api-abstraction/

// Primitives supported by draw-primitive API
enum PRIMITIVETYPE {
	PT_POINTLIST = 1,
	PT_LINELIST = 2,
	PT_LINESTRIP = 3,
	PT_TRIANGLELIST = 4,
	PT_TRIANGLESTRIP = 5,
	PT_TRIANGLEFAN = 6,
	PT_FORCE_DWORD = 0x7fffffff, /* force 32-bit size enum */
};

/* Flags to construct RS_COLORWRITEENABLE */
#define COLORWRITEENABLE_RED     (1L<<0)
#define COLORWRITEENABLE_GREEN   (1L<<1)
#define COLORWRITEENABLE_BLUE    (1L<<2)
#define COLORWRITEENABLE_ALPHA   (1L<<3)

// ResourceUsage mapping and usage:
//	D3DPOOL_MANAGED - ResourceUsage::IMMUTABLE
//	D3DPOOL_DEFAULT - ResourceUsage::DYNAMIC

enum class ResourceUsage
{
	DEFAULT,
	IMMUTABLE,	// Static resource, will never change
	DYNAMIC,	// Dynamic resource, CPU Write, update on any Unmap
	STAGING
};

enum class Mapping
{
	MAP_READ,
	MAP_WRITE,
	MAP_READ_WRITE,
	MAP_WRITE_DISCARD,
	MAP_WRITE_NO_OVERWRITE
};

enum class TextureType
{
	TEXTURE_1D,
	TEXTURE_2D,
	TEXTURE_3D
};

struct TextureDesc
{
	TextureType textureType;
	u32 width = 1;
	u32 height = 1;
	u32 mipmapLevel = 0;
	u32 arraySize = 0;
	u32 depth = 0;
	PixelFormat format;
	ResourceUsage usage;
	bool renderTargetUsage;
	bool cubemap;
	bool dynamic; // CPU Write
};

enum RESOURCE_DIMENSION
{
	RESOURCE_DIMENSION_UNKNOWN = 0,
	RESOURCE_DIMENSION_BUFFER = 1,
	RESOURCE_DIMENSION_TEXTURE1D = 2,
	RESOURCE_DIMENSION_TEXTURE2D = 3,
	RESOURCE_DIMENSION_TEXTURE3D = 4
};

//////////////////
// D3D9 Copy-paste

/* Vertex Buffer Description */
struct VERTEXBUFFER_DESC
{
	D3DFORMAT           Format;
	D3DRESOURCETYPE     Type;
	DWORD               Usage;
	D3DPOOL             Pool;
	UINT                Size;

	DWORD               FVF;

};

/* Index Buffer Description */
struct INDEXBUFFER_DESC
{
	D3DFORMAT           Format;
	D3DRESOURCETYPE     Type;
	DWORD               Usage;
	D3DPOOL             Pool;
	UINT                Size;
};

struct SUBRESOURCE_DATA
{
	const void* pSysMem;
	u32 SysMemPitch;
	u32 SysMemSlicePitch;
};

struct MAPPED_SUBRESOURCE {
	void* pData;
	u32 RowPitch;
	u32 DepthPitch;
};

/////////////////////////////////////////////////
// RHI Objects

class IGraphicsResource
{
public:
	virtual ~IGraphicsResource() = default;
	inline bool IsValid() const { return m_InternalResource != nullptr; }

	std::shared_ptr<void> m_InternalResource;
	RESOURCE_DIMENSION m_RESOURCE_DIMENSION;

	// IUnknown interface
	u64 AddRef();
	u64 Release();

	// ID3D11Resource-like methods

	void GetType(RESOURCE_DIMENSION* pResourceDimension);
};

inline u64 IGraphicsResource::AddRef()
{
	return 0;
}

inline u64 IGraphicsResource::Release()
{
	return 0;
}

///////////////////////////////////////////////////////////

class IBufferBase : public IGraphicsResource
{
};

class IVertexBuffer : public IBufferBase
{
public:
	void GetDesc(VERTEXBUFFER_DESC* pDesc);
	VERTEXBUFFER_DESC m_Desc;
};

inline void IVertexBuffer::GetDesc(VERTEXBUFFER_DESC* pDesc)
{
	R_ASSERT(pDesc);
	*pDesc = m_Desc;
}

class IIndexBuffer : public IGraphicsResource
{
public:
	void GetDesc(INDEXBUFFER_DESC* pDesc);
	INDEXBUFFER_DESC m_Desc;
};

inline void IIndexBuffer::GetDesc(INDEXBUFFER_DESC* pDesc)
{
	R_ASSERT(pDesc);
	*pDesc = m_Desc;
}

///////////////////////////////////////////////////////////
// Texture stuff

class IBaseTexture : public IGraphicsResource
{};

class ITexture1D : public IBaseTexture
{};

class ITexture2D : public IBaseTexture
{};

class ITexture3D : public IBaseTexture
{};

///////////////////////////////////////////////////////////
// Shader stuff

class IVertexShader : public IGraphicsResource
{
};

class IPixelShader : public IGraphicsResource
{
};

#endif // !r_rhiH