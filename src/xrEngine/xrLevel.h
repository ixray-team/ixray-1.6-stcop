#ifndef xrLevelH
#define xrLevelH

#pragma once

struct xrGUID
{
	u64 g[2];

	ICF bool operator==(const xrGUID& o) const
	{
		return ((g[0] == o.g[0]) && (g[1] == o.g[1]));
	}

	ICF bool operator!=(const xrGUID& o) const
	{
		return !(*this == o);
	}

	ICF void LoadLTX(CInifile& ini, LPCSTR section, LPCSTR name)
	{
		string128 buff;

		g[0] = ini.r_u64(section, xr_strconcat(buff, name, "_g0"));
		g[1] = ini.r_u64(section, xr_strconcat(buff, name, "_g1"));
	}

	ICF void SaveLTX(CInifile& ini, LPCSTR section, LPCSTR name)
	{
		string128 buff;

		ini.w_u64(section, xr_strconcat(buff, name, "_g0"), g[0]);
		ini.w_u64(section, xr_strconcat(buff, name, "_g1"), g[1]);
	}
};

enum fsL_Chunks
{
	fsL_HEADER = 1,
	//*
	fsL_SHADERS = 2,
	//*
	fsL_VISUALS = 3,
	//*
	fsL_PORTALS = 4,
	//*		- Portal polygons
	fsL_LIGHT_DYNAMIC = 6,
	//*
	fsL_GLOWS = 7,
	//*		- All glows inside level
	fsL_SECTORS = 8,
	//*		- All sectors on level
	fsL_VB = 9,
	//*		- Static geometry
	fsL_IB = 10,
	//*
	fsL_SWIS = 11,
	//*		- collapse info, usually for trees
	fsL_forcedword = 0xFFFFFFFF
};

enum fsESectorChunks
{
	fsP_Portals = 1,
	// - portal polygons
	fsP_Root,
	// - geometry root
	fsP_forcedword = u32(-1)
};

enum fsSLS_Chunks
{
	fsSLS_Description = 1,
	// Name of level
	fsSLS_ServerState,
	fsSLS_forcedword = u32(-1)
};

enum EBuildQuality
{
	ebqDraft = 0,
	ebqHigh,
	ebqCustom,
	ebq_force_u16 = u16(-1)
};

#pragma pack(push,8)
struct hdrLEVEL
{
	u16 XRLC_version;
	u16 XRLC_quality;
};

struct hdrCFORM
{
	u32 version;
	u32 vertcount;
	u32 facecount;
	Fbox aabb;
};

struct hdrNODES
{
	u32 version;
	u32 count;
	float size;
	float size_y;
	Fbox aabb;
	xrGUID guid;
};
#pragma pack(pop)

#pragma pack(push,1)
#pragma pack(1)
#ifdef AI_MAP_26_BIT
class NodePosition4 {
#else
class NodePosition
{
#endif
public:
	u8 data[5];

	ICF void xz(u32 value) { CopyMemory(data, &value, 3); }
	ICF void y(u16 value) { CopyMemory(data + 3, &value, 2); }

#ifdef AI_MAP_26_BIT 
	// xz-coordinates are packed into 3 bytes
    static constexpr u32 MAX_XZ = (1 << 24) - 1;
    // y-coordinate is packed into 2 bytes
    static constexpr u32 MAX_Y = (1 << 16) - 1;
#endif

	ICF u32 xz() const
	{
		return ((*((u32*)data)) & 0x00ffffff);
	}

	ICF u32 x(u32 row) const
	{
		return (xz() / row);
	}

	ICF u32 z(u32 row) const
	{
		return (xz() % row);
	}

	ICF u32 y() const
	{
		return (*((u16*)(data + 3)));
	}

	friend class CLevelGraph;
	friend struct CNodePositionCompressor;
	friend struct CNodePositionConverter;
};

#ifdef AI_MAP_26_BIT 
class NodePosition12
{
public:
    u32 m_xz;
    u16 m_y;

    ICF void xz(u32 value) { m_xz = value; }
    ICF void y(u16 value) { m_y = value; }

    // xz-coordinates are packed into 4 bytes
    static constexpr u32 MAX_XZ = std::numeric_limits<u32>::max();
    // y-coordinate is packed into 2 bytes
    static constexpr u32 MAX_Y = std::numeric_limits<u16>::max();

    NodePosition12() = default;

    explicit NodePosition12(const NodePosition4& old)
        : m_xz(old.xz()), m_y(old.y()) {}

    NodePosition12& operator=(const NodePosition4& old)
    {
        m_xz = old.xz();
        m_y  = old.y();
        return *this;
    }

    [[nodiscard]]
    ICF u32 xz() const { return m_xz; }

    [[nodiscard]]
    ICF u32 x(u32 row) const { return (xz() / row); }

    [[nodiscard]]
    ICF u32 z(u32 row) const { return (xz() % row); }

    [[nodiscard]]
    ICF u32 y() const { return m_y; }

    friend class CLevelGraph;
    friend struct CNodePositionCompressor;
    friend struct CNodePositionConverter;
    friend class NodePosition4;
};

struct NodeCover5
{
    u16 cover0 : 4;
    u16 cover1 : 4;
    u16 cover2 : 4;
    u16 cover3 : 4;

    ICF u16 cover(u8 index) const
    {
        switch (index)
        {
        case 0: return cover0;
        case 1: return cover1;
        case 2: return cover2;
        case 3: return cover3;
        default: NODEFAULT;
        }
        return u8(-1);
    }
};

struct NodeCompressed12 {
public:
	//
	u8 data[13];
	static const u32 NODE_BIT_COUNT = 25;
	static const u32 LINK_MASK_0 = (1 << NODE_BIT_COUNT) - 1;
	static const u32 LINK_MASK_1 = LINK_MASK_0 << 1;
	static const u32 LINK_MASK_2 = LINK_MASK_0 << 2;
	static const u32 LINK_MASK_3 = LINK_MASK_0 << 3;

private:
	ICF void link(u8 link_index, u32 value)
	{
		value &= LINK_MASK_0;
		switch (link_index)
		{
		case 0: {
			value |= (*(u32*)data) & ~LINK_MASK_0;
			CopyMemory(data, &value, sizeof(u32));
			break;
		}
		case 1: {
			value <<= 1;
			value |= (*(u32*)(data + 3)) & ~LINK_MASK_1;
			CopyMemory(data + 3, &value, sizeof(u32));
			break;
		}
		case 2: {
			value <<= 2;
			value |= (*(u32*)(data + 6)) & ~LINK_MASK_2;
			CopyMemory(data + 6, &value, sizeof(u32));
			break;
		}
		case 3: {
			value <<= 3;
			value |= (*(u32*)(data + 9)) & ~LINK_MASK_3;
			CopyMemory(data + 9, &value, sizeof(u32));
			break;
		}
		}
	}

public:
	struct SCover
	{
		u16 cover0 : 4;
		u16 cover1 : 4;
		u16 cover2 : 4;
		u16 cover3 : 4;

		ICF u16 cover(u8 index) const
		{
			switch (index)
			{
			case 0: return (cover0);
			case 1: return (cover1);
			case 2: return (cover2);
			case 3: return (cover3);
			default: NODEFAULT;
			}
#ifdef DEBUG
			return (u8(-1));
#endif
		}
	};

	NodeCover5 high;
	NodeCover5 low;
	u16 plane;
	NodePosition12 p;
	// 13 + 2 + 2 + 2 + 5 = 24 bytes

	ICF u32 link(u8 index) const
	{
		switch (index)
		{
		case 0: return ((*(u32*)data) & LINK_MASK_0);
		case 1: return (((*(u32*)(data + 3)) >> 1) & LINK_MASK_0);
		case 2: return (((*(u32*)(data + 6)) >> 2) & LINK_MASK_0);
		case 3: return (((*(u32*)(data + 9)) >> 3) & LINK_MASK_0);
		default: NODEFAULT;
		}
#ifdef DEBUG
		return (0);
#endif
	}

	friend class CLevelGraph;
	friend struct CNodeCompressed;
	friend class CNodeRenumberer;
	friend class CRenumbererConverter;
};

struct NodeCompressed10
{
#else
struct NodeCompressed {
#endif
public:
	u8 data[12];

	ICF void link(u8 link_index, u32 value)
	{
		value &= 0x007fffff;
		switch (link_index)
		{
		case 0:
			{
				value |= (*(u32*)data) & 0xff800000;
				CopyMemory(data, &value, sizeof(u32));
				break;
			}
		case 1:
			{
				value <<= 7;
				value |= (*(u32*)(data + 2)) & 0xc000007f;
				CopyMemory(data + 2, &value, sizeof(u32));
				break;
			}
		case 2:
			{
				value <<= 6;
				value |= (*(u32*)(data + 5)) & 0xe000003f;
				CopyMemory(data + 5, &value, sizeof(u32));
				break;
			}
		case 3:
			{
				value <<= 5;
				value |= (*(u32*)(data + 8)) & 0xf000001f;
				CopyMemory(data + 8, &value, sizeof(u32));
				break;
			}
		}
	}
#ifndef AI_MAP_26_BIT
	ICF void light(u8 value)
	{
		data[10] |= value << 4;
	}
#endif
public:
	struct SCover
	{
		u16 cover0 : 4;
		u16 cover1 : 4;
		u16 cover2 : 4;
		u16 cover3 : 4;

		ICF u16 cover(u8 index) const
		{
			switch (index)
			{
			case 0: return (cover0);
			case 1: return (cover1);
			case 2: return (cover2);
			case 3: return (cover3);
			default: NODEFAULT;
			}
#ifdef DEBUG
			return (u8(-1));
#endif
		}
	};

	SCover high;
	SCover low;
	u16 plane;
#ifdef AI_MAP_26_BIT
	NodePosition4	p;
#else
	NodePosition p;
#endif 
	// 32 + 16 + 40 + 92 = 180 bits = 24.5 bytes => 25 bytes

	ICF u32 link(u8 index) const
	{
		switch (index)
		{
		case 0: return ((*(u32*)data) & 0x007fffff);
		case 1: return (((*(u32*)(data + 2)) >> 7) & 0x007fffff);
		case 2: return (((*(u32*)(data + 5)) >> 6) & 0x007fffff);
		case 3: return (((*(u32*)(data + 8)) >> 5) & 0x007fffff);
		default: NODEFAULT;
		}
#ifdef DEBUG
		return (0);
#endif
	}

	friend class CLevelGraph;
	friend struct CNodeCompressed;
	friend class CNodeRenumberer;
	friend class CRenumbererConverter;
};

#ifdef AI_MAP_26_BIT
struct NodeCompressed13
{    
public:
    static constexpr u32 NODE_BIT_COUNT = 26;
    static constexpr u32 LINK_MASK = (1 << NODE_BIT_COUNT) - 1;
    static constexpr u32 LINK_MASK_0 = LINK_MASK;
    static constexpr u32 LINK_MASK_1 = LINK_MASK_0 << 2;
    static constexpr u32 LINK_MASK_2 = LINK_MASK_0 << 4;
    static constexpr u32 LINK_MASK_3 = LINK_MASK_0 << 6;

    u8 data[13];

public:
    ICF void link(u8 link_index, u32 value)
    {
        value &= LINK_MASK_0;
        switch (link_index)
        {
        case 0:
        {
            value |= (*(u32*)data) & ~LINK_MASK_0;
            CopyMemory(data, &value, sizeof(u32));
            break;
        }
        case 1:
        {
            value <<= 2;
            value |= (*(u32*)(data + 3)) & ~LINK_MASK_1;
            CopyMemory(data + 3, &value, sizeof(u32));
            break;
        }
        case 2:
        {
            value <<= 4;
            value |= (*(u32*)(data + 6)) & ~LINK_MASK_2;
            CopyMemory(data + 6, &value, sizeof(u32));
            break;
        }
        case 3:
        {
            value <<= 6;
            value |= (*(u32*)(data + 9)) & ~LINK_MASK_3;
            CopyMemory(data + 9, &value, sizeof(u32));
            break;
        }
        }
    }

public:
    NodeCover5 high;
    NodeCover5 low;
    u16 plane;
    NodePosition12 p;
    // 13 + 2 + 2 + 2 + 6 = 25 bytes

    ICF u32 link(u8 index) const
    {
        switch (index)
        {
        case 0: return ((*(u32*)data) & LINK_MASK_0);
        case 1: return (((*(u32*)(data + 3)) >> 2) & LINK_MASK_0);
        case 2: return (((*(u32*)(data + 6)) >> 4) & LINK_MASK_0);
        case 3: return (((*(u32*)(data + 9)) >> 6) & LINK_MASK_0);
        default: NODEFAULT;
        }
        return 0;
    }

    friend class CLevelGraph;
    friend struct CNodeCompressed;
    friend class CNodeRenumberer;
    friend class CRenumbererConverter;
};
#endif

#ifdef AI_COMPILER
struct NodeCompressed6 {
public:
	u8				data[11];
private:

	ICF	void link(u8 link_index, u32 value)
	{
		value &= 0x001fffff;
		switch (link_index) {
		case 0: {
			value |= (*(u32*)data) & 0xffe00000;
			CopyMemory(data, &value, sizeof(u32));
			break;
		}
		case 1: {
			value <<= 5;
			value |= (*(u32*)(data + 2)) & 0xfc00001f;
			CopyMemory(data + 2, &value, sizeof(u32));
			break;
		}
		case 2: {
			value <<= 2;
			value |= (*(u32*)(data + 5)) & 0xff800003;
			CopyMemory(data + 5, &value, sizeof(u32));
			break;
		}
		case 3: {
			value <<= 7;
			value |= (*(u32*)(data + 7)) & 0xf000007f;
			CopyMemory(data + 7, &value, sizeof(u32));
			break;
		}
		}
	}
#ifndef AI_MAP_26_BIT
	ICF	void light(u8 value)
	{
		data[10] |= value << 4;
	}
#endif
public:
	u16				cover0 : 4;
	u16				cover1 : 4;
	u16				cover2 : 4;
	u16				cover3 : 4;
	u16				plane;
#ifdef AI_MAP_26_BIT
	NodePosition4	p;
#else
	NodePosition	p;
#endif 

	ICF	u32	link(u8 index) const
	{
		switch (index) {
		case 0:	return	((*(u32*)data) & 0x001fffff);
		case 1:	return	(((*(u32*)(data + 2)) >> 5) & 0x001fffff);
		case 2:	return	(((*(u32*)(data + 5)) >> 2) & 0x001fffff);
		case 3:	return	(((*(u32*)(data + 7)) >> 7) & 0x001fffff);
		default:	NODEFAULT;
		}
#ifdef DEBUG
		return			(0);
#endif
	}
#ifndef AI_MAP_26_BIT
	ICF	u8	light() const
	{
		return			(data[10] >> 4);
	}
#endif
	ICF	u16	cover(u8 index) const
	{
		switch (index) {
		case 0: return(cover0);
		case 1: return(cover1);
		case 2: return(cover2);
		case 3: return(cover3);
		default: NODEFAULT;
		}
#ifdef DEBUG
		return				(u8(-1));
#endif
	}

	friend class CLevelGraph;
	friend struct CNodeCompressed;
	friend class CNodeRenumberer;
};									// 2+5+2+11 = 20b
#endif


struct SNodePositionOld
{
	s16 x;
	u16 y;
	s16 z;
};

#pragma pack	(pop)

const u32 XRCL_CURRENT_VERSION = 18; //17;	// input
const u32 XRCL_PRODUCTION_VERSION = 14; // output 
const u32 CFORM_CURRENT_VERSION = 4;
#ifndef AI_MAP_26_BIT
const u32 MAX_NODE_BIT_COUNT = 23;
const u32 XRAI_CURRENT_VERSION = 10;
#else
const u32 XRAI_CURRENT_VERSION		=	13;
using NodeCompressed = NodeCompressed13;
using NodePosition   = NodePosition12;
const u32 MAX_AI_NODES			=	NodeCompressed::LINK_MASK_0;
#endif 

#endif // xrLevelH