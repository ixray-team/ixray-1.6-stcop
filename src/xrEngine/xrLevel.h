#pragma once
#pragma warning(push)
#pragma warning(disable: 4715)

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
class NodePosition
{
public:
	u8 data[5];

	static const u32 MAX_XZ = (1 << 24) - 1;
	static const u32 MAX_Y = (1 << 16) - 1;

	ICF void xz(u32 value) { CopyMemory(data, &value, 3); }
	ICF void y(u16 value) { CopyMemory(data + 3, &value, 2); }
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

struct NodeCompressed
#ifndef IXRAY_AI_OLD_FORMAT
{
public:
	u8 data[13];
	static constexpr u32 NODE_BIT_COUNT = 25;
	static constexpr u32 LINK_MASK_0 = (1 << NODE_BIT_COUNT) - 1;
	static constexpr u32 LINK_MASK_1 = LINK_MASK_0 << 1;
	static constexpr u32 LINK_MASK_2 = LINK_MASK_0 << 2;
	static constexpr u32 LINK_MASK_3 = LINK_MASK_0 << 3;

public:
	ICF	void link(u8 link_index, u32 value)
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
				value <<= 1;
				value |= (*(u32*)(data + 3)) & ~LINK_MASK_1;
				CopyMemory(data + 3, &value, sizeof(u32));
				break;
			}
			case 2:
			{
				value <<= 2;
				value |= (*(u32*)(data + 6)) & ~LINK_MASK_2;
				CopyMemory(data + 6, &value, sizeof(u32));
				break;
			}
			case 3:
			{
				value <<= 3;
				value |= (*(u32*)(data + 9)) & ~LINK_MASK_3;
				CopyMemory(data + 9, &value, sizeof(u32));
				break;
			}
		}
	}

	ICF	void light(u8 value)
	{
		data[12] = (data[12] & 0x0f) | (value << 4);
	}

	struct SCover
	{
		u16 cover0 : 4;
		u16 cover1 : 4;
		u16 cover2 : 4;
		u16 cover3 : 4;

		ICF	u16	cover(u8 index) const
		{
			switch (index)
			{
			case 0: return(cover0);
			case 1: return(cover1);
			case 2: return(cover2);
			case 3: return(cover3);
			default: NODEFAULT;
			}
		}
	};

	SCover			high;
	SCover			low;
	u16				plane;
	NodePosition	p;
	// 13 + 2 + 2 + 2 + 5 = 24 bytes

	ICF	u32	link(u8 index) const
	{
		switch (index)
		{
		case 0:	return ((*(u32*)data) & LINK_MASK_0);
		case 1:	return (((*(u32*)(data + 3)) >> 1) & LINK_MASK_0);
		case 2:	return (((*(u32*)(data + 6)) >> 2) & LINK_MASK_0);
		case 3:	return (((*(u32*)(data + 9)) >> 3) & LINK_MASK_0);
		default: NODEFAULT;
		}
	}

	friend class	CLevelGraph;
	friend struct	CNodeCompressed;
	friend class	CNodeRenumberer;
	friend class	CRenumbererConverter;
};

struct NodeCompressed10
#endif
{
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

	ICF void light(u8 value)
	{
		data[10] |= value << 4;
	}

	u8 light() const { return data[11] >> 4; }
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
		}
	};

	SCover high;
	SCover low;
	u16 plane;
	NodePosition p;
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
	}

	friend class CLevelGraph;
	friend struct CNodeCompressed;
	friend class CNodeRenumberer;
	friend class CRenumbererConverter;
};

#ifdef AI_COMPILER
struct NodeCompressed6
{
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

	ICF	void light(u8 value)
	{
		data[10] |= value << 4;
	}

public:
	u16				cover0 : 4;
	u16				cover1 : 4;
	u16				cover2 : 4;
	u16				cover3 : 4;
	u16				plane;
	NodePosition	p;

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

	ICF	u8	light() const
	{
		return			(data[10] >> 4);
	}

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


constexpr u32 XRCL_CURRENT_VERSION = 18; //17;	// input
constexpr u32 XRCL_PRODUCTION_VERSION = 14; // output 
constexpr u32 CFORM_CURRENT_VERSION = 4;

#ifdef IXRAY_AI_OLD_FORMAT
const u32 MAX_NODE_BIT_COUNT = 23;
constexpr u32 MAX_AI_NODES = (1 << MAX_NODE_BIT_COUNT) - 1;
constexpr u32 XRAI_CURRENT_VERSION = 10;
#else
constexpr u32 MAX_AI_NODES = NodeCompressed::LINK_MASK_0;
constexpr u32 MAX_NODE_XZ = NodePosition::MAX_XZ;
constexpr u32 XRAI_CURRENT_VERSION = 11;
#endif

constexpr u32 XRAI_SOC_VERSION = 8;
constexpr u32 XRAI_MINIMAL_VERSION = 10;
#pragma warning(pop)
