#pragma once

#include "level_graph.h"
#include "../../xrCore/FS.h"

namespace LevelGraphFormat
{
#pragma pack(push, 1)
struct NodeCompressed8
{
	u8 data[12];
	SNodeCover Cover;
	u16 plane;
	NodePosition p;
};
#pragma pack(pop)
static_assert(sizeof(NodeCompressed8) == 21);
static_assert(sizeof(NodeCompressed10) == 23);
static_assert(sizeof(NodeCompressed) == 24);
static_assert(sizeof(LevelGraph::NodeUncompressed) == 25);

struct Limits
{
	u32 InvalidLink;
	u32 MaxXZ;
	u32 RecordSize;
};

inline bool GetLimits(u32 Version, Limits& Result)
{
	switch (Version)
	{
		case XRAI_SOC_VERSION: Result = {0x007fffff, NodePosition::MAX_XZ, sizeof(NodeCompressed8)}; return true;
		case XRAI_MINIMAL_VERSION: Result = {0x007fffff, NodePosition::MAX_XZ, sizeof(NodeCompressed10)}; return true;
		case XRAI_CURRENT_VERSION: Result = {NodeCompressed::LINK_MASK_0, NodePosition::MAX_XZ, sizeof(NodeCompressed)}; return true;
		case XRAI_LARGE_VERSION: Result = {LevelGraph::NodeUncompressed::LINK_MASK_0, LevelGraph::CPosition::MAX_XZ, sizeof(LevelGraph::NodeUncompressed)}; return true;
		default: return false;
	}
}

template <typename T>
inline void Fill(T& Destination, const ILevelGraph::CVertex& Source, const u32 (&Links)[4])
{
	for (u8 D = 0; D < 4; ++D)
	{
		Destination.link(D, Links[D]);
	}

	Destination.high = Source.UncompressedNode.high;
	Destination.low = Source.UncompressedNode.low;
	Destination.plane = Source.plane();
	Destination.p.xz(Source.position().xz());
	Destination.p.y(u16(Source.position().y()));
}


template <typename T>
inline void Unpack(const T& Source, ILevelGraph::CVertex& Destination)
{
	for (u8 D = 0; D < 4; ++D)
	{
		Destination.link(D, Source.link(D));
	}
	Destination.UncompressedNode.high = Source.high;
	Destination.UncompressedNode.low = Source.low;
	Destination.UncompressedNode.plane = Source.plane;
	Destination.UncompressedNode.p.xz(Source.p.xz());
	Destination.UncompressedNode.p.y(u16(Source.p.y()));
}

inline bool ReadNode(IReader& Reader, u32 Version, ILevelGraph::CVertex& Destination)
{
	switch (Version)
	{
		case XRAI_SOC_VERSION:
		{
			NodeCompressed8 Source{};
			Reader.r(&Source, sizeof(Source));
			NodeCompressed10 Converted{};
			CopyMemory(Converted.data, Source.data, sizeof(Source.data));
			Converted.high = Converted.low = Source.Cover;
			Converted.plane = Source.plane;
			Converted.p = Source.p;
			Unpack(Converted, Destination);
			return true;
		}
		case XRAI_MINIMAL_VERSION:
		{
			NodeCompressed10 Source{};
			Reader.r(&Source, sizeof(Source));
			Unpack(Source, Destination);
			return true;
		}
		case XRAI_CURRENT_VERSION:
		{
			NodeCompressed Source{};
			Reader.r(&Source, sizeof(Source));
			Unpack(Source, Destination);
			return true;
		}
		case XRAI_LARGE_VERSION:
			Reader.r(&Destination.UncompressedNode, sizeof(Destination.UncompressedNode));
			return true;
		default: return false;
	}
}

// Reserved high nibble carries legacy light/padding in existing maps.
inline void WriteNode(IWriter& Writer, u32 Version, const ILevelGraph::CVertex& Source, const u32 (&Links)[4], u8 Reserved)
{
	switch (Version)
	{
		case XRAI_SOC_VERSION:
		{
			NodeCompressed10 LinkData{};
			for (u8 D = 0; D < 4; ++D)
			{
				LinkData.link(D, Links[D]);
			}
			NodeCompressed8 Destination{};
			CopyMemory(Destination.data, LinkData.data, sizeof(Destination.data));
			Destination.data[11] |= Reserved & 0xf0;
			Destination.Cover = Source.UncompressedNode.high;
			Destination.plane = Source.plane();
			Destination.p.xz(Source.position().xz());
			Destination.p.y(u16(Source.position().y()));
			Writer.w(&Destination, sizeof(Destination));
			break;
		}
		case XRAI_MINIMAL_VERSION:
		{
			NodeCompressed10 Destination{};
			Fill(Destination, Source, Links);
			Destination.data[11] |= Reserved & 0xf0;
			Writer.w(&Destination, sizeof(Destination));
			break;
		}
		case XRAI_CURRENT_VERSION:
		{
			NodeCompressed Destination{};
			Fill(Destination, Source, Links);
			Destination.data[12] |= Reserved & 0xf0;
			Writer.w(&Destination, sizeof(Destination));
			break;
		}
		case XRAI_LARGE_VERSION:
		{
			LevelGraph::NodeUncompressed Destination{};
			Fill(Destination, Source, Links);
			Writer.w(&Destination, sizeof(Destination));
			break;
		}
	}
}
} // namespace LevelGraphFormat
