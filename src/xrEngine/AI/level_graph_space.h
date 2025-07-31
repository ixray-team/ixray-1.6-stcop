////////////////////////////////////////////////////////////////////////////
//	Module 		: level_graph_space.h
//	Created 	: 02.10.2001
//  Modified 	: 08.12.2004
//	Author		: Dmitriy Iassenev
//	Description : Level graph space
////////////////////////////////////////////////////////////////////////////
#pragma once

namespace LevelGraph
{
	class CHeader : 
		public hdrNODES 
	{
	private:
		friend class CRenumbererConverter;

	public:
		ICF	u32				version					() const;
		ICF	u32				vertex_count			() const;
		ICF	float			cell_size				() const;
		ICF	float			factor_y				() const;
		ICF	const Fbox		&box					() const;
		ICF const xrGUID	&guid					() const;
	};

#pragma pack(push, 1)
	struct NodeUncompressed
	{
		u8 data[13];
		static constexpr u32 NODE_BIT_COUNT = 26;
		static constexpr u32 LINK_MASK_0 = (1 << NODE_BIT_COUNT) - 1;
		static constexpr u32 LINK_MASK_1 = LINK_MASK_0 << 2;
		static constexpr u32 LINK_MASK_2 = LINK_MASK_0 << 4;
		static constexpr u32 LINK_MASK_3 = LINK_MASK_0 << 6;

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

		SNodeCover		high;
		SNodeCover		low;
		u16				plane;

		struct UncompressedNodePosition
		{
			u32 m_xz;
			u16 m_y;

			// xz-coordinates are packed into 4 bytes
			static constexpr u32 MAX_XZ = std::numeric_limits<u32>::max();
			// y-coordinate is packed into 2 bytes
			static constexpr u32 MAX_Y = std::numeric_limits<u16>::max();

			ICF void xz(u32 value) { m_xz = value; }
			ICF void y(u16 value) { m_y = value; }
			ICF u32 xz() const
			{
				return m_xz;
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
				return m_y;
			}
		} p;
		// 13 + 2 + 2 + 2 + 5 = 24 bytes

		ICF	u32	link(u8 index) const
		{
			switch (index)
			{
			case 0:	return ((*(u32*)data) & LINK_MASK_0);
			case 1:	return (((*(u32*)(data + 3)) >> 2) & LINK_MASK_0);
			case 2:	return (((*(u32*)(data + 6)) >> 4) & LINK_MASK_0);
			case 3:	return (((*(u32*)(data + 9)) >> 6) & LINK_MASK_0);
			default: NODEFAULT; return 0;
			}
		}
	};
#pragma pack(pop)

	using CPosition = NodeUncompressed::UncompressedNodePosition;

	class CVertex
	{
		friend class CRenumbererConverter;

	public:
		NodeUncompressed UncompressedNode;

	public:
		ICF	void			link(u8 link_index, u32 value) { UncompressedNode.link(link_index, value); }
		ICF	u32				link					(int i) const;
		ICF	u16				high_cover				(u8 index) const;
		ICF	u16				low_cover				(u8 index) const;
		ICF	u16				plane					() const;
		ICF	const CPosition &position				() const;
		ICF	bool			operator<				(const LevelGraph::CVertex &vertex) const;
		ICF	bool			operator>				(const LevelGraph::CVertex &vertex) const;
		ICF	bool			operator==				(const LevelGraph::CVertex &vertex) const;
		friend class ILevelGraph;
	};

	struct SSegment 
	{
		Fvector v1;
		Fvector v2;
	};
	
	struct SContour : public SSegment
	{
		Fvector v3;
		Fvector v4;
	};
};