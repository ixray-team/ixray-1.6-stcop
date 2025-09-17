////////////////////////////////////////////////////////////////////////////
//	Module 		: level_graph.cpp
//	Created 	: 02.10.2001
//  Modified 	: 11.11.2003
//	Author		: Oles Shihkovtsov, Dmitriy Iassenev
//	Description : Level graph
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "level_graph.h"
#include "../xrEngine/Editor/XrEditorSceneInterface.h"

#pragma pack(push,1)
struct SOCNodeCompressed 
{
public:
	u8				data[12];
private:

	ICF	void link(u8 link_index, u32 value)
	{
		value &= 0x007fffff;
		switch (link_index) {
		case 0: {
			value |= (*(u32*)data) & 0xff800000;
			CopyMemory(data, &value, sizeof(u32));
			break;
		}
		case 1: {
			value <<= 7;
			value |= (*(u32*)(data + 2)) & 0xc000007f;
			CopyMemory(data + 2, &value, sizeof(u32));
			break;
		}
		case 2: {
			value <<= 6;
			value |= (*(u32*)(data + 5)) & 0xe000003f;
			CopyMemory(data + 5, &value, sizeof(u32));
			break;
		}
		case 3: {
			value <<= 5;
			value |= (*(u32*)(data + 8)) & 0xf000001f;
			CopyMemory(data + 8, &value, sizeof(u32));
			break;
		}
		}
	}

public:
	u16 cover0 : 4;
	u16 cover1 : 4;
	u16 cover2 : 4;
	u16 cover3 : 4;
	u16 plane;
	NodePosition p;
	// 4 + 4 + 4 + 4 + 16 + 40 + 96 = 168 bits = 21 byte

	ICF	u32	link(u8 index) const
	{
		switch (index) {
		case 0:	return	((*(u32*)data) & 0x007fffff);
		case 1:	return	(((*(u32*)(data + 2)) >> 7) & 0x007fffff);
		case 2:	return	(((*(u32*)(data + 5)) >> 6) & 0x007fffff);
		case 3:	return	(((*(u32*)(data + 8)) >> 5) & 0x007fffff);
		default:	NODEFAULT; return 0;
		}
	}

	ICF	u16	cover(u8 index) const
	{
		switch (index) {
		case 0: return(cover0);
		case 1: return(cover1);
		case 2: return(cover2);
		case 3: return(cover3);
		default:	NODEFAULT; return 0;
		}
	}
};
#pragma pack(pop)

LPCSTR LEVEL_GRAPH_NAME = "level.ai";
CLevelGraph::CLevelGraph()
{
	VERIFY(Device.IsEditorMode() == false);
	string_path file_name;

#ifndef AI_COMPILER
	FS.update_path(file_name, "$level$", LEVEL_GRAPH_NAME);
#else
	strconcat(sizeof(file_name), file_name, filename, LEVEL_GRAPH_NAME);
#endif

	m_reader = FS.r_open(file_name);

	// m_header & data
	m_header = (CHeader*)m_reader->pointer();
	const u32 AIVersion = header().version();

	R_ASSERT2(CHECK_SPAWN_VERSION(AIVersion), "Unsupported AI-Map version!");
	m_reader->advance(sizeof(CHeader));

	switch (AIVersion)
	{
		case XRAI_SOC_VERSION: // ver 8 - SoC format
		{
			SOCNodeCompressed* Src = (SOCNodeCompressed*)m_reader->pointer();
			m_nodes = new CVertex[m_header->vertex_count()];

			for (size_t i = 0; i < m_header->vertex_count(); i++)
			{
				// Конвертация линков через правильный метод
				for (u8 j = 0; j < 4; ++j)
				{
					u32 link_value = Src[i].link(j); // 23-битный линк
					m_nodes[i].UncompressedNode.link(j, link_value); // упаковываем в 26-битное поле
				}

				// Каверы: SOC хранит только "high", копируем в оба
				m_nodes[i].UncompressedNode.high.cover0 = Src[i].cover0;
				m_nodes[i].UncompressedNode.high.cover1 = Src[i].cover1;
				m_nodes[i].UncompressedNode.high.cover2 = Src[i].cover2;
				m_nodes[i].UncompressedNode.high.cover3 = Src[i].cover3;
				m_nodes[i].UncompressedNode.low = m_nodes[i].UncompressedNode.high;

				// Плоскость
				m_nodes[i].UncompressedNode.plane = Src[i].plane;

				// Позиция
				m_nodes[i].UncompressedNode.p.xz(Src[i].p.xz());
				m_nodes[i].UncompressedNode.p.y(Src[i].p.y());
			}
			break;
		}
		case XRAI_MINIMAL_VERSION: // ver 10 - CS/CoP format
		{
			NodeCompressed10* Src = (NodeCompressed10*)m_reader->pointer();
			m_nodes = new CVertex[header().vertex_count()];

			for (u32 i = 0; i < header().vertex_count(); ++i)
			{
				for (u8 j = 0; j < 4; ++j)
				{
					u32 link_value = Src[i].link(j);
					m_nodes[i].UncompressedNode.link(j, link_value);
				}

				// Остальные поля
				m_nodes[i].UncompressedNode.high = Src[i].high;
				m_nodes[i].UncompressedNode.low = Src[i].low;
				m_nodes[i].UncompressedNode.plane = Src[i].plane;

				m_nodes[i].UncompressedNode.p.xz(Src[i].p.xz());
				m_nodes[i].UncompressedNode.p.y(Src[i].p.y());
			}
			break;
		}
		case XRAI_CURRENT_VERSION: // ver 11 - 25-bit format
		{
			NodeCompressed* compressed_nodes = (NodeCompressed*)m_reader->pointer();
			m_nodes = new CVertex[header().vertex_count()];

			for (size_t i = 0; i < header().vertex_count(); ++i)
			{
				for (u8 link_idx = 0; link_idx < 4; ++link_idx)
				{
					u32 old_link = compressed_nodes[i].link(link_idx);
					m_nodes[i].UncompressedNode.link(link_idx, old_link);
				}

				m_nodes[i].UncompressedNode.high = compressed_nodes[i].high;
				m_nodes[i].UncompressedNode.low = compressed_nodes[i].low;
				m_nodes[i].UncompressedNode.plane = compressed_nodes[i].plane;

				m_nodes[i].UncompressedNode.p.xz(compressed_nodes[i].p.xz());
				m_nodes[i].UncompressedNode.p.y(compressed_nodes[i].p.y());
			}
			break;
		}
		case XRAI_LARGE_VERSION: // ver 13 - 26 bit (слишком огромная, двиг не расчитан на такую геометрию)
			m_nodes = (CVertex*)m_reader->pointer();
	}

	m_row_length				= iFloor((header().box().max.z - header().box().min.z)/header().cell_size() + EPS_L + 1.5f);
	m_column_length				= iFloor((header().box().max.x - header().box().min.x)/header().cell_size() + EPS_L + 1.5f);
	m_access_mask.assign		(header().vertex_count(),true);
	unpack_xz					(vertex_position(header().box().max),m_max_x,m_max_z);

#ifdef DEBUG
#	ifndef AI_COMPILER
	sh_debug->create("debug\\ai_nodes", "$null");
	m_current_level_id = -1;
	m_current_actual = false;
	m_current_center = Fvector().set(flt_max,flt_max,flt_max);
	m_current_radius = Fvector().set(flt_max,flt_max,flt_max);
#	endif
#endif
}

CLevelGraph::~CLevelGraph()
{
	if (m_header->version() < XRAI_CURRENT_VERSION)
	{
		xr_delete(m_nodes);
	}

	VERIFY(Device.IsEditorMode() == false);
	FS.r_close(m_reader);
}
