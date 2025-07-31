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

	ICF	void light(u8 value)
	{
		data[11] &= 0x0f;
		data[11] |= value << 4;
	}

public:
	u16				cover0 : 4;
	u16				cover1 : 4;
	u16				cover2 : 4;
	u16				cover3 : 4;
	u16				plane;
	NodePosition	p;
	// 4 + 4 + 4 + 4 + 16 + 40 + 96 = 168 bits = 21 byte

	ICF	u32	link(u8 index) const
	{
		switch (index) {
		case 0:	return	((*(u32*)data) & 0x007fffff);
		case 1:	return	(((*(u32*)(data + 2)) >> 7) & 0x007fffff);
		case 2:	return	(((*(u32*)(data + 5)) >> 6) & 0x007fffff);
		case 3:	return	(((*(u32*)(data + 8)) >> 5) & 0x007fffff);
		default:	NODEFAULT;
		}
#ifdef DEBUG
		return			(0);
#endif
	}

	ICF	u8	light() const
	{
		return			(data[11] >> 4);
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

	friend class	CLevelGraph;
	friend struct	CNodeCompressed;
	friend class	CNodeRenumberer;
	friend class	CRenumbererConverter;
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
	R_ASSERT(AIVersion >= XRAI_SOC_VERSION && AIVersion <= XRAI_CURRENT_VERSION);
	m_reader->advance(sizeof(CHeader));

	switch (AIVersion)
	{
		case XRAI_SOC_VERSION: // ver 8 - SoC format
		{
			CVertex* temp_nodes = new CVertex[m_header->vertex_count()];
			NodeCompressed10* Dst = (NodeCompressed10*)temp_nodes;
			SOCNodeCompressed* Src = (SOCNodeCompressed*)m_reader->pointer();
			NodeCompressed10 Temp;
			m_nodes = new CVertex[m_header->vertex_count()];
			for (size_t i = 0; i < m_header->vertex_count(); i++)
			{
				memcpy(Temp.data, Src[i].data, 12);
				Temp.high.cover0 = Src[i].cover0;
				Temp.high.cover1 = Src[i].cover1;
				Temp.high.cover2 = Src[i].cover2;
				Temp.high.cover3 = Src[i].cover3;
				Temp.low = Temp.high;
				Temp.p = Src[i].p;
				Temp.plane = Src[i].plane;
				Dst[i] = Temp;

				std::memcpy(&m_nodes[i].high, &Dst[i].high, sizeof(Dst[i].high) + sizeof(Dst[i].low) + sizeof(Dst[i].plane) + sizeof(Dst[i].p));

				for (u8 j = 0; j < 4; ++j)
				{
					m_nodes[i].link(j, Dst[i].link(j));
				}
				m_nodes[i].light(Dst[i].light());
			}
		}
		case XRAI_MINIMAL_VERSION: // ver 10 - CS/CoP format
		{
			NodeCompressed10* temp = (NodeCompressed10*)m_reader->pointer();
			m_nodes = new CVertex[header().vertex_count()];

			for (u32 i = 0; i < header().vertex_count(); ++i)
			{
				std::memcpy(&m_nodes[i].high, &temp[i].high, sizeof(temp[i].high) + sizeof(temp[i].low) + sizeof(temp[i].plane) + sizeof(temp[i].p));

				for (u8 j = 0; j < 4; ++j)
				{
					m_nodes[i].link(j, temp[i].link(j));
				}
				m_nodes[i].light(temp[i].light());
			}
			break;
		}
		case XRAI_CURRENT_VERSION: // ver 11 - 25 bit format
		{
			m_nodes = (CVertex*)m_reader->pointer();
			break;
		}
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
