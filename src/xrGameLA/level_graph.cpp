////////////////////////////////////////////////////////////////////////////
//	Module 		: level_graph.cpp
//	Created 	: 02.10.2001
//  Modified 	: 11.11.2003
//	Author		: Oles Shihkovtsov, Dmitriy Iassenev
//	Description : Level graph
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "level_graph.h"
#include "profiler.h"
#pragma pack(push,1)
struct SOCNodeCompressed {
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
#ifndef AI_COMPILER
#ifdef DEBUG
	sh_debug->create				("debug\\ai_nodes","$null");
#endif
	string_path					file_name;
	FS.update_path				(file_name,"$level$",LEVEL_GRAPH_NAME);
#else
	string256					file_name;
	xr_strconcat				(filename,LEVEL_GRAPH_NAME);
#endif
	m_reader					= FS.r_open	(file_name);

	// m_header & data
	m_header					= (CHeader*)m_reader->pointer();
	R_ASSERT					(header().version() == XRAI_SOC_CURRENT_VERSION);
	m_reader->advance			(sizeof(CHeader));
	m_nodes = xr_alloc<CVertex>(m_header->vertex_count());
	{
		NodeCompressed* Dst = (NodeCompressed*)m_nodes;
		SOCNodeCompressed* Src = (SOCNodeCompressed*)m_reader->pointer();
		for (size_t i =0;i< m_header->vertex_count(); i++)
		{
			NodeCompressed Temp;
			memcpy(Temp.data ,Src[i].data,12);
			Temp.high.cover0 = Src[i].cover0;
			Temp.high.cover1 = Src[i].cover1;
			Temp.high.cover2 = Src[i].cover2;
			Temp.high.cover3 = Src[i].cover3;
			Temp.low = Temp.high;
			Temp.p = Src[i].p;
			Temp.plane = Src[i].plane;
			Dst[i] = Temp;
		}
	}

	m_row_length				= iFloor((header().box().max.z - header().box().min.z)/header().cell_size() + EPS_L + 1.5f);
	m_column_length				= iFloor((header().box().max.x - header().box().min.x)/header().cell_size() + EPS_L + 1.5f);
	m_access_mask.assign		(header().vertex_count(),true);
	unpack_xz					(vertex_position(header().box().max),m_max_x,m_max_z);

#ifdef DEBUG
#	ifndef AI_COMPILER
		m_current_level_id		= -1;
		m_current_actual		= false;
		m_current_center		= Fvector().set(flt_max,flt_max,flt_max);
		m_current_radius		= Fvector().set(flt_max,flt_max,flt_max);
#	endif
#endif
}

CLevelGraph::~CLevelGraph()
{
		VERIFY(Device.IsEditorMode() == false);
	FS.r_close(m_reader);
	xr_free(m_nodes);
}
