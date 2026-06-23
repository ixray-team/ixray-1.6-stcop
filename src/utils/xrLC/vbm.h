#pragma once

#include "../../xrEngine/Fmesh.h"
#include "../../xrCore/FS.h"
#include "../xrCore/FormatParsers/LevelGeom/vbm.h"

// Vertex containers
class VBContainer: public VBContainerBase
{
	xr_vector<VDeclarator> vDcl;
	xr_vector<xr_vector<BYTE>> vContainers;

	// Recording
	VDeclarator R_DCL;
	xr_vector<BYTE> R_DATA;

public:
	// Constructor & destructor
	VBContainer()
	{
		R_DCL.clear();
	}

	// Methods
	bool is_empty() const
	{
		return vDcl.empty() && vContainers.empty() && R_DCL .empty() && R_DATA.empty();
	}
	
	void Begin(u32 dwFVF)
	{
		R_ASSERT(R_DCL.empty());
		R_ASSERT(R_DATA.empty());
		R_DCL.set(dwFVF);
	}
	
	void Begin(const VDeclarator& D)
	{
		R_ASSERT(R_DCL.empty());
		R_ASSERT(R_DATA.empty());
		R_DCL.set(D);
	}
	
	void Add(void* PTR, u32 cnt)
	{
		R_ASSERT(R_DCL.size());
		BYTE* P	= (BYTE*)PTR;
		R_DATA.insert(R_DATA.end(),P,P+cnt);
	}
	
	void End(u32* dwContainerID, u32 *dwIndexStart)
	{
		R_ASSERT(!R_DCL.empty());
		R_ASSERT(!R_DATA.empty());
		
		u32 dwSize = R_DCL.vertex();
		R_ASSERT(R_DATA.size()%dwSize == 0);
		
		// Search for container capable of handling data
		u32 bytes_collected	= (u32)R_DATA.size();
		u32 vertices_collected= bytes_collected/dwSize;
		for (u32 CID = 0; CID<vDcl.size(); CID++)
		{
			if (!vDcl[CID].equal(R_DCL))
			{
				continue;
			}
			
			u32 bytes_already = (u32)vContainers[CID].size();
			if ((bytes_already+bytes_collected)>c_VB_maxSize)
			{
				continue;
			}
			u32 vertices_already = bytes_already/dwSize;
			if ((vertices_already+vertices_collected)>c_VB_maxVertices)
			{
				continue;
			}
			
			// If we get here - container CID can take the data
			*dwContainerID = CID;
			*dwIndexStart = vertices_already;
			vContainers[CID].insert(vContainers[CID].end(),R_DATA.begin(),R_DATA.end());
			R_DCL.clear();
			R_DATA.clear();
			return;
		}
		
		// No such format found
		// Simple add it and register
		*dwContainerID = (u32)vDcl.size();
		*dwIndexStart = 0;
		vDcl.push_back(R_DCL);
		R_DCL.clear();
		vContainers.push_back(R_DATA);
		R_DATA.clear();
	}

	virtual size_t size() const override
	{
		size_t Size = sizeof(u32);
		for (u32 i=0; i<vDcl.size(); i++)
		{
			auto& vDclElem = vDcl[i];
			auto& vContainerElem = vContainers[i];
			
			u32 dwOneSize = vDclElem.vertex();
			u32 dwTotalSize = (u32)vContainerElem.size();
			u32 dwVertCount = dwTotalSize/dwOneSize;

			R_ASSERT(dwVertCount*dwOneSize == dwTotalSize);
			
			Size += vDclElem.size()*sizeof(D3DVERTEXELEMENT9); // Vertex format
			Size += sizeof(u32); // Number of vertices
			Size += dwTotalSize;
		}
		return Size;
	}

	void VerifyForSave() const
	{
		R_ASSERT(R_DCL.empty());
		R_ASSERT(R_DATA.empty());
	}
	
	virtual void Save(IWriter &fs) const override
	{
		fs.w_u32((u32)vDcl.size());
		for (u32 i=0; i<vDcl.size(); i++)
		{
			u32 dwOneSize = vDcl[i].vertex();
			u32 dwTotalSize = (u32)vContainers[i].size();
			u32 dwVertCount = dwTotalSize/dwOneSize;

			R_ASSERT(dwVertCount*dwOneSize == dwTotalSize);
			
			fs.w(vDcl[i].begin(), vDcl[i].size()*sizeof(D3DVERTEXELEMENT9));	// Vertex format
			fs.w_u32(dwVertCount);													// Number of vertices
			fs.w(vContainers[i].data(),dwTotalSize);
		}
	}

	void Clear()
	{
		vDcl.clear();
		vDcl.shrink_to_fit();

		vContainers.clear();
		vContainers.shrink_to_fit();
	}
};

class IBContainer: public IBContainerBase
{
	xr_vector<xr_vector<u16>> data;
	
	enum {
		LIMIT = 1024ul * 1024ul
	};
public:
	
	bool is_empty()const	
	{
		return data.empty();
	}
	
	void Register(u16* begin, u16* end, u32* dwContainerID, u32 *dwStart)
	{
		u32 size = (u32)(end-begin);

		// 
		for	(u32 ID=0; ID<data.size(); ID++)
		{
			if ((data[ID].size()+size) < LIMIT)	
			{
				*dwContainerID = ID;
				*dwStart = (u32)data[ID].size();
				data[ID].insert(data[ID].end(),begin,end);
				return;
			}
		}

		// Can't find suitable container - register new
		*dwContainerID = (u32)data.size();
		*dwStart = 0;
		data.push_back(xr_vector<u16>());
		data.back().assign(begin,end);
	}

	virtual size_t size() const override
	{
		size_t Size = sizeof(u32);
		for (auto& elem : data)
		{
			Size += sizeof(u32);
			Size += elem.size()*sizeof(u16);
		}
		return Size;
	}
	
	virtual void Save(IWriter &fs) const override
	{
		fs.w_u32((u32)data.size());
		for (u32 i=0; i<data.size(); i++)
		{
			fs.w_u32((u32)data[i].size());
			fs.w(data[i].data(),(u32)data[i].size()*2);
		}
	}

	void Clear()
	{
		data.clear();
		data.shrink_to_fit();
	}
};

class SWIContainer: public SWIContainerBase
{
	xr_vector<FSlideWindowItem*> data;
	
public:
	
	bool is_empty() const
	{ 
		return data.empty(); 
	}
	
	void Register(u32* id, FSlideWindowItem* item)
	{
		data.push_back	(item);
		*id				= (u32)data.size()-1;
	}

	virtual size_t size() const override
	{
		size_t Size = sizeof(u32);
		for (auto& elem : data)
		{
			Size += sizeof(u32)*5;
			Size += sizeof(FSlideWindow)*elem->count;
		}
		return Size;
	}
	
	virtual void Save(IWriter &fs) const override
	{
		fs.w_u32((u32)data.size());
		for (u32 i=0; i<data.size(); i++)
		{
			fs.w_u32(data[i]->reserved[0]);
			fs.w_u32(data[i]->reserved[1]);
			fs.w_u32(data[i]->reserved[2]);
			fs.w_u32(data[i]->reserved[3]);
			fs.w_u32(data[i]->count);
			fs.w(data[i]->sw,sizeof(FSlideWindow)*data[i]->count);
		}
	}

	void Clear()
	{
		data.clear();
		data.shrink_to_fit();
	}
};

struct MUGeomData
{
	shared_str SavePath;
	VBContainer VB;
	IBContainer IB;
	SWIContainer SWI;
};

class xrMU_Model;

extern SWIContainer g_SWI, x_SWI;
extern VBContainer g_VB, x_VB;
extern IBContainer g_IB, x_IB;
extern xr_hash_map<xrMU_Model*, MUGeomData> g_MUGeomData;