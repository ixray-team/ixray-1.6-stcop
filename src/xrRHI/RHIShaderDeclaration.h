#pragma once
#include "RHITypes.h"

enum
{
	//	Don't change this since some code relies on magic numbers
	RC_dest_pixel = (1 << 0),
	RC_dest_vertex = (1 << 1),
	RC_dest_sampler = (1 << 2),	//	For DX10 it's either sampler or texture
	RC_dest_geometry = (1 << 3),	//	DX10 only
	RC_dest_hull = (1 << 4),	//	DX11 only
	RC_dest_domain = (1 << 5),	//	DX11 only
	RC_dest_compute = (1 << 6),	//	DX11 only
	RC_dest_compute_cb_index_mask = 0xF0000000,	//	Buffer index == 0..14
	RC_dest_compute_cb_index_shift = 28,
	RC_dest_domain_cb_index_mask = 0x0F000000,	//	Buffer index == 0..14
	RC_dest_domain_cb_index_shift = 24,
	RC_dest_hull_cb_index_mask = 0x00F00000,	//	Buffer index == 0..14
	RC_dest_hull_cb_index_shift = 20,
	RC_dest_pixel_cb_index_mask = 0x000F0000,	//	Buffer index == 0..14
	RC_dest_pixel_cb_index_shift = 16,
	RC_dest_vertex_cb_index_mask = 0x0000F000,	//	Buffer index == 0..14
	RC_dest_vertex_cb_index_shift = 12,
	RC_dest_geometry_cb_index_mask = 0x00000F00,	//	Buffer index == 0..14
	RC_dest_geometry_cb_index_shift = 8,
};

struct RHI_API RHIShaderConstant :
	public xr_resource
{
	struct Loader
	{
		u16 index;		// linear index (pixel)
		u16 cls;		// element class

		Loader() : index(u16(-1)), cls(u16(-1)) {};

		IC bool equal(Loader& C)
		{
			return (index == C.index) && (cls == C.cls);
		}
	};

	class Setup
	{
	public:
		virtual void setup(RHIShaderConstant* C) = 0;
		virtual ~Setup() {}
	};

	shared_str name;		// HLSL-name
	u16 type;		// float=0/integer=1/boolean=2
	u32 destination;// pixel/vertex/(or both)/sampler

	Loader ps;
	Loader vs;
	Loader gs;
	Loader hs;
	Loader ds;
	Loader cs;

	RHIShaderConstant::Loader samp;
	Setup* handler;
	s8 fixed_id;
	u32 name_hash;

	RHIShaderConstant() : type(u16(-1)), destination(0), handler(nullptr), fixed_id(-1), name_hash(0) {};
	RHIShaderConstant& operator=(const RHIShaderConstant& Other) = delete;

	IC RHIShaderConstant::Loader& get_load(u32 destination_)
	{
		static RHIShaderConstant::Loader	fake;
		switch (destination_ & 0xFF)
		{
			case RC_dest_vertex: return vs;
			case RC_dest_pixel: return ps;
			case RC_dest_geometry: return gs;
			case RC_dest_hull: return hs;
			case RC_dest_domain: return ds;
			case RC_dest_compute: return cs;
			case RC_dest_sampler: return samp;
		}
		return fake;
	}

	IC bool equal(RHIShaderConstant& C)
	{
		return (0 == xr_strcmp(name, C.name)) && (type == C.type) && (destination == C.destination) && ps.equal(C.ps) && vs.equal(C.vs) && samp.equal(C.samp) && handler == C.handler;
	}
	IC bool equal(RHIShaderConstant* C)
	{
		return equal(*C);
	}
};

class RHI_API IRHIShaderDeclaration
{
public:
	xr_vector<RHIInputElementDesc> Descriptors;
	u32 VertexSize = (u32)-1;

public:
	IRHIShaderDeclaration(const RHIInputElementDesc* DescList, size_t DescCount);

	virtual ~IRHIShaderDeclaration() {};
	virtual void GenerateLayerDescriptors(void* Signature) = 0;
	virtual void ApplyLayout() = 0;
};

namespace RHIUtils::Shader
{
	RHI_API bool CreateInputLayoutFromFVF(uint32_t fvfCode, xr_vector<RHIInputElementDesc>& il);
	RHI_API size_t ComputeVertexStride(const xr_vector<RHIInputElementDesc>& il);
}