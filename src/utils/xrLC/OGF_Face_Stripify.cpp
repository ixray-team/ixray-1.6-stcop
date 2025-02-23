#include "StdAfx.h"
#include "Build.h"
#include "OGF_Face.h"
#include "nv_library/NvTriStrip.h"
#include "nv_library/VertexCache.h"
#include <DirectXMesh.h>

int xrSimulate (xr_vector<u16> &indices, int iCacheSize )
{
	VertexCache C(iCacheSize);

	int count=0;
	for (u32 i=0; i<indices.size(); i++)
	{
		int id = indices[i];
		if (C.InCache(id)) continue;
		count++;
		C.AddEntry(id);
	}
	return count;
}

void xrStripify		(xr_vector<u16> &indices, xr_vector<u16> &perturb, int iCacheSize, int iMinStripLength)
{
	SetCacheSize	(iCacheSize);
	SetMinStripSize	(iMinStripLength);
	SetListsOnly	(true);

	// Generate strips
	xr_vector<PrimitiveGroup>	PGROUP;
	GenerateStrips	(&*indices.begin(),(u32)indices.size(),PGROUP);
	R_ASSERT		(PGROUP.size()==1);
	R_ASSERT		(PGROUP[0].type==PT_LIST);
	if (indices.size()!=PGROUP[0].numIndices)	throw "Stripify failed.";

	// Remap indices
	xr_vector<PrimitiveGroup>	xPGROUP;
	RemapIndices	(PGROUP,u16(perturb.size()),xPGROUP);
	R_ASSERT		(xPGROUP.size()==1);
	R_ASSERT		(xPGROUP[0].type==PT_LIST);

	// Build perturberation table
	for(u32 index = 0; index < PGROUP[0].numIndices; index++)
	{
		u16 oldIndex = PGROUP[0].indices	[index];
		int newIndex = xPGROUP[0].indices	[index];
		R_ASSERT(oldIndex<(int)perturb.size());
		R_ASSERT(newIndex<(int)perturb.size());
		perturb[newIndex] = oldIndex;
	}

	// Copy indices
	Memory.mem_copy	(&*indices.begin(),xPGROUP[0].indices,(u32)indices.size()*sizeof(u16));

	// Release memory
	xPGROUP.clear	();
	PGROUP.clear	();
}

void OGF::Stripify()
{
	// Mesh already progressive - don't stripify it
	if (progressive_test())
		return;

	// fast verts
	if (!fast_path_data.vertices.empty() && !fast_path_data.faces.empty())
	{
		// Optimize face order
		{
			xr_vector<u32> remap(fast_path_data.faces.size());

			HRESULT rhr = DirectX::OptimizeFacesLRU
			(
				&fast_path_data.faces.front().v[0],
				fast_path_data.faces.size(),
				remap.data()
			);

			R_CHK(rhr);

			vecOGF_F _source = fast_path_data.faces;
			for (u32 it = 0; it < _source.size(); ++it)
			{
				fast_path_data.faces[it] = _source[remap[it]];
			}
		}

		// Optimize vertex order
		{
			xr_vector<u32> remap(fast_path_data.vertices.size());

			HRESULT rhr = DirectX::OptimizeVertices
			(
				&fast_path_data.faces.front().v[0],
				fast_path_data.faces.size(),
				fast_path_data.vertices.size(),
				remap.data()
			);

			R_CHK(rhr);

			vec_XV _source = fast_path_data.vertices;
			for (u32 it = 0; it < _source.size(); ++it)
			{
				fast_path_data.vertices[remap[it]] = _source[it];
			}

			for (u32 it = 0; it < fast_path_data.faces.size(); ++it)
			{
				for (u32 j = 0; j < 3; ++j)
				{
					fast_path_data.faces[it].v[j] = static_cast<u16>(remap[fast_path_data.faces[it].v[j]]);
				}
			}
		}
	}

	// normal verts
	try {
		xr_vector<u16>	indices, permute;

		// Stripify
		u16* F = (u16*)&*data.faces.begin();
		indices.assign(F, F + (data.faces.size() * 3));
		permute.resize(data.vertices.size());
		xrStripify(indices, permute, c_vCacheSize, 0);

		// Copy faces
		CopyMemory(&*data.faces.begin(), &*indices.begin(), (u32)indices.size() * sizeof(u16));

		// Permute vertices
		vecOGF_V temp_list = data.vertices;
		for (u32 i = 0; i < temp_list.size(); i++)
			data.vertices[i] = temp_list[permute[i]];
	}
	catch (...) {
		clMsg("ERROR: [slow-vert] Stripifying failed. Dump below.");
		DumpFaces();
	}
}

void OGF::DumpFaces()
{
	clMsg	("normal:");
	for (u32 i=0; i<data.faces.size(); i++)
		clMsg("face #%4d: %4d %4d %4d",i,int(data.faces[i].v[0]),int(data.faces[i].v[1]),int(data.faces[i].v[2]));
	clMsg	("fast:");
	for (u32 i=0; i<fast_path_data.faces.size(); i++)
		clMsg("face #%4d: %4d %4d %4d",i,int(fast_path_data.faces[i].v[0]),int(fast_path_data.faces[i].v[1]),int(fast_path_data.faces[i].v[2]));
}
