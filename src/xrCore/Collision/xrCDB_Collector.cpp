#include "stdafx.h"
#pragma hdrstop

#include "xrCDB.h"

namespace CDB
{
	u32		Collector::VPack	(const Fvector& V, float eps)
	{
		xr_vector<Fvector>::iterator I,E;
		I=verts.begin();	E=verts.end();
		for (;I!=E;I++)		if (I->similar(V,eps)) return u32(I-verts.begin());
		verts.push_back		(V);
		return (u32)verts.size	()-1;
	}

	void	Collector::add_face_D	(
		const Fvector& v0, const Fvector& v1, const Fvector& v2,	// vertices
		u32 dummy								// misc
		)
	{
		TRI T;
		T.verts		[0] = (u32)verts.size();
		T.verts		[1] = (u32)verts.size()+1;
		T.verts		[2] = (u32)verts.size()+2;  
		T.dummy			= dummy;

		verts.push_back(v0);
		verts.push_back(v1);
		verts.push_back(v2);
		faces.push_back(T);
	}

	void	Collector::add_face		(	const Fvector& v0, const Fvector& v1, const Fvector& v2, u16 material, u16 sector )
	{
		TRI			T;
		T.verts	[0]		= (u32)verts.size();
		T.verts	[1]		= (u32)verts.size()+1;
		T.verts	[2]		= (u32)verts.size()+2;
		T.material		= material;
		T.sector		= sector;

		verts.push_back(v0);
		verts.push_back(v1);
		verts.push_back(v2);
		faces.push_back(T);
	}

	void	Collector::add_face_packed	(
		const Fvector& v0, const Fvector& v1, const Fvector& v2,	// vertices
		u16		material, u16 sector,								// misc
		float	eps
		)
	{
		TRI T;
		T.verts	[0]		= VPack(v0,eps);
		T.verts	[1]		= VPack(v1,eps);
		T.verts	[2]		= VPack(v2,eps);
		T.material		= material;
		T.sector		= sector;
		faces.push_back(T);
	}

	void	Collector::add_face_packed_D	(
		const Fvector& v0, const Fvector& v1, const Fvector& v2,	// vertices
		u32		dummy,	float eps
		)
	{
		TRI T;
		T.verts	[0] = VPack(v0,eps);
		T.verts	[1] = VPack(v1,eps);
		T.verts	[2] = VPack(v2,eps);
		T.dummy			= dummy;
		faces.push_back(T);
	}

#pragma warning(push)
#pragma warning(disable:4995)
#include <malloc.h>
#pragma warning(pop)

#pragma pack(push,1)
	struct edge {
		u32		face_id		: 30;
		u32		edge_id		: 2;
		u16		vertex_id0;
		u16		vertex_id1;
	};
#pragma pack(pop)

	struct sort_predicate {
		IC	bool	operator()	(const edge &edge0, const edge &edge1) const
		{
			if (edge0.vertex_id0 < edge1.vertex_id0)
				return				(true);

			if (edge1.vertex_id0 < edge0.vertex_id0)
				return				(false);

			if (edge0.vertex_id1 < edge1.vertex_id1)
				return				(true);

			if (edge1.vertex_id1 < edge0.vertex_id1)
				return				(false);

			return					(edge0.face_id < edge1.face_id);
		}
	};

	void	Collector::calc_adjacency	(xr_vector<u32>& dest)
	{
#if 1
		VERIFY							(faces.size() < 65536);
		const u32						edge_count = (u32)faces.size()*3;
		edge							*edges = (edge*)_alloca(edge_count*sizeof(edge));

		edge							*i = edges;
		xr_vector<TRI>::const_iterator	B = faces.begin(), I = B;
		xr_vector<TRI>::const_iterator	E = faces.end();
		for ( ; I != E; ++I) {
			u32							face_id = u32(I - B);

			(*i).face_id				= face_id;
			(*i).edge_id				= 0;
			(*i).vertex_id0				= (u16)(*I).verts[0];
			(*i).vertex_id1				= (u16)(*I).verts[1];
			if ((*i).vertex_id0 > (*i).vertex_id1)
				std::swap				((*i).vertex_id0,(*i).vertex_id1);
			++i;
			
			(*i).face_id				= face_id;
			(*i).edge_id				= 1;
			(*i).vertex_id0				= (u16)(*I).verts[1];
			(*i).vertex_id1				= (u16)(*I).verts[2];
			if ((*i).vertex_id0 > (*i).vertex_id1)
				std::swap				((*i).vertex_id0,(*i).vertex_id1);
			++i;
			
			(*i).face_id				= face_id;
			(*i).edge_id				= 2;
			(*i).vertex_id0				= (u16)(*I).verts[2];
			(*i).vertex_id1				= (u16)(*I).verts[0];
			if ((*i).vertex_id0 > (*i).vertex_id1)
				std::swap				((*i).vertex_id0,(*i).vertex_id1);
			++i;
		}

		std::sort						(edges,edges + edge_count,sort_predicate());

		dest.assign						(edge_count,u32(-1));

		{
			edge						*I_ = edges, *J;
			edge						*E_ = edges + edge_count;
			for ( ; I_ != E_; ++I_) {
				if (I_ + 1 == E_)
					continue;

				J							= I_ + 1;

				if ((*I_).vertex_id0 != (*J).vertex_id0)
					continue;

				if ((*I_).vertex_id1 != (*J).vertex_id1)
					continue;

				dest[(*I_).face_id*3 + (*I_).edge_id]	= (*J).face_id;
				dest[(*J).face_id*3 + (*J).edge_id]	= (*I_).face_id;
			}
		}
#	if 0
		xr_vector<u32>	test = dest;

		dest.assign		(faces.size()*3,0xffffffff);
		// Dumb algorithm O(N^2) :)
		for (u32 f=0; f<faces.size(); f++)
		{
			for (u32 t=0; t<faces.size(); t++)
			{
				if (t==f)	continue;

				for (u32 f_e=0; f_e<3; f_e++)
				{
					u32 f1	= faces[f].verts[(f_e+0)%3];
					u32 f2	= faces[f].verts[(f_e+1)%3];
					if (f1>f2)	std::swap(f1,f2);

					for (u32 t_e=0; t_e<3; t_e++)
					{
						u32 t1	= faces[t].verts[(t_e+0)%3];
						u32 t2	= faces[t].verts[(t_e+1)%3];
						if (t1>t2)	std::swap(t1,t2);

						if (f1==t1 && f2==t2)
						{
							// f.edge[f_e] linked to t.edge[t_e]
							dest[f*3+f_e]	= t;
							break;
						}
					}
				}
			}
		}

		{
			xr_vector<u32>::const_iterator	I = test.begin();
			xr_vector<u32>::const_iterator	E = test.end();
			xr_vector<u32>::const_iterator	J = dest.begin();
			for ( ; I != E; ++I, ++J) {
				VERIFY	(*I == *J);
			}
		}
#	endif
#else
		dest.assign		(faces.size()*3,0xffffffff);
		// Dumb algorithm O(N^2) :)
		for (u32 f=0; f<faces.size(); f++)
		{
			for (u32 t=0; t<faces.size(); t++)
			{
				if (t==f)	continue;

				for (u32 f_e=0; f_e<3; f_e++)
				{
					u32 f1	= faces[f].verts[(f_e+0)%3];
					u32 f2	= faces[f].verts[(f_e+1)%3];
					if (f1>f2)	std::swap(f1,f2);

					for (u32 t_e=0; t_e<3; t_e++)
					{
						u32 t1	= faces[t].verts[(t_e+0)%3];
						u32 t2	= faces[t].verts[(t_e+1)%3];
						if (t1>t2)	std::swap(t1,t2);

						if (f1==t1 && f2==t2)
						{
							// f.edge[f_e] linked to t.edge[t_e]
							dest[f*3+f_e]	= t;
							break;
						}
					}
				}
			}
		}
#endif
	}
    IC BOOL similar(TRI& T1, TRI& T2)
    {
        if ((T1.verts[0]==T2.verts[0]) && (T1.verts[1]==T2.verts[1]) && (T1.verts[2]==T2.verts[2]) && (T1.dummy==T2.dummy)) return TRUE;
        if ((T1.verts[0]==T2.verts[0]) && (T1.verts[2]==T2.verts[1]) && (T1.verts[1]==T2.verts[2]) && (T1.dummy==T2.dummy)) return TRUE;
        if ((T1.verts[2]==T2.verts[0]) && (T1.verts[0]==T2.verts[1]) && (T1.verts[1]==T2.verts[2]) && (T1.dummy==T2.dummy)) return TRUE;
        if ((T1.verts[2]==T2.verts[0]) && (T1.verts[1]==T2.verts[1]) && (T1.verts[0]==T2.verts[2]) && (T1.dummy==T2.dummy)) return TRUE;
        if ((T1.verts[1]==T2.verts[0]) && (T1.verts[0]==T2.verts[1]) && (T1.verts[2]==T2.verts[2]) && (T1.dummy==T2.dummy)) return TRUE;
        if ((T1.verts[1]==T2.verts[0]) && (T1.verts[2]==T2.verts[1]) && (T1.verts[0]==T2.verts[2]) && (T1.dummy==T2.dummy)) return TRUE;
        return FALSE;
    }
    void Collector::remove_duplicate_T( )
    {
		for (u32 f=0; f<faces.size(); f++)
		{
			for (u32 t=f+1; t<faces.size();)
			{
				if (t==f)	continue;
                TRI& T1	= faces[f];
                TRI& T2	= faces[t];
                if (similar(T1,T2)){
                	faces[t] = faces.back();
                    faces.pop_back();
                }else{
                	t++;
                }
            }
        }
    }

	CollectorPacked::CollectorPacked(const Fbox &bb, int apx_vertices, int apx_faces)
	{
		HDIM_X = 1024;
		HDIM_Y = 1024;
		HDIM_Z = 1024;

		// Params
 		VMscale.set(bb.max.x - bb.min.x, bb.max.y - bb.min.y, bb.max.z - bb.min.z);
		VMmin.set(bb.min);

		scale.set(float(HDIM_X), float(HDIM_Y), float(HDIM_Z));
		scale.div(VMscale);

		Msg("*** Set Hash Scale for Compacting: {%f, %f, %f}", VPUSH(scale));

		// Preallocate memory
		verts.reserve	(apx_vertices);
		faces.reserve	(apx_faces);
		flags.reserve	(apx_faces);
	}

	u32		CollectorPacked::VPack(const Fvector& V)
	{

		u32 ix = iFloor((V.x - VMmin.x) * scale.x);
		u32 iy = iFloor((V.y - VMmin.y) * scale.y);
		u32 iz = iFloor((V.z - VMmin.z) * scale.z);

		// Generate hash key
		size_t hashKey = std::hash<u32>()(ix) ^ std::hash<u32>()(iy) ^ std::hash<u32>()(iz);

		// Search for similar vertices
		auto itHash = hashTable.find(hashKey);
		if (itHash != hashTable.end())
		{
			for (auto& v : itHash->second)
			{
				if (v.vertex.similar(V, EPS_L))
				{
					return v.PrimID; // Ќашли похожий используем его индекс
				}
			}
		}
		u32 P = (u32)verts.size();
		verts.push_back(V);

		VertexData data;
		data.PrimID = P;
		data.vertex = V;
		hashTable[hashKey].push_back(data);

		return P;
	}

	void	CollectorPacked::clear()
	{
		verts.clear();
		faces.clear();
		flags.clear();

		for (auto vec : hashTable)
			vec.second.clear();
	}

	void	CollectorPacked::add_face(
		const Fvector& v0, const Fvector& v1, const Fvector& v2,	// vertices
		u16 material, u16 sector, u32 _flags									// misc
		)
	{
		TRI T;
		T.verts	[0] = VPack(v0);
		T.verts	[1] = VPack(v1);
		T.verts	[2] = VPack(v2);
		T.material		= material;
		T.sector		= sector;
		flags.push_back(_flags);
		faces.push_back(T);

	}

	void	CollectorPacked::add_face_D(
		const Fvector& v0, const Fvector& v1, const Fvector& v2,	// vertices
		u32 dummy, u32 _flags										// misc
		)
	{
		TRI T;
		T.verts	[0] = VPack(v0);
		T.verts	[1] = VPack(v1);
		T.verts	[2] = VPack(v2);
		T.dummy		= dummy;
		faces.push_back(T);
		flags.push_back(_flags);
	}


};
