#include "StdAfx.h"
#include "Build.h"

#include	"MeshMenderLayerOrdinaryStatic.h"
#include	"../xrLC_Light/xrLC_GlobalData.h"

static u32 find_same_vertex( const xr_vector<u32> &m, const Fvector2& Ftc, const xr_vector< MeshMender::Vertex > &theVerts )
{
	// Search
	for (u32 it=0; it<m.size(); it++)
	{
		u32	m_id = m[it];
		float tc[2] = { theVerts[m_id].s, theVerts[m_id].t }; 
		if (!fsimilar(tc[0],Ftc.x))
		{
			continue;
		}
		if (!fsimilar( tc[1], Ftc.y))
		{
			continue;
		}
		return m_id;
	}
	return u32(-1);
}

static u32 add_vertex(const	TVertex& V,
				 const Fvector2& Ftc,
				 xr_vector<MeshMender::Vertex>& theVerts)
{
	MeshMender::Vertex new_vertex;
	set_vertex( new_vertex, V, Ftc );
	theVerts.push_back( new_vertex );
	return theVerts.size() - 1;
}

static void	add_face(const vecVertex& Verts, const TFace& F, 
					xr_vector< MeshMender::Vertex >& theVerts,
					xr_vector< unsigned int >& theIndices,
					xr_vector<xr_vector<u32> >	&remap )
{
	for (u32 v=0; v<3; v++)
	{
		const TVertex* V = F.v[v];	
		u32 ID	= u32(std::ranges::lower_bound(Verts,V)-Verts.begin());
		xr_vector<u32>& m	= remap[ID];
		Fvector2 Ftc = F.tc.front().uv[v];

		u32 vertex_index = find_same_vertex( m, Ftc, theVerts );
		
		// Register new if not found
		if ( vertex_index == u32(-1) )
		{
			vertex_index = add_vertex( *V, Ftc, theVerts );
			remap[ID].push_back( vertex_index );
		}
		
		theIndices.push_back(vertex_index);
	}
}
 
void CBuild::xrPhase_TangentBasis(vecVertex& Verts, vecFace& Faces)
{
	if (!gCompilerMode.LC_OGF_TANGENT) return;
	Phase("Building tangent-basis ...");

 	xr_vector< MeshMender::Vertex > mender_in_out_verts;
	xr_vector< unsigned int > mender_in_out_indices;
	xr_vector< unsigned int > mender_mapping_out_to_in_vert;

	// ************************************* Declare inputs
	Status("Declarator...");
	u32 v_count_reserve = iFloor(float(Verts.size())*1.33f);
	u32 i_count_reserve = 3*Faces.size();
	
	mender_in_out_verts.clear( );
	mender_in_out_indices.clear( );
	mender_mapping_out_to_in_vert.clear( );


	mender_in_out_verts.reserve( v_count_reserve );
	mender_in_out_indices.reserve( i_count_reserve );
	mender_mapping_out_to_in_vert.reserve( v_count_reserve );


	// ************************************* Build vectors + expand TC if nessesary
	Status("Building inputs...");
	std::ranges::sort(Verts);
	xr_vector<xr_vector<u32> >	remap;
	remap.resize(Verts.size());
	for (u32 f = 0; f < Faces.size(); f++)
	{
		Progress(float(f) / float(Faces.size()));
		TFace* F = Faces[f];
		add_face(Verts, *F, mender_in_out_verts, mender_in_out_indices, remap);
	}
	remap.clear();
 
	// **************************************************** 

	u32 v_was	 = Verts.size();
	u32 v_become = mender_in_out_verts.size();
	clMsg("duplication: was[%d] / become[%d] - %2.1f%%",v_was,v_become,100.f*float(v_become-v_was)/float(v_was));

	// ************************************* Perform mungle
	Status("Calculating basis...");
	
	CTimer tState; tState.Start();
	MeshMender	mender	;
 	if ( !mender.Mend		(
		  mender_in_out_verts,
		  mender_in_out_indices,
		  mender_mapping_out_to_in_vert,
		  1,
		  0.5,
		  0.5,
		  0.0f,
		  MeshMender::DONT_CALCULATE_NORMALS,
		  MeshMender::RESPECT_SPLITS,
		  MeshMender::DONT_FIX_CYLINDRICAL
		)
	)
	{
		Debug.fatal	(DEBUG_INFO, "NVMeshMender failed " );
	}
	AditionalData("MeshMender: %u ms", tState.GetElapsed_ms() );
	
	// ************************************* Retreive data
	Status("Retreiving basis...");
	for (u32 f = 0; f < Faces.size(); f++)
	{
		TFace* F = Faces[f];
		u32	id0 = mender_in_out_indices[f * 3 + 0];	// vertex index
		u32	id1 = mender_in_out_indices[f * 3 + 1];	// vertex index
		u32	id2 = mender_in_out_indices[f * 3 + 2];	// vertex index
		MeshMender::Vertex verts[3] = { mender_in_out_verts[id0], mender_in_out_verts[id1], mender_in_out_verts[id2] };
		set_face(*F, verts);
	}

	mender_in_out_verts.clear( );
	mender_in_out_indices.clear( );
	mender_mapping_out_to_in_vert.clear( );
}

