
//----------------------------------------------------
// file: ExportSkeleton.cpp
//----------------------------------------------------
#include "stdafx.h"
#pragma hdrstop

#ifndef	_EDITOR
//
#include "../../xrEngine/defines.h"
//
#include "../../xrCore/xrCore.h"

#include "../../../Layers/xrRender/hwcaps.h"
#include "../../../Layers/xrRender/hw.h"
#include "../../../xrEngine/pure.h"
class CGameFont;

#include "..\..\xrEngine\fmesh.h"
#include "..\..\xrEngine\_d3d_extensions.h"
#include "..\..\xrEngine\properties.h"

//


DEFINE_VECTOR(FVF::L,FLvertexVec,FLvertexIt);
DEFINE_VECTOR(FVF::TL,FTLvertexVec,FTLvertexIt);
DEFINE_VECTOR(FVF::LIT,FLITvertexVec,FLITvertexIt);
DEFINE_VECTOR(shared_str,RStrVec,RStrVecIt);

#endif


#include "ExportSkeleton.h"

#include "../engine/nvMeshMender.h"
#include	"../../common/NvMender2003/nvMeshMender.cpp"
#include	"../../common/NvMender2003/nvMeshMender.h"
#include	"../../common/NvMender2003/mender_input_output.h"
#include	"../../common/NvMender2003/remove_isolated_verts.h"

void 	CExportSkeleton::SSplit::OptimizeTextureCoordinates()
{
	// Optimize texture coordinates
    // 1. Calc bounds
    Fvector2 	Tdelta;
    Fvector2 	Tmin,Tmax;
    Tmin.set	(flt_max,flt_max);
    Tmax.set	(flt_min,flt_min);

	u32	v_cnt	= m_Verts.size();

    for ( u32 v_idx=0; v_idx!=v_cnt; v_idx++ ){
        SSkelVert	&iV = m_Verts[v_idx];
        Tmin.min	(iV.uv);
        Tmax.max	(iV.uv);
    }
    Tdelta.x 	= floorf((Tmax.x-Tmin.x)/2+Tmin.x);
    Tdelta.y 	= floorf((Tmax.y-Tmin.y)/2+Tmin.y);

    Fvector2	Tsize;
    Tsize.sub	(Tmax,Tmin);
    if ((Tsize.x>32)||(Tsize.y>32))
    	Msg		("#!Surface [T:'%s', S:'%s'] has UV tiled more than 32 times.",*m_Texture,*m_Shader);
    {
      // 2. Recalc UV mapping
      for ( u32 v_idx=0; v_idx!=v_cnt; v_idx++ ){
          SSkelVert	&iV = m_Verts[v_idx];
          iV.uv.sub	(Tdelta);
      }
    }
}

IC void	set_vertex( MeshMender::Vertex &out_vertex, const SSkelVert& in_vertex )
{
			cv_vector( out_vertex.pos,		in_vertex.offs );
			cv_vector( out_vertex.normal,	in_vertex.norm );
			out_vertex.s		= in_vertex.uv.x;
			out_vertex.t		= in_vertex.uv.y;
			//out_vertex.tangent;
			//out_vertex.binormal;
}

IC void	set_vertex( SSkelVert& out_vertex,  const SSkelVert& in_old_vertex, const MeshMender::Vertex &in_vertex )
{
			out_vertex = in_old_vertex;

			cv_vector( out_vertex.offs, in_vertex.pos );//?
			cv_vector( out_vertex.norm, in_vertex.normal );//?

			out_vertex.uv.x	= in_vertex.s;
			out_vertex.uv.y	= in_vertex.t;
			Fvector tangent; Fvector binormal;
			out_vertex.tang.set( cv_vector( tangent, in_vertex.tangent ) );
			out_vertex.binorm.set( cv_vector( binormal, in_vertex.binormal ) );
}


IC u16	&face_vertex( SSkelFace &F, u32 vertex_index )
{
	VERIFY( vertex_index < 3 );
	return F.v[vertex_index];
}

IC const u16 &face_vertex( const SSkelFace &F, u32 vertex_index )
{
	VERIFY( vertex_index < 3 );
	return F.v[vertex_index];
}


void 	CExportSkeleton::SSplit::CalculateTB	()
{
	xr_vector<MeshMender::Vertex>	mender_in_out_verts;
	xr_vector< unsigned int >		mender_in_out_indices;
	xr_vector< unsigned int >		mender_mapping_out_to_in_vert;

	fill_mender_input( m_Verts, m_Faces, mender_in_out_verts, mender_in_out_indices );

	MeshMender	mender	;
	if
	(
		!mender.Mend
		(
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
		Debug.fatal	( DEBUG_INFO, "NVMeshMender failed " );
		//Debug.fatal	(DEBUG_INFO,"NVMeshMender failed (%s)",mender.GetLastError().c_str());
	}

	retrive_data_from_mender_otput( m_Verts, m_Faces, mender_in_out_verts, mender_in_out_indices, mender_mapping_out_to_in_vert  );

	//t_remove_isolated_verts( m_Verts, m_Faces );

	mender_in_out_verts				.clear( );
	mender_in_out_indices			.clear( );
	mender_mapping_out_to_in_vert	.clear( );

	OptimizeTextureCoordinates();
}