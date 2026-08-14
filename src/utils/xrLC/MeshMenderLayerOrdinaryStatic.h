#pragma once

#include <NVMeshMender.h>
#include <mender_input_output.h>

#include "../xrLC_Light/xrFace.h"

IC void	set_vertex( MeshMender::Vertex &out_vertex, const TVertex& in_veretex, const Fvector2 Ftc )
{
			cv_vector( out_vertex.pos, in_veretex.P );
			cv_vector( out_vertex.normal, in_veretex.N );
			out_vertex.s		= Ftc.x;
			out_vertex.t		= Ftc.y;
			//out_vertex.tangent;
			//out_vertex.binormal;
}


IC void	set_face( TFace &out_face, const MeshMender::Vertex in_vertices[3] )
{
	for( u16 v = 0; v< 3; ++v )
	{
		out_face.tc.front().uv[v]	.set( in_vertices[v].s, in_vertices[v].t );					
		Fvector tangent; Fvector binormal;
		out_face.basis_tangent[v].set( cv_vector( tangent , in_vertices[v].tangent ) );								
		out_face.basis_binormal[v].set( cv_vector( binormal, in_vertices[v].binormal ) ); 		
	}
}