#pragma once

#include <NVMeshMender.h>
#include <convert.h>
#include "../ExportObjectOGF.h"
#include "../ExportSkeleton.h"

inline void set_vertex(MeshMender::Vertex& OutVertex, const SOGFVert& InVertex)
{
	cv_vector(OutVertex.pos, InVertex.P);
	cv_vector(OutVertex.normal, InVertex.N);
	OutVertex.s = InVertex.UV.x;
	OutVertex.t = InVertex.UV.y;
}

inline void set_vertex(SOGFVert& OutVertex, const SOGFVert& InOldVertex, const MeshMender::Vertex& InVertex)
{
	OutVertex = InOldVertex;
	cv_vector(OutVertex.P, InVertex.pos);
	cv_vector(OutVertex.N, InVertex.normal);
	OutVertex.UV.x = InVertex.s;
	OutVertex.UV.y = InVertex.t;
	Fvector Tangent;
	Fvector Binormal;
	OutVertex.T.set(cv_vector(Tangent, InVertex.tangent));
	OutVertex.B.set(cv_vector(Binormal, InVertex.binormal));
}

inline WORD& face_vertex(SOGFFace& Face, u32 VertexIndex)
{
	VERIFY(VertexIndex < 3);
	return Face.v[VertexIndex];
}

inline const WORD& face_vertex(const SOGFFace& Face, u32 VertexIndex)
{
	VERIFY(VertexIndex < 3);
	return Face.v[VertexIndex];
}

inline void set_vertex(MeshMender::Vertex& OutVertex, const SSkelVert& InVertex)
{
	cv_vector(OutVertex.pos, InVertex.offs);
	cv_vector(OutVertex.normal, InVertex.norm);
	OutVertex.s = InVertex.uv.x;
	OutVertex.t = InVertex.uv.y;
}

inline void set_vertex(SSkelVert& OutVertex, const SSkelVert& InOldVertex, const MeshMender::Vertex& InVertex)
{
	OutVertex = InOldVertex;
	cv_vector(OutVertex.offs, InVertex.pos);
	cv_vector(OutVertex.norm, InVertex.normal);
	OutVertex.uv.x = InVertex.s;
	OutVertex.uv.y = InVertex.t;
	Fvector Tangent;
	Fvector Binormal;
	OutVertex.tang.set(cv_vector(Tangent, InVertex.tangent));
	OutVertex.binorm.set(cv_vector(Binormal, InVertex.binormal));
}

inline u16& face_vertex(SSkelFace& Face, u32 VertexIndex)
{
	VERIFY(VertexIndex < 3);
	return Face.v[VertexIndex];
}

inline const u16& face_vertex(const SSkelFace& Face, u32 VertexIndex)
{
	VERIFY(VertexIndex < 3);
	return Face.v[VertexIndex];
}
