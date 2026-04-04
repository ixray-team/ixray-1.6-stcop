#include "stdafx.h"
#include "xrMU_Model.h"
//#include "build.h"
#include "../../xrCore/xrPool.h"
poolSS<_face,8*1024>	&mu_faces_pool();
poolSS<_vertex,8*1024>	&mu_vertices_pool();
 
void xrMU_Model::Load	( IReader& F, u32 version )
{
	F.r_stringZ			(m_name);

	// READ: vertices
	xr_vector<b_vertex>	b_vertices;
	b_vertices.resize	(F.r_u32());
	m_vertices.reserve	(b_vertices.size());
	F.r					(b_vertices.data(),(u32)b_vertices.size()*sizeof(b_vertex));

	// READ: faces
	xr_vector<b_face>	b_faces;
	b_faces.resize		(F.r_u32());
	m_faces.reserve		(b_faces.size());
	for (auto& f : b_faces)
	{
		f.v[0] = F.r_u32();
		f.v[1] = F.r_u32();
		f.v[2] = F.r_u32();
		F.r(&f.t[0], sizeof(Fvector2));
		F.r(&f.t[1], sizeof(Fvector2));
		F.r(&f.t[2], sizeof(Fvector2));
		f.dwMaterial = F.r_u16();
		F.r(&f.flags, sizeof(b_face_flags));
		f.dwMaterialGame = F.r_u32();
	}

	// READ: lod-ID
	F.r					(&m_lod_ID,2);

	xr_vector<u32>			sm_groups;
	sm_groups.resize		(b_faces.size());
	R_ASSERT				( version > 17 );
	F.r	(&*sm_groups.begin(),(u32)sm_groups.size()*sizeof(u32));

	// CONVERT and OPTIMIZE
	for (u32 v_it=0; v_it<b_vertices.size(); v_it++)
	{
		create_vertex	(b_vertices[v_it]);
	}
	for (u32 f_it=0; f_it<b_faces.size(); f_it++)
	{
		b_face&	r_face		= b_faces[f_it];
		_face *face = create_face		(m_vertices[r_face.v[0]],m_vertices[r_face.v[1]],m_vertices[r_face.v[2]],r_face);
		face->sm_group = sm_groups[f_it];
	}

	// 
	clMsg	("* Loading model: '%s' - v(%d/%d), f(%d/%d)",*m_name,m_vertices.size(),b_vertices.size(),m_faces.size(),b_faces.size());
}

void xrMU_Model::Load_Embree(IReader& F, xr_vector<FaceDataEmbree>& faces)
{
	u16 lodID; 

	shared_str name;
	F.r_stringZ(name);

	// READ: vertices
	xr_vector<b_vertex>	b_vertices;
	b_vertices.resize(F.r_u32());
 	F.r(&*b_vertices.begin(), (u32)b_vertices.size() * sizeof(b_vertex));

	// READ: faces
	xr_vector<b_face>	b_faces;
	b_faces.resize(F.r_u32());
	for (auto& f : b_faces)
	{
		f.v[0] = F.r_u32();
		f.v[1] = F.r_u32();
		f.v[2] = F.r_u32();
		F.r(&f.t[0], sizeof(Fvector2));
		F.r(&f.t[1], sizeof(Fvector2));
		F.r(&f.t[2], sizeof(Fvector2));
		f.dwMaterial = F.r_u16();
		F.r(&f.flags, sizeof(b_face_flags));
		f.dwMaterialGame = F.r_u32();
	}


	// READ: lod-ID
	F.r(&lodID, 2);

	xr_vector<u32>			sm_groups;
	sm_groups.resize(b_faces.size());
 	F.r(&*sm_groups.begin(), (u32)sm_groups.size() * sizeof(u32));


	for (auto& Face : b_faces)
	{
		FaceDataEmbree faceNew;
		faceNew.dwMaterial = Face.dwMaterial;
		faceNew.dwMaterialGame = Face.dwMaterialGame;
		faceNew.bOpaque = false;
		faceNew.bSharedMaterial = (bool)(Face.flags&b_face_flags::UseSharedMaterial);
		faceNew.v1 = b_vertices[Face.v[0]];
		faceNew.v2 = b_vertices[Face.v[1]];
		faceNew.v3 = b_vertices[Face.v[2]];

		// tc
		faceNew.TC[0] = Face.t[0];
		faceNew.TC[1] = Face.t[1];
		faceNew.TC[2] = Face.t[2];
 
		faces.push_back(faceNew);
  	}

	clMsg("* Loading model: '%s' - v(%d), f(%d)", *m_name, b_vertices.size(), b_faces.size());
}

_face* xrMU_Model::create_face(_vertex* v0, _vertex* v1, _vertex* v2, b_face& B)
{
	_face*	_F			= mu_faces_pool().create();
	_F->dwMaterial		= u16(B.dwMaterial);
	_F->dwMaterialGame	= B.dwMaterialGame;
	R_ASSERT			(B.dwMaterialGame<65536);
	_F->flags.bSharedMaterial = !!(B.flags & b_face_flags::UseSharedMaterial);

	// Vertices and adjacement info
	_F->SetVertex		(0,v0);
	_F->SetVertex		(1,v1);
	_F->SetVertex		(2,v2);

	// tc
	_F->tc[0]			= B.t[0];
	_F->tc[1]			= B.t[1];
	_F->tc[2]			= B.t[2];
	_F->CalcNormal		();

	// register
	m_faces.push_back	(_F);
	return _F;
}

_face* xrMU_Model::load_create_face(Fvector& P1, Fvector& P2, Fvector& P3, b_face& B)
{
	return create_face(load_create_vertex(P1),load_create_vertex(P2),load_create_vertex(P3),B);
}

_vertex* xrMU_Model::create_vertex(Fvector& P)
{
	_vertex*	_V		= mu_vertices_pool().create();
	_V->P				= P;
	_V->N.set			(0,0,0);
	m_vertices.push_back(_V);
	return				_V;
}

_vertex* xrMU_Model::load_create_vertex(Fvector& P)
{
	// find similar
	for (u32 it=0; it<m_vertices.size(); it++)
	{
		if (m_vertices[it]->P.similar(P,.001f))	
			return m_vertices[it];
	}
	// create new
	return create_vertex(P);
}
