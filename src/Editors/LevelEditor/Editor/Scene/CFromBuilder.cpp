#include "stdafx.h"
#include "mesh_data.h"
#include "../../xrphysics/iphworld.h"
#include "../../xrcdb/xr_area.h"

CFromBuilder::CFromBuilder()
{
	m_Box.invalidate();
}

CFromBuilder::~CFromBuilder()
{
}

bool CFromBuilder::build()
{
	clear();
	auto GetBox = [](Fbox& box, const Fvector* verts, u32 cnt)
	{
		box.invalidate();
		for (u32 i = 0; i < cnt; ++i)
			box.modify(verts[i]);
	};

	mesh_build_data build_data = {};
	auto t_it = Scene->FirstTool();
	auto t_end = Scene->LastTool();
	for (; t_it != t_end; ++t_it)
	{
		ESceneToolBase* mt = t_it->second;
		if (mt)
			mt->GetStaticDesc(build_data.l_vert_cnt, build_data.l_face_cnt, false, true);

	}
	m_Faces.resize(build_data.l_face_cnt);
	m_Vertex.resize(build_data.l_vert_cnt);

	build_data.l_faces = m_Faces.data();
	build_data.l_verts = m_Vertex.data();



	bool bResult = false;
	t_it = Scene->FirstTool();
	t_end = Scene->LastTool();
	for (; t_it != t_end; ++t_it)
	{
		ESceneToolBase* mt = t_it->second;
		if (mt)
		{
			if (!mt->GetStaticCformData(build_data, false))
			{
				bResult = false; break;
			}
		}
	}
	if (build_data.l_face_it == 0)
	{
		build_data.l_faces = 0;
		build_data.l_verts = 0;
		return false;
	}
	m_Faces.resize(build_data.l_face_it);
	m_Vertex.resize(build_data.l_vert_it);
	m_Box.invalidate();
	GetBox(m_Box, build_data.l_verts, build_data.l_vert_it);
	build_data.l_faces = 0;
	build_data.l_verts = 0;
	return true;
}

bool CFromBuilder::empty() const
{
    return m_Faces.empty();
}

void CFromBuilder::clear()
{
	m_Box.invalidate();
	m_Vertex.clear();
	m_Faces.clear();
}

void CFromBuilder::Load(CObjectSpace* To, CDB::build_callback cb)
{
	hdrCFORM H = {};
	H.vertcount = m_Vertex.size();
	H.facecount = m_Faces.size();
	H.version = CFORM_CURRENT_VERSION;
	H.aabb = m_Box;
	To->Create(m_Vertex.data(),m_Faces.data(), H, cb);
}
