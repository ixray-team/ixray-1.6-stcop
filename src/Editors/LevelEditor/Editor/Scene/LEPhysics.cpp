#include "stdafx.h"
#include "LEPhysics.h"
#include "mesh_data.h"
#include "../../xrPhysics/iphworld.h"
#include "../../xrCore/Collision/xr_area.h"
#include "../Entry/Terrain/Terrain.h"

CScenePhysics g_scene_physics;

bool CScenePhysics::Simulating()
{
	return !!physics_world();
}

BOOL  GetStaticCformData(const Fmatrix& parent, CEditableMesh* mesh, CEditableObject* object, Fvector* verts, int& vert_cnt, int& vert_it, CDB::TRI* faces, int& face_cnt, int& face_it, CSceneObject* obj);

template<class ObjectClass>
BOOL GetStaticCformData(ObjectClass* obj, mesh_build_data& data, bool b_selected_only)
{
	Fmatrix T = obj->_Transform();
	CEditableObject* O = obj->GetReference();

	for (EditMeshIt M = O->FirstMesh(); M != O->LastMesh(); M++)
	{
		CSceneObject* SurfaceOwner = nullptr;
		if constexpr (std::is_same_v<ObjectClass, CSceneObject>)
		{
			SurfaceOwner = obj;
		}

		if (!::GetStaticCformData(T, *M, O, data.l_verts, data.l_vert_cnt, data.l_vert_it, data.l_faces, data.l_face_cnt, data.l_face_it, SurfaceOwner))
			return FALSE;
	}

	return FALSE;
}

BOOL GetStaticCformData(ObjectList& lst, mesh_build_data& data, bool b_selected_only)
{
	BOOL bResult = TRUE;

	for (ObjectIt _F = lst.begin(); _F != lst.end(); _F++)
	{
		if (b_selected_only && !(*_F)->Selected())
			continue;

		switch ((*_F)->FClassID)
		{
		case OBJCLASS_SCENEOBJECT:
		{
			CSceneObject* obj = (CSceneObject*)(*_F);
			if (obj->IsStatic() || obj->IsMUStatic())
			{
				bResult = GetStaticCformData(obj, data, b_selected_only);
			}

			break;
		}
		case OBJCLASS_TERRAIN:
		{
			CTerrain* obj = (CTerrain*)(*_F);
			bResult = GetStaticCformData(obj, data, b_selected_only);
			break;
		}
		}

	}

	return bResult;
}

void GetBox(Fbox& box, const Fvector* verts, u32 cnt)
{
	box.invalidate();
	for (u32 i = 0; i < cnt; ++i)
		box.modify(verts[i]);
}

void	CScenePhysics::OnSceneModified()
{
	ObjClassID cls = LTools->CurrentClassID();
	if (cls == OBJCLASS_SCENEOBJECT || cls == OBJCLASS_GROUP)
		UpdateLevelCollision();

}

bool CScenePhysics::CreateObjectSpace(bool b_selected_only)
{
	ObjClassID cls = LTools->CurrentClassID();
	if (cls == OBJCLASS_DUMMY)
		return false;

	ESceneToolBase* pCurrentTool = Scene->GetOTool(cls);

	bool bResult = true;

	mesh_build_data build_data;

	if (b_selected_only)
	{
		if (pCurrentTool)
			pCurrentTool->GetStaticDesc(build_data.l_vert_cnt, build_data.l_face_cnt, b_selected_only, true);
	}
	else
	{
		for (SceneToolsMapPairIt t_it = Scene->FirstTool(); t_it != Scene->LastTool(); ++t_it)
		{
			ESceneToolBase* mt = t_it->second;
			if (mt)
				mt->GetStaticDesc(build_data.l_vert_cnt, build_data.l_face_cnt, b_selected_only, true);
		}
	}

	if (build_data.l_face_cnt == 0)
	{
		Msg("! Empty scene!");
		return false;
	}

	build_data.l_faces = xr_alloc<CDB::TRI>(build_data.l_face_cnt);
	build_data.l_verts = xr_alloc<Fvector>(build_data.l_vert_cnt);

	if (b_selected_only)
	{
		if (pCurrentTool)
			if (!pCurrentTool->GetStaticCformData(build_data, b_selected_only))
				bResult = false;
	}
	else
	{
		for (SceneToolsMapPairIt t_it = Scene->FirstTool(); t_it != Scene->LastTool(); ++t_it)
		{
			ESceneToolBase* mt = t_it->second;
			if (mt)
			{
				mt->GetStaticCformData(build_data, b_selected_only);
			}
		}
	}

	for (u32 i = 0; i < build_data.l_vert_cnt; ++i)
	{
		Fvector& v = build_data.l_verts[i];
		if (!_finite(v.x) || !_finite(v.y) || !_finite(v.z))
		{
			Msg("Bad vertex %u: %f %f %f", i, v.x, v.y, v.z);
			bResult = false;
		}
	}

	for (u32 i = 0; i < build_data.l_face_cnt; ++i)
	{
		CDB::TRI& t = build_data.l_faces[i];
		if (t.verts[0] >= build_data.l_vert_cnt || t.verts[1] >= build_data.l_vert_cnt || t.verts[2] >= build_data.l_vert_cnt)
		{
			Msg("Bad triangle %u: %u %u %u", i, t.verts[0], t.verts[1], t.verts[2]);
			bResult = false;
		}
	}

	if (!bResult)
	{
		xr_free(build_data.l_faces);
		xr_free(build_data.l_verts);
		return false;
	}

	CDB::Collector CL;
	CL.reserve(build_data.l_face_cnt);
	for (u32 i = 0; i < build_data.l_face_cnt; ++i)
	{
		CDB::TRI& F = build_data.l_faces[i];
		CL.add_face_D(build_data.l_verts[F.verts[0]], build_data.l_verts[F.verts[1]], build_data.l_verts[F.verts[2]], i);
	}

	XRay::CForm::CFormatVanilla CForm;
	CForm.AddStaticGeom(CL.getVSpan(), CL.getTSpan());

	VERIFY(!m_object_space);
	m_object_space = mesh_create_object_space(CForm, nullptr);

	xr_free(build_data.l_faces);
	xr_free(build_data.l_verts);

	b_update_level_collision = false;

	return bResult;
}


CScenePhysics::~CScenePhysics()
{
	R_ASSERT(!m_object_space);
}

void CScenePhysics::DestroyObjectSpace()
{
	destroy_object_space(m_object_space);
}
void  CScenePhysics::DestroyWorld()
{
	if (physics_world())
		destroy_physics_world();

}

void CScenePhysics::GenerateCForm(CObjectSpace* To, CDB::build_callback cb)
{

	bool bResult = true;

	mesh_build_data build_data;


	SceneToolsMapPairIt t_it = Scene->FirstTool();
	SceneToolsMapPairIt t_end = Scene->LastTool();
	for (; t_it != t_end; ++t_it)
	{
		ESceneToolBase* mt = t_it->second;
		if (mt)
			mt->GetStaticDesc(build_data.l_vert_cnt, build_data.l_face_cnt, false, true);

		// if (!mt->ExportStatic(this,b_selected_only))
			// {bResult = FALSE; break;}
	}

	build_data.l_faces = xr_alloc<CDB::TRI>(build_data.l_face_cnt);
	build_data.l_verts = xr_alloc<Fvector>(build_data.l_vert_cnt);

	t_it = Scene->FirstTool();
	t_end = Scene->LastTool();
	for (; t_it != t_end; ++t_it)
	{
		ESceneToolBase* mt = t_it->second;
		if (mt)
			if (!mt->GetStaticCformData(build_data, false))
			{
				bResult = false; break;
			}
	}
	VERIFY(!m_object_space);
	XRay::CForm::CFormatVanilla CForm;
	CForm.AddStaticGeom({build_data.l_verts, (size_t)build_data.l_vert_it}, {build_data.l_faces, (size_t)build_data.l_face_it});
	To->Create(CForm, cb, nullptr, false);
	/*hdrCFORM H;
	H.vertcount = build_data.l_vert_it;
	H.facecount = build_data.l_face_it;
	H.version = CFORM_CURRENT_VERSION;
	GetBox(H.aabb, build_data.l_verts, build_data.l_vert_it);
	VERIFY(!m_object_space);
	To->Create(build_data.l_verts, build_data.l_faces, H, cb, nullptr, false);*/

	xr_free(build_data.l_faces);
	xr_free(build_data.l_verts);

	b_update_level_collision = false;

}

void  CScenePhysics::CreateWorld()
{
	VERIFY(!physics_world());
	VERIFY(m_object_space);
	create_physics_world(false, m_object_space, 0);
}

void CreatePhysicsShellsSelected()
{
	ObjectList lst;
	if (Scene->GetQueryObjects(lst, OBJCLASS_SPAWNPOINT, 1, 1, 0)) {
		for (ObjectIt it = lst.begin(); it != lst.end(); it++) {
			CSpawnPoint* O = smart_cast<CSpawnPoint*>(*it); R_ASSERT(O);
			if (O->Selected() && O->ObjectKinematics())
				O->CreatePhysicsShell(&O->FTransform);
		}
	}
}
void   CScenePhysics::UseSimulatePoses()
{
	ObjectList lst;
	if (Scene->GetQueryObjects(lst, OBJCLASS_SPAWNPOINT, 1, 1, 0)) {
		for (ObjectIt it = lst.begin(); it != lst.end(); it++) {
			CSpawnPoint* O = smart_cast<CSpawnPoint*>(*it); R_ASSERT(O);
			if (O->Selected())
				O->UseSimulatePose();
		}
	}
}

void DestroyPhysicsShells()
{
	ObjectList lst;
	if (Scene->GetQueryObjects(lst, OBJCLASS_SPAWNPOINT, -1, -1, 0)) {
		for (ObjectIt it = lst.begin(); it != lst.end(); it++) {
			CSpawnPoint* O = smart_cast<CSpawnPoint*>(*it); R_ASSERT(O);
			O->DeletePhysicsShell();
		}
	}
}

void CScenePhysics::CreateShellsSelected()
{
	if (b_update_level_collision)
		DestroyObjectSpace();

	bool HasSpace = true;

	if (!m_object_space)
	{
		HasSpace = CreateObjectSpace(false);
	}

	if (HasSpace)
	{
		CreateWorld();
		CreatePhysicsShellsSelected();
	}
}

void CScenePhysics::DestroyAll()
{
	DestroyPhysicsShells();
	DestroyWorld();
}