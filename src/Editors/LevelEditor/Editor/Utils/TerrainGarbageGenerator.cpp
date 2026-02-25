//---------------------------------------------------------------------------
#include "stdafx.h"
#include "TerrainGarbageGenerator.h"
//---------------------------------------------------------------------------

void EGarbageGenerator::Generate(CSceneObject* terrain)
{
	ESceneObjectTool* ot = smart_cast<ESceneObjectTool*>(Scene->GetTool(OBJCLASS_SCENEOBJECT));
	if (!ot->m_AppendRandomObjects.size())
	{
		ELog.DlgMsg(mtError, "Append random object list is empty!");
		return;
	}

	Fbox bbox;
	terrain->GetBox(bbox);
	constexpr float objects_per_m2 = 0.02f;
	float area = (bbox.x2 - bbox.x1) * (bbox.z2 - bbox.z1);
	int nobjects = iFloor(objects_per_m2 * area + 1.f);
	if (ELog.DlgMsg(mtInformation, (mbYes | mbNo), "Terrain area is %f quadratic meters, %d object will be generated. Do you want to continue?", area, nobjects) != mrYes)
		return;

	Scene->SelectObjects(false, OBJCLASS_SCENEOBJECT);

	for (int i = 0; i < nobjects;)
	{
		float rnd_x = ::Random.randF() * (bbox.x2 - bbox.x1) + bbox.x1;
		float rnd_z = ::Random.randF() * (bbox.z2 - bbox.z1) + bbox.z1;
		Fvector start; start.set(rnd_x, bbox.y2 + 1.f, rnd_z);
		Fvector dir; dir.set(0.f, -1.f, 0.f);
		SPickQuery PQ;
		PQ.prepare_rq(
			start,
			dir,
			bbox.y2 - bbox.y1 + 1.f,
			CDB::OPT_CULL | CDB::OPT_ONLYNEAREST
		);
		terrain->RayQuery(PQ);

		if (PQ.r_count())
		{
			Fvector pos = start;
			pos.mad(dir, PQ.r_begin()->range);
			Fvector up;
			if (true)
				up.mknormal(
					PQ.r_begin()->verts[0],
					PQ.r_begin()->verts[1],
					PQ.r_begin()->verts[2]
				);
			else
				up.set(0.f, 1.f, 0.f);
			LPCSTR N = ot->m_AppendRandomObjects[::Random.randI(ot->m_AppendRandomObjects.size())].c_str();
			string256 namebuffer;
			Scene->GenObjectName(OBJCLASS_SCENEOBJECT, namebuffer, N);
			CSceneObject* obj = new CSceneObject((LPVOID)0, namebuffer);
			//ELog.Msg(mtInformation, "Obj%d: x %f, y %f, z %f", i, pos.x, pos.y, pos.z);
			CEditableObject* ref = obj->SetReference(N);
			if (!ref) {
				ELog.DlgMsg(mtError, "Can't load reference object.");
				xr_delete(obj);
				return;
			}

			Fvector p;
			p.set(::Random.randF(ot->m_AppendRandomMinRotation.x, ot->m_AppendRandomMaxRotation.x),
				::Random.randF(ot->m_AppendRandomMinRotation.y, ot->m_AppendRandomMaxRotation.y),
				::Random.randF(ot->m_AppendRandomMinRotation.z, ot->m_AppendRandomMaxRotation.z));
			obj->SetRotation(p);

			Fvector s;
			if (ot->IsAppendRandomScaleProportional())
			{
				s.x = ::Random.randF(ot->m_AppendRandomMinScale.x, ot->m_AppendRandomMaxScale.x);
				s.set(s.x, s.x, s.x);
			}
			else
			{
				s.set(::Random.randF(ot->m_AppendRandomMinScale.x, ot->m_AppendRandomMaxScale.x),
					::Random.randF(ot->m_AppendRandomMinScale.y, ot->m_AppendRandomMaxScale.y),
					::Random.randF(ot->m_AppendRandomMinScale.z, ot->m_AppendRandomMaxScale.z));
			}
			obj->SetScale(s);

			obj->MoveTo(pos, up);
			//obj->Select(TRUE);
			Scene->AppendObject(obj, false);
			i++;
		}
	}
}