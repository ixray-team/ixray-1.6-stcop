#include "stdafx.h"

#include "EditMesh.h"
#include "EditObject.h"
#include "ui_main.h"
#include "pick_defs.h"
#include "src/xrCore/Collision/override/Model.h"

static IntVec		sml_processed;
static Fvector		sml_normal;
static float		m_fSoftAngle;
ECORE_API CDB::COLLIDER XRC;
//----------------------------------------------------

//----------------------------------------------------
// номер face должен соответствовать списку
//----------------------------------------------------
void CEditableMesh::GenerateCFModel()
{
	UnloadCForm();
	thread_local CDB::Collector CL;
	CL.clear();

	// double sided
	CL.reserve(m_FaceCount);
	for (auto& Surf : m_SurfFaces)
	{
		for (int face_id : Surf.second)
		{
			if (!IVERIFY(face_id >=0 && face_id < m_FaceCount))
			{
				Msg("Invalid face id [%d] in surface [%s] in mesh [%s]!", face_id, Surf.first->_Name(), m_Name.c_str());
				continue;
			}
			st_Face& F = m_Faces[face_id];
			CL.add_face_D(m_Vertices[F.pv[0].pindex], m_Vertices[F.pv[1].pindex], m_Vertices[F.pv[2].pindex], face_id);
			if (Surf.first->_flags().is(SSurfaceData::sf2Sided))
			{
				CL.add_face_D(m_Vertices[F.pv[2].pindex], m_Vertices[F.pv[1].pindex], m_Vertices[F.pv[0].pindex], face_id);
			}
		}
	}
	if (I_ASSERT(CL.getVS() >= 4 && CL.getTS() >= 2))
	{
		m_CFModel = new CDB::MODEL();
		m_CFModel->verts = CL.verts;
		m_CFModel->tris = CL.faces;
		m_CFModel->build_simple();
	}
}

void CEditableMesh::RayQuery(SPickQuery& pinf)
{
	if (!m_CFModel)
	{
		GenerateCFModel();
		m_CFModel->wait_loading();
	}

	XRC.ray_query(m_CFModel, pinf.m_Start, pinf.m_Direction, pinf.m_Dist);
	for (auto& elem : XRC.r_vec())
	{
		pinf.append(elem, m_Parent, this);
	}
}

void CEditableMesh::RayQuery(const Fmatrix& parent, const Fmatrix& inv_parent, SPickQuery& pinf)
{
	if (!m_CFModel)
	{
		GenerateCFModel();
		m_CFModel->wait_loading();
	}

	Fvector S, D;
	inv_parent.transform_tiny(S, pinf.m_Start);
	inv_parent.transform_dir(D, pinf.m_Direction);

	XRC.ray_query(m_CFModel, S, D, pinf.m_Dist);
	for (auto& elem : XRC.r_vec())
	{
		pinf.append_mtx(parent, elem, m_Parent, this);
	}
}

void CEditableMesh::BoxQuery(const Fmatrix& parent, const Fmatrix& inv_parent, SPickQuery& pinf)
{
	if (!m_CFModel)
	{
		GenerateCFModel();
		m_CFModel->wait_loading();
	}

	Fbox dest;
	dest.xform(pinf.m_BB, inv_parent);
	Fvector c, d;
	dest.getcenter(c);
	dest.getradius(d);

	XRC.box_query(m_CFModel, c, d);
	for (auto& elem : XRC.r_vec())
	{
		pinf.append_mtx(parent, elem, m_Parent, this);
	}
}

static const float _sqrt_flt_max = _sqrt(flt_max*0.5f);

bool CEditableMesh::RayPick(float& distance, const Fvector& start, const Fvector& direction, const Fmatrix& inv_parent, SRayPickInfo* pinf)
{
	if (!m_Flags.is(flVisible))
	{
		return false;
	}

	if (!m_CFModel)
	{
		GenerateCFModel();
		m_CFModel->wait_loading();
	}
	if (!m_CFModel)
	{
		return false;
	}
	if (!m_CFModel)
	{
		return false;
	}

	XRC.ray_options(CDB::OPT_ONLYNEAREST | CDB::OPT_CULL);

	Fvector S, D;
	inv_parent.transform_tiny(S, start);
	inv_parent.transform_dir(D, direction);

	XRC.ray_query(m_CFModel, S, D, _sqrt_flt_max);

	if (XRC.r_count())
	{
		auto& I = XRC.r_any();
		if (I.range < distance)
		{
			if (pinf)
			{
				pinf->SetRESULT(m_CFModel, I);
				pinf->e_obj = m_Parent;
				pinf->e_mesh = this;
				pinf->pt.mul(direction, pinf->inf.range);
				pinf->pt.add(start);
			}

			distance = I.range;
			return true;
		}
	}
	return false;
}

bool CEditableMesh::CHullPickMesh(PlaneVec& pl, const Fmatrix& parent)
{
	Fvector p;
	boolVec inside;
	inside.assign(m_VertCount, true);
	for (u32 v_id = 0; v_id < m_VertCount; v_id++)
	{
		parent.transform_tiny(p, m_Vertices[v_id]);
		for (PlaneIt p_it = pl.begin(); p_it != pl.end(); p_it++)
		{
			if (p_it->classify(p) > EPS_L)
			{
				inside[v_id] = false;
				break;
			}
		}
	}

	for (u32 f_id = 0; f_id < m_FaceCount; f_id++)
	{
		if (inside[m_Faces[f_id].pv[0].pindex] && inside[m_Faces[f_id].pv[1].pindex] && inside[m_Faces[f_id].pv[2].pindex])
		{
			return true;
		}
	}
	return false;
}

void CEditableMesh::RecurseTri(int id)
{
	// Check if triangle already processed
	if (std::find(sml_processed.begin(), sml_processed.end(), id) != sml_processed.end())
	{
		return;
	}

	sml_processed.push_back(id);

	// recurse
	for (int k = 0; k < 3; k++)
	{
		IntVec& PL = (*m_Adjs)[m_Faces[id].pv[k].pindex];
		for (IntIt pl_it = PL.begin(); pl_it != PL.end(); pl_it++)
		{
			Fvector& test_normal = m_FaceNormals[*pl_it];
			float cosa = test_normal.dotproduct(sml_normal);
			if (cosa < m_fSoftAngle)
			{
				continue;
			}
			RecurseTri(*pl_it);
		}
	}
}

void CEditableMesh::GetTiesFaces(int start_id, U32Vec& fl, float fSoftAngle, bool bRecursive)
{
	R_ASSERT(start_id < int(m_FaceCount));
	m_fSoftAngle = cosf(deg2rad(fSoftAngle));
	GenerateFNormals();
	GenerateAdjacency();
	VERIFY(m_FaceNormals);

	if (bRecursive)
	{
		sml_processed.clear();
		sml_normal.set(m_FaceNormals[start_id]);
		RecurseTri(start_id);
		fl.insert(fl.begin(), sml_processed.begin(), sml_processed.end());
	}
	else
	{
		for (int k = 0; k < 3; k++)
		{
			fl.insert(fl.end(), (*m_Adjs)[m_Faces[start_id].pv[k].pindex].begin(), (*m_Adjs)[m_Faces[start_id].pv[k].pindex].end());
		}
		std::sort(fl.begin(), fl.end());
		fl.erase(std::unique(fl.begin(), fl.end()), fl.end());
	}
	UnloadFNormals();
	UnloadAdjacency();
}

bool CEditableMesh::BoxPick(const Fbox& box, const Fmatrix& inv_parent, SBoxPickInfoVec& pinf)
{
	if (!m_CFModel)
	{
		GenerateCFModel();
		m_CFModel->wait_loading();
	}

	Fbox dest;
	dest.xform(box, inv_parent);
	Fvector c, d;
	dest.getcenter(c);
	dest.getradius(d);

	XRC.box_query(m_CFModel, c, d);
	if (XRC.r_count())
	{
		pinf.emplace_back();
		pinf.back().e_obj = m_Parent;
		pinf.back().e_mesh = this;
		for (auto& elem : XRC.r_vec())
		{
			pinf.back().AddRESULT(m_CFModel, elem);
		}

		return true;
	}

	return false;
}

bool CEditableMesh::FrustumPick(const CFrustum& frustum, const Fmatrix& parent)
{
	if (!m_Flags.is(flVisible))
	{
		return false;
	}

	Fvector p[3];
	for (u32 i = 0; i < m_FaceCount; i++)
	{
		for (int k = 0; k < 3; k++)
		{
			parent.transform_tiny(p[k], m_Vertices[m_Faces[i].pv[k].pindex]);
		}

		if (frustum.testPolyInside(p, 3))
		{
			return true;
		}
	}

	return false;
}

void CEditableMesh::FrustumPickFaces(const CFrustum& frustum, const Fmatrix& parent, U32Vec& fl)
{
	if (!m_Flags.is(flVisible))
	{
		return;
	}

	Fvector p[3];
	bool bCulling = EPrefs->bp_cull;
	for (u32 p_id = 0; p_id < m_FaceCount; p_id++)
	{
		for (int k = 0; k < 3; ++k)
		{
			parent.transform_tiny(p[k], m_Vertices[m_Faces[p_id].pv[k].pindex]);
		}

		if (bCulling)
		{
			Fplane P;
			P.build(p[0], p[1], p[2]);
			if (P.classify(UI->CurrentView().m_Camera.GetPosition()) < 0)
			{
				continue;
			}
		}
		if (frustum.testPolyInside(p, 3))
		{
			fl.push_back(p_id);
		}
	}
}