#include "stdafx.h"
#include "planar.h"

#define PLANAR_CHUNK_PARAMS 0x2100

static IC float PlanarAxis(const Fvector& p, int axis)
{
	return (axis == 0) ? p.x : ((axis == 1) ? p.y : p.z);
}

static void PlanarClipPlane(const xr_vector<Fvector>& in, xr_vector<Fvector>& out, int axis, float sign, float limit)
{
	out.clear();
	if (in.size() < 3)
		return;

	auto Dist = [axis, sign, limit](const Fvector& p)
	{
		return sign * PlanarAxis(p, axis) - limit;
	};

	Fvector prev = in.back();
	float prev_d = Dist(prev);

	for (const Fvector& curr : in)
	{
		float curr_d = Dist(curr);
		const bool prev_in = prev_d <= EPS;
		const bool curr_in = curr_d <= EPS;

		if (prev_in && curr_in)
		{
			out.push_back(curr);
		}
		else if (prev_in && !curr_in)
		{
			float denom = prev_d - curr_d;
			float t = (fabsf(denom) > EPS) ? (prev_d / denom) : 0.5f;
			Fvector hit;
			hit.lerp(prev, curr, t);
			out.push_back(hit);
		}
		else if (!prev_in && curr_in)
		{
			float denom = prev_d - curr_d;
			float t = (fabsf(denom) > EPS) ? (prev_d / denom) : 0.5f;
			Fvector hit;
			hit.lerp(prev, curr, t);
			out.push_back(hit);
			out.push_back(curr);
		}

		prev = curr;
		prev_d = curr_d;
	}
}

static void PlanarClipAABB(xr_vector<Fvector>& poly, xr_vector<Fvector>& tmp)
{
	const float h = 0.5f;
	PlanarClipPlane(poly, tmp, 0, 1.f, h);
	PlanarClipPlane(tmp, poly, 0, -1.f, h);
	PlanarClipPlane(poly, tmp, 1, 1.f, h);
	PlanarClipPlane(tmp, poly, 1, -1.f, h);
	PlanarClipPlane(poly, tmp, 2, 1.f, h);
	PlanarClipPlane(tmp, poly, 2, -1.f, h);
}

CPlanar::CPlanar(LPVOID data, const char* name) :
	CEditShape(data, name)
{
	Construct(data);
	FScale.set(2.f, 1.f, 2.f);
	m_RT_Flags.set(flRT_Visible, true);
	UpdateTransform(true);
}

void CPlanar::Construct(LPVOID data)
{
	CEditShape::Construct(data);

	FClassID = OBJCLASS_PLANARS;
	m_shape_type = CShapeData::cfBox;
	m_Stiffness = 1.f;

	add_box(Fidentity);

	m_DrawTranspColor = color_rgba(20, 160, 150, 110);
	m_DrawEdgeColor = color_rgba(40, 230, 210, 255);
}

CPlanar::~CPlanar()
{
}

void CPlanar::MoveTo(const Fvector& pos, const Fvector& /*up*/)
{
	SetPosition(pos);
	UI->UpdateScene();
}

void CPlanar::RebuildProjection()
{
	m_ProjVerts.clear();

	if (!Scene)
		return;

	ObjectList query;
	ObjectList* snap = Scene->GetSnapList(true);
	if (snap && !snap->empty())
	{
		query = *snap;
	}
	else
	{
		ObjectList& scene_obj = Scene->ListObj(OBJCLASS_SCENEOBJECT);
		query.insert(query.end(), scene_obj.begin(), scene_obj.end());
		ObjectList& terrain = Scene->ListObj(OBJCLASS_TERRAIN);
		query.insert(query.end(), terrain.begin(), terrain.end());
	}

	if (query.empty())
		return;

	Fbox world_bb;
	if (!GetBox(world_bb))
		return;
	world_bb.grow(EPS_L);

	SPickQuery BQ;
	if (!Scene->BoxQuery(BQ, world_bb, CDB::OPT_FULL_TEST, &query))
		return;

	Fvector proj_dir;
	proj_dir.set(FTransform.j);
	if (proj_dir.square_magnitude() < EPS_S)
		proj_dir.set(0.f, 1.f, 0.f);
	else
		proj_dir.normalize();

	const float min_facing = 0.1f + m_Stiffness * 0.7f;

	xr_vector<Fvector> poly;
	xr_vector<Fvector> tmp;
	poly.reserve(16);
	tmp.reserve(16);

	for (int k = 0; k < BQ.r_count(); ++k)
	{
		SPickQuery::SResult* R = BQ.r_begin() + k;

		Fvector n;
		n.mknormal(R->verts[0], R->verts[1], R->verts[2]);
		if (n.dotproduct(proj_dir) > -min_facing)
			continue;

		poly.clear();
		for (int v = 0; v < 3; ++v)
		{
			Fvector lp;
			FITransform.transform_tiny(lp, R->verts[v]);
			poly.push_back(lp);
		}

		PlanarClipAABB(poly, tmp);
		if (poly.size() < 3)
			continue;

		Fvector n_off;
		n_off.mul(n, 0.02f);

		Fvector w0;
		FTransform.transform_tiny(w0, poly[0]);
		w0.add(n_off);

		for (u32 i = 1; i + 1 < poly.size(); ++i)
		{
			Fvector w1, w2;
			FTransform.transform_tiny(w1, poly[i]);
			FTransform.transform_tiny(w2, poly[i + 1]);
			w1.add(n_off);
			w2.add(n_off);

			m_ProjVerts.push_back(w0);
			m_ProjVerts.push_back(w1);
			m_ProjVerts.push_back(w2);
		}
	}
}

bool CPlanar::LoadLTX(CInifile& ini, const char* sect_name)
{
	bool Result = CEditShape::LoadLTX(ini, sect_name);
	m_Stiffness = ini.line_exist(sect_name, "stiffness") ? ini.r_float(sect_name, "stiffness") : 1.f;
	clamp(m_Stiffness, 0.f, 1.f);
	OnUpdateTransform();
	return Result;
}

void CPlanar::SaveLTX(CInifile& ini, const char* sect_name)
{
	CEditShape::SaveLTX(ini, sect_name);
	ini.w_float(sect_name, "stiffness", m_Stiffness);
}

void CPlanar::OnUpdateTransform()
{
	CEditShape::OnUpdateTransform();
	ComputeBounds();
	RebuildProjection();
}

bool CPlanar::LoadStream(IReader& F)
{
	if (!CEditShape::LoadStream(F))
		return false;

	m_Stiffness = 1.f;
	if (F.find_chunk(PLANAR_CHUNK_PARAMS))
	{
		m_Stiffness = F.r_float();
		clamp(m_Stiffness, 0.f, 1.f);
	}
	return true;
}

void CPlanar::SaveStream(IWriter& F)
{
	CEditShape::SaveStream(F);

	F.open_chunk(PLANAR_CHUNK_PARAMS);
	F.w_float(m_Stiffness);
	F.close_chunk();
}

void CPlanar::OnFrame()
{
	CCustomObject::OnFrame();
}

void CPlanar::Render(int priority, bool strictB2F)
{
	CEditShape::Render(priority, strictB2F);

	if (priority != 1 || !strictB2F || m_ProjVerts.size() < 3)
		return;

	u32 clr_s = Selected() ? color_rgba(60, 255, 230, 180) : color_rgba(30, 200, 180, 140);
	u32 clr_w = color_rgba(180, 255, 245, 255);

	for (u32 i = 0; i + 2 < m_ProjVerts.size(); i += 3)
	{
		DU_impl.DrawFace(m_ProjVerts[i], m_ProjVerts[i + 1], m_ProjVerts[i + 2], clr_s, clr_w, true, false);
	}
}

void CPlanar::FillProp(const char* pref, PropItemVec& items)
{
	CEditShape::FillProp(pref, items);
	PHelper().CreateCaption(items, PrepareKey(pref, "Planar"), "Box is a projector volume (position / 3-axis rotation / size). Local Y is projection axis. Intersecting geometry receives the planar surface like a decal.");

	FloatValue* Stiffness = PHelper().CreateFloat(items, PrepareKey(pref, "Stiffness"), &m_Stiffness, 0.f, 1.f, 0.01f, 2);
	Stiffness->OnChangeEvent.bind(this, &CPlanar::OnStiffnessChange);
	Stiffness->Owner()->m_Flags.set(PropItem::flSlider, TRUE);
}

void CPlanar::OnStiffnessChange(PropValue* /*prop*/)
{
	clamp(m_Stiffness, 0.f, 1.f);
	RebuildProjection();
	UI->RedrawScene();
}

void CPlanar::OnShowHint(AStringVec& dest)
{
	inherited::OnShowHint(dest);
	dest.push_back(xr_string("Box: ") + xr_string::ToString(FScale.x) + " " + xr_string::ToString(FScale.y) + " " + xr_string::ToString(FScale.z));
	dest.push_back(xr_string("Projected tris: ") + xr_string::ToString((int)(m_ProjVerts.size() / 3)));
	dest.push_back(xr_string("Stiffness: ") + xr_string::ToString(m_Stiffness));
}
