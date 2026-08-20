#include "stdafx.h"
#include "ESceneTerrainTools.h"
#include "ESceneTerrainControls.h"
#include "UI/Tools/UITerrainTool.h"

ESceneTerrainTool::ESceneTerrainTool() :
	ESceneCustomOTool(OBJCLASS_TERRAIN)
{
	m_BrushSize = 10;
	m_BrushStrength = 0.02f;
	m_BrushMode = bmRaise;
	m_BrushActive = false;
	m_EditedTerrain = nullptr;
	m_FlattenTarget = 0.5f;
	Clear();
}

ESceneTerrainTool::~ESceneTerrainTool()
{
}

void ESceneTerrainTool::Clear(bool bSpecific)
{
	inherited::Clear(bSpecific);

	lcontrol_last_idx	= 0;
	lcontrols.clear		();
	m_BrushActive = false;
	m_EditedTerrain = nullptr;
}

void ESceneTerrainTool::BeforeRender()
{
	
}

void ESceneTerrainTool::AfterRender()
{
}

void ESceneTerrainTool::OnRender(int priority, bool strictB2F)
{
	for (ObjectIt it=m_Objects.begin(); it!=m_Objects.end(); it++)
    	(*it)->Render(priority,strictB2F);

	if (GetSubTarget(0) == estTerrainSculpt)
		RenderBrush();
}

void ESceneTerrainTool::OnControlAppendClick(ButtonValue* sender, bool& bDataModified, bool& bSafe)
{
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	bDataModified = true;
}

void ESceneTerrainTool::OnControlRenameRemoveClick(ButtonValue* V, bool& bDataModified, bool& bSafe)
{
}

void ESceneTerrainTool::OnDeactivate()
{
	// При выходе из режима Terrain высотная карта (HMap) — источник правды,
	// а отображаемый/экспортируемый меш (TerrainObject/EditObject) мог остаться
	// старым, если скульптинг шёл без Preview. Перестраиваем меш под текущие данные.
	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); ++it)
	{
		CTerrain* t = (CTerrain*)(*it);
		if (t && t->TerrainObject)
		{
			t->RebuildMesh();
		}
	}

	inherited::OnDeactivate();
}

void ESceneTerrainTool::FillProp(const char* pref, PropItemVec& items)
{
	inherited::FillProp(pref, items);
}

bool ESceneTerrainTool::_AppendObject(CCustomObject* object)
{
	CTerrain* terr = (CTerrain*)object;

	// высотная карта уже построена (например, создана пустая плоскость) — просто добавляем
	if (!terr->HMap.Data)
	{
		bool loaded = false;
		if (object->GetName())
		{
			string_path Path = {};
			FS.update_path(Path, "$server_data_root$", object->GetName());
			xr_strcat(Path, ".r16");

			if (FS.TryLoad(Path))
			{
				IReader* Stream = FS.r_open(Path);
				if (Stream)
				{
					loaded = terr->LoadStream(*Stream);
					FS.r_close(Stream);
				}
			}
		}

		if (!loaded)
		{
			// нет внешнего .r16 — создаём пустую плоскость по умолчанию
			terr->InitializeHeightmap(129, 129, 0.5f);
		}
	}

	return inherited::_AppendObject(object);
}

bool ESceneTerrainTool::Validate(bool full_test)
{
	return true;
}

void ESceneTerrainTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
	AddControl(new TUI_ControlTerrainSculpt(estTerrainSculpt, etaAdd, this));
	pForm = new UITerrainTool();
	((UITerrainTool*)pForm)->tool = this;
}
 
void ESceneTerrainTool::RemoveControls()
{
	inherited::RemoveControls();
}

CTerrain* ESceneTerrainTool::PickTerrain(float& dist, Fvector& point)
{
	CTerrain* best = nullptr;
	float bestDist = UI->ZFar();

	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); ++it)
	{
		CTerrain* t = (CTerrain*)(*it);
		if (!t->TerrainObject)
		{
			continue;
		}

		float d = bestDist;
		SRayPickInfo pinf;
		if (t->TerrainObject->RayPick(d, UI->m_CurrentRStart, UI->m_CurrentRDir, t->_ITransform(), &pinf) && d < bestDist)
		{
			bestDist = d;
			best = t;
		}
	}

	if (best)
	{
		point.mad(UI->m_CurrentRStart, UI->m_CurrentRDir, bestDist);
	}

	dist = bestDist;
	return best;
}

void ESceneTerrainTool::BeginSculpt(CTerrain* obj, const Fvector& worldPoint)
{
	m_EditedTerrain = obj;
	m_BrushPos = worldPoint;
	m_BrushActive = true;

	Fvector local;
	obj->_ITransform().transform_tiny(local, worldPoint);

	const float StepHM = (obj->HMap.Width > 1) ? obj->HMap.Size.x : 1.f;
	const float SizeHM = (obj->HMap.Width - 1) * StepHM;
	const float HalfHM = SizeHM * 0.5f;

	float gx = (HalfHM - local.x) / StepHM;
	float gz = (local.z + HalfHM) / StepHM;
	int cgx = clampr(iFloor(gx + 0.5f), 0, (int)obj->HMap.Width - 1);
	int cgz = clampr(iFloor(gz + 0.5f), 0, (int)obj->HMap.Height - 1);

	m_FlattenTarget = obj->HMap.GetHeight(cgx, cgz);
}

void ESceneTerrainTool::SculptTerrain(CTerrain* obj, const Fvector& worldPoint)
{
	XRay::Editor::HeightmapUtils::SHeightMap& H = obj->HMap;
	if (!H.Data)
	{
		return;
	}

	Fvector local;
	obj->_ITransform().transform_tiny(local, worldPoint);

	const float StepHM = (H.Width > 1) ? H.Size.x : 1.f;
	const float SizeHM = (H.Width - 1) * StepHM;
	const float HalfHM = SizeHM * 0.5f;

	float gx = (HalfHM - local.x) / StepHM;
	float gz = (local.z + HalfHM) / StepHM;

	float rGrid = m_BrushSize / StepHM;
	int minX = std::max(0, iFloor(gx - rGrid));
	int maxX = std::min((int)H.Width - 1, iCeil(gx + rGrid));
	int minZ = std::max(0, iFloor(gz - rGrid));
	int maxZ = std::min((int)H.Height - 1, iCeil(gz + rGrid));

	for (int z = minZ; z <= maxZ; ++z)
	{
		for (int x = minX; x <= maxX; ++x)
		{
			float dx = (x - gx) * StepHM;
			float dz = (z - gz) * StepHM;
			float dist = sqrtf(dx * dx + dz * dz);
			if (dist > m_BrushSize)
			{
				continue;
			}

			float falloff = 1.f - dist / m_BrushSize;
			falloff = falloff * falloff * (3.f - 2.f * falloff); // smoothstep

			u32 idx = u32(z * H.Width + x);
			float& h = H.Data[idx];

			switch (m_BrushMode)
			{
				case bmRaise:	h += m_BrushStrength * falloff; break;
				case bmLower:	h -= m_BrushStrength * falloff; break;
				case bmFlatten: h += (m_FlattenTarget - h) * m_BrushStrength * falloff; break;
				case bmSmooth:
				{
					float avg = 0.f;
					int cnt = 0;
					for (int nz = -1; nz <= 1; ++nz)
					{
						for (int nx = -1; nx <= 1; ++nx)
						{
							int sx = x + nx;
							int sz = z + nz;
							if (sx >= 0 && sz >= 0 && sx < (int)H.Width && sz < (int)H.Height)
							{
								avg += H.GetHeight(sx, sz);
								++cnt;
							}
						}
					}
					if (cnt)
					{
						avg /= float(cnt);
					}
					h += (avg - h) * m_BrushStrength * falloff;
					break;
				}
			}

			h = clampr(h, 0.f, 1.f);
		}
	}

	H.MarkDirty();
}

void ESceneTerrainTool::RenderBrush()
{
	if (!m_BrushActive)
	{
		return;
	}

	const int segments = 32;
	Fvector prev, cur;
	for (int i = 0; i <= segments; ++i)
	{
		float a = i * PI * 2.f / segments;
		cur.set(m_BrushPos.x + cosf(a) * m_BrushSize, m_BrushPos.y + 0.2f, m_BrushPos.z + sinf(a) * m_BrushSize);

		if (i > 0)
		{
			DU_impl.DrawLine(prev, cur, 0xFFFFFF00);
		}
		prev = cur;
	}
}

void ESceneTerrainTool::CreateTerrain(LPCSTR name, u32 w, u32 h, float fill)
{
	string256 namebuffer;
	Scene->GenObjectName(OBJCLASS_TERRAIN, namebuffer, name);
	CTerrain* obj = new CTerrain(nullptr, namebuffer);
	obj->InitializeHeightmap(w, h, fill);
	obj->SetLoadedState();
	Scene->SelectObjects(false, OBJCLASS_TERRAIN);
	Scene->AppendObject(obj);
}

CCustomObject* ESceneTerrainTool::CreateObject(LPVOID data, const char* name)
{
	CCustomObject* O = new CTerrain(data, name);
	O->FParentTools = this;
	return O;
}

void ESceneTerrainTool::OnDrawUI()
{
	xr_string result;
	bool ok = false;
	if (UITextForm::GetResult(ok,result))
	{
		if (ok)
		{
		}
	}

	UITextForm::Update();
}

void ESceneTerrainTool::GetStaticDesc(int& v_cnt, int& f_cnt, bool b_selected_only, bool b_cform)
{
	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); it++)
	{
		CTerrain* obj = (CTerrain*)(*it);

		if (b_selected_only && !obj->Selected())
		{
			continue;
		}

		f_cnt += obj->TerrainObject->GetFaceCount();
		v_cnt += obj->TerrainObject->GetVertexCount();
	}
}