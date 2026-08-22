#include "stdafx.h"
#include "Terrain.h"

CTerrain::CTerrain(LPVOID data, const char* name)
	: inherited(data, name ? name : "terrain"), TerrainObject(new CEditableObject(name ? name : "terrain"))
{
	Construct(data);
	FScale.set(1, 1, 1);
	m_RT_Flags.set(flRT_Visible, true);
}

void CTerrain::Construct(LPVOID data)
{
	FClassID = OBJCLASS_TERRAIN;

	SurfaceShader = "levels\\zaton_earth";
	SurfaceShaderXRLC = "default";
	SurfaceGameMtl = "materials\\earth";
	SurfaceTexture = "terrain\\terrain_mp_atp";
}

CTerrain::~CTerrain()
{
	if (TerrainObject)
	{
		for (auto _M : TerrainObject->Meshes())
		{
			_M->RemoveColor(this);
		}
		TerrainObject->EvictObject();
	}

	xr_delete(TerrainObject);
}

void CTerrain::OnUpdateTransform()
{
	inherited::OnUpdateTransform();

	// update bounding volume
	if (TerrainObject)
	{
		m_TBBox.set(TerrainObject->GetBox());
		m_TBBox.xform(_Transform());
	}
}

bool CTerrain::LoadStream(IReader& F)
{
	if (F.length() == 0)
	{
		return false;
	}

	HMap.LoadSteam(&F);

	char Buf[512];
	F.r_stringZ(Buf, sizeof(Buf));
	SurfaceShader = Buf;
	F.r_stringZ(Buf, sizeof(Buf));
	SurfaceShaderXRLC = Buf;
	F.r_stringZ(Buf, sizeof(Buf));
	SurfaceGameMtl = Buf;
	F.r_stringZ(Buf, sizeof(Buf));
	SurfaceTexture = Buf;

	XRay::Editor::HeightmapUtils::GenerateMeshByHeightmap(HMap, TerrainObject, ScaleY, SurfaceTemplate());

	return true;
}

void CTerrain::InitializeHeightmap(u32 w, u32 h, float fill)
{
	HMap.Create(w, h, fill);
	XRay::Editor::HeightmapUtils::GenerateMeshByHeightmap(HMap, TerrainObject, ScaleY, SurfaceTemplate());
	HMap.MarkDirty();
	OnUpdateTransform();
}

void CTerrain::RebuildMesh()
{
	CEditableObject* Old = TerrainObject;
	TerrainObject = new CEditableObject(GetName());
	XRay::Editor::HeightmapUtils::GenerateMeshByHeightmap(HMap, TerrainObject, ScaleY, SurfaceTemplate());

	UI->CommandList[TUI::ECommandListID::NextFrame].push_back(
		[Old]()
		{
			Old->EvictObject();
			xr_delete(Old);
		}
	);
	OnUpdateTransform();
	HMap.MarkDirty();
}

void CTerrain::OnChangePreview(PropValue* sender)
{
	if (IsPreview)
	{
		RebuildMesh();
	}
}

void CTerrain::SaveStream(IWriter& F)
{
	HMap.SaveSteam(&F);

	F.w_stringZ(*SurfaceShader);
	F.w_stringZ(*SurfaceShaderXRLC);
	F.w_stringZ(*SurfaceGameMtl);
	F.w_stringZ(*SurfaceTexture);

	inherited::SaveStream(F);
}

XRay::Editor::HeightmapUtils::STerrainSurfaceTemplate CTerrain::SurfaceTemplate() const
{
	return {*SurfaceShader, *SurfaceShaderXRLC, *SurfaceGameMtl, *SurfaceTexture};
}

void CTerrain::OnDeviceDestroy()
{
	HMap.RenderData.InvalidateGpu();
	HMap.RenderData.IsDirty = true;
	inherited::OnDeviceDestroy();
}

void CTerrain::OnFrame()
{
	CCustomObject::OnFrame();

	auto fequal = [](float a, float b, float eps = EPS)
	{
		return fabs(a - b) < eps;
	};

	if (!fequal(HMap.Pos.x, FPosition.x) || !fequal(HMap.Pos.y, FPosition.y) || !fequal(HMap.Pos.z, FPosition.z))
	{
		HMap.Pos = FPosition;
		HMap.MarkDirty();
	}

	if (!fequal(HMap.Size.x, FScale.x) || !fequal(HMap.Size.y, FScale.y) || !fequal(HMap.Size.z, FScale.z))
	{
		HMap.Size = FScale;
		HMap.MarkDirty();
	}
}

bool CTerrain::RayPick(float& dist, const Fvector& S, const Fvector& D, SRayPickInfo* pinf)
{
	if (!IsLoaded && !pinf->IsForcePickup)
	{
		return false;
	}

	if (LTools->GetTarget() == OBJCLASS_TERRAIN)
	{
		if (HMap.RayPick(dist, S, D, _ITransform(), pinf))
		{
			if (pinf)
			{
				pinf->s_obj = this;
			}
			return true;
		}
	}
	else
	{
		if (TerrainObject->RayPick(dist, S, D, _ITransform(), pinf))
		{
			if (pinf)
			{
				pinf->s_obj = this;
			}
			return true;
		}
	}

	return false;
}

void CTerrain::Render(int priority, bool strictB2F)
{
	if (LTools->GetTarget() == OBJCLASS_TERRAIN && !IsPreview)
	{
		if (priority == 1 && !strictB2F)
		{
			HMap.Draw(ScaleY, 1.f);

			if (Selected())
			{
				EDevice->SetShader(EDevice->m_WireShader);
				RCache.set_xform_world(_Transform());
				u32 clr = Locked() ? 0xFFFF0000 : 0xFFFFFFFF;
				DU_impl.DrawSelectionBoxB(TerrainObject->GetBox(), &clr);
			}
		}
	}
	else
	{
		TerrainObject->Render(this, _Transform(), priority, strictB2F);
	}
}

void CTerrain::Move(Fvector& amount)
{
	inherited::Move(amount);
	HMap.MarkDirty();
	HMap.Pos = FPosition;
}

void CTerrain::Scale(Fvector& amount)
{
	inherited::Scale(amount);
	HMap.MarkDirty();
	HMap.Size = FScale;
}

void CTerrain::FillProp(const char* pref, PropItemVec& items)
{
	inherited::FillProp(pref, items);

	PropValue* PreviewVal = PHelper().CreateBool(items, "Height Map\\Preview", &IsPreview);
	PreviewVal->OnChangeEvent.bind(this, &CTerrain::OnChangePreview);

	S32Value* ScaleEdit = PHelper().CreateS32(items, "Height Map\\Multiply Y", &ScaleY);
	ScaleEdit->OnAfterEditEvent.bind(this, &CTerrain::OnChangeHMData);

	shared_str Pref1 = PrepareKey(pref, "Surfaces").c_str();
	shared_str Pref2 = PrepareKey(Pref1.c_str(), "terrain").c_str();
	{
		PropValue* V;
		V = PHelper().CreateChoose(items, PrepareKey(Pref2.c_str(), "Texture"), &SurfaceTexture, smTexture);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeSurfaceProp);
		V = PHelper().CreateChoose(items, PrepareKey(Pref2.c_str(), "Shader"), &SurfaceShader, smEShader);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeSurfaceProp);
		V = PHelper().CreateChoose(items, PrepareKey(Pref2.c_str(), "Compile"), &SurfaceShaderXRLC, smCShader);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeSurfaceProp);
		V = PHelper().CreateChoose(items, PrepareKey(Pref2.c_str(), "Game Mtl"), &SurfaceGameMtl, smGameMaterial);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeSurfaceProp);
	}
}

void CTerrain::OnChangeSurfaceProp(PropValue* sender)
{
	ApplySurfaceTemplate();
}

void CTerrain::ApplySurfaceTemplate()
{
	if (!TerrainObject || TerrainObject->Surfaces().empty())
	{
		return;
	}

	CSurface* S = *TerrainObject->FirstSurface();
	if (SurfaceTexture[0])
	{
		S->SetTexture(*SurfaceTexture);
	}
	if (SurfaceShader[0])
	{
		S->SetShader(*SurfaceShader);
	}
	if (SurfaceShaderXRLC[0])
	{
		S->SetShaderXRLC(*SurfaceShaderXRLC);
	}
	if (SurfaceGameMtl[0])
	{
		S->SetGameMtl(*SurfaceGameMtl);
	}
}

bool CTerrain::OnChangeHMData(PropValue* sender, int& NewValue)
{
	if (NewValue < 0)
	{
		return false;
	}

	if (NewValue == m_AppliedHMScale)
	{
		return true;
	}

	m_AppliedHMScale = NewValue;
	ScaleY = NewValue;

	HMap.MarkDirty();

	if (IsPreview)
	{
		RebuildMesh();
	}

	return true;
}

void CTerrain::BoxQuery(SPickQuery& pinf)
{
	if (!TerrainObject)
	{
		return;
	}

	TerrainObject->BoxQuery(_Transform(), _ITransform(), pinf);
}

void CTerrain::RayQuery(SPickQuery& pinf)
{
	if (!TerrainObject)
	{
		return;
	}

	TerrainObject->RayQuery(_Transform(), _ITransform(), pinf);
}

bool CTerrain::GetBox(Fbox& box)
{
	if (!TerrainObject)
	{
		return false;
	}
	box.set(m_TBBox);
	return true;
}

bool CTerrain::BoxPick(const Fbox& box, SBoxPickInfoVec& pinf)
{
	if (!TerrainObject)
	{
		return false;
	}

	return TerrainObject->BoxPick(this, box, _ITransform(), pinf);
}

bool CTerrain::GetUTBox(Fbox& box)
{
	if (!TerrainObject)
	{
		return false;
	}
	box.set(TerrainObject->GetBox());

	return true;
}
