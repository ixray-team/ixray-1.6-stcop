#include "stdafx.h"

#include "../../../../xrECore/Editor/EditorRenderBackend.h"

namespace
{
constexpr std::uint64_t EditorGlowFnvOffset = 14695981039346656037ull;
constexpr std::uint64_t EditorGlowFnvPrime = 1099511628211ull;

void HashEditorGlowBytes(std::uint64_t& Hash, const void* Data, const std::size_t Size)
{
	const auto* Bytes = static_cast<const std::uint8_t*>(Data);
	for (std::size_t Index = 0; Index < Size; ++Index)
	{
		Hash ^= Bytes[Index];
		Hash *= EditorGlowFnvPrime;
	}
}

void HashEditorGlowString(std::uint64_t& Hash, const std::string_view Text)
{
	HashEditorGlowBytes(Hash, Text.data(), Text.size());
	const std::uint8_t Separator = 0;
	HashEditorGlowBytes(Hash, &Separator, sizeof(Separator));
}

void CaptureEditorGlowSprite(const CGlow& Glow, const Fvector& Position, const bool FixedSize)
{
	if (!IsEditorDebugDrawCaptureActive() || !EDevice)
	{
		return;
	}

	FVF::TL Projected;
	Projected.transform(Position, EDevice->mFullTransform);
	const float ProjectionScale = std::abs(EDevice->mProject._11);
	if (!std::isfinite(Projected.p.w) || Projected.p.w <= 0.0f ||
		ProjectionScale <= EPS_S)
	{
		return;
	}

	const float HalfSize = FixedSize
							   ? Glow.m_fRadius * Projected.p.w / ProjectionScale
							   : 2.0f * Glow.m_fRadius / ProjectionScale;
	if (!std::isfinite(HalfSize) || HalfSize <= 0.0f)
	{
		return;
	}

	std::string TextureName = Glow.m_TexName.size()
								  ? *Glow.m_TexName
								  : std::string{};
	if (TextureName.empty())
	{
		TextureName = "textures/default/default_white";
	}
	constexpr std::string_view ShaderName = "editor\\glow_sprite";

	std::uint64_t MaterialHash = EditorGlowFnvOffset;
	HashEditorGlowString(MaterialHash, ShaderName);
	HashEditorGlowString(MaterialHash, TextureName);
	if (MaterialHash == 0)
	{
		MaterialHash = 1;
	}

	std::uint64_t MeshHash = EditorGlowFnvOffset;
	HashEditorGlowString(MeshHash, "legacy-editor-glow-billboard-v1");
	const auto GlowIdentity = reinterpret_cast<std::uintptr_t>(&Glow);
	HashEditorGlowBytes(MeshHash, &MaterialHash, sizeof(MaterialHash));
	if (MeshHash == 0)
	{
		MeshHash = 1;
	}

	constexpr std::array<std::array<float, 3>, 4> Positions = {
		std::array<float, 3>{-1.0f, -1.0f, 0.0f},
		std::array<float, 3>{-1.0f, 1.0f, 0.0f},
		std::array<float, 3>{1.0f, -1.0f, 0.0f},
		std::array<float, 3>{1.0f, 1.0f, 0.0f}
	};
	constexpr std::array<std::array<float, 2>, 4> TexCoords = {
		std::array<float, 2>{0.0f, 1.0f},
		std::array<float, 2>{0.0f, 0.0f},
		std::array<float, 2>{1.0f, 1.0f},
		std::array<float, 2>{1.0f, 0.0f}
	};

	std::uint64_t ObjectId = reinterpret_cast<std::uintptr_t>(
		GetEditorTransientObjectIdentity()
	);
	if (ObjectId == 0)
	{
		ObjectId = GlowIdentity;
	}
	if (ObjectId == 0)
	{
		ObjectId = 1;
	}

	FEditorTransientMeshCapture Capture;
	Capture.MeshId = {MeshHash};
	Capture.ObjectId = {ObjectId};
	Capture.MaterialSlot = {MaterialHash};
	Capture.Revision = MeshHash;
	Capture.ShaderName = ShaderName;
	Capture.TextureName = std::move(TextureName);
	Capture.SurfaceName = Glow.m_ShaderName.size()
							  ? "Legacy glow: " + std::string(*Glow.m_ShaderName)
							  : "Legacy glow";
	Capture.MaterialFlags = EEditorMaterialSlotFlags::TwoSided;
	Capture.InstanceFlags = EEditorSceneInstanceFlags::TwoSided;
	Fmatrix LocalToWorld;
	LocalToWorld.set(Fvector(EDevice->vCameraRight).mul(HalfSize), Fvector(EDevice->vCameraTop).mul(HalfSize), Fvector(EDevice->vCameraDirection).invert(), Position);
	std::copy_n(LocalToWorld.mm, Capture.LocalToWorld.size(), Capture.LocalToWorld.begin());
	Capture.Vertices.reserve(Positions.size());
	for (std::size_t Index = 0; Index < Positions.size(); ++Index)
	{
		FEditorStaticMeshVertex& Vertex = Capture.Vertices.emplace_back();
		Vertex.Position = Positions[Index];
		Vertex.Normal = {0.0f, 0.0f, 1.0f};
		Vertex.Tangent = {1.0f, 0.0f, 0.0f, 1.0f};
		Vertex.TexCoord = TexCoords[Index];
	}
	Capture.Indices = {0, 1, 2, 3, 2, 1};
	CaptureEditorTransientMesh(std::move(Capture));
}
} // namespace

#define GLOW_VERSION 0x0012

#define GLOW_CHUNK_VERSION 0xC411
#define GLOW_CHUNK_PARAMS 0xC413
#define GLOW_CHUNK_SHADER 0xC414
#define GLOW_CHUNK_TEXTURE 0xC415
#define GLOW_CHUNK_FLAGS 0xC416


#define VIS_RADIUS 0.25f

CGlow::CGlow(LPVOID data, const char* name)
	: CCustomObject(data, name)
{
	Construct(data);
}

void CGlow::Construct(LPVOID data)
{
	FClassID = OBJCLASS_GLOW;
	m_GShader = 0;
	m_fRadius = 0.5f;
	m_bDefLoad = false;
	m_Flags.zero();
	m_ShaderName = "effects\\glow";
}

CGlow::~CGlow()
{
	OnDeviceDestroy();
}

void CGlow::OnDeviceCreate()
{
	if (m_bDefLoad)
	{
		return;
	}
	// создать заново shaders
	if (m_TexName.size() && m_ShaderName.size())
	{
		m_GShader.create(*m_ShaderName, *m_TexName);
	}
	m_bDefLoad = true;
}

void CGlow::OnDeviceDestroy()
{
	m_bDefLoad = false;
	// удалить shaders
	m_GShader.destroy();
}

void CGlow::ShaderChange(PropValue* value)
{
	OnDeviceDestroy();
}

bool CGlow::GetBox(Fbox& box)
{
	box.set(GetPosition(), GetPosition());
	box.min.sub(m_fRadius);
	box.max.add(m_fRadius);
	return true;
}

void CGlow::Render(int priority, bool strictB2F)
{
	if ((1 == priority) && (true == strictB2F))
	{
		if (!m_bDefLoad)
		{
			OnDeviceCreate();
		}
		ESceneGlowTool* gt = smart_cast<ESceneGlowTool*>(FParentTools);
		VERIFY(gt);
		RCache.set_xform_world(Fidentity);

		if (gt->m_Flags.is(ESceneGlowTool::flTestVisibility))
		{
			Fvector D;
			D.sub(EDevice->vCameraPosition, GetPosition());
			float dist = D.normalize_magn();
			if (!Scene->RayPickObject(dist, GetPosition(), D, OBJCLASS_SCENEOBJECT, 0, 0))
			{
				if (m_GShader)
				{
					EDevice->SetShader(m_GShader);
				}
				else
				{
					EDevice->SetShader(EDevice->m_WireShader);
				}
				Fvector p = GetPosition();
				CaptureEditorGlowSprite(*this, p, m_Flags.is(gfFixedSize));
				m_RenderSprite.Render(p, m_fRadius, m_Flags.is(gfFixedSize));
				DU_impl.DrawRomboid(p, VIS_RADIUS, 0x00FF8507);
			}
			else
			{
				// рендерим bounding sphere
				EDevice->SetShader(EDevice->m_WireShader);
				DU_impl.DrawRomboid(GetPosition(), VIS_RADIUS, 0x00FF8507);
			}
		}
		else
		{
			if (m_GShader)
			{
				EDevice->SetShader(m_GShader);
			}
			else
			{
				EDevice->SetShader(EDevice->m_WireShader);
			}
			Fvector p = GetPosition();
			CaptureEditorGlowSprite(*this, p, m_Flags.is(gfFixedSize));
			m_RenderSprite.Render(p, m_fRadius, m_Flags.is(gfFixedSize));
		}
		if (Selected())
		{
			Fbox bb;
			GetBox(bb);
			u32 clr = Locked() ? 0xFFFF0000 : 0xFFFFFFFF;
			EDevice->SetShader(EDevice->m_WireShader);
			DU_impl.DrawSelectionBoxB(bb, &clr);
			if (gt->m_Flags.is(ESceneGlowTool::flDrawCross))
			{
				Fvector sz;
				bb.getradius(sz);
				DU_impl.DrawCross(GetPosition(), sz.x, sz.y, sz.z, sz.x, sz.y, sz.z, 0xFFFFFFFF, false);
			}
		}
	}
}

bool CGlow::FrustumPick(const CFrustum& frustum)
{
	return (frustum.testSphere_dirty(GetPosition(), m_fRadius)) ? true : false;
}

bool CGlow::RayPick(float& distance, const Fvector& start, const Fvector& direction, SRayPickInfo* pinf)
{
	Fvector ray2;
	ray2.sub(GetPosition(), start);

	float d = ray2.dotproduct(direction);
	if (d > 0)
	{
		float d2 = ray2.magnitude();
		if (((d2 * d2 - d * d) < (m_fRadius * m_fRadius)) && (d > m_fRadius))
		{
			if (d < distance)
			{
				distance = d;
				return true;
			}
		}
	}
	return false;
}

bool CGlow::LoadLTX(CInifile& ini, const char* sect_name)
{
	u32 version = ini.r_u32(sect_name, "version");

	if (version != GLOW_VERSION)
	{
		ELog.DlgMsg(mtError, "CGlow: Unsupported version.");
		return false;
	}

	CCustomObject::LoadLTX(ini, sect_name);

	m_ShaderName = ini.r_string(sect_name, "shader_name");

	m_TexName = ini.r_string(sect_name, "texture_name");

	m_fRadius = ini.r_float(sect_name, "radius");

	m_Flags.assign(ini.r_u32(sect_name, "flags"));

	return true;
}

void CGlow::SaveLTX(CInifile& ini, const char* sect_name)
{
	CCustomObject::SaveLTX(ini, sect_name);

	ini.w_u16(sect_name, "version", GLOW_VERSION);

	ini.w_float(sect_name, "radius", m_fRadius);

	ini.w_string(sect_name, "shader_name", m_ShaderName.c_str());

	ini.w_string(sect_name, "texture_name", m_TexName.c_str());

	ini.w_u16(sect_name, "flags", m_Flags.get());
}

bool CGlow::LoadStream(IReader& F)
{
	u16 version = 0;

	R_ASSERT(F.r_chunk(GLOW_CHUNK_VERSION, &version));
	if ((version != 0x0011) && (version != GLOW_VERSION))
	{
		ELog.DlgMsg(mtError, "CGlow: Unsupported version.");
		return false;
	}

	CCustomObject::LoadStream(F);

	if (F.find_chunk(GLOW_CHUNK_SHADER))
	{
		F.r_stringZ(m_ShaderName);
	}

	R_ASSERT(F.find_chunk(GLOW_CHUNK_TEXTURE));
	F.r_stringZ(m_TexName);

	R_ASSERT(F.find_chunk(GLOW_CHUNK_PARAMS));
	m_fRadius = F.r_float();
	if (version == 0x0011)
	{
		F.r_fvector3(FPosition);
		UpdateTransform();
	}

	if (F.find_chunk(GLOW_CHUNK_FLAGS))
	{
		m_Flags.assign(F.r_u16());
	}

	return true;
}

void CGlow::SaveStream(IWriter& F)
{
	CCustomObject::SaveStream(F);

	F.open_chunk(GLOW_CHUNK_VERSION);
	F.w_u16(GLOW_VERSION);
	F.close_chunk();

	F.open_chunk(GLOW_CHUNK_PARAMS);
	F.w_float(m_fRadius);
	F.close_chunk();

	F.open_chunk(GLOW_CHUNK_SHADER);
	F.w_stringZ(m_ShaderName);
	F.close_chunk();

	F.open_chunk(GLOW_CHUNK_TEXTURE);
	F.w_stringZ(m_TexName);
	F.close_chunk();

	F.open_chunk(GLOW_CHUNK_FLAGS);
	F.w_u16(m_Flags.get());
	F.close_chunk();
}


void CGlow::FillProp(const char* pref, PropItemVec& items)
{
	inherited::FillProp(pref, items);
	PropValue* V = 0;
	V = PHelper().CreateChoose(items, PrepareKey(pref, "Texture"), &m_TexName, smTexture);
	V->OnChangeEvent.bind(this, &CGlow::ShaderChange);
	V = PHelper().CreateChoose(items, PrepareKey(pref, "Shader"), &m_ShaderName, smEShader);
	V->OnChangeEvent.bind(this, &CGlow::ShaderChange);
	PHelper().CreateFloat(items, PrepareKey(pref, "Radius"), &m_fRadius, 0.01f, 10000.f);
	//.	PHelper().CreateFlag<Flags8>(items,PHelper().PrepareKey(pref,"Fixed size"),	&m_Flags, 		gfFixedSize);
}


bool CGlow::GetSummaryInfo(SSceneSummary* inf)
{
	inherited::GetSummaryInfo(inf);
	xr_string temp = ChangeFileExt(*m_TexName, "");
	xr_strlwr(temp);
	if (m_TexName.size())
	{
		inf->AppendTexture(temp.c_str(), SSceneSummary::sttGlow, 0, 0, "$GLOW$");
	}
	inf->glow_cnt++;
	return true;
}
