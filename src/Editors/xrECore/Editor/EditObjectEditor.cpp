#include "stdafx.h"


#include "EditObject.h"
#include "EditMesh.h"
#include "../../xrEngine/motion.h"
#include "../../xrEngine/bone.h"
#include "ExportSkeleton.h"
#include "ExportObjectOGF.h"
#include "D3DUtils.h"
#include "ui_main.h"
#include "../../../Layers/xrRenderPC_R4/RenderEditorApi.h"
#include "../Public/PropertiesListHelper.h"
#include "../../Layers/xrRenderPC_R4/ResourceManager.h"
#include "ImageManager.h"
#include "EditorInstancing.h"
#include "EditorSceneSubmit.h"

const float tex_w	= LOD_SAMPLE_COUNT*LOD_IMAGE_SIZE;
const float tex_h	= 1*LOD_IMAGE_SIZE;
const float half_p_x= 0.5f*(1.f/tex_w);
const float half_p_y= 0.5f*(1.f/tex_h);
const float offs_x 	= 1.f/tex_w;
const float offs_y 	= 1.f/tex_h;

static Fvector LOD_pos[4] =
{
	{-1.0f + offs_x, 1.0f - offs_y, 0.0f},
	{ 1.0f - offs_x, 1.0f - offs_y, 0.0f},
	{ 1.0f - offs_x,-1.0f + offs_y, 0.0f},
	{-1.0f + offs_x,-1.0f + offs_y, 0.0f}
};

static FVF::LIT LOD[6] =
{
	{{-1.0f, 1.0f, 0.0f},  0xFFFFFFFF, {0.0f,0.0f}}, // F 0
	{{ 1.0f, 1.0f, 0.0f},  0xFFFFFFFF, {0.0f,0.0f}}, // F 1
	{{ 1.0f,-1.0f, 0.0f},  0xFFFFFFFF, {0.0f,0.0f}}, // F 2

	{{-1.0f, 1.0f, 0.0f},  0xFFFFFFFF, {0.0f,0.0f}}, // F 0
	{{ 1.0f,-1.0f, 0.0f},  0xFFFFFFFF, {0.0f,0.0f}}, // F 2
	{{-1.0f,-1.0f, 0.0f},  0xFFFFFFFF, {0.0f,0.0f}}, // F 3
};

bool CEditableObject::Reload()
{
	ClearGeometry();
	return Load(m_LoadName.c_str());
}

bool CEditableObject::RayPick(float& dist, const Fvector& S, const Fvector& D, const Fmatrix& inv_parent, SRayPickInfo* pinf)
{
	bool picked = false;
	for(EditMeshIt m = m_Meshes.begin();m!=m_Meshes.end();m++)
	{
		float prev_dist = dist;
		if( (*m)->RayPick( dist, S, D, inv_parent, pinf ) )
		{
			if (pinf && pinf->e_mesh)
			{
				CSurface* surf = pinf->e_mesh->GetSurfaceByFaceID(pinf->inf.id);
				if (surf && !surf->m_bEditorVisible)
				{
					dist = prev_dist;
					continue;
				}
			}
			picked = true;
		}
	}
	return picked;
}

void CEditableObject::RayQuery(SPickQuery& pinf)
{
	for(EditMeshIt m = m_Meshes.begin();m!=m_Meshes.end();m++)
		(*m)->RayQuery(pinf);
}

void CEditableObject::RayQuery(const Fmatrix& parent, const Fmatrix& inv_parent, SPickQuery& pinf)
{
	for(EditMeshIt m = m_Meshes.begin();m!=m_Meshes.end();m++)
		(*m)->RayQuery(parent, inv_parent, pinf);
}

void CEditableObject::BoxQuery(const Fmatrix& parent, const Fmatrix& inv_parent, SPickQuery& pinf)
{
	for(EditMeshIt m = m_Meshes.begin();m!=m_Meshes.end();m++)
		(*m)->BoxQuery(parent, inv_parent, pinf);
}

#if 1
bool CEditableObject::FrustumPick(const CFrustum& frustum, const Fmatrix& parent){
	for(EditMeshIt m = m_Meshes.begin();m!=m_Meshes.end();m++)
		if((*m)->FrustumPick(frustum, parent))	return true;
	return false;
}

bool CEditableObject::BoxPick(CCustomObject* obj, const Fbox& box, const Fmatrix& inv_parent, SBoxPickInfoVec& pinf){
	bool picked = false;
	for(EditMeshIt m = m_Meshes.begin();m!=m_Meshes.end();m++)
		if ((*m)->BoxPick(box, inv_parent, pinf)){
			pinf.back().s_obj = obj;
			picked = true;
		}
	return picked;
}
#endif

ref_shader CSurface::_ShaderInstanced()
{
	if (m_RTFlags.is(rtInstancedShaderTried))
	{
		return m_ShaderInstanced;
	}

	m_RTFlags.set(rtInstancedShaderTried, true);

	if (!_Shader() || !m_ShaderName.size() || !m_Texture.size())
	{
		return m_ShaderInstanced;
	}

	RImplementation.SetEditorInstancing(true);
	m_ShaderInstanced.create(*m_ShaderName, *m_Texture);
	RImplementation.SetEditorInstancing(false);

	if (!CEditorInstanceBatcher::IsShaderSupported(&*m_ShaderInstanced))
	{
		m_ShaderInstanced.destroy();
	}

	return m_ShaderInstanced;
}

extern float ssaLIMIT;
extern float g_fSCREEN;
static const float ssaLim = 64.f * 64.f / (640 * 480);

void CEditableObject::Render(CCustomObject* pParent, const Fmatrix& parent, int priority, bool strictB2F, SurfaceVec* surfaces)
{
	PROF_EVENT("CEditableObject::Render")
	if (!(m_LoadState.is(LS_RBUFFERS)))
	{
		PROF_EVENT("CEditableObject::DefferedLoadRP")
		DefferedLoadRP();
	}

	Fvector v;
	float r;
	Fbox bb;
	bb.xform(m_BBox, parent);
	bb.getsphere(v, r);

	const bool SubmitPass = GEditorDraw.IsSubmitPass();
	const bool OverlayPass = GEditorDraw.IsOverlayPass();

	if (EPrefs->object_flags.is(epoDrawLOD) && (m_objectFlags.is(eoUsingLOD) && (CalcSSA(v, r) < ssaLim)))
	{
		if ((1 == priority) && (true == strictB2F) && !SubmitPass)
		{
			RenderLOD(parent);
		}
	}
	else 
	{
		RCache.set_xform_world(parent);

		if (m_objectFlags.is(eoHOM))
		{
			if ((1 == priority) && (false == strictB2F))
			{
				RenderEdge(pParent, 0, 0x40B64646);
			}

			if ((2 == priority) && (true == strictB2F))
			{
				RenderSelection(pParent, 0, 0xA0FFFFFF);
			}

		}
	//	else 
		if (m_objectFlags.is(eoSoundOccluder))
		{
			if ((1 == priority) && (false == strictB2F))
				RenderEdge(pParent, 0, 0xFF000000);

			if ((2 == priority) && (true == strictB2F))
				RenderSelection(pParent, 0, 0xA00000FF);
		}
	//	else 
		{
			if (psDeviceFlags.is(rsEdgedFaces) && (1 == priority) && (false == strictB2F))
			{
				PROF_EVENT("CEditableObject::RenderEdge")
		 		RenderEdge(pParent);
			}

			if (OverlayPass)
			{
				return;
			}

			if (IsSkeleton())
			{
				Engine.External.SetSkinningMode(4);
			}

			if (SubmitPass && !IsSkeleton())
			{
				SubmitSurfaces(pParent, parent, priority, strictB2F, surfaces);
				return;
			}

			size_t s_id = 0;
			for (auto s_it : m_Surfaces)
			{
				if (!s_it->m_bEditorVisible)
				{
					s_id++;
					continue;
				}

				int pr = s_it->_Priority();
				bool strict = s_it->_StrictB2F();

				if ((priority == pr) && (strictB2F == strict))
				{
					// FX: Панда написал систему инстансов для объектов, 
					// однако, если мы релоадим объекты и в новом объекте
					// полей больше, чем в старом, то получам выход за 
					// пределы. Поэтому просто регаем дефолтный материал.

					CSurface* DrawSurface = (surfaces != nullptr && surfaces->size() > s_id) ? (*surfaces)[s_id] : s_it;

					if (SubmitPass)
					{
						// Repeated placements of a surface merge into instanced draws (opaque only:
						// strict-B2F surfaces are sorted per copy)
						ref_shader InstancedShader;
						if (!strictB2F && !IsSkeleton())
						{
							InstancedShader = DrawSurface->_ShaderInstanced();
						}

						for (auto _M : m_Meshes)
						{
							_M->Submit(pParent, parent, s_it, DrawSurface->_Shader(), InstancedShader, IsSkeleton());
						}
						s_id++;
						continue;
					}

					EDevice->SetShader(DrawSurface->_Shader());

					ref_shader InstancedShader;
					if (!strictB2F && !IsSkeleton() && GEditorInstancing.IsActive())
					{
						InstancedShader = DrawSurface->_ShaderInstanced();
					}

					for (auto _M : m_Meshes)
					{
						if (IsSkeleton())
						{
							_M->RenderSkeleton(pParent, parent, s_it);
						}
						else if (!InstancedShader || !_M->SubmitInstance(pParent, parent, s_it, InstancedShader))
						{
							_M->Render(pParent, parent, s_it);
						}
					}
				}
				s_id++;
			}

			Engine.External.SetSkinningMode();
		}
	}
}

void CEditableObject::SubmitSurfaces(CCustomObject* pParent, const Fmatrix& parent, int priority, bool strictB2F, SurfaceVec* surfaces)
{
	// The scene passes use the pairs collected for the frame; shadow passes have their own
	// frustums and collect again
	if (!GEditorSceneSubmit.ShadowCollect)
	{
		if (SDeferredSubmit* Job = GEditorSceneSubmit.FindPrepared(pParent); Job != nullptr && Job->Object == this)
		{
			GEditorSceneSubmit.ApplyPrepared(*Job, priority, strictB2F);
			return;
		}
	}

	// While the scene pass collects objects the work runs later, spread over worker threads
	if (GEditorSceneSubmit.IsDeferring())
	{
		SDeferredSubmit& Job = GEditorSceneSubmit.AddDeferred();
		Job.Object = this;
		Job.Parent = pParent;
		Job.Transform = parent;
		Job.Priority = priority;
		Job.StrictB2F = strictB2F;
		Job.Surfaces = surfaces;
		GEditorSceneSubmit.LaunchDeferred(Job);
		return;
	}

	PROF_EVENT("CEditableObject::SubmitSurfaces")
	static SDeferredSubmit Job;
	Job.Object = this;
	Job.Parent = pParent;
	Job.Transform = parent;
	Job.Priority = priority;
	Job.StrictB2F = strictB2F;
	Job.Surfaces = surfaces;
	CollectSubmitPairs(Job);
	GEditorSceneSubmit.ApplyDeferred(Job);
}

void CEditableObject::CollectSubmitPairs(SDeferredSubmit& Job) const
{
	Job.Selected.clear();
	Job.Pairs.clear();
	Job.Fallback.clear();

	// Surfaces of this pass, once per object instead of once per mesh
	for (size_t s_id = 0; s_id < m_Surfaces.size(); ++s_id)
	{
		CSurface* Surface = m_Surfaces[s_id];
		if (!Surface->m_bEditorVisible)
		{
			continue;
		}

		const int Priority = Surface->_Priority();
		const bool StrictB2F = Surface->_StrictB2F();
		if (Job.Priority != SDeferredSubmit::AllPriorities && (Priority != Job.Priority || StrictB2F != Job.StrictB2F))
		{
			continue;
		}

		SDeferredSubmit::SSelected& Sel = Job.Selected.emplace_back();
		Sel.Surface = Surface;
		Sel.Priority = Priority;
		Sel.StrictB2F = StrictB2F;
		Sel.DrawSurface = (Job.Surfaces != nullptr && Job.Surfaces->size() > s_id) ? (*Job.Surfaces)[s_id] : Surface;
	}

	if (Job.Selected.empty())
	{
		return;
	}

	// Walk the surfaces each mesh has faces of: a large object has hundreds of meshes and
	// surfaces, and most pairs are empty
	for (CEditableMesh* Mesh : m_Meshes)
	{
		const RBMap* Buffers = Mesh->GetRenderBuffers();
		if (Buffers == nullptr)
		{
			// Submit generates the buffers on first use, on the primary thread
			Job.Fallback.push_back(Mesh);
			continue;
		}

		if (!Mesh->m_Flags.is(CEditableMesh::flVisible))
		{
			continue;
		}

		Fbox Box;
		Mesh->GetBox(Box);
		Box.xform(Job.Transform);
		if (!::Render->occ_visible(Box))
		{
			continue;
		}

		for (const auto& [Surface, Vector] : *Buffers)
		{
			for (u32 Index = 0; Index < u32(Job.Selected.size()); ++Index)
			{
				if (Job.Selected[Index].Surface == Surface)
				{
					Job.Pairs.push_back({ Mesh, Index });
					break;
				}
			}
		}
	}
}

u32 CEditableObject::RenderPriorityMask() const
{
    u32 m = (1u << 1); // base priority (edge / LOD / most surfaces)
    if (m_objectFlags.is(eoHOM) || m_objectFlags.is(eoSoundOccluder))
        m |= (1u << 2);
    for (CSurface* s : m_Surfaces)
    {
        int p = s->_Priority();
        if (p >= 1 && p <= 3) m |= (1u << p);
    }
    return m;
}

void CEditableObject::RenderSingle(CCustomObject* pParent, const Fmatrix& parent)
{
	for (int i=0; i<4; i++)
	{
		Render(pParent, parent, i, false);
		Render(pParent, parent, i, true);
	}
}

void CEditableObject::RenderAnimation(const Fmatrix&)
{
}

void CEditableObject::RenderEdge(CCustomObject* parent, CEditableMesh* mesh, u32 color)
{
	if (!(m_LoadState.is(LS_RBUFFERS)))
	{
		DefferedLoadRP();
	}

	if (mesh)
	{
		mesh->RenderEdge(parent, color);
	}
	else
	{
		for (auto _M : m_Meshes)
		{
			_M->RenderEdge(parent, color);
		}
	}
}

void CEditableObject::RenderSelection(CCustomObject* parent, CEditableMesh* mesh, u32 color)
{
	if (!(m_LoadState.is(LS_RBUFFERS)))
	{
		DefferedLoadRP();
	}

	if (mesh)
	{
		mesh->RenderSelection(parent, color);
	}
	else
	{
		for (auto _M : m_Meshes)
		{
			_M->RenderSelection(parent, color);
		}
	}
}

IC static void CalculateLODTC(int frame, int w_cnt, int h_cnt, Fvector2& lt, Fvector2& rb)
{
	Fvector2	ts;
	ts.set		(1.f/(float)w_cnt,1.f/(float)h_cnt);
	lt.x        = (frame%w_cnt+0)*ts.x+half_p_x;
	lt.y        = (frame/w_cnt+0)*ts.y+half_p_y;
	rb.x        = (frame%w_cnt+1)*ts.x-half_p_x;
	rb.y        = (frame/w_cnt+1)*ts.y-half_p_y;
}

void CEditableObject::GetLODFrame(int frame, Fvector p[4], Fvector2 t[4], const Fmatrix* parent)
{
	R_ASSERT(m_objectFlags.is(eoUsingLOD));
	Fvector P,S;
	m_BBox.get_CD	(P,S);
	float r 		= std::max(S.x,S.z);//sqrtf(S.x*S.x+S.z*S.z);
	Fmatrix T,matrix,rot;
	T.scale			(r,S.y,r);
	T.translate_over(P);
	if (parent) 
		T.mulA_43(*parent);

	float angle 	= frame*(PI_MUL_2/float(LOD_SAMPLE_COUNT));
	rot.rotateY(-angle);
	matrix.mul(T,rot);
	Fvector2 lt, rb;
	CalculateLODTC(frame,LOD_SAMPLE_COUNT,1,lt,rb);
	t[0].set(lt);
	t[1].set(rb.x,lt.y);
	t[2].set(rb);
	t[3].set(lt.x,rb.y);
	matrix.transform_tiny(p[0],LOD_pos[0]);
	matrix.transform_tiny(p[1],LOD_pos[1]);
	matrix.transform_tiny(p[2],LOD_pos[2]);
	matrix.transform_tiny(p[3],LOD_pos[3]);
}

void CEditableObject::RenderLOD(const Fmatrix& parent)
{
	Fvector C;
	C.sub(parent.c, EContext.UI->CurrentView().m_Camera.GetPosition());
	C.y = 0;
	float m = C.magnitude();
	if (m < EPS)
	{
		return;
	}
	
	C.div(m);
	int max_frame;
	float max_dot = 0;
	Fvector HPB;
	parent.getHPB(HPB);

	for (int frame = 0; frame < LOD_SAMPLE_COUNT; frame++)
	{
		float angle = angle_normalize(frame * (PI_MUL_2 / float(LOD_SAMPLE_COUNT)) + HPB.x);

		Fvector D;
		D.setHP(angle, 0);
		float dot = C.dotproduct(D);
		if (dot < 0.7072f) continue;

		if (dot > max_dot)
		{
			max_dot = dot;
			max_frame = frame;
		}
	}

	Fvector p[4];
	Fvector2 t[4];
	GetLODFrame(max_frame, p, t);

	LOD[0].p.set(p[0]); LOD[0].t.set(t[0]); LOD[0].color = 0xFFFFFFFF;
	LOD[1].p.set(p[1]); LOD[1].t.set(t[1]); LOD[1].color = 0xFFFFFFFF;
	LOD[2].p.set(p[2]); LOD[2].t.set(t[2]); LOD[2].color = 0xFFFFFFFF;

	LOD[3].p.set(p[0]); LOD[3].t.set(t[0]); LOD[3].color = 0xFFFFFFFF;
	LOD[4].p.set(p[2]); LOD[4].t.set(t[2]); LOD[4].color = 0xFFFFFFFF;
	LOD[5].p.set(p[3]); LOD[5].t.set(t[3]); LOD[5].color = 0xFFFFFFFF;

	RCache.set_xform_world(parent);
	EDevice->SetShader(m_LODShader ? m_LODShader : GEditorDraw.WireShader);
	DU_impl.DrawPrimitiveLIT(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 2, LOD, 6, true, false);
}

xr_string CEditableObject::GetLODTextureName()
{
	string512 nm; 	strcpy	(nm,m_LibName.c_str()); _ChangeSymbol(nm,'\\','_');
	xr_string 	l_name;
	l_name 			= xr_string("lod_")+nm;
	return ImageLib.UpdateFileName(l_name);
}

void CEditableObject::OnDeviceCreate()
{
}

void CEditableObject::OnDeviceDestroy()
{
	DefferedUnloadRP();
}

static RHIInputElementDesc dwDecl_4W[] =
{
	{ "POSITION",  0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0,  ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL",    0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT",   0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 32, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL",  0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 48, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD",  0, ERHI_FORMAT::R32G32_FLOAT,       0, 64, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD",  1, ERHI_FORMAT::B8G8R8A8_UNORM,     0, 72, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
};

void CEditableObject::DefferedLoadRP()
{
	if (m_LoadState.is(LS_RBUFFERS)) return;

	// skeleton
	vs_SkeletonGeom.create(dwDecl_4W, std::size(dwDecl_4W), RCache.Vertex.Buffer(), RCache.Index.Buffer());

	// создать LOD shader
	xr_string l_name = GetLODTextureName();
	xr_string fname = xr_string(l_name)+xr_string(".dds");
	m_LODShader.destroy();

	if (m_objectFlags.is(eoUsingLOD))
		m_LODShader.create(GetLODShaderName(),l_name.c_str());

	m_LoadState.set(LS_RBUFFERS,true);
}
void CEditableObject::DefferedUnloadRP()
{
	if (!(m_LoadState.is(LS_RBUFFERS)))
	{
		return;
	}
	// skeleton
	vs_SkeletonGeom.destroy();
	// удалить буфера
	for (EditMeshIt _M = m_Meshes.begin(); _M != m_Meshes.end(); _M++)
	{
		if (*_M)
		{
			(*_M)->GenerateRenderBuffers();
		}
	}
	// удалить shaders
	for (SurfaceIt s_it = m_Surfaces.begin(); s_it != m_Surfaces.end(); s_it++)
	{
		(*s_it)->OnDeviceDestroy();
	}
	// LOD
	m_LODShader.destroy();
	m_LoadState.set(LS_RBUFFERS, false);
}

void CEditableObject::EvictObject()
{
	EditMeshIt m 				= m_Meshes.begin();
	for(;m!=m_Meshes.end();m++){
		(*m)->UnloadCForm		();
		(*m)->UnloadVNormals	(true);
		(*m)->UnloadSVertices	(true);
		(*m)->UnloadFNormals	(true);
	}
	DefferedUnloadRP			();
}

void CEditableObject::OnChangeTransform(PropValue*)
{
	EContext.UI->RedrawScene();
}

IC bool BE(bool A, bool B)
{
	bool a = !!A;
	bool b = !!B;
	return a == b;
}

bool CEditableObject::CheckShaderCompatible()
{
	bool bRes 			= true;
	for(SurfaceIt s_it=m_Surfaces.begin(); s_it!=m_Surfaces.end(); s_it++)
	{
		IBlender* 		B = DEV->_FindBlender(*(*s_it)->m_ShaderName);
		Shader_xrLC* 	C = EDevice->ShaderXRLC.Get(*(*s_it)->m_ShaderXRLCName);
		if (!B||!C){
			ELog.Msg	(mtError,"Object '%s': invalid or missing shader [E:'%s', C:'%s']",m_LibName.c_str(),(*s_it)->m_ShaderName.c_str(),(*s_it)->m_ShaderXRLCName.c_str());
			bRes 		= false;
		}else{
			if (!BE(B->canBeLMAPped(),!C->flags.bLIGHT_Vertex)){
				ELog.Msg	(mtError,"Object '%s', material '%s': engine shader '%s' non compatible with compiler shader '%s'", m_LibName.c_str(),
					(*s_it)->m_Name.c_str(), (*s_it)->m_ShaderName.c_str(), (*s_it)->m_ShaderXRLCName.c_str());
				
				bRes 		= false;
			}
		}
	}
	return bRes;
}

bool CEditableObject::CreateBone(shared_str Name)
{
	if (!m_Bones.empty() || !m_BoneParts.empty() || m_objectFlags.test(eoDynamic))
		return false;
	
	m_LoadState.set(LS_RBUFFERS, false);

	CBone* B = new CBone();
	B->SetName(Name.c_str());
	B->SetWMap("");
	B->SetRestParams(0.01f, Fvector().set(0, 0, 0), Fvector().set(0, 0, 0));

	B->ResetData();
	B->Reset();

	B->SetParentName("");
	B->parent = nullptr;
   
	SBonePart& BP = m_BoneParts.emplace_back(SBonePart());
	BP.alias = "default";

	m_objectFlags.set(eoDynamic, true);

	BP.bones.push_back(B->Name());
	m_Bones.push_back(B);
	PrepareBones();

	for (CEditableMesh* _M : m_Meshes)
	{
		_M->AssignMesh(Name);
	}

	return true;
}

void CEditableObject::AddBone(CBone* parent_bone)
{
	CBone* B 			= new CBone();

	string256			new_name;
	u32 i				= 0;

	do{
		sprintf				(new_name,"bone%.3d", i++);
	}while(    FindBoneByName(new_name) );

	B->SetName			(new_name);
	B->SetWMap			("");
	B->SetRestParams	(0.01f, Fvector().set(0,0,0), Fvector().set(0,0,0) );

	B->ResetData		();
	B->Reset			();

	if(parent_bone)
	{
		B->SetParentName(parent_bone->Name().c_str());
	}else
	{
		int	bid = 		GetRootBoneID	();
		GetBone(bid)->SetParentName		(B->Name().c_str());
		B->SetParentName				("");
	}

	m_BoneParts[0].bones.push_back(B->Name());
	m_Bones.push_back	(B);
	PrepareBones		();

	for (EditMeshIt _M=m_Meshes.begin(); _M!=m_Meshes.end(); _M++)
	{
		(*_M)->UnloadSVertices();
	}
}

CBone* 	bone_to_delete = nullptr;
u32 	bone_to_delete_frame = 0;

void CEditableObject::DeleteBone(CBone* bone)
{
	CBone* PB = bone->Parent();

	for (BoneIt b_it=m_Bones.begin(); b_it!=m_Bones.end(); ++b_it)
	{
		CBone* B = *b_it;
		if(B->Parent()==bone)
			B->SetParentName(PB?PB->Name().c_str():"");
	}
	BoneIt bit 		= std::find(m_Bones.begin(), m_Bones.end(), bone);
	if(bit==m_Bones.end())
		Msg("! bone not found -((");
	else
		m_Bones.erase	(bit);

	BPIt bpit 			= BonePart(bone);
	RStringVec::iterator iit 	= (*bpit).bones.begin();
	RStringVec::iterator iit_e 	= (*bpit).bones.end();
	for( ;iit!=iit_e; ++iit)
	{
		if(*iit==bone->Name())
		{
			(*bpit).bones.erase(iit);
			break;
		}
	}


	bone_to_delete = bone;
	bone_to_delete_frame = EDevice->dwFrame;
	PrepareBones	();

	for (EditMeshIt _M=m_Meshes.begin(); _M!=m_Meshes.end(); _M++)
	{
		(*_M)->UnloadSVertices();
	}
}

BPIt CEditableObject::BonePart(CBone* B)
{
	BPIt it 		= m_BoneParts.begin();
	BPIt it_e 		= m_BoneParts.end();
	for( ;it!=it_e; ++it)
	{
		RStringVec::iterator iit 	= (*it).bones.begin();
		RStringVec::iterator iit_e 	= (*it).bones.end();
		for( ;iit!=iit_e; ++iit)
		{
			if(*iit==B->Name())
				return it;
		}
	}
	return it_e;
}

void CEditableObject::RenameBone(CBone* bone, const char* new_name)
{
	BPIt bpit = BonePart(bone);
	RStringVec::iterator iit = (*bpit).bones.begin();
	RStringVec::iterator iit_e = (*bpit).bones.end();

	for (; iit != iit_e; ++iit)
	{
		if (*iit == bone->Name())
		{
			*iit = new_name;
			break;
		}
	}

	for (BoneIt b_it = m_Bones.begin(); b_it != m_Bones.end(); ++b_it)
	{
		CBone* B = *b_it;
		if (B->ParentName() == bone->Name())
		{
			B->SetParentName(new_name);
		}
	}

	for (SMotionIt s_it = m_SMotions.begin(); s_it != m_SMotions.end(); ++s_it)
	{
		CSMotion* M = *s_it;
		for (BoneMotionIt bm_it = M->BoneMotions().begin(); bm_it != M->BoneMotions().end(); ++bm_it)
		{
			if (bm_it->name == bone->Name())
			{
				bm_it->name = new_name;
			}
		}
	}

	bOnModified = true;
}