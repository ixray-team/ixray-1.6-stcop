#include "stdafx.h"
#include "../../../xrECore/Editor/EditorInstancing.h"
#include "../../../xrECore/Editor/EditorSceneSubmit.h"

struct RenderBuckets
{
    xr_vector<CCustomObject*> Normal[4];
    xr_vector<CCustomObject*> Alpha[4];
};

static RenderBuckets* RenderBucketsData = nullptr;

// Values attached to the enclosing profiler event (Optick only)
#if defined(DEBUG_DRAW) && defined(IXRAY_PROFILER)
#	define PROF_TAG(Name, Value) OPTICK_TAG(Name, Value);
#else
#	define PROF_TAG(Name, Value)
#endif

struct tools_rp_pred
{
    IC bool operator()(ESceneToolBase* x, ESceneToolBase* y) const
    {	return x->RenderPriority()<y->RenderPriority();	}
};

#define DEFINE_MSET_PRED(T, N, I, Priority) \
	typedef xr_multiset<T, Priority> N;     \
	typedef N::iterator I;

DEFINE_MSET_PRED(ESceneToolBase*,SceneMToolsSet,SceneMToolsIt,tools_rp_pred);
DEFINE_MSET_PRED(ESceneCustomOTool*,SceneOToolsSet,SceneOToolsIt,tools_rp_pred);

void EScene::Render(const Fmatrix& camera)
{
	PROF_EVENT("EScene::Render")
	if (!valid())
	{
		return;
	}

	// extract and sort object tools
	SceneOToolsSet ObjTools;
	SceneMToolsSet SceneTools;
	{
		PROF_EVENT("EScene::Render BeforeRender")
		SceneToolsMapPairIt t_it = m_SceneTools.begin();
		SceneToolsMapPairIt t_end = m_SceneTools.end();
		for (; t_it != t_end; t_it++)
		{
			if (t_it->second)
			{
				// before render
				t_it->second->BeforeRender();
				// sort tools
				ESceneCustomOTool* CustomTool = smart_cast<ESceneCustomOTool*>(t_it->second);
				if (CustomTool)
				{
					ObjTools.insert(CustomTool);
				}
				SceneTools.insert(t_it->second);
			}
		}
	}

	// collect visible objects into a reusable vector (no per-frame sort)
	static xr_vector<CCustomObject*> RenderList;
	RenderList.reserve(8192);
	RenderList.clear();

	// The partition hands over only the objects of the cells around the camera
	static xr_vector<CCustomObject*> Candidates;
	Candidates.clear();
	if (GWorldPartition.CollectActive(Candidates))
	{
		PROF_EVENT("EScene::Render collect partition")
		for (CCustomObject* Obj : Candidates)
		{
			ESceneCustomOTool* SceneTool = GetOTool(Obj->FClassID);
			if (SceneTool && SceneTool->IsLoaded && SceneTool->IsVisible() && Obj->Visible() && Obj->IsRender())
			{
				RenderList.push_back(Obj);
			}
		}
	}
	else
	{
		PROF_EVENT("EScene::Render collect all")
		for (auto SceneTool : ObjTools)
		{
			if (!SceneTool->IsLoaded || !SceneTool->IsVisible())
			{
				continue;
			}

			ObjectList& lst = SceneTool->GetObjects();

			for (CCustomObject* Obj : lst)
			{
				if (Obj->Visible() && Obj->IsRender())
				{
					RenderList.push_back(Obj);
				}
			}
		}
	}

	auto RENDER_SCENE_TOOLS = [&SceneTools](int P, bool B)
	{
		SceneMToolsIt s_it = SceneTools.begin();
		SceneMToolsIt s_end = SceneTools.end();
		for (; s_it != s_end; s_it++)
		{
			PROF_EVENT("EScene::Render tool")
			PROF_TAG("ClassID", (u32)(*s_it)->FClassID);
			EDevice->SetShader(B ? GEditorDraw.SelectionShader : GEditorDraw.WireShader);
			RCache.set_xform_world(Fidentity);
			(*s_it)->OnRenderRoot(P, B);
		}
	};

	RenderBuckets RBucket;
	RenderBucketsData = &RBucket;
	for (CCustomObject* Object : RenderList)
	{
		u32 RenderPriorityMask = Object->RenderPriorityMask();
		for (u32 P = 1; P <= 3; ++P)
		{
			if (RenderPriorityMask & (1u << P))
			{
				RenderBucketsData->Normal[P].push_back(Object);
				RenderBucketsData->Alpha[P].push_back(Object);
			}
		}
	}

	RenderBucketsData = nullptr;

	// Static objects queue their submit work and it runs in parallel after the walk
	const bool DeferSubmit = GEditorDraw.IsSubmitPass();
	if (DeferSubmit)
	{
		GEditorSceneSubmit.BeginDeferred();
	}

	for (u32 Priority = 1; Priority <= 3; ++Priority)
	{
		{
			PROF_EVENT("EScene::Render objects")
			PROF_TAG("Count", (u32)RBucket.Normal[Priority].size());
			GEditorInstancing.Begin();
			for (CCustomObject* Object : RBucket.Normal[Priority])
			{
				Object->Render((int)Priority, false);
			}
			GEditorInstancing.End();
		}

		{
			PROF_EVENT("EScene::Render objects alpha")
			// alpha (strict B2F) pass: far-to-near -> reverse the near-to-far bucket
			for (int Iter = (int)RBucket.Alpha[Priority].size() - 1; Iter >= 0; --Iter)
			{
				RBucket.Alpha[Priority][Iter]->Render((int)Priority, true);
			}
		}

		RENDER_SCENE_TOOLS((int)Priority, false);
		RENDER_SCENE_TOOLS((int)Priority, true);
	}

	if (DeferSubmit)
	{
		GEditorSceneSubmit.FlushDeferred();
	}

	{
		PROF_EVENT("EScene::Render FlushDU")
		FlushDU();
	}

	{
		PROF_EVENT("EScene::Render snap list")
		RenderSnapList();
	}

	// clear
	RenderList.clear();


	PROF_EVENT("EScene::Render AfterRender")
	SceneMToolsIt s_it = SceneTools.begin();
	SceneMToolsIt s_end = SceneTools.end();
	for (; s_it != s_end; s_it++)
	{
		(*s_it)->AfterRender();
	}
}

void EScene::PrepareSubmit()
{
	if (!valid())
	{
		return;
	}

	ESceneCustomOTool* SceneTool = GetOTool(OBJCLASS_SCENEOBJECT);
	if (SceneTool == nullptr || !SceneTool->IsLoaded || !SceneTool->IsVisible())
	{
		return;
	}

	auto Prepare = [](CCustomObject* Object)
	{
		if (Object->FClassID == OBJCLASS_SCENEOBJECT && Object->Visible() && Object->IsRender())
		{
			static_cast<CSceneObject*>(Object)->PrepareSubmit();
		}
	};

	static xr_vector<CCustomObject*> Candidates;
	Candidates.clear();
	if (GWorldPartition.CollectActive(Candidates))
	{
		for (CCustomObject* Object : Candidates)
		{
			Prepare(Object);
		}
		return;
	}

	for (CCustomObject* Object : SceneTool->GetObjects())
	{
		Prepare(Object);
	}
}

void EScene::RenderShadowCasters()
{
	PROF_EVENT("EScene::RenderShadowCasters")
	if (!valid())
	{
		return;
	}

	// Classes whose objects add geometry to the scene graph; the others only draw helpers
	static const ObjClassID CasterClasses[] = { OBJCLASS_SCENEOBJECT, OBJCLASS_SPAWNPOINT, OBJCLASS_TERRAIN };

	auto RenderCaster = [](CCustomObject* Object)
	{
		// IsRender tests the current view, which is the light frustum here
		if (!Object->Visible() || !Object->IsRender())
		{
			return;
		}

		const u32 RenderPriorityMask = Object->RenderPriorityMask();
		for (u32 Priority = 1; Priority <= 3; ++Priority)
		{
			if (RenderPriorityMask & (1u << Priority))
			{
				Object->Render((int)Priority, false);
				Object->Render((int)Priority, true);
			}
		}
	};

	// Queued like the main pass; flushed when this function returns
	struct SDeferScope
	{
		SDeferScope() { GEditorSceneSubmit.BeginDeferred(); }
		~SDeferScope() { GEditorSceneSubmit.FlushDeferred(); }
	} DeferScope;

	static xr_vector<CCustomObject*> Candidates;
	Candidates.clear();
	if (GWorldPartition.CollectActive(Candidates))
	{
		for (CCustomObject* Object : Candidates)
		{
			if (Object->FClassID != OBJCLASS_SCENEOBJECT && Object->FClassID != OBJCLASS_SPAWNPOINT && Object->FClassID != OBJCLASS_TERRAIN)
			{
				continue;
			}

			ESceneCustomOTool* SceneTool = GetOTool(Object->FClassID);
			if (SceneTool && SceneTool->IsLoaded && SceneTool->IsVisible())
			{
				RenderCaster(Object);
			}
		}
		return;
	}

	for (ObjClassID ClassID : CasterClasses)
	{
		ESceneCustomOTool* SceneTool = GetOTool(ClassID);
		if (SceneTool == nullptr || !SceneTool->IsLoaded || !SceneTool->IsVisible())
		{
			continue;
		}

		for (CCustomObject* Object : SceneTool->GetObjects())
		{
			RenderCaster(Object);
		}
	}
}
