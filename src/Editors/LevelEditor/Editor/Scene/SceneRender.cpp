#include "stdafx.h"

struct RenderBuckets
{
    xr_vector<CCustomObject*> Normal[4];
    xr_vector<CCustomObject*> Alpha[4];
};

static RenderBuckets* RenderBucketsData = nullptr;

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
	if (!valid())
	{
		return;
	}

	// extract and sort object tools
	SceneOToolsSet ObjTools;
	SceneMToolsSet SceneTools;
	{
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

	auto RENDER_SCENE_TOOLS = [&SceneTools](int P, bool B)
	{
		SceneMToolsIt s_it = SceneTools.begin();
		SceneMToolsIt s_end = SceneTools.end();
		for (; s_it != s_end; s_it++)
		{
			EDevice->SetShader(B ? EDevice->m_SelectionShader : EDevice->m_WireShader);
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

	for (u32 Priority = 1; Priority <= 3; ++Priority)
	{
		// normal pass: near-to-far
		for (CCustomObject* Object : RBucket.Normal[Priority])
		{
			Object->Render((int)Priority, false);
		}

		// alpha (strict B2F) pass: far-to-near -> reverse the near-to-far bucket
		for (int Iter = (int)RBucket.Alpha[Priority].size() - 1; Iter >= 0; --Iter)
		{
			RBucket.Alpha[Priority][Iter]->Render((int)Priority, true);
		}

		RENDER_SCENE_TOOLS((int)Priority, false);
		RENDER_SCENE_TOOLS((int)Priority, true);
	}

	FlushDU();

	// render snap
	RenderSnapList();

	// clear
	RenderList.clear();


	SceneMToolsIt s_it = SceneTools.begin();
	SceneMToolsIt s_end = SceneTools.end();
	for (; s_it != s_end; s_it++)
	{
		(*s_it)->AfterRender();
	}
}