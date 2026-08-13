#include "stdafx.h"

static constexpr size_t	s_arena_size = 8 * 1024 * 1024;
static char s_fake_array[s_arena_size];

doug_lea_area_allocator	g_render_lua_allocator_area(s_fake_array,"render:sdk", s_arena_size);

struct RenderBuckets
{
    xr_vector<CCustomObject*> Normal[4];
    xr_vector<CCustomObject*> Alpha[4];
};

static RenderBuckets* RenderBucketsData = nullptr;

static void collect_object(EScene::mapObject_Node* N)
{
    CCustomObject* o = N->val;
    u32 m = o->RenderPriorityMask();
    for (u32 P = 1; P <= 3; ++P)
    {
        if (m & (1u << P))
        {
            RenderBucketsData->Normal[P].push_back(o);
            RenderBucketsData->Alpha[P].push_back(o);
        }
    }
}

struct tools_rp_pred
{
    IC bool operator()(ESceneToolBase* x, ESceneToolBase* y) const
    {	return x->RenderPriority()<y->RenderPriority();	}
};

#define DEFINE_MSET_PRED(T,N,I,P)	typedef xr_multiset< T, P > N;		typedef N::iterator I;

DEFINE_MSET_PRED(ESceneToolBase*,SceneMToolsSet,SceneMToolsIt,tools_rp_pred);
DEFINE_MSET_PRED(ESceneCustomOTool*,SceneOToolsSet,SceneOToolsIt,tools_rp_pred);

void EScene::Render(const Fmatrix& camera)
{
	if (!valid())
	{
		return;
	}

	// extract and sort object tools
	SceneOToolsSet object_tools;
	SceneMToolsSet scene_tools;
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
				ESceneCustomOTool* mt = smart_cast<ESceneCustomOTool*>(t_it->second);
				if (mt)
				{
					object_tools.insert(mt);
				}
				scene_tools.insert(t_it->second);
			}
		}
	}

	// insert objects
	for (auto SceneTool : object_tools)
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
				float distSQ = EDevice->vCameraPosition.distance_to_sqr(Obj->FPosition);
				mapRenderObjects.insertInAnyWay(distSQ, Obj);
			}
		}
	}

	auto RENDER_SCENE_TOOLS = [scene_tools](int P, bool B)
	{
		SceneMToolsIt s_it = scene_tools.begin();
		SceneMToolsIt s_end = scene_tools.end();
		for (; s_it != s_end; s_it++)
		{
			EDevice->SetShader(B ? EDevice->m_SelectionShader : EDevice->m_WireShader);
			RCache.set_xform_world(Fidentity);
			(*s_it)->OnRenderRoot(P, B);
		}
	};

	RenderBuckets rb;
	RenderBucketsData = &rb;
	mapRenderObjects.traverseLR(collect_object);
	RenderBucketsData = nullptr;

	for (u32 P = 1; P <= 3; ++P)
	{
		// normal pass: near-to-far
		for (CCustomObject* o : rb.Normal[P])
		{
			o->Render((int)P, false);
		}

		RENDER_SCENE_TOOLS((int)P, false);
		FlushDU();

		// alpha (strict B2F) pass: far-to-near -> reverse the near-to-far bucket
		for (int i = (int)rb.Alpha[P].size() - 1; i >= 0; --i)
		{
			rb.Alpha[P][i]->Render((int)P, true);
		}

		RENDER_SCENE_TOOLS((int)P, true);
		FlushDU();
	}

	// render snap
	RenderSnapList();

	// clear
	mapRenderObjects.clear();


	SceneMToolsIt s_it = scene_tools.begin();
	SceneMToolsIt s_end = scene_tools.end();
	for (; s_it != s_end; s_it++)
	{
		(*s_it)->AfterRender();
	}
}