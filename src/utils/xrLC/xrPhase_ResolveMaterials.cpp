#include "StdAfx.h"
#include "Build.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"

extern void		Detach		(vecFace* S);

struct _counter
{
	u16	dwMaterial;
	u32	dwCount;
};


void	CBuild::xrPhase_ResolveMaterials()
{
	CTimer  tProcecss; tProcecss.Start();
	
 	// Count number of materials
 	// Calculating materials
	auto& faces = lc_global_data()->g_faces();
	std::unordered_map<u16, size_t> matToIndex;
 
	// Локальные хранилища для потоков -> потом сведём в общий map
	concurrency::combinable<std::unordered_map<u16, u32>> localCounts;

 	xr_parallel_foreach(faces.begin(), faces.end(), [&](Face* F)
		{
			localCounts.local()[F->dwMaterial] += 1;
		});

	// Слияние локальных карт в глобальную
	std::unordered_map<u16, u32> globalCounts;
	localCounts.combine_each([&](const std::unordered_map<u16, u32>& lm)
		{
			for (const auto& kv : lm)
				globalCounts[kv.first] += kv.second;
		});


	// ======================================================
	// 2) Вектор счётчиков + карта material -> index (SC)
	// ======================================================
	xr_vector<_counter> count;
	count.reserve(globalCounts.size());
	matToIndex.reserve(globalCounts.size());

	size_t idx = 0;
	for (const auto& kv : globalCounts)
	{
		const u16 mat = kv.first;
		const u32 cnt = kv.second;
		count.push_back(_counter{ mat, cnt });
		matToIndex[mat] = idx++;
	}

	// Performing Subdivs
	concurrency::concurrent_vector<concurrency::concurrent_vector<Face*>> bins;
	bins.reserve(count.size());
	bins.resize(count.size());
 	xr_parallel_foreach(faces.begin(), faces.end(), [&](Face* F)
		{
			if (!F->Shader().flags.bRendering) return;

			auto it = matToIndex.find(F->dwMaterial);
			if (it != matToIndex.end())
			{
				bins[it->second].push_back(F);
			}
		});
 
	// Переносим в итоговый g_XSplit
	g_XSplit.reserve(count.size());
	g_XSplit.resize(count.size());

	for (size_t i = 0; i < g_XSplit.size(); ++i)
	{
		// vecFace имеет конструктор от итераторов
		g_XSplit[i] = new vecFace(bins[i].begin(), bins[i].end());
	}
 
	// Старый код
 	{
		for (int SP = 0; SP<int(g_XSplit.size()); SP++)
		{
			if (g_XSplit[SP]->empty())
				xr_delete(g_XSplit[SP]);
		}
		g_XSplit.erase(std::remove(g_XSplit.begin(),g_XSplit.end(),(vecFace*) NULL),g_XSplit.end());
	}
   
 	for (auto F : g_XSplit)
 		Detach(F);
 
	clMsg				("Material %u subdivisions. %u ms", g_XSplit.size(), tProcecss.GetElapsed_ms());
}
