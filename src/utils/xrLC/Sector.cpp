// Sector.cpp: implementation of the CSector class.
//
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "Build.h"
#include "Sector.h"
#include "OGF_Face.h"
#include <execution>
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CSector::CSector(u32 ID)
{
	SelfID = ID;
	TreeRoot=0;
}

CSector::~CSector()
{

}
 
/* 
#include <../xrForms/CompilersUI.h>
extern CompilersMode gCompilerMode;

void CSector::BuildHierrarhy	()
{
	Fvector		scene_size;
	float		delimiter;
	BOOL		bAnyNode		= FALSE;

	// calc scene BB
	Fbox&		scene_bb		= pBuild->scene_bb;
	scene_bb.invalidate			();

	for (OGF_Base* Tree : g_tree)
	{
		Fbox& BoxBB = Tree->bbox;
		scene_bb.merge(BoxBB);
	}
 	scene_bb.grow(EPS_L);
 
	// 
	scene_bb.getsize(scene_size);
	delimiter = _max(scene_size.x, _max(scene_size.y, scene_size.z));
	delimiter *= 2;

	int		iLevel					= 2;
	float	SizeLimit				= c_SS_maxsize/4.f;
	if		(SizeLimit<4.f)			SizeLimit=4.f;

	// just very small level
	if (delimiter <= SizeLimit)
		delimiter *= 2; 

	int ProgressID = 0;
	struct GridKey
	{
		int x, y;

		bool operator==(const GridKey& other) const { return x == other.x && y == other.y; }

		struct Hash
		{
			std::size_t operator()(const GridKey& k) const
			{
				return std::hash<int>()(k.x) ^ (std::hash<int>()(k.y) << 1);
			}
		};
	};

	struct OGF_Data
	{
		OGF_Base* node;
		u32 ID;
		int cellX;
		int cellZ;
		GridKey key;
	};

	// Фикс гиганской сцены когда ловим Inf 64k макс  
	if (delimiter > 64 * 1024)
		delimiter = 64 * 1024;
	
	CTimer tGlobalCalculateBounds;
  	for (; SizeLimit<=delimiter; SizeLimit*=2)
	{
		ProgressID = 0;
		int iSize			= (int)g_tree.size();

		u32 GridSize = SizeLimit;
		xr_vector<OGF_Data> data;
		std::unordered_map<GridKey, xr_vector<OGF_Data>, GridKey::Hash> grid_map;

		bool use_zero = SizeLimit <= float(delimiter / 1.25);
   		for (u32 oID = 0; oID < g_tree.size(); oID++)
		{
			if (use_zero)
			{
				auto O = g_tree[oID];
				if (!O->bConnected && O->Sector == SelfID)
				{
					GridKey key = { 0, 0 };
					OGF_Data OData = { O, oID, 0, 0, key };
					data.push_back(OData);
					grid_map[key].push_back(OData);
				}
			}
			else
			{
				auto O = g_tree[oID];
				if (!O->bConnected && O->Sector == SelfID)
				{
					Fvector Center;
					O->bbox.getcenter(Center);
					int cell_x = static_cast<int>(std::floor(Center.x / GridSize));
					int cell_z = static_cast<int>(std::floor(Center.z / GridSize));
					GridKey key = { cell_x, cell_z };
					OGF_Data OData = { O, oID, cell_x, cell_z, key };
					data.push_back(OData);
					grid_map[key].push_back(OData);
				}
			}				
		}
 
  		u64 count_connected = 0;
   		for (auto& Ogf : data)
		{
			Progress(float(ProgressID) / float(data.size()));
			ProgressID++;
			AditionalData("Sz: %.0f iter: %u | conn: %u/%u", SizeLimit, count_connected, ProgressID, data.size());

			int I = Ogf.ID;
			if (g_tree[I]->bConnected)	
				continue;
			 
			OGF_Node* pNode					= new OGF_Node(iLevel,u16(SelfID));
			pNode->AddChield				(I);

  			GridKey selected_grid			= Ogf.key;
				 
 			for (;;)
			{
				auto ValidateMerging = [&](Fbox& bb_base, Fbox& bb, float& volume, float SLimit)
				{
					// Size
					Fbox	merge;
					merge.merge(bb_base, bb);
			
					Fvector sz;
					merge.getsize(sz);
					sz.add(EPS_L);
			
					if (sz.x > SLimit || sz.y > SLimit || sz.z > SLimit)
						return FALSE;
 
					// Volume
					volume = merge.getvolume();
					return TRUE;
				};
				 
				int		best_id = -1;
				float	best_volume = flt_max;
  			
				for (auto& FOgf : grid_map[selected_grid])
				{
					OGF_Base* candidate = g_tree[FOgf.ID];
					if (candidate->bConnected || candidate->Sector != SelfID)
						continue;

					float V;
					if (ValidateMerging(pNode->bbox, candidate->bbox, V, SizeLimit))
					{
						if (V < best_volume)
						{
							best_volume = V;
							best_id = FOgf.ID;
 						}
					}
				}
				 
				// Analyze
				if (best_id < 0)
					break;
  				
				pNode->AddChield(best_id);
				count_connected += 1;
			}
		 
   			if (pNode->chields.size()>1)	
			{
   				pNode->CalcBounds		(true);
  				g_tree.push_back		(pNode);
  				bAnyNode				= TRUE;
			}
			else
			{
				g_tree[I]->bConnected	= false;
				xr_delete				(pNode);
			}
  		}
	
		if (iSize != (int)g_tree.size())
			iLevel++;
	}
 
	TreeRoot = 0;
	if (bAnyNode)
	{
		TreeRoot = g_tree.back();
	}
	else 
	{
		for (u32 I=0; I<g_tree.size(); I++)
		{
			if (g_tree[I]->bConnected)		 continue;
			if (g_tree[I]->Sector != SelfID) continue;
			R_ASSERT	(0==TreeRoot);
			TreeRoot	= g_tree[I];
		}
	}
	if (0==TreeRoot)
		clMsg("Can't build hierrarhy for sector #%d",SelfID);
}

void CSector::Validate()
{
	std::sort(Portals.begin(),Portals.end());
	R_ASSERT(std::unique(Portals.begin(),Portals.end())==Portals.end());
	R_ASSERT(TreeRoot);
	R_ASSERT(TreeRoot->Sector == SelfID);
}

void CSector::Save(IWriter &fs)
{
	// Root
	xr_vector<OGF_Base *>::iterator F = std::find(g_tree.begin(),g_tree.end(),TreeRoot);
	R_ASSERT(F!=g_tree.end());
	u32 ID = u32(F-g_tree.begin());
	fs.w_chunk(fsP_Root,&ID,sizeof(u32));

	// Portals
	fs.w_chunk(fsP_Portals,&*Portals.begin(), (u32)Portals.size()*sizeof(u16));
}
*/


IC BOOL	ValidateMerge(Fbox& bb_base, Fbox& bb, float& volume, float SLimit)
{
	// Size
	Fbox	merge;	merge.merge(bb_base, bb);
	Fvector sz;		merge.getsize(sz);	sz.add(EPS_L);
	if (sz.x > SLimit)		return FALSE;	// Don't exceed limits (4/3 GEOM)
	if (sz.y > SLimit)		return FALSE;
	if (sz.z > SLimit)		return FALSE;

	// Volume
	volume = merge.getvolume();

	// OK
	return TRUE;
}

void CSector::BuildHierrarhy()
{
	Fvector		scene_size;
	float		delimiter;
	BOOL		bAnyNode = FALSE;

	// calc scene BB
	Fbox& scene_bb = pBuild->scene_bb;
	scene_bb.invalidate();
	for (int I = 0; I < s32(g_tree.size()); I++)
		scene_bb.merge(g_tree[I]->bbox);
	scene_bb.grow(EPS_L);

	// 
	scene_bb.getsize(scene_size);
	delimiter = _max(scene_size.x, _max(scene_size.y, scene_size.z));
	delimiter *= 2;

	int		iLevel = 2;
	float	SizeLimit = c_SS_maxsize / 4.f;
	if (SizeLimit < 4.f)			SizeLimit = 4.f;
	if (delimiter <= SizeLimit)	delimiter *= 2;		// just very small level
 
	for (; SizeLimit <= delimiter; SizeLimit *= 2)
	{
		// Собираем кандидатов только этого сектора
		std::vector<int> candidates;
		for (int idx = 0; idx < g_tree.size(); ++idx) {
			if (!g_tree[idx]->bConnected && g_tree[idx]->Sector == SelfID)
				candidates.push_back(idx);
		}

		// Сортируем по центру (для быстрой фильтрации по X)
		std::sort(std::execution::par, candidates.begin(), candidates.end(), [&](int a, int b) 
		{
			Fvector C1, C2;
			g_tree[a]->bbox.getcenter(C1);
			g_tree[b]->bbox.getcenter(C2);
			return C1.x < C2.x;
		});

		std::vector<OGF_Node*> new_nodes;

		AditionalData("Process : %.0f / %.0f | candidates: %u", SizeLimit, delimiter, candidates.size());

		for (int id : candidates)
		{
			if (g_tree[id]->bConnected) continue;

			OGF_Node* pNode = new OGF_Node(iLevel, u16(SelfID));
			pNode->AddChield(id);

			for (;;) {
				int best_id = -1;
				float best_volume = flt_max;

				// Поиск только рядом по X
				Fvector Center;
				g_tree[id]->bbox.getcenter(Center);
				const float cx = Center.x;
				for (int cand : candidates) {
					if (g_tree[cand]->bConnected) continue;
					Fvector c2;
					g_tree[cand]->bbox.getcenter(c2);
					if (fabsf(c2.x - cx) > SizeLimit)
						continue;

					float V;
					if (ValidateMerge(pNode->bbox, g_tree[cand]->bbox, V, SizeLimit) && V < best_volume) {
						best_volume = V;
						best_id = cand;
					}
				}

				if (best_id < 0) break;
				pNode->AddChield(best_id);
			}

			if (pNode->chields.size() > 1) {
				pNode->CalcBounds();
				new_nodes.push_back(pNode);
				bAnyNode = TRUE;
			}
			else {
				g_tree[id]->bConnected = false;
				xr_delete(pNode);
			}
		}

		if (!new_nodes.empty()) {
			g_tree.insert(g_tree.end(), new_nodes.begin(), new_nodes.end());
			iLevel++;
		}
	}


	TreeRoot = 0;
	if (bAnyNode) TreeRoot = g_tree.back();
	else {
		for (u32 I = 0; I < g_tree.size(); I++)
		{
			if (g_tree[I]->bConnected)		 continue;
			if (g_tree[I]->Sector != SelfID) continue;
			R_ASSERT(0 == TreeRoot);
			TreeRoot = g_tree[I];
		}
	}
	if (0 == TreeRoot) {
		clMsg("Can't build hierrarhy for sector #%d", SelfID);
	}
}

void CSector::Validate()
{
	std::sort(Portals.begin(), Portals.end());
	R_ASSERT(std::unique(Portals.begin(), Portals.end()) == Portals.end());
	R_ASSERT(TreeRoot);
	R_ASSERT(TreeRoot->Sector == SelfID);
}

void CSector::Save(IWriter& fs)
{
	// Root
	xr_vector<OGF_Base*>::iterator F = std::find(g_tree.begin(), g_tree.end(), TreeRoot);
	R_ASSERT(F != g_tree.end());
	u32 ID = u32(F - g_tree.begin());
	fs.w_chunk(fsP_Root, &ID, sizeof(u32));

	// Portals
	fs.w_chunk(fsP_Portals, &*Portals.begin(), (u32)Portals.size() * sizeof(u16));
}