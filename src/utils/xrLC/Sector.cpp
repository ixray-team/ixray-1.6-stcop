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

	// clMsg("Scene Fbox min{%.2f,%.2f,%.2f}, max{%.2f,%.2f,%.2f}, Delimiter: %.3f",
	// 	VPUSH(scene_bb.min), VPUSH(scene_bb.max), delimiter);

	int		iLevel					= 2;
	float	SizeLimit				= c_SS_maxsize/4.f;
	if		(SizeLimit<4.f)			SizeLimit=4.f;

	// just very small level
	if (delimiter <= SizeLimit)
		delimiter *= 2;


	int ProgressID = 0;

	u64 ticks_find = 0;
	u64 ticks_bounds = 0;


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

	struct OGF_Data {
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
	u64 TotalMS = 0;
	
	#define GRIDING_SIZE 128

	for (; SizeLimit<=delimiter; SizeLimit*=2)
	{
		ProgressID = 0;
		int iSize			= (int)g_tree.size();
		xr_vector<OGF_Data> data;
		std::unordered_map<GridKey, xr_vector<OGF_Data>, GridKey::Hash> grid_map;
  		for (u32 oID = 0; oID < g_tree.size(); oID ++)
		{
			auto O = g_tree[oID];
			if (!O->bConnected && O->Sector == SelfID )
			{
				int cell_x = static_cast<int>(std::floor(O->bbox.min.x / GRIDING_SIZE));
				int cell_z = static_cast<int>(std::floor(O->bbox.min.z / GRIDING_SIZE));
				GridKey key = { cell_x, cell_z };
 				OGF_Data OData = { O, oID, cell_x, cell_z, key };
 				data.push_back(OData);
  				grid_map[key].push_back(OData);
			}
 		}
		
		bool use_grid  = SizeLimit <= GRIDING_SIZE ? true : false;	
		u64 count_connected = 0;
   		for (auto& Ogf : data)
		{
 			Progress( float( ProgressID ) / float(data.size()));
			ProgressID++; 

			AditionalData("Sz: %.0f finded: %u | conn: %u/%u", SizeLimit, count_connected, ProgressID, data.size());

 			int I = Ogf.ID;
			if (g_tree[I]->bConnected)	
				continue;
 
			OGF_Node* pNode					= new OGF_Node(iLevel,u16(SelfID));
			pNode->AddChield				(I);

  			GridKey selected_grid = Ogf.key;
				 
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

				if (use_grid)
				{
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
				}
				else
				{
  					for (auto& Ogf : data)
					{
						OGF_Base* candidate = Ogf.node;
						if (candidate->bConnected)			continue;
						if (candidate->Sector != SelfID)	continue;

						float V;
						if (ValidateMerging(pNode->bbox, candidate->bbox, V, SizeLimit))
						{
							if (V < best_volume) {
								best_volume = V;
								best_id = Ogf.ID;
							}
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
				tGlobalCalculateBounds.Start();
   				pNode->CalcBounds		(true);
				TotalMS+=tGlobalCalculateBounds.GetElapsed_ms();
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
	if (0==TreeRoot) {
		clMsg("Can't build hierrarhy for sector #%d",SelfID);
	}

	if (TotalMS > 2000)
		clMsg("Building Hierarhy Time: %u Ms", TotalMS);
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
