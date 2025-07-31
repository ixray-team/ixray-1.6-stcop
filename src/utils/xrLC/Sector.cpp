// Sector.cpp: implementation of the CSector class.
//
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "Build.h"
#include "Sector.h"
#include "OGF_Face.h"
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

IC BOOL	ValidateMerge	(Fbox& bb_base, Fbox& bb, float& volume, float SLimit)
{
	// Size
	Fbox	merge;	merge.merge		(bb_base,bb);
	Fvector sz;		merge.getsize	(sz);	sz.add	(EPS_L);
	if (sz.x>SLimit)		return FALSE;	// Don't exceed limits (4/3 GEOM)
	if (sz.y>SLimit)		return FALSE;
	if (sz.z>SLimit)		return FALSE;

	// Volume
	volume		= merge.getvolume	();

	// OK
	return TRUE;
}

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

	if (CheckInfinity_FBOX(scene_bb))
	{
		clMsg("Scene Fbox is corupted !!! What is Brocken: ");
		
		int index_tree = 0;
		for (auto tree : g_tree)
		{
			if ( CheckInfinity_FBOX( tree->bbox ) )
			{
				clMsg("Tree [%u] is dead min {%.2f,%.2f,%.2f} max {%.2f,%.2f,%.2f}", index_tree, VPUSH(tree->bbox.min), VPUSH(tree->bbox.max));

				xr_vector<Fvector> V; 
				tree->GetGeometry(V);

				int IDX_V = 0;
				for (auto Vert : V)
				{
					Msg("V[%u] is pos {%.2f,%.2f,%.2f}", IDX_V, VPUSH(Vert));
					IDX_V++;
				}
			}
			index_tree++;
		}
	}

	// 
	scene_bb.getsize(scene_size);
	delimiter = _max(scene_size.x, _max(scene_size.y, scene_size.z));
	delimiter *= 2;

	clMsg("Scene Fbox min{%.2f,%.2f,%.2f}, max{%.2f,%.2f,%.2f}, Delimiter: %.3f", VPUSH(scene_bb.min), VPUSH(scene_bb.max), delimiter);

	int		iLevel					= 2;
	float	SizeLimit				= c_SS_maxsize/4.f;
	if		(SizeLimit<4.f)			SizeLimit=4.f;

	// just very small level
	if (delimiter <= SizeLimit)
		delimiter *= 2;

	struct OGF_Data{ 
		OGF_Base* node;
		u32 ID; 
	};

	for (; SizeLimit<=delimiter; SizeLimit*=2)
	{
		int iSize			= (int)g_tree.size();

		xr_vector<OGF_Data> data;

		u32 IDx = 0;
		for (auto O : g_tree)
		{
			if (!O->bConnected && O->Sector == SelfID)
			{
				data.push_back(OGF_Data{O, IDx});
			}
			IDx++;
		}

		Status("Sector (%d/%d) noconn[%d]", (u32) SizeLimit, (u32) delimiter, data.size());
		  
		for (auto& Ogf : data)
		{
			int I = Ogf.ID;
			if (g_tree[I]->bConnected)		 continue;
			if (g_tree[I]->Sector != SelfID) continue;

			OGF_Node* pNode					= new OGF_Node(iLevel,u16(SelfID));
			pNode->AddChield				(I);

			// Find best object to connect with
			for (;;) 
			{
				// Find best object to connect with
				int		best_id		= -1;
				float	best_volume	= flt_max;
				 
				// Fast Finding By No Connected
				for (auto& O : data)
				{
					int J = O.ID;
					OGF_Base* candidate = g_tree[J];
					if ( candidate->bConnected)		
						continue;
					if ( candidate->Sector != SelfID)
						continue;
				
					float V;
					if (ValidateMerge(pNode->bbox,candidate->bbox,V,SizeLimit))
					{
						if (V<best_volume)	{
							best_volume		= V;
							best_id			= J;
						}
					}
				}

				// Analyze
				if (best_id<0)	
					break;
				pNode->AddChield	(best_id);
			}

			if (pNode->chields.size()>1)	
			{
				pNode->CalcBounds		();
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
	if (bAnyNode) TreeRoot = g_tree.back();
	else {
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
