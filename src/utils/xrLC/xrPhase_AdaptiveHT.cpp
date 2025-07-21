#include "StdAfx.h"
#include "Build.h"

#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/light_point.h"
#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/xrFace.h"
#include "../xrLC_Light/face_smoth_flags.h"

#include "../../xrCDB/xrCDB.h"
#include "../xrForms/xrThread.h"

const	float	aht_max_edge	= c_SS_maxsize/2.5f;	// 2.0f;			// 2 m
 

bool	is_CCW	(int _1, int _2)
{
	if (0==_1 && 1==_2)	return true;
	if (1==_1 && 2==_2) return true;
	if (2==_1 && 0==_2)	return true;
	return	false;
}

// Iterate on edges - select longest
int		callback_edge_longest	( const Face* F)
{
	float	max_err				= -1;
	int		max_id				= -1;
	for (u32 e=0; e<3; e++)
	{
		Vertex					*V1,*V2;
		F->EdgeVerts			(e,&V1,&V2);
		float len				= V1->P.distance_to	(V2->P);	// len
		if (len<aht_max_edge)	continue;
		if (len>max_err)
		{
			max_err = len;
			max_id	= e;
		}
	}
	return	max_id;
}

xrCriticalSection csA;
int ThreadWorkID_Adaptive = 0;

class CPrecalcBaseHemiThread: 
public CThread
{
 	CDB::COLLIDER	DB;
	
public:
	CPrecalcBaseHemiThread(u32 ID ): CThread(ID)
	{
	}
virtual	void Execute()
	{
		DB.ray_options	(0);
		while (true)
		{
			csA.Enter();
			int ID = ThreadWorkID_Adaptive;
			if (ID >= lc_global_data()->g_vertices().size())
			{
				csA.Leave();
				break;
			}
			ThreadWorkID_Adaptive++;
			csA.Leave();

			base_color_c		vC;
		 
			vecVertex			&verts = lc_global_data()->g_vertices();
 			Vertex*		V		= verts[ID];

 			V->normalFromAdj	();

			u32 flags = LP_dont_rgb + LP_dont_sun;
			LightPoint			(&DB, lc_global_data()->RCAST_Model(), vC, V->P, V->N, pBuild->L_static(), flags,0);
			vC.mul				(0.5f);
			V->C._set			(vC);
		}
	}
};

CThreadManager	precalc_base_hemi; 
void CBuild::xrPhase_AdaptiveHT	()
{
	CDB::COLLIDER	DB;
	DB.ray_options	(0);

	if (!lc_global_data()->GetSkipTesselate())
	{
		Status("Tesselating...");
		for (u32 fit=0; fit<lc_global_data()->g_faces().size(); fit++)	{		// clear split flag from all faces + calculate normals
			lc_global_data()->g_faces()[fit]->flags.bSplitted		= false;
			lc_global_data()->g_faces()[fit]->flags.bLocked			= true;
			lc_global_data()->g_faces()[fit]->CalcNormal			();
		}
		u_Tesselate		(callback_edge_longest,0,0);		// tesselate
	}

	// Tesselate + calculate
	Status			("Precalculating...");
	{
		mem_Compact					();

		// Build model
		FPU::m64r					();
		BuildRapid					(FALSE);

		// Prepare
		FPU::m64r					();
		Status						("Precalculating : base hemisphere ...");
		mem_Compact					();
		Light_prepare				();
 
		ThreadWorkID_Adaptive = 0;
		for (u32 thID = 0; thID < CPU::ID.n_threads; thID++)
			precalc_base_hemi.start(new CPrecalcBaseHemiThread(thID));

		precalc_base_hemi.wait();
	}

	//////////////////////////////////////////////////////////////////////////
	Status				("Gathering lighting information...");
	u_SmoothVertColors	(5);
}
void CollectProblematicFaces(const Face &F, int max_id, xr_vector<Face*> & reult, Vertex** V1, Vertex** V2 )
{
	xr_vector<Face*>			&adjacent_vec = reult;
	adjacent_vec.reserve		(6*2*3);
	// now, we need to tesselate all faces which shares this 'problematic' edge
	// collect all this faces

	F.EdgeVerts		( max_id, V1, V2 );
	adjacent_vec.clear	();
	for ( u32 adj=0; adj<(*V1)->m_adjacents.size(); ++adj )
	{
		Face* A					= (*V1)->m_adjacents[adj];
		if ( A->flags.bSplitted )	
			continue;

		if ( A->VContains( *V2 ) )	
			adjacent_vec.push_back	( A );
	}

	std::sort			(adjacent_vec.begin(), adjacent_vec.end());
	adjacent_vec.erase	(std::unique(adjacent_vec.begin(),adjacent_vec.end()),adjacent_vec.end());
}

bool check_and_destroy_splited( u32 face_it )
{
	Face* F	= lc_global_data()->g_faces()[face_it];
	VERIFY( F );
	if (F->flags.bSplitted)	
	{
		if (!F->flags.bLocked)	
			lc_global_data()->destroy_face	(lc_global_data()->g_faces()[face_it]);
		return false;//continue;
	}
	return true;
}
bool do_tesselate_face( const Face &F, tesscb_estimator* cb_E, int &max_id )
{
	if (F.CalcArea()<EPS_L)	
		return false;//continue;
	max_id				= cb_E(&F);
	if		(max_id<0)		
		return false;//continue;	// nothing selected
	return true;
}

void	tessalate_faces( xr_vector<Face*> & faces, Vertex* V1, Vertex* V2,  tesscb_face* cb_F, tesscb_vertex* cb_V  )
{
		xr_vector<Face*> & adjacent_vec = faces;
		// create new vertex (lerp)
		Vertex*		V		= lc_global_data()->create_vertex();
		V->P.lerp			(V1->P, V2->P, .5f);

		// iterate on faces which share this 'problematic' edge
		for (u32 af_it=0; af_it<adjacent_vec.size(); ++af_it)
		{
			Face*	AF			= adjacent_vec[af_it];
			VERIFY				(false==AF->flags.bSplitted);
			AF->flags.bSplitted	= true;
			_TCF&	atc			= AF->tc.front();

			// indices & tc
			int id1				= AF->VIndex(V1);
			VERIFY				( id1>=0 && id1<=2 );
			int id2				= AF->VIndex(V2);
			VERIFY				( id2>=0 && id2<=2 );
			int idB				= 3-(id1+id2);
			VERIFY				( idB>=0 && idB<=2 );

			Fvector2			UV;
			UV.averageA			(atc.uv[id1],atc.uv[id2]);

			// Create F1 & F2
			Face* F1			= lc_global_data()->create_face();
			F1->flags.bSplitted	= false;
			F1->flags.bLocked	= false;
			F1->dwMaterial		= AF->dwMaterial;
			F1->dwMaterialGame	= AF->dwMaterialGame;
			Face* F2			= lc_global_data()->create_face();
			F2->flags.bSplitted	= false;
			F2->flags.bLocked	= false;
			F2->dwMaterial		= AF->dwMaterial;
			F2->dwMaterialGame	= AF->dwMaterialGame;


			set_backface( F1->sm_group, is_backface( AF->sm_group ) );
			set_backface( F2->sm_group, is_backface( AF->sm_group ) );

			if (is_CCW(id1,id2))	
			{
				bool id1_id2_soft = is_soft_edge( AF->sm_group, id1 );
				bool id2_idB_soft = is_soft_edge( AF->sm_group, id2 );
				bool idB_id1_soft = is_soft_edge( AF->sm_group, idB );
				
				// F1
				F1->SetVertices		( AF->v[idB],	AF->v[id1],		V );
				F1->AddChannel		( atc.uv[idB],	atc.uv[id1],	UV );

				set_soft_edge		( F1->sm_group, 0, idB_id1_soft );
				set_soft_edge		( F1->sm_group, 1, id1_id2_soft );
				set_soft_edge		( F1->sm_group, 2, true );

				// F2
				F2->SetVertices		(AF->v[idB],	V,				AF->v[id2]);
				F2->AddChannel		(atc.uv[idB],	UV,				atc.uv[id2]);

				set_soft_edge		( F2->sm_group, 0, true );
				set_soft_edge		( F2->sm_group, 1, id1_id2_soft );
				set_soft_edge		( F2->sm_group, 2, id2_idB_soft );

			} else {

				bool id1_id2_soft = is_soft_edge( AF->sm_group, id2 );
				bool id2_idB_soft = is_soft_edge( AF->sm_group, idB );
				bool idB_id1_soft = is_soft_edge( AF->sm_group, id1 );

				// F1
				F1->SetVertices		(AF->v[idB],	V,				AF->v[id1]);
				F1->AddChannel		(atc.uv[idB],	UV,				atc.uv[id1]);

				set_soft_edge		( F1->sm_group, 0, true );
				set_soft_edge		( F1->sm_group, 1, id1_id2_soft );
				set_soft_edge		( F1->sm_group, 2, idB_id1_soft );

				// F2
				F2->SetVertices		(AF->v[idB],	AF->v[id2],		V);
				F2->AddChannel		(atc.uv[idB],	atc.uv[id2],	UV);

				set_soft_edge		( F2->sm_group, 0, id2_idB_soft );
				set_soft_edge		( F2->sm_group, 1, id1_id2_soft );
				set_soft_edge		( F2->sm_group, 2, true );
			}

			// Normals and checkpoint
			F1->N		= AF->N;		if (cb_F)	cb_F(F1);
			F2->N		= AF->N;		if (cb_F)	cb_F(F2);
		}
		// calc vertex attributes
		{
			V->normalFromAdj		();
			if (cb_V)				cb_V	(V);
		}
}

void CBuild::u_Tesselate(tesscb_estimator* cb_E, tesscb_face* cb_F, tesscb_vertex* cb_V)
{
	// main process
	FPU::m64r					();
	Status						("Tesselating...");
	g_bUnregister				= false;

	u32		counter_create		= 0;
	u32 cnt_verts = (u32)lc_global_data()->g_vertices().size();
	//u32		cnt_faces			= g_faces.size();
	
	for (u32 I=0; I<lc_global_data()->g_faces().size(); ++I)
	{
		Face* F					= lc_global_data()->g_faces()[I];
		if (0==F)				
			continue;
		if( !check_and_destroy_splited( I ) )
			continue;

		Progress				(float(I)/float(lc_global_data()->g_faces().size()));
		int max_id = -1;
		if( !do_tesselate_face( *F, cb_E, max_id ) )
			continue;

		xr_vector<Face*>		adjacent_vec;
		Vertex					*V1,*V2;
		CollectProblematicFaces( *F, max_id, adjacent_vec, &V1, &V2 );
		++counter_create;
		if (0==(counter_create%10000))	
		{
			for (u32 I=0; I<lc_global_data()->g_vertices().size(); ++I)	
				if (lc_global_data()->g_vertices()[I]->m_adjacents.empty())	
					lc_global_data()->destroy_vertex	(lc_global_data()->g_vertices()[I]);

			Status				("Working: %d verts created, %d(now) / %d(was) ...",counter_create,lc_global_data()->g_vertices().size(),cnt_verts);
			xrLogger::FlushLog			();
		}

		tessalate_faces( adjacent_vec, V1, V2, cb_F, cb_V  );
	}

	// Cleanup
	for (u32 I=0; I<lc_global_data()->g_faces().size(); ++I)	
		if (0!=lc_global_data()->g_faces()[I] && lc_global_data()->g_faces()[I]->flags.bSplitted)	
			lc_global_data()->destroy_face	(lc_global_data()->g_faces()[I]);

	for (u32 I=0; I<lc_global_data()->g_vertices().size(); ++I)	
		if (lc_global_data()->g_vertices()[I]->m_adjacents.empty())				
			lc_global_data()->destroy_vertex	(lc_global_data()->g_vertices()[I]);

	lc_global_data()->g_faces().erase		(std::remove(lc_global_data()->g_faces().begin(),lc_global_data()->g_faces().end(),(Face*)0),lc_global_data()->g_faces().end());
	lc_global_data()->g_vertices().erase	(std::remove(lc_global_data()->g_vertices().begin(),lc_global_data()->g_vertices().end(),(Vertex*)0),lc_global_data()->g_vertices().end());
	g_bUnregister		= true;
}

void CBuild::u_SmoothVertColors(int count)
{
	for (int iteration=0; iteration<count; ++iteration)
	{
		// Gather
		xr_vector<base_color>	colors;
		colors.resize			(lc_global_data()->g_vertices().size());
		for (u32 it=0; it<lc_global_data()->g_vertices().size(); ++it)
		{
			// Circle
			xr_vector<Vertex*>	circle_vec;
			Vertex*		V		= lc_global_data()->g_vertices()[it];

			for (u32 fit=0; fit<V->m_adjacents.size(); ++fit)
			{
				Face*	F		= V->m_adjacents[fit];
				circle_vec.push_back(F->v[0]);
				circle_vec.push_back(F->v[1]);
				circle_vec.push_back(F->v[2]);
			}
			std::sort				(circle_vec.begin(),circle_vec.end());
			circle_vec.erase		(std::unique(circle_vec.begin(),circle_vec.end()),circle_vec.end());

			// Average
			base_color_c		avg,tmp;
			for (u32 cit=0; cit<circle_vec.size(); ++cit)
			{
				circle_vec[cit]->C._get	(tmp);
				avg.add					(tmp);
			}
			avg.scale			(circle_vec.size());
			colors[it]._set		(avg);
		}

		// Transfer
		for (u32 it=0; it<lc_global_data()->g_vertices().size(); ++it)
			lc_global_data()->g_vertices()[it]->C	= colors[it];
	}
}
