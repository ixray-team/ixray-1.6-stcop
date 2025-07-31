#include "StdAfx.h"

#include "Build.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"
#include "execution"

const int	 HDIM_X = 512;
const int	 HDIM_Y = 512;
const int	 HDIM_Z = 512;

const int    FLOOR_VALUE = 1;


//extern volatile u32	dwInvalidFaces;

IC bool				FaceEqual(Face& F1, Face& F2)
{
	// Test for 6 variations
	if ((F1.v[0]==F2.v[0]) && (F1.v[1]==F2.v[1]) && (F1.v[2]==F2.v[2])) return true;
	if ((F1.v[0]==F2.v[0]) && (F1.v[2]==F2.v[1]) && (F1.v[1]==F2.v[2])) return true;
	if ((F1.v[2]==F2.v[0]) && (F1.v[0]==F2.v[1]) && (F1.v[1]==F2.v[2])) return true;
	if ((F1.v[2]==F2.v[0]) && (F1.v[1]==F2.v[1]) && (F1.v[0]==F2.v[2])) return true;
	if ((F1.v[1]==F2.v[0]) && (F1.v[0]==F2.v[1]) && (F1.v[2]==F2.v[2])) return true;
	if ((F1.v[1]==F2.v[0]) && (F1.v[2]==F2.v[1]) && (F1.v[0]==F2.v[2])) return true;
	return false;
}

#include <unordered_map>

void CBuild::PreOptimize()
{
    if (lc_global_data()->GetSkipWeld())
        return;


    std::unordered_map<size_t, vecVertex> hashTable;
    Fvector VMmin, VMscale, scale;

    // Calculate offset, scale, epsilon
    Fbox bb = scene_bb;
    VMscale.set(bb.max.x - bb.min.x, bb.max.y - bb.min.y, bb.max.z - bb.min.z);
    VMmin.set(bb.min);
     
    scale.set(float(HDIM_X), float(HDIM_Y), float(HDIM_Z));
    scale.div(VMscale);
 
    u32 Vcount = lc_global_data()->g_vertices().size();
    u32 Fcount = lc_global_data()->g_faces().size();
     u32 Vremoved = 0;
    u32 Fremoved = 0;

    Status("Processing...");
    g_bUnregister = false;



    for (int it = 0; it < (int)lc_global_data()->g_vertices().size(); it++)
    {
        if (it >= (int)lc_global_data()->g_vertices().size()) 
            break;

        if (Vremoved % 100000 == 0)
        {
           /// clMsg("%u vertex removed. Size:  %u/%u", Vremoved, it, lc_global_data()->g_vertices().size());
        
            Progress(float ( it ) / float( lc_global_data()->g_vertices().size())  );
        }

        Vertex* pTest = lc_global_data()->g_vertices()[it];
        Fvector& V = pTest->P;
         
 
        u32 ix = iFloor( (V.x - VMmin.x) * scale.x);
        u32 iy = iFloor( (V.y - VMmin.y) * scale.y);
        u32 iz = iFloor( (V.z - VMmin.z) * scale.z);
 
        // Generate hash key
        size_t hashKey = std::hash<u32>()(ix) ^ std::hash<u32>()(iy) ^ std::hash<u32>()(iz);

        // Search for similar vertices
        auto itHash = hashTable.find(hashKey);
        if (itHash != hashTable.end())
        {
            Vertex* parsed = nullptr;
            for (auto& v : itHash->second)
            {
                if (v->similar(*pTest, g_params().m_weld_distance))
                {
                    parsed = v;
                    break;
                }
            }

            if (parsed)
            {
                while (!pTest->m_adjacents.empty())
                    pTest->m_adjacents.front()->VReplace(pTest, parsed);
                lc_global_data()->destroy_vertex(lc_global_data()->g_vertices()[it]);
                Vremoved++;
                continue;
            }
        }

        // Register new vertex
        hashTable[hashKey].push_back(pTest);
    }

	Status("Removing degenerated/duplicated faces...");
	g_bUnregister = false;
	for (u32 it = 0; it < lc_global_data()->g_faces().size(); it++)
	{
		R_ASSERT(it >= 0 && it < (int)lc_global_data()->g_faces().size());
		Face* F = lc_global_data()->g_faces()[it];
		if (F->isDegenerated()) {
			lc_global_data()->destroy_face(lc_global_data()->g_faces()[it]);
			Fremoved++;
		}
		else {
			// Check validity
			F->Verify();
		}
		Progress(float(it) / float(lc_global_data()->g_faces().size()));
	}

	if (InvalideFaces())
	{
		err_save();
		if (lc_global_data()->GetSkipInvalid()) 
        {
			clMsg("* Total %d invalid faces. Do something.", InvalideFaces());
		}
		else {
			Debug.fatal(DEBUG_INFO, "* FATAL: %d invalid faces. Compilation aborted", InvalideFaces());
		}
	}

	Status("Adjacency check...");
	g_bUnregister = false;

	for (u32 it = 0; it < lc_global_data()->g_vertices().size(); ++it)
	{
		if (lc_global_data()->g_vertices()[it] && (lc_global_data()->g_vertices()[it]->m_adjacents.empty()))
		{
			lc_global_data()->destroy_vertex(lc_global_data()->g_vertices()[it]);
			++Vremoved;
		}
	}

	Status("Cleanup...");
	lc_global_data()->g_vertices().erase(std::remove(lc_global_data()->g_vertices().begin(), lc_global_data()->g_vertices().end(), (Vertex*)0), lc_global_data()->g_vertices().end());
	lc_global_data()->g_faces().erase(std::remove(lc_global_data()->g_faces().begin(), lc_global_data()->g_faces().end(), (Face*)0), lc_global_data()->g_faces().end());
 
	mem_Compact();
	clMsg("%d vertices removed. (%d left)", Vcount - lc_global_data()->g_vertices().size(), lc_global_data()->g_vertices().size());
	clMsg("%d faces removed. (%d left)", Fcount - lc_global_data()->g_faces().size(), lc_global_data()->g_faces().size());
}

void CBuild::IsolateVertices	(BOOL bProgress)
{
	isolate_vertices<Vertex>( bProgress, lc_global_data()->g_vertices() );
}
