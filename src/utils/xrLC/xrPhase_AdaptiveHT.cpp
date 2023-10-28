#include "stdafx.h"
#include "build.h"

#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/light_point.h"
#include "../xrLC_Light/xrdeflector.h"
#include "../xrLC_Light/xrface.h"

#include "../../xrcdb/xrcdb.h"
#include "../../common/face_smoth_flags.h"
#include "../xrLC_Light/xrThread.h"

#include "../xrLC_Light/xrHardwareLight.h"

const	float	aht_max_edge = c_SS_maxsize / 2.5f;	// 2.0f;			// 2 m
//const	float	aht_min_edge	= .2f;					// 20 cm
//const	float	aht_min_err		= 16.f/255.f;			// ~10% error

bool	is_CCW(int _1, int _2) {
    if (0 == _1 && 1 == _2)	return true;
    if (1 == _1 && 2 == _2) return true;
    if (2 == _1 && 0 == _2)	return true;
    return	false;
}

// Iterate on edges - select longest
int		callback_edge_longest(const Face* F) {
    float	max_err = -1;
    int		max_id = -1;
    for (u32 e = 0; e < 3; e++)
    {
        Vertex* V1, * V2;
        F->EdgeVerts(e, &V1, &V2);
        float len = V1->P.distance_to(V2->P);	// len
        if (len < aht_max_edge)	continue;
        if (len > max_err)
        {
            max_err = len;
            max_id = e;
        }
    }
    return	max_id;
}

class CPrecalcBaseHemiThread :
    public CThread
{
    u32 _from, _to;
    CDB::COLLIDER	DB;

public:
    CPrecalcBaseHemiThread(u32 ID, u32 from, u32 to) : CThread(ID), _from(from), _to(to) {
        R_ASSERT(from != u32(-1));
        R_ASSERT(to != u32(-1));
        R_ASSERT(from < to);
        R_ASSERT(from >= 0);
        R_ASSERT(to > 0);
    }
    virtual	void Execute() {
        //DB.ray_options	(0);
        for (u32 vit = _from; vit < _to; vit++)
        {
            base_color_c		vC;
            R_ASSERT(vit != u32(-1));
            vecVertex& verts = lc_global_data()->g_vertices();
            R_ASSERT(vit >= 0);
            R_ASSERT(vit < verts.size());
            Vertex* V = verts[vit];

            R_ASSERT(V);
            //V->normalFromAdj	();
            LightPoint(&DB, lc_global_data()->RCAST_Model(), vC, V->P, V->N, pBuild->L_static(), LP_dont_rgb + LP_dont_sun, 0);
            vC.mul(0.5f);
            V->C._set(vC);
        }
    }
};

CThreadManager	precalc_base_hemi;

void CBuild::xrPhase_AdaptiveHT() {
    CDB::COLLIDER	DB;
    DB.ray_options(0);

    Status("Tesselating...");
    if (1)
    {
        for (u32 fit = 0; fit < lc_global_data()->g_faces().size(); fit++) {		// clear split flag from all faces + calculate normals
            lc_global_data()->g_faces()[fit]->flags.bSplitted = false;
            lc_global_data()->g_faces()[fit]->flags.bLocked = true;
            lc_global_data()->g_faces()[fit]->CalcNormal();
        }
        u_Tesselate(callback_edge_longest, 0, 0);		// tesselate
    }

    base_lighting& StaticLighting = lc_global_data()->L_static();

    // Tesselate + calculate
    Status("Precalculating...");
    {
        mem_Compact();

        // Build model
        BuildRapid(TRUE);

        // Prepare
        Status("Precalculating : base hemisphere ...");
        mem_Compact();
        Light_prepare();


        //calc approximate normals for vertices + base lighting
        for (u32 vit = 0; vit < lc_global_data()->g_vertices().size(); vit++)
        {
            //base_color_c		vC;
            Vertex* V = lc_global_data()->g_vertices()[vit];
            V->normalFromAdj();
            // 			LightPoint(&DB, lc_global_data()->RCAST_Model(), vC, V->P, V->N, pBuild->L_static(), LP_dont_rgb + LP_dont_sun, 0);
            // 			vC.mul(0.5f);
            // 			V->C._set(vC);
        }
        if (g_build_options.b_optix_accel)
        {
            Status("Setup OptiX scene ...");

            xrHardwareLight& LightCalculator = xrHardwareLight::Get();
            LightCalculator.LoadLevel(lc_global_data()->RCAST_Model(), lc_global_data()->L_static(), lc_global_data()->textures());

            Status("Calculate ...");

            LightCalculator.PerformAdaptiveHT();
        }
        else
        {
            u32	stride = u32(-1);

            u32 threads = u32(-1);
            u32 rest = u32(-1);
            //precalc_base_hemi.start(xr_new<CPrecalcBaseHemiThread>(0, 0, lc_global_data()->g_vertices().size()));
            get_intervals(8, lc_global_data()->g_vertices().size(), threads, stride, rest);
            for (u32 thID = 0; thID < threads; thID++)
                precalc_base_hemi.start(xr_new<CPrecalcBaseHemiThread>(thID, thID * stride, thID * stride + stride));
            if (rest > 0)
                precalc_base_hemi.start(xr_new<CPrecalcBaseHemiThread>(threads, threads * stride, threads * stride + rest));
            precalc_base_hemi.wait();
        }

        //precalc_base_hemi
    }

    //////////////////////////////////////////////////////////////////////////
    /*
    Status				("Adaptive tesselation...");
    {
    for (u32 fit=0; fit<g_faces.size(); fit++)	{					// clear split flag from all faces + calculate normals
    g_faces[fit]->flags.bSplitted	= false;
    g_faces[fit]->flags.bLocked		= true;
    }
    u_Tesselate		(callback_edge_error,0,callback_vertex_hemi);	// tesselate
    }
    */

    //////////////////////////////////////////////////////////////////////////
    Status("Gathering lighting information...");
    u_SmoothVertColors(5);

    //Status("Test Export as TestLevelWithLights.ply");

    //visualize a light sources

    //WARNING: For debug only, not allowed in production
    auto CreateCubeWithColor = [](vecVertex& VertexBuffer, vecFace& TrisBuffer, Fvector Position, base_color_c Color)
        {
            //jitter data
            static Fvector Point1{ -.01f, -.01f, .01f };
            static Fvector Point2{ .01f, -.01f, .01f };
            static Fvector Point3{ .01f, .01f, .01f };
            static Fvector Point4{ -.01f, .01f, .01f };

            static Fvector Point5{ -.01f, -.01f, -.01f };
            static Fvector Point6{ .01f, -.01f, -.01f };
            static Fvector Point7{ .01f, .01f, -.01f };
            static Fvector Point8{ -.01f, .01f, -.01f };

            //index data
            const int PolyIndex1[] = { 2, 4, 1 };
            const int PolyIndex2[] = { 8, 6, 5 };
            const int PolyIndex3[] = { 5, 2, 1 };
            const int PolyIndex4[] = { 6, 3, 2 };
            const int PolyIndex5[] = { 3, 8, 4 };
            const int PolyIndex6[] = { 1, 8, 5 };
            const int PolyIndex7[] = { 2, 3, 4 };
            const int PolyIndex8[] = { 8, 7, 6 };
            const int PolyIndex9[] = { 5, 6, 2 };
            const int PolyIndex10[] = { 6, 7, 3 };
            const int PolyIndex11[] = { 3, 7, 8 };
            const int PolyIndex12[] = { 1, 4, 8 };

            //vertexes...
            //that shit probably never be deleted. So please don not use this function in production

            Vertex* pVert1 = new Vertex();
            pVert1->P = Position;
            pVert1->P.add(Point1);
            pVert1->C._set(Color);
            Vertex* pVert2 = new Vertex();
            pVert2->P = Position;
            pVert2->P.add(Point2);
            pVert2->C._set(Color);
            Vertex* pVert3 = new Vertex();
            pVert3->P = Position;
            pVert3->P.add(Point3);
            pVert3->C._set(Color);
            Vertex* pVert4 = new Vertex();
            pVert4->P = Position;
            pVert4->P.add(Point4);
            pVert4->C._set(Color);

            Vertex* pVert5 = new Vertex();
            pVert5->P = Position;
            pVert5->P.add(Point5);
            pVert5->C._set(Color);
            Vertex* pVert6 = new Vertex();
            pVert6->P = Position;
            pVert6->P.add(Point6);
            pVert6->C._set(Color);
            Vertex* pVert7 = new Vertex();
            pVert7->P = Position;
            pVert7->P.add(Point7);
            pVert7->C._set(Color);
            Vertex* pVert8 = new Vertex();
            pVert8->P = Position;
            pVert8->P.add(Point8);
            pVert8->C._set(Color);

            //must be signed int, not unsigned
            int VertexCountBefore = VertexBuffer.size();

            VertexBuffer.push_back(pVert1);
            VertexBuffer.push_back(pVert2);
            VertexBuffer.push_back(pVert3);
            VertexBuffer.push_back(pVert4);

            VertexBuffer.push_back(pVert5);
            VertexBuffer.push_back(pVert6);
            VertexBuffer.push_back(pVert7);
            VertexBuffer.push_back(pVert8);

            //TRIS

            //face data comes from blender cube example
            //Fix indexes, blender start indexing from one, not zero
            VertexCountBefore--;

            //shit, that looks awful. MACRO TIME!

#define IMPLEMENT_FACE(FaceObject, Index) \
	FaceObject->SetVertex (0, VertexBuffer[VertexCountBefore + PolyIndex##Index[0]]); \
	FaceObject->SetVertex (1, VertexBuffer[VertexCountBefore + PolyIndex##Index[1]]); \
	FaceObject->SetVertex (2, VertexBuffer[VertexCountBefore + PolyIndex##Index[2]])

            Face* pFace1 = new Face();
            IMPLEMENT_FACE(pFace1, 1);

            Face* pFace2 = new Face();
            IMPLEMENT_FACE(pFace2, 2);

            Face* pFace3 = new Face();
            IMPLEMENT_FACE(pFace3, 3);

            Face* pFace4 = new Face();
            IMPLEMENT_FACE(pFace4, 4);

            Face* pFace5 = new Face();
            IMPLEMENT_FACE(pFace5, 5);

            Face* pFace6 = new Face();
            IMPLEMENT_FACE(pFace6, 6);

            Face* pFace7 = new Face();
            IMPLEMENT_FACE(pFace7, 7);

            Face* pFace8 = new Face();
            IMPLEMENT_FACE(pFace8, 8);

            Face* pFace9 = new Face();
            IMPLEMENT_FACE(pFace9, 9);

            Face* pFace10 = new Face();
            IMPLEMENT_FACE(pFace10, 10);

            Face* pFace11 = new Face();
            IMPLEMENT_FACE(pFace11, 11);

            Face* pFace12 = new Face();
            IMPLEMENT_FACE(pFace12, 12);

#undef IMPLEMENT_FACE

            TrisBuffer.push_back(pFace1);
            TrisBuffer.push_back(pFace2);
            TrisBuffer.push_back(pFace3);
            TrisBuffer.push_back(pFace4);

            TrisBuffer.push_back(pFace5);
            TrisBuffer.push_back(pFace6);
            TrisBuffer.push_back(pFace7);
            TrisBuffer.push_back(pFace8);

            TrisBuffer.push_back(pFace9);
            TrisBuffer.push_back(pFace10);
            TrisBuffer.push_back(pFace11);
            TrisBuffer.push_back(pFace12);

        };

    // 	vecVertex ExportModelVertexes = lc_global_data()->g_vertices();
    // 	vecFace ExportModelFaces = lc_global_data()->g_faces();
    // 
    // 
    // 	base_color_c LightColor;
    // 	LightColor.hemi = 1.0f;
    // 	for (R_Light& RGBLight : StaticLighting.rgb)
    // 	{
    // 		CreateCubeWithColor(ExportModelVertexes, ExportModelFaces, RGBLight.position, LightColor);
    // 	}
    // 
    // 	PLYExporter::ExportAsPly("TestLevelWithLights.ply", ExportModelVertexes, ExportModelFaces);

    //////////////////////////////////////////////////////////////////////////
    /*
    Status				("Exporting to SMF...");
    {
    string_path			fn;
    GSaveAsSMF			(strconcat(fn,pBuild->path,"hemi_source.smf"));
    }
    */
}
void CollectProblematicFaces(const Face& F, int max_id, xr_vector<Face*>& reult, Vertex** V1, Vertex** V2) {
    xr_vector<Face*>& adjacent_vec = reult;
    adjacent_vec.reserve(6 * 2 * 3);
    // now, we need to tesselate all faces which shares this 'problematic' edge
    // collect all this faces

    F.EdgeVerts(max_id, V1, V2);
    adjacent_vec.clear();
    for (u32 adj = 0; adj < (*V1)->m_adjacents.size(); ++adj)
    {
        Face* A = (*V1)->m_adjacents[adj];
        if (A->flags.bSplitted)
            continue;

        if (A->VContains(*V2))
            adjacent_vec.push_back(A);
    }

    std::sort(adjacent_vec.begin(), adjacent_vec.end());
    adjacent_vec.erase(std::unique(adjacent_vec.begin(), adjacent_vec.end()), adjacent_vec.end());
}

bool check_and_destroy_splited(u32 face_it) {
    Face* F = lc_global_data()->g_faces()[face_it];
    VERIFY(F);
    if (F->flags.bSplitted)
    {
        if (!F->flags.bLocked)
            lc_global_data()->destroy_face(lc_global_data()->g_faces()[face_it]);
        return false;//continue;
    }
    return true;
}
bool do_tesselate_face(const Face& F, tesscb_estimator* cb_E, int& max_id) {
    if (F.CalcArea() < EPS_L)
        return false;//continue;
    max_id = cb_E(&F);
    if (max_id < 0)
        return false;//continue;	// nothing selected
    return true;
}

void	tessalate_faces(xr_vector<Face*>& faces, Vertex* V1, Vertex* V2, tesscb_face* cb_F, tesscb_vertex* cb_V) {
    xr_vector<Face*>& adjacent_vec = faces;
    // create new vertex (lerp)
    Vertex* V = lc_global_data()->create_vertex();
    V->P.lerp(V1->P, V2->P, .5f);

    // iterate on faces which share this 'problematic' edge
    for (u32 af_it = 0; af_it < adjacent_vec.size(); ++af_it)
    {
        Face* AF = adjacent_vec[af_it];
        VERIFY(false == AF->flags.bSplitted);
        AF->flags.bSplitted = true;
        _TCF& atc = AF->tc.front();

        // indices & tc
        int id1 = AF->VIndex(V1);
        VERIFY(id1 >= 0 && id1 <= 2);
        int id2 = AF->VIndex(V2);
        VERIFY(id2 >= 0 && id2 <= 2);
        int idB = 3 - (id1 + id2);
        VERIFY(idB >= 0 && idB <= 2);

        Fvector2			UV;
        UV.averageA(atc.uv[id1], atc.uv[id2]);

        // Create F1 & F2
        Face* F1 = lc_global_data()->create_face();
        F1->flags.bSplitted = false;
        F1->flags.bLocked = false;
        F1->dwMaterial = AF->dwMaterial;
        F1->dwMaterialGame = AF->dwMaterialGame;
        Face* F2 = lc_global_data()->create_face();
        F2->flags.bSplitted = false;
        F2->flags.bLocked = false;
        F2->dwMaterial = AF->dwMaterial;
        F2->dwMaterialGame = AF->dwMaterialGame;


        set_backface(F1->sm_group, is_backface(AF->sm_group));
        set_backface(F2->sm_group, is_backface(AF->sm_group));

        if (is_CCW(id1, id2))
        {
            bool id1_id2_soft = is_soft_edge(AF->sm_group, id1);
            bool id2_idB_soft = is_soft_edge(AF->sm_group, id2);
            bool idB_id1_soft = is_soft_edge(AF->sm_group, idB);

            // F1
            F1->SetVertices(AF->v[idB], AF->v[id1], V);
            F1->AddChannel(atc.uv[idB], atc.uv[id1], UV);

            set_soft_edge(F1->sm_group, 0, idB_id1_soft);
            set_soft_edge(F1->sm_group, 1, id1_id2_soft);
            set_soft_edge(F1->sm_group, 2, true);

            // F2
            F2->SetVertices(AF->v[idB], V, AF->v[id2]);
            F2->AddChannel(atc.uv[idB], UV, atc.uv[id2]);

            set_soft_edge(F2->sm_group, 0, true);
            set_soft_edge(F2->sm_group, 1, id1_id2_soft);
            set_soft_edge(F2->sm_group, 2, id2_idB_soft);

        }
        else {

            bool id1_id2_soft = is_soft_edge(AF->sm_group, id2);
            bool id2_idB_soft = is_soft_edge(AF->sm_group, idB);
            bool idB_id1_soft = is_soft_edge(AF->sm_group, id1);

            // F1
            F1->SetVertices(AF->v[idB], V, AF->v[id1]);
            F1->AddChannel(atc.uv[idB], UV, atc.uv[id1]);

            set_soft_edge(F1->sm_group, 0, true);
            set_soft_edge(F1->sm_group, 1, id1_id2_soft);
            set_soft_edge(F1->sm_group, 2, idB_id1_soft);

            // F2
            F2->SetVertices(AF->v[idB], AF->v[id2], V);
            F2->AddChannel(atc.uv[idB], atc.uv[id2], UV);

            set_soft_edge(F2->sm_group, 0, id2_idB_soft);
            set_soft_edge(F2->sm_group, 1, id1_id2_soft);
            set_soft_edge(F2->sm_group, 2, true);
        }

        // Normals and checkpoint
        F1->N = AF->N;		if (cb_F)	cb_F(F1);
        F2->N = AF->N;		if (cb_F)	cb_F(F2);
        //smoth groups
        //F1->sm_group= AF->sm_group;
        //F2->sm_group= AF->sm_group;
        // don't destroy old face	(it can be used as occluder during ray-trace)
        // if (AF->bLocked)	continue;
        // FacePool.destroy	(g_faces[I]);
    }
    // calc vertex attributes
    {
        V->normalFromAdj();
        if (cb_V)				cb_V(V);
    }
}
void CBuild::u_Tesselate(tesscb_estimator* cb_E, tesscb_face* cb_F, tesscb_vertex* cb_V) {
    // main process
    Status("Tesselating...");
    g_bUnregister = false;

    u32		counter_create = 0;
    u32		cnt_verts = lc_global_data()->g_vertices().size();
    //u32		cnt_faces			= g_faces.size();

    for (u32 I = 0; I < lc_global_data()->g_faces().size(); ++I)
    {
        Face* F = lc_global_data()->g_faces()[I];
        if (0 == F)
            continue;
        if (!check_and_destroy_splited(I))
            continue;

        Progress(float(I) / float(lc_global_data()->g_faces().size()));
        int max_id = -1;
        if (!do_tesselate_face(*F, cb_E, max_id))
            continue;

        xr_vector<Face*>		adjacent_vec;
        Vertex* V1, * V2;
        CollectProblematicFaces(*F, max_id, adjacent_vec, &V1, &V2);
        ++counter_create;
        if (0 == (counter_create % 10000))
        {
            for (u32 I = 0; I < lc_global_data()->g_vertices().size(); ++I)
                if (lc_global_data()->g_vertices()[I]->m_adjacents.empty())
                    lc_global_data()->destroy_vertex(lc_global_data()->g_vertices()[I]);

            Status("Working: %d verts created, %d(now) / %d(was) ...", counter_create, lc_global_data()->g_vertices().size(), cnt_verts);
            FlushLog();
        }

        tessalate_faces(adjacent_vec, V1, V2, cb_F, cb_V);
    }

    // Cleanup
    for (u32 I = 0; I < lc_global_data()->g_faces().size(); ++I)
        if (0 != lc_global_data()->g_faces()[I] && lc_global_data()->g_faces()[I]->flags.bSplitted)
            lc_global_data()->destroy_face(lc_global_data()->g_faces()[I]);

    for (u32 I = 0; I < lc_global_data()->g_vertices().size(); ++I)
        if (lc_global_data()->g_vertices()[I]->m_adjacents.empty())
            lc_global_data()->destroy_vertex(lc_global_data()->g_vertices()[I]);

    lc_global_data()->g_faces().erase(std::remove(lc_global_data()->g_faces().begin(), lc_global_data()->g_faces().end(), (Face*)0), lc_global_data()->g_faces().end());
    lc_global_data()->g_vertices().erase(std::remove(lc_global_data()->g_vertices().begin(), lc_global_data()->g_vertices().end(), (Vertex*)0), lc_global_data()->g_vertices().end());
    g_bUnregister = true;
}

void CBuild::u_SmoothVertColors(int count) {
    for (int iteration = 0; iteration < count; ++iteration)
    {
        float ProgressSlice = 1.0f / float(count);
        // Gather
        xr_vector<base_color>	colors;
        u32 VertexesSize = lc_global_data()->g_vertices().size();
        colors.resize(VertexesSize);
        for (u32 it = 0; it < VertexesSize; ++it)
        {
            // Circle
            xr_vector<Vertex*>	circle_vec;
            Vertex* V = lc_global_data()->g_vertices()[it];

            for (u32 fit = 0; fit < V->m_adjacents.size(); ++fit)
            {
                Face* F = V->m_adjacents[fit];
                circle_vec.push_back(F->v[0]);
                circle_vec.push_back(F->v[1]);
                circle_vec.push_back(F->v[2]);
            }
            std::sort(circle_vec.begin(), circle_vec.end());
            circle_vec.erase(std::unique(circle_vec.begin(), circle_vec.end()), circle_vec.end());

            // Average
            base_color_c		avg, tmp;
            for (u32 cit = 0; cit < circle_vec.size(); ++cit)
            {
                circle_vec[cit]->C._get(tmp);
                avg.add(tmp);
            }
            avg.scale(circle_vec.size());
            colors[it]._set(avg);

            float CurrentStageProgress = float(it) / float(VertexesSize);
            Progress((ProgressSlice * float(iteration)) + (ProgressSlice * CurrentStageProgress));
        }

        // Transfer
        for (u32 it = 0; it < VertexesSize; ++it)
            lc_global_data()->g_vertices()[it]->C = colors[it];
    }
}