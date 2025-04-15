#include "StdAfx.h"
#include "Build.h"
#include "../../xrCDB/xrCDB.h"
#include "../xrLC_Light/face_smoth_flags.h"
#include <meshoptimizer/meshoptimizer.h>

#define MAX_DECIMATE_ERROR 0.0005f
#define COMPACTNESS_RATIO  0.001f

void SaveAsSMF			(LPCSTR fname, CDB::CollectorPacked& CL)
{
	IWriter* W			= FS.w_open(fname);
	string256 tmp;
	// vertices
	for (u32 v_idx=0; v_idx<CL.getVS(); v_idx++){
		Fvector* v		= CL.getV()+v_idx;
		xr_sprintf			(tmp,"v %f %f %f",v->x,v->y,-v->z);
		W->w_string		(tmp);
	}
	// transfer faces
	for (u32 f_idx=0; f_idx<CL.getTS(); f_idx++){
		CDB::TRI& t		= CL.getT(f_idx);
		xr_sprintf			(tmp,"f %d %d %d",t.verts[0]+1,t.verts[2]+1,t.verts[1]+1);
		W->w_string		(tmp);
	}
	FS.w_close	(W);
}

struct face_props	{
	u16		material;
	u16		sector;
	u32		flags;
	Fvector	norm;
	void	set		(u16 mtl, u16 sect, const Fvector& n, u32 _flags){material=mtl;sector=sect;norm.set(n);flags =_flags;}
};

using FPVec = xr_vector<face_props>;
using FPVecIt = FPVec::iterator;

#if 1
bool SaveCForm = false;
void SimplifyCFORM(CDB::CollectorPacked& CL)
{
	// Подготовка данных исходной модели
	u32 base_verts_cnt = u32(CL.getVS());
	u32 base_faces_cnt = u32(CL.getTS());

	// Сохранение исходной модели (если требуется)
	const bool keep_temp_files = SaveCForm;
	if (keep_temp_files) 
	{
		string_path fn;
		SaveAsSMF(xr_strconcat(fn, pBuild->path, "cform_source.obj"), CL);
	}

	// Копирование вершин
	xr_vector<float> vertices;
	vertices.reserve(base_verts_cnt * 3);
	for (u32 v_idx = 0; v_idx < base_verts_cnt; v_idx++) 
	{
		Fvector* v = CL.getV() + v_idx;
		vertices.push_back(v->x);
		vertices.push_back(v->y);
		vertices.push_back(v->z);
	}

	// Копирование индексов
	xr_vector<unsigned int> indices;
	indices.reserve(base_faces_cnt * 3);
	for (u32 f_idx = 0; f_idx < base_faces_cnt; f_idx++) 
	{
		CDB::TRI& t = CL.getT(f_idx);
		indices.push_back(t.verts[0]);
		indices.push_back(t.verts[1]);
		indices.push_back(t.verts[2]);
	}

	// Оптимизация порядка индексов для кэш-эффективности
	meshopt_optimizeVertexCache(indices.data(), indices.data(), indices.size(), base_verts_cnt);

	// Упрощение модели с использованием MeshOptimizer
	float target_error = MAX_DECIMATE_ERROR; // Максимальная ошибка упрощения
	size_t target_index_count = indices.size() / 2; // Целевое количество индексов (примерно половина)

	xr_vector<unsigned int> simplified_indices(indices.size());
	size_t simplified_index_count = meshopt_simplify
	(
		simplified_indices.data(),
		indices.data(),
		indices.size(),
		&vertices[0],
		base_verts_cnt,
		sizeof(float) * 3,
		target_index_count,
		target_error
	);

	simplified_indices.resize(simplified_index_count);

	// Уплотнение данных (удаление неиспользуемых вершин)
	xr_vector<unsigned int> remap(base_verts_cnt);
	size_t simplified_vertex_count = meshopt_optimizeVertexFetchRemap
	(
		remap.data(),
		simplified_indices.data(),
		simplified_index_count,
		base_verts_cnt
	);

	xr_vector<float> simplified_vertices(simplified_vertex_count * 3);
	meshopt_remapVertexBuffer
	(
		simplified_vertices.data(),
		vertices.data(),
		base_verts_cnt,
		sizeof(float) * 3,
		remap.data()
	);

	meshopt_remapIndexBuffer
	(
		simplified_indices.data(),
		simplified_indices.data(),
		simplified_index_count,
		remap.data()
	);

	// Очистка старых данных
	CL.clear();

	// Перенос упрощённой модели в CDB
	for (size_t i = 0; i < simplified_indices.size(); i += 3)
	{
		Fvector v0 = *((Fvector*)&simplified_vertices[simplified_indices[i + 0] * 3]);
		Fvector v1 = *((Fvector*)&simplified_vertices[simplified_indices[i + 1] * 3]);
		Fvector v2 = *((Fvector*)&simplified_vertices[simplified_indices[i + 2] * 3]);

		CL.add_face(v0, v1, v2, 0, 0, 0); // Замените материал, сектор и флаги на нужные значения
	}

	// Сохранение оптимизированной модели (если требуется)
	if (keep_temp_files) 
	{
		string_path fn;
		SaveAsSMF(xr_strconcat(fn, pBuild->path, "cform_optimized.obj"), CL);
	}
}

#else

IC u32 common_edge_idx(const MxFace& base_f, u32 base_edge_idx, const MxFace& test_f)
{
	VERIFY(base_edge_idx < 3);
	MxVertexID bv0 = base_f[base_edge_idx];
	MxVertexID bv1 = base_f[(base_edge_idx + 1) % 3];
	if (bv0 > bv1)
		swap(bv0, bv1);

	for (u8 i = 0; i < 3; ++i)
	{
		MxVertexID tv0 = test_f[i];
		MxVertexID tv1 = test_f[(i + 1) % 3];
		if (tv0 > tv1)
			swap(tv0, tv1);
		if (bv0 == tv0 && bv1 == tv1)
			return i;
	}
	return u32(-1);
}
bool do_constrain(u32 base_edge_idx, u32 test_edg_idx, face_props& base_fprops, face_props& test_fprops)
{
	return (test_fprops.material != base_fprops.material) || (test_fprops.sector != base_fprops.sector)
		|| !do_connect_faces_by_faces_edge_flags(base_fprops.flags, test_fprops.flags, base_edge_idx, test_edg_idx);
}

void SimplifyCFORM(CDB::CollectorPacked& CL)
{
	FPVec FPs;

	u32 base_verts_cnt = u32(CL.getVS());
	u32 base_faces_cnt = u32(CL.getTS());

	// save source SMF
	bool keep_temp_files = !!strstr(Core.Params, "-save_cform_opt");
	if (keep_temp_files) {
		string_path			fn;
		SaveAsSMF(xr_strconcat(fn, pBuild->path, "cform_source.obj"), CL);
	}

	// prepare model
	MxStdModel* mdl = new MxStdModel(base_verts_cnt, base_faces_cnt);

	// transfer vertices
	for (u32 v_idx = 0; v_idx < base_verts_cnt; v_idx++) {
		Fvector* v = CL.getV() + v_idx;
		mdl->add_vertex(v->x, v->y, v->z);
	}
	// transfer faces
	FPs.resize(base_faces_cnt);
	for (u32 f_idx = 0; f_idx < base_faces_cnt; f_idx++) {
		CDB::TRI& t = CL.getT(f_idx);
		mdl->add_face(t.verts[0], t.verts[1], t.verts[2]);
		FPs[f_idx].set(t.material, t.sector, Fvector().mknormal(*(CL.getV() + t.verts[0]), *(CL.getV() + t.verts[1]), *(CL.getV() + t.verts[2])), CL.getfFlags(f_idx));
	}
	CL.clear();

	// create and initialize qslim
	MxEdgeQSlim* slim = new MxEdgeQSlim(mdl);
	slim->boundary_weight = 1000000.f;
	slim->compactness_ratio = COMPACTNESS_RATIO;
	slim->meshing_penalty = 1000000.f;
	slim->placement_policy = MX_PLACE_ENDPOINTS;//MX_PLACE_ENDPOINTS;//MX_PLACE_ENDORMID;//MX_PLACE_OPTIMAL;
	slim->weighting_policy = MX_WEIGHT_UNIFORM;//MX_WEIGHT_UNIFORM;//MX_WEIGHT_AREA;
	slim->initialize();

	// constraint material&sector vertex
	Ivector2 f_rm[3] = { {0,1}, {1,2}, {2,0} };
	for (u32 f_idx = 0; f_idx < slim->valid_faces; f_idx++) 
	{
		if (mdl->face_is_valid(f_idx)) 
		{
			MxFace& base_f = mdl->face(f_idx);
			for (u32 edge_idx = 0; edge_idx < 3; edge_idx++) 
			{
				int K;
				u32 I = f_rm[edge_idx].x;
				u32 J = f_rm[edge_idx].y;
				const MxFaceList& N0 = mdl->neighbors(base_f[I]);
				const MxFaceList& N1 = mdl->neighbors(base_f[J]);

				for (K = 0; K < N1.length(); K++) mdl->face_mark(N1[K], 0);
				for (K = 0; K < N0.length(); K++) mdl->face_mark(N0[K], 1);
				for (K = 0; K < N1.length(); K++) mdl->face_mark(N1[K], mdl->face_mark(N1[K]) + 1);

				const MxFaceList& N = (N0.size() < N1.size()) ? N0 : N1;
				face_props& base_t = FPs[f_idx];

				if (N.size()) 
				{
					u32 cnt_pos = 0, cnt_neg = 0;
					bool need_constraint = false;
					for (K = 0; K < N.length(); K++)
					{
						u32 fff = N[K];
						MxFace& cur_f = mdl->face(fff);
						unsigned char mk = mdl->face_mark(fff);
						if ((f_idx != N[K]) && (mdl->face_mark(N[K]) == 2)) 
						{
							face_props& cur_t = FPs[N[K]];
							u32 cur_edge_idx = common_edge_idx(base_f, edge_idx, cur_f);
							if (do_constrain(edge_idx, cur_edge_idx, base_t, cur_t)) {
								need_constraint = true;
								break;
							}
							float dot = base_t.norm.dotproduct(cur_t.norm);
							if (fsimilar(dot, -1.f, EPS))	cnt_neg++;
							else if (fsimilar(dot, 1.f, EPS))		cnt_pos++;
						}
					}

					if (need_constraint || ((0 == cnt_pos) && (1 == cnt_neg))) 
					{
						slim->constraint_manual(base_f[I], base_f[J], f_idx);
					}
				}
			}
		}
	}
	// collect edges
	slim->collect_edges();

	// decimate
	slim->decimate(0, MAX_DECIMATE_ERROR);
	mdl->compact_vertices();

	// rebuild CDB
	for (u32 f_idx = 0; f_idx < mdl->face_count(); f_idx++) 
	{
		if (mdl->face_is_valid(f_idx)) {
			MxFace& F = mdl->face(f_idx);
			face_props& FP = FPs[f_idx];
			CL.add_face(*((Fvector*)&mdl->vertex(F[0])),
				*((Fvector*)&mdl->vertex(F[1])),
				*((Fvector*)&mdl->vertex(F[2])),
				FP.material, FP.sector, FP.flags);
		}
	}

	// save source CDB
	if (keep_temp_files)
	{
		string_path			fn;
		SaveAsSMF(xr_strconcat(fn, pBuild->path, "cform_optimized.obj"), CL);
	}

	xr_delete(slim);
	xr_delete(mdl);
}
#endif