#include "StdAfx.h"
#include "Build.h"
#include "../../xrCDB/xrCDB.h"
#include "../xrLC_Light/face_smoth_flags.h"
#include <meshoptimizer/meshoptimizer.h>

#define MAX_DECIMATE_ERROR 0.0005f
#define COMPACTNESS_RATIO  0.001f

void SaveAsSMF(LPCSTR fname, CDB::CollectorPacked& CL)
{
	IWriter* W = FS.w_open(fname);
	string256 tmp;

	// vertices
	for (u32 v_idx = 0; v_idx < CL.getVS(); v_idx++) 
	{
		Fvector* v = CL.getV() + v_idx;
		xr_sprintf(tmp, "v %f %f %f", v->x, v->y, -v->z);
		W->w_string(tmp);
	}

	// transfer faces
	for (u32 f_idx = 0; f_idx < CL.getTS(); f_idx++) 
	{
		CDB::TRI& t = CL.getT(f_idx);
		xr_sprintf(tmp, "f %d %d %d", t.verts[0] + 1, t.verts[2] + 1, t.verts[1] + 1);
		W->w_string(tmp);
	}
	FS.w_close(W);
}

bool SaveCForm = false;

void SimplifyCFORM(CDB::CollectorPacked& CL)
{
#pragma todo(FX to ALL: Fix me)
#if 0
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
#endif
}