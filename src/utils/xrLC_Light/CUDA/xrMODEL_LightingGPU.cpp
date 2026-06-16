#include "stdafx.h"

#include "xrFace.h"
#include "xrMU_Model.h"
#include "xrMU_Model_Reference.h"
#include "xrLC_GlobalData.h"
#include "mu_model_light.h"
#include "../xrForms/CompilersUI.h"
#include "xrMU_Model_Reference_Regression.h"


// **** CUDA CODE  **** // 
#ifdef LCCUDA_BUILD
#include "xrCuda_PackedLights.h"
#include "light_point.h"

void RunMURefsGPU()
{
	GPUTaskinSystem.RestartALL();
	GPUTaskinSystem.ColorsMapType = eMumodel;
	GPUTaskinSystem.current_flags = LGetCurrentFlags();

	// Gathering
	CTimer tStats; tStats.Start();

	xr_atomic_u32 REF_INDEX = 0;
	xr_std_parallel_for([&REF_INDEX]()
	{
			while (true)
			{
				u32 IndexTask = REF_INDEX.fetch_add(1);
				if (IndexTask >= inlc_global_data()->mu_refs().size()) break;

				AditionalData("REF LIGHT: %u/%u", IndexTask, inlc_global_data()->mu_refs().size());
				auto MRef = inlc_global_data()->mu_refs()[IndexTask];
				MRef->calc_lighting_cuda_1();
			};

			// Завершаем накопленые данные
			GPUTaskinSystem.LightPointPacked_run_tasks();
	}, gCompilerMode.ThreadsPerWork);
	Msg("[MURefs] Elapsed For Compute: %u ms", tStats.GetElapsed_ms());

	// APPLY

	tStats.Start();
	REF_INDEX = 0;
	xr_std_parallel_for([&REF_INDEX]()
	{
		while (true)
		{
			u32 Index = REF_INDEX.fetch_add(1);
			if (Index >= inlc_global_data()->mu_refs().size()) break;

			inlc_global_data()->mu_refs()[Index]->calc_lighting_cuda_2();
			inlc_global_data()->mu_refs()[Index]->calc_lighting_cuda_3();

			AditionalData("REF LIGHT APPLY: %u/%u", Index, inlc_global_data()->mu_refs().size());
		}
	}, gCompilerMode.ThreadsPerWork);

	Msg("[MURefs] Elapsed For Apply Colors: %u ms", tStats.GetElapsed_ms());

	GPUTaskinSystem.RestartALL(); // Выгружаем все Это последнее освещение 
}

// Capture RAYS
void xrMU_Reference::calc_lighting_cuda_1()
{
	// calc pure rotation matrix
	Fmatrix Rxform, tmp, R;
	R.set(xform);
	R.translate_over(0, 0, 0);
	tmp.transpose(R);
	Rxform.invert(tmp);

	u32 SampleMAX = gCompilerMode.IsOverloadedSettings ? gCompilerMode.LC_JSampleMU : 6;
	const int n_samples = (g_params().m_quality == ebqDraft) ? 1 : SampleMAX;

	// Perform lighting
	for (u32 I = 0; I < model->m_vertices.size(); I++)
	{
		_vertex& V = *model->m_vertices[I];

		Fvector					vP, vN;
		xform.transform_tiny(vP, V.P);
		Rxform.transform_dir(vN, V.N);
		exact_normalize(vN);

		// multi-sample
		for (u32 sample = 0; sample < (u32)n_samples; sample++)
		{
			float				a = 0.2f * float(sample) / float(n_samples);
			Fvector				P, N;
			N.random_dir(vN, deg2rad(30.f));
			P.mad(vP, N, a);

			GPUTaskinSystem.LightPointPacked_add_task(I, this, P, N, 0);
		}
	}
}

// MU-MODEL Code
void xrMU_Reference::calc_lighting_cuda_2()
{
	// trans-map
	typedef	xr_multimap<float, xrMU_Model::v_vertices>	mapVert;
	typedef	mapVert::iterator				mapVertIt;
	mapVert									g_trans;

	// trans-epsilons
	const float eps = EPS_L;
	const float eps2 = 2.f * eps;

	// calc pure rotation matrix
	Fmatrix Rxform, tmp, R;
	R.set(xform);
	R.translate_over(0, 0, 0);
	tmp.transpose(R);
	Rxform.invert(tmp);

	u32 SampleMAX = gCompilerMode.IsOverloadedSettings ? gCompilerMode.LC_JSampleMU : 6;
	const int n_samples = (g_params().m_quality == ebqDraft) ? 1 : SampleMAX;

	xr_vector<_vertex>								  SafeVertices;
	SafeVertices.resize(model->m_vertices.size());
	for (size_t Iter = 0; Iter < model->m_vertices.size(); Iter++)
		SafeVertices[Iter] = *model->m_vertices[Iter];

	// Perform lighting
	for (u32 I = 0; I < SafeVertices.size(); I++)
	{
		_vertex& V = SafeVertices[I];
		base_color_c			vC = colors_cuda[I];

		// Get ambient factor
		float		v_amb = 0.f;
		float		v_trans = 0.f;
		for (u32 f = 0; f < V.m_adjacents.size(); f++)
		{
			_face* F = V.m_adjacents[f];
			v_amb += F->Shader().vert_ambient;
			v_trans += F->Shader().vert_translucency;
		}
		v_amb /= float(V.m_adjacents.size());
		v_trans /= float(V.m_adjacents.size());
		float v_inv = 1.f - v_amb;

		vC.scale(n_samples);
		vC._tmp_ = v_trans;
		vC.hemi += v_amb;


		V.C._set(vC);

		// Search
		const float key = V.P.x;
		mapVertIt	it = g_trans.lower_bound(key);
		mapVertIt	it2 = it;

		// Decrement to the start and inc to end
		while (it != g_trans.begin() && ((it->first + eps2) > key)) it--;
		while (it2 != g_trans.end() && ((it2->first - eps2) < key)) it2++;
		if (it2 != g_trans.end())	it2++;

		// Search
		bool found = false;
		for (; it != it2; it++)
		{
			xrMU_Model::v_vertices& VL = it->second;
			_vertex* Front = VL.front();
			R_ASSERT(Front);
			if (Front->P.similar(V.P, eps))
			{
				found = true;
				VL.push_back(&V);
			}
		}

		// Register
		if (!found)
		{
			mapVertIt	ins = g_trans.insert(std::make_pair(key, xrMU_Model::v_vertices()));
			ins->second.reserve(32);
			ins->second.push_back(&V);
		}
	}

	// Process all groups
	for (auto& map : g_trans)
	{
		// Unique
		xrMU_Model::v_vertices& VL = map.second;
		std::sort(VL.begin(), VL.end());
		VL.erase(std::unique(VL.begin(), VL.end()), VL.end());

		// Calc summary color
		base_color_c	C;
		for (int v = 0; v<int(VL.size()); v++)
		{
			base_color_c	vC;
			VL[v]->C._get(vC);
			C.max(vC);
		}

		// Calculate final vertex color
		for (u32 v = 0; v<int(VL.size()); v++)
		{
			base_color_c		vC;
			VL[v]->C._get(vC);

			// trans-level
			float	level = vC._tmp_;

			// 
			base_color_c		R;
			R.lerp(vC, C, level);
			R.max(vC);
			R.mul(.5f);
			VL[v]->C._set(R);
		}
	}

	// Transfer colors to destination
	color.resize(SafeVertices.size());
	for (u32 I = 0; I < SafeVertices.size(); I++)
	{
		base_color	ptColor = SafeVertices[I].C;
		color[I] = ptColor;
	}

	colors_cuda.clear();

	// se7kills: ’от€бы пам€ть убрать из юза
	SafeVertices.clear();
	SafeVertices.shrink_to_fit();
}

// Ref Code
void xrMU_Reference::calc_lighting_cuda_3()
{
	R_ASSERT(color.size() == model->color.size());

	// A*C + D = B
	// build data
	{
		xr_vector<double> A;	A.resize(color.size());
		xr_vector<double> B;	B.resize(color.size());
		float* _s = (float*)&c_scale;
		float* _b = (float*)&c_bias;
		for (u32 i = 0; i < 5; i++) {
			for (u32 it = 0; it < color.size(); it++)
			{
				base_color_c		__A;	model->color[it]._get(__A);
				base_color_c		__B;	color[it]._get(__B);
				A[it] = (__A.hemi);
				B[it] = ((float*)&__B)[i];
			}
			vfComputeLinearRegression(A, B, _s[i], _b[i]);
		}

		for (u32 index = 0; index < 5; index++)
			o_test(4, index, (u32)color.size(), &model->color.front(), &color.front(), _s[index], _b[index]);
	}
}
#endif