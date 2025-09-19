#include "stdafx.h"

#include "xrMU_Model_Reference.h"
#include "xrMU_Model.h"
#include "light_point.h"
#include "xrFace.h"
#include "xrLC_GlobalData.h"

#include "../xrForms/CompilersUI.h"

template<typename T, typename T2>
void vfComputeLinearRegression(xr_vector<T>& A, xr_vector<T>& B, T2& C, T2& D)
{
	u32 N = (u32)A.size();
	T sx = T(0), sy = T(0), sxy = T(0), sx2 = T(0), l_tDenominator;

	sx = std::accumulate(A.begin(), A.end(), sx);
	sy = std::accumulate(B.begin(), B.end(), sy);
	sxy = std::inner_product(A.begin(), A.end(), B.begin(), sxy);
	sx2 = std::inner_product(A.begin(), A.end(), A.begin(), sx2);

	l_tDenominator = T(N) * sx2 - sx * sx;

	C = T2(0);
	D = T2(0);

	if (_abs(l_tDenominator) > EPS_S)
	{
		C = T2((T(N) * sxy - sx * sy) / l_tDenominator);
	}

	if (N)
	{
		D = T2((sy - C * sx) / T(N));
	}
}

template <typename T, typename T2>
T simple_optimize(xr_vector<T>& A, xr_vector<T>& B, T2& _scale, T2& _bias)
{
	T accum;
	u32 it;

	T scale = _scale;
	T bias = _bias;
	T error = flt_max;
	T elements = T(A.size());
	u32 count = 0;

	for (;;)
	{
		count++;
		if (count > 128)
		{
			_scale = (T2)scale;
			_bias = (T2)bias;
			return error;
		}

		T old_scale = scale;
		T old_bias = bias;

		//1. scale
		u32 _ok = 0;
		for (accum = 0, it = 0; it < A.size(); it++)
			if (_abs(A[it]) > EPS_L)
			{
				accum += (B[it] - bias) / A[it];
				_ok += 1;
			}
		T s = _ok ? (accum / _ok) : scale;

		//2. bias
		T b = bias;
		if (_abs(scale) > EPS)
		{
			for (accum = 0, it = 0; it < A.size(); it++)
				accum += B[it] - A[it] / scale;
			b = accum / elements;
		}

		// mix
		T conv = 7;
		scale = ((conv - 1) * scale + s) / conv;
		bias = ((conv - 1) * bias + b) / conv;

		// error
		for (accum = 0, it = 0; it < A.size(); it++)
			accum += B[it] - (A[it] * scale + bias);
		T err = accum / elements;

		if (err < error)
		{
			// continue?
			error = err;
			if (error < EPS)
			{
				_scale = (T2)scale;
				_bias = (T2)bias;
				return error;
			}
		}
		else
		{
			// exit
			_scale = (T2)old_scale;
			_bias = (T2)old_bias;
			return error;
		}
	}
}

void o_test(int iA, int iB, int count, base_color* A, base_color* B, float& C, float& D)
{
	xr_vector<double>	_A, _B;
	_A.resize(count);
	_B.resize(count);
	for (int it = 0; it < count; it++)
	{
		base_color_c _a;	A[it]._get(_a);	float* f_a = (float*)&_a;
		base_color_c _b;	B[it]._get(_b);	float* f_b = (float*)&_b;
		_A[it] = f_a[iA];
		_B[it] = f_b[iB];
	}
	// C=1, D=0;
	simple_optimize(_A, _B, C, D);
}

void xrMU_Reference::calc_lighting()
{
	u32 flags =  (inlc_global_data()->b_nosun() ? LP_dont_sun : 0) | LP_DEFAULT;
	model->calc_lighting(color, xform, inlc_global_data()->RCAST_Model(), inlc_global_data()->L_static(), flags);

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

// **** CUDA CODE  **** // 
#ifdef LCCUDA_BUILD
#include "xrDeflectorLight_Packed.h"

// Capture RAYS
void xrMU_Reference::calc_lighting_cuda_1()
{
	u32 flags = (inlc_global_data()->b_nosun() ? LP_dont_sun : 0) | LP_DEFAULT;

	// calc pure rotation matrix
	Fmatrix Rxform, tmp, R;
	R.set(xform);
	R.translate_over(0, 0, 0);
	tmp.transpose(R);
	Rxform.invert(tmp);

 	u32 SampleMAX = lc_global_data()->GetOverrideSettings() ? lc_global_data()->GetJitterMU() : 6;
	const int n_samples = (g_params().m_quality == ebqDraft) ? 1 : SampleMAX;

	// Perform lighting
	for (u32 I = 0; I < model->m_vertices.size(); I++)
	{
		_vertex& V = *model->m_vertices[I];
 
		Fvector					vP, vN;
		xform.transform_tiny	(vP, V.P);
		Rxform.transform_dir	(vN, V.N);
		exact_normalize			(vN);

		// multi-sample
		for (u32 sample = 0; sample < (u32)n_samples; sample++)
		{
			float				a = 0.2f * float(sample) / float(n_samples);
			Fvector				P, N;
			N.random_dir(vN, deg2rad(30.f));
			P.mad(vP, N, a);
			GPUTaskinSystem.LightPointPacked_MODEL(this, I, P, N, flags, 0);
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

	u32 SampleMAX = lc_global_data()->GetOverrideSettings() ? lc_global_data()->GetJitterMU() : 6;
	const int n_samples = (g_params().m_quality == ebqDraft) ? 1 : SampleMAX;
 
	xr_vector<_vertex>								  SafeVertices;
	SafeVertices.resize(model->m_vertices.size());
	for (size_t Iter = 0; Iter < model->m_vertices.size(); Iter++)
		SafeVertices[Iter] = *model->m_vertices[Iter];

	// Perform lighting
	for (u32 I = 0; I < SafeVertices.size(); I++)
	{
		_vertex& V = SafeVertices[I];
		base_color_c			vC		= colors_cuda[I];
 
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
		BOOL found = FALSE;
		for (; it != it2; it++)
		{
			xrMU_Model::v_vertices& VL = it->second;
			_vertex* Front = VL.front();
			R_ASSERT(Front);
			if (Front->P.similar(V.P, eps))
			{
				found = TRUE;
				VL.push_back(&V);
			}
		}

		// Register
		if (!found) 
		{
			mapVertIt	ins = g_trans.insert(std::make_pair(key, xrMU_Model::v_vertices() ));
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
		Fvector		ptPos = SafeVertices[I].P;
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