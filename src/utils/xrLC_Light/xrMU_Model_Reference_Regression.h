#pragma once

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

	if (std::abs(l_tDenominator) > EPS_S)
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
			if (std::abs(A[it]) > EPS_L)
			{
				accum += (B[it] - bias) / A[it];
				_ok += 1;
			}
		T s = _ok ? (accum / _ok) : scale;

		//2. bias
		T b = bias;
		if (std::abs(scale) > EPS)
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

static void o_test(int iA, int iB, int count, base_color* A, base_color* B, float& C, float& D)
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