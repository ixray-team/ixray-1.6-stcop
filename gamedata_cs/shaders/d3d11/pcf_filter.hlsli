/*
The MIT License (MIT)

Copyright (c) 2016 MJP

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#ifndef PCF_FILTER_H

//http://the-witness.net/news/2013/09/shadow-mapping-summary-part-1/
//https://github.com/TheRealMJP

float pcf_3x3(Texture2DArray<float> shadow_tex, SamplerComparisonState shadow_comp_sampler, float3 tc, float2 shadow_res, float bias, int index)
{
	tc.z -= bias;

	float2 uv = tc.xy * shadow_res.x;

    float2 base_uv = floor(uv.xy + 0.5);
    float2 st = (uv.xy + 0.5 - base_uv.xy);

    base_uv -= float2(0.5, 0.5);
    base_uv *= shadow_res.y;
	
	float uw0 = (3.0 - 2.0 * st.x);
	float uw1 = (1.0 + 2.0 * st.x);

	float u0 = (2.0 - st.x) / uw0 - 1.0;
	float u1 = st.x / uw1 + 1.0;

	float vw0 = (3.0 - 2.0 * st.y);
	float vw1 = (1.0 + 2.0 * st.y);

	float v0 = (2.0 - st.y) / vw0 - 1.0;
	float v1 = st.y / vw1 + 1.0;

	float sum = 0.0;
	sum += uw0 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v0) * shadow_res.y, index), tc.z);
	sum += uw1 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v0) * shadow_res.y, index), tc.z);
	sum += uw0 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v1) * shadow_res.y, index), tc.z);
	sum += uw1 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v1) * shadow_res.y, index), tc.z);

	return sum / 16.0;	
}

float pcf_5x5(Texture2DArray<float> shadow_tex, SamplerComparisonState shadow_comp_sampler, float3 tc, float2 shadow_res, float bias, int index)
{
	tc.z -= bias;

	float2 uv = tc.xy * shadow_res.x;

    float2 base_uv = floor(uv.xy + 0.5);
    float2 st = (uv.xy + 0.5 - base_uv.xy);

    base_uv -= float2(0.5, 0.5);
    base_uv *= shadow_res.y;

	float uw0 = (4.0 - 3.0 * st.x);
	float uw1 = 7.0;
	float uw2 = (1.0 + 3.0 * st.x);

	float u0 = (3.0 - 2.0 * st.x) / uw0 - 2;
	float u1 = (3.0 + st.x) / uw1;
	float u2 = st.x / uw2 + 2.0;

	float vw0 = (4.0 - 3.0 * st.y);
	float vw1 = 7.0;
	float vw2 = (1.0 + 3.0 * st.y);

	float v0 = (3.0 - 2.0 * st.y) / vw0 - 2.0;
	float v1 = (3.0 + st.y) / vw1;
	float v2 = st.y / vw2 + 2.0;

	float sum = 0.0;
	sum += uw0 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v0) * shadow_res.y, index), tc.z);
	sum += uw1 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v0) * shadow_res.y, index), tc.z);
	sum += uw2 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u2, v0) * shadow_res.y, index), tc.z);

	sum += uw0 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v1) * shadow_res.y, index), tc.z);
	sum += uw1 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v1) * shadow_res.y, index), tc.z);
	sum += uw2 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u2, v1) * shadow_res.y, index), tc.z);

	sum += uw0 * vw2 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v2) * shadow_res.y, index), tc.z);
	sum += uw1 * vw2 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v2) * shadow_res.y, index), tc.z);
	sum += uw2 * vw2 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u2, v2) * shadow_res.y, index), tc.z);

	return sum / 144.0;
}

float pcf_7x7(Texture2DArray<float> shadow_tex, SamplerComparisonState shadow_comp_sampler, float3 tc, float2 shadow_res, float bias, int index)
{
	tc.z -= bias;

	float2 uv = tc.xy * shadow_res.x;

    float2 base_uv = floor(uv.xy + 0.5);
    float2 st = (uv.xy + 0.5 - base_uv.xy);

    base_uv -= float2(0.5, 0.5);
    base_uv *= shadow_res.y;

	float uw0 = (5.0 * st.x - 6.0);
	float uw1 = (11.0 * st.x - 28.0);
	float uw2 = -(11.0 * st.x + 17.0);
	float uw3 = -(5.0 * st.x + 1.0);

	float u0 = (4.0 * st.x - 5.0) / uw0 - 3.0;
	float u1 = (4.0 * st.x - 16.0) / uw1 - 1.0;
	float u2 = -(7.0 * st.x + 5.0) / uw2 + 1.0;
	float u3 = -st.x / uw3 + 3.0;

	float vw0 = (5.0 * st.y - 6.0);
	float vw1 = (11.0 * st.y - 28.0);
	float vw2 = -(11.0 * st.y + 17.0);
	float vw3 = -(5.0 * st.y + 1.0);

	float v0 = (4.0 * st.y - 5.0) / vw0 - 3.0;
	float v1 = (4.0 * st.y - 16.0) / vw1 - 1.0;
	float v2 = -(7.0 * st.y + 5.0) / vw2 + 1.0;
	float v3 = -st.y / vw3 + 3.0;

	float sum = 0.0;
	sum += uw0 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v0) * shadow_res.y, index), tc.z);
	sum += uw1 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v0) * shadow_res.y, index), tc.z);
	sum += uw2 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u2, v0) * shadow_res.y, index), tc.z);
	sum += uw3 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u3, v0) * shadow_res.y, index), tc.z);

	sum += uw0 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v1) * shadow_res.y, index), tc.z);
	sum += uw1 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v1) * shadow_res.y, index), tc.z);
	sum += uw2 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u2, v1) * shadow_res.y, index), tc.z);
	sum += uw3 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u3, v1) * shadow_res.y, index), tc.z);

	sum += uw0 * vw2 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v2) * shadow_res.y, index), tc.z);
	sum += uw1 * vw2 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v2) * shadow_res.y, index), tc.z);
	sum += uw2 * vw2 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u2, v2) * shadow_res.y, index), tc.z);
	sum += uw3 * vw2 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u3, v2) * shadow_res.y, index), tc.z);

	sum += uw0 * vw3 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v3) * shadow_res.y, index), tc.z);
	sum += uw1 * vw3 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v3) * shadow_res.y, index), tc.z);
	sum += uw2 * vw3 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u2, v3) * shadow_res.y, index), tc.z);
	sum += uw3 * vw3 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u3, v3) * shadow_res.y, index), tc.z);

	return sum / 2704.0;
}

//LVutner: Duplicate, for Tex2D. We use it for local lights
float pcf_3x3(Texture2D<float> shadow_tex, SamplerComparisonState shadow_comp_sampler, float3 tc, float2 shadow_res, float bias)
{
	tc.z -= bias;

	float2 uv = tc.xy * shadow_res.x;

    float2 base_uv = floor(uv.xy + 0.5);
    float2 st = (uv.xy + 0.5 - base_uv.xy);

    base_uv -= float2(0.5, 0.5);
    base_uv *= shadow_res.y;
	
	float uw0 = (3.0 - 2.0 * st.x);
	float uw1 = (1.0 + 2.0 * st.x);

	float u0 = (2.0 - st.x) / uw0 - 1.0;
	float u1 = st.x / uw1 + 1.0;

	float vw0 = (3.0 - 2.0 * st.y);
	float vw1 = (1.0 + 2.0 * st.y);

	float v0 = (2.0 - st.y) / vw0 - 1.0;
	float v1 = st.y / vw1 + 1.0;

	float sum = 0.0;
	sum += uw0 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, base_uv + float2(u0, v0) * shadow_res.y, tc.z);
	sum += uw1 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, base_uv + float2(u1, v0) * shadow_res.y, tc.z);
	sum += uw0 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, base_uv + float2(u0, v1) * shadow_res.y, tc.z);
	sum += uw1 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, base_uv + float2(u1, v1) * shadow_res.y, tc.z);

	return sum / 16.0;	
}

#endif //PCF_FILTER_H