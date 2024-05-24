#ifndef	common_functions_h_included
#define	common_functions_h_included

//	contrast function
float Contrast(float Input, float ContrastPower)
{
     //piecewise contrast function
     bool IsAbovefloat = Input > 0.5f ;
     float ToRaise = saturate(2.0f*(IsAbovefloat ? 1.0f-Input : Input));
     float Output = 0.5f*pow(ToRaise, ContrastPower); 
     Output = IsAbovefloat ? 1.0f-Output : Output;
     return Output;
}

float3 tonemap(float3 rgb, float scale)
{
	rgb = rgb * scale;

	const float fWhiteIntensity = 1.7f;
	const float fWhiteIntensitySQR = fWhiteIntensity * fWhiteIntensity;

	return rgb * (1.0f + rgb / fWhiteIntensitySQR) / (rgb + 1.0f);
}

// Функции генерации случайных чисел [0, 1]
// START

float Hash(float n) { return frac(sin(n) * 43758.5453123f); }
float Hash(float2 n) { return Hash(Hash(n.x) + n.y); }
float Hash(float3 n) { return Hash(Hash(dot(n.xy, float2(12.989, 78.233))) + n.z); }

float2 Hash22(float2 value) {
	return float2(
		Hash(dot(value, float2(12.989, 78.233))),
		Hash(dot(value, float2(39.346, 11.135)))
	);
}

float3 Hash23(float2 value) {
	return float3(
		Hash(dot(value, float2(12.989, 78.233))),
		Hash(dot(value, float2(39.346, 11.135))),
		Hash(dot(value, float2(73.156, 52.235)))
	);
}

float2 Hash32(float3 value) {
	return float2(
		Hash(dot(value, float3(12.989, 78.233, 123.134f))),
		Hash(dot(value, float3(39.346, 11.135, 543.142f)))
	);
}

float3 Hash33(float3 value) {
	return float3(
		Hash(dot(value, float3(12.989, 78.233, 123.134f))),
		Hash(dot(value, float3(39.346, 11.135, 543.142f))),
		Hash(dot(value, float3(73.156, 52.235, 143.425f)))
	);
}

// END

//Hashed Alpha Testing
//The implementation was taken from https://cwyman.org/papers/i3d17_hashedAlpha.pdf document by Chris Wyman and Morgan McGuire
float hashed_alpha_test(float3 position)
{
	//Find the discretized derivatives of our coordinates
	float maxDeriv = max(length(ddx(position.xyz)), length(ddy(position.xyz)));
	float pixScale = rcp(def_aref * maxDeriv); //Let's use def_aref as temporary pixel scale
	float pixScaleLog2 = log2(pixScale);

	//Find two nearest log-discretized noise scales
	float2 pixScales = float2(exp2(floor(pixScaleLog2)), exp2(ceil(pixScaleLog2)));

	//Compute alpha thresholds at our two noise scales
	float2 alpha = float2(Hash(floor(pixScales.x * position.xyz)), Hash(floor(pixScales.y * position.xyz)));

	//Factor to interpolate lerp with
	float lerpFactor = frac(log2(pixScale));

	//Interpolate alpha threshold from noise at two scales
	float x = lerp(alpha.x, alpha.y, lerpFactor);

	//Pass into CDF to compute uniformly distrib threshold
	float a = min(lerpFactor, 1.0 - lerpFactor);
	float3 cases;
	cases.x = x * x / (2.0 * a * (1.0 - a));
	cases.y = (x - 0.5 * a) / (1.0 - a);
	cases.z = 1.0 - ((1.0 - x) * (1.0 - x) / (2.0 * a * (1.0 - a)));

	//Find our final, uniformly distributed alpha threshold
	float thresh = (x < (1.0 - a)) ? ((x < a) ? cases.x : cases.y) : cases.z;

	//R1 sequence to animate our noise for TAA/FSR/DLSS
	//Todo: Check if player has enabled TAA/upscaling to enable anim
#ifdef USE_JITTER_FOR_TAA
	int jitter_phase = 16; //Seems okay
	thresh = frac(thresh + float(m_taa_jitter.w % jitter_phase) * 0.38196601125);
#endif

	//Clamp alpha
	return clamp(thresh, 1e-5, 1.0);
}

float4 combine_bloom( float3  low, float4 high)	
{
	return float4( low + high*high.a, 1.f);
}

float calc_fogging( float4 w_pos )      
{
	return dot(w_pos,fog_plane);         
}

float2 unpack_tc_base( float2 tc, float du, float dv )
{
		return (tc.xy + float2	(du,dv))*(32.f/32768.f); //!Increase from 32bit to 64bit floating point
}

float3 calc_sun_r1( float3 norm_w )    
{
	return L_sun_color*saturate(dot((norm_w),-L_sun_dir_w));                 
}

float3 calc_model_hemi_r1( float3 norm_w )    
{
 return max(0,norm_w.y)*L_hemi_color;
}

float3 calc_model_lq_lighting( float3 norm_w )    
{
	return L_material.x*calc_model_hemi_r1(norm_w) + L_ambient + L_material.y*calc_sun_r1(norm_w);
}

float3 	unpack_normal( float3 v )	{ return 2*v-1; }
float3 	unpack_bx2( float3 v )	{ return 2*v-1; }
float3 	unpack_bx4( float3 v )	{ return 4*v-2; } //!reduce the amount of stretching from 4*v-2 and increase precision
float2 	unpack_tc_lmap( float2 tc )	{ return tc*(1.f/32768.f);	} // [-1  .. +1 ] 
float4	unpack_color( float4 c ) { return c.bgra; }
float4	unpack_D3DCOLOR( float4 c ) { return c.bgra; }
float3	unpack_D3DCOLOR( float3 c ) { return c.bgr; }

float3   p_hemi( float2 tc )
{
//	float3	t_lmh = tex2D (s_hemi, tc);
//	float3	t_lmh = s_hemi.Sample( smp_rtlinear, tc);
//	return	dot(t_lmh,1.h/4.h);
	float4	t_lmh = s_hemi.Sample( smp_rtlinear, tc);
	return	t_lmh.a;
}

float   get_hemi( float4 lmh)
{
	return lmh.a;
}

float   get_sun( float4 lmh)
{
	return lmh.g;
}

float3	v_hemi(float3 n)
{
	return L_hemi_color*(.5f + .5f*n.y);                   
}

float3	v_sun(float3 n)                        	
{
	return L_sun_color*dot(n,-L_sun_dir_w);                
}

float3	calc_reflection( float3 pos_w, float3 norm_w )
{
    return reflect(normalize(pos_w-eye_position), norm_w);
}

#define USABLE_BIT_1                uint(0x00002000)
#define USABLE_BIT_2                uint(0x00004000)
#define USABLE_BIT_3                uint(0x00008000)
#define USABLE_BIT_4                uint(0x00010000)
#define USABLE_BIT_5                uint(0x00020000)
#define USABLE_BIT_6                uint(0x00040000)
#define USABLE_BIT_7                uint(0x00080000)
#define USABLE_BIT_8                uint(0x00100000)
#define USABLE_BIT_9                uint(0x00200000)
#define USABLE_BIT_10               uint(0x00400000)
#define USABLE_BIT_11               uint(0x00800000)   // At least two of those four bit flags must be mutually exclusive (i.e. all 4 bits must not be set together)
#define USABLE_BIT_12               uint(0x01000000)   // This is because setting 0x47800000 sets all 5 FP16 exponent bits to 1 which means infinity
#define USABLE_BIT_13               uint(0x02000000)   // This will be translated to a +/-MAX_FLOAT in the FP16 render target (0xFBFF/0x7BFF), overwriting the 
#define USABLE_BIT_14               uint(0x04000000)   // mantissa bits where other bit flags are stored.
#define USABLE_BIT_15               uint(0x80000000)
#define MUST_BE_SET                 uint(0x40000000)   // This flag *must* be stored in the floating-point representation of the bit flag to store

/*
float2 gbuf_pack_normal( float3 norm )
{
   float2 res;

   res = 0.5 * ( norm.xy + float2( 1, 1 ) ) ;
   res.x *= ( norm.z < 0 ? -1.0 : 1.0 );

   return res;
}

float3 gbuf_unpack_normal( float2 norm )
{
   float3 res;

   res.xy = ( 2.0 * abs( norm ) ) - float2(1,1);

   res.z = ( norm.x < 0 ? -1.0 : 1.0 ) * sqrt( abs( 1 - res.x * res.x - res.y * res.y ) );

   return res;
}
*/

// Holger Gruen AMD - I change normal packing and unpacking to make sure N.z is accessible without ALU cost
// this help the HDAO compute shader to run more efficiently
float2 gbuf_pack_normal( float3 norm )
{
   float2 res;

   res.x  = norm.z;
   res.y  = 0.5f * ( norm.x + 1.0f ) ;
   res.y *= ( norm.y < 0.0f ? -1.0f : 1.0f );

   return res;
}

float3 gbuf_unpack_normal( float2 norm )
{
   float3 res;

   res.z  = norm.x;
   res.x  = ( 2.0f * abs( norm.y ) ) - 1.0f;
   res.y = ( norm.y < 0 ? -1.0 : 1.0 ) * sqrt( abs( 1 - res.x * res.x - res.z * res.z ) );

   return res;
}

float gbuf_pack_hemi_mtl(float hemi, float mtl, float sss = 1) {
	uint pack_hemi = saturate(hemi) * 0x3FF;
	uint pack_mtl = uint(mtl * 0x1F) & 0x1F;
	
	uint pack_sss = uint(sss * 0x01);
	uint packed = (pack_hemi << 6) | (pack_mtl << 1) | (pack_sss);

	return float(packed) / 65535.0f;
}

float gbuf_unpack_hemi(float mtl_hemi) {
	uint packed = mtl_hemi * 0xffff;
	return float(packed >> 6) / 1023.0f;
}

float gbuf_unpack_mtl(float mtl_hemi) {
	uint packed = mtl_hemi * 0xffff;
	return float((packed >> 1) & 0x1F) / 31.0f;
}

float gbuf_unpack_sss(float mtl_hemi) {
	uint packed = mtl_hemi * 0xffff;
	return float(packed & 0x01);
}

f_deffer pack_gbuffer(float4 Normal, float4 Point, float4 Color, float SSS = 0.0f)
{
	f_deffer res;
	
	Normal.z = -Normal.z;
	res.Ne.xyz = saturate(Normal.xyz * 0.5f + 0.5f);
	res.Ne.w = gbuf_pack_hemi_mtl(Normal.w, Point.w, SSS);
	res.C = Color;

	return res;
}

gbuffer_data gbuffer_load_data( float2 tc : TEXCOORD, float2 pos2d)
{
	gbuffer_data gbd;

	gbd.N = float3(0.0f, 0.0f, 1.0f);
	gbd.P = float3(0.0f, 0.0f, 0.0f);
	
	gbd.P_hud = float3(0.0f, 0.0f, 0.0f);
	gbd.P_real = float3(0.0f, 0.0f, 0.0f);
	
	gbd.hemi = 0.0f;
	gbd.mtl = 0.0f;
	gbd.C = 0.0f;

	gbd.depth = s_position.Sample(smp_nofilter, tc).x;
	
	pos2d = pos2d - m_taa_jitter.xy * float2(0.5f, -0.5f) * pos_decompression_params2.xy;
	float3 P = float3(pos2d * pos_decompression_params.zw - pos_decompression_params.xy, 1.0f);
	
	gbd.P = P * m_P._34 / (gbd.depth - m_P._33);
	gbd.P_hud = P * m_P_hud._34 / (min(1.0f, gbd.depth * 50.0f) - m_P_hud._33);
	gbd.P_real = gbd.depth > 0.02f ? gbd.P : gbd.P_hud;
		
	float4 N = s_normal.Sample(smp_nofilter, tc);

	// reconstruct N
	gbd.N = normalize(N.xyz - 0.5f);
	gbd.N.z = -gbd.N.z;

	// reconstruct material
	gbd.mtl	= gbuf_unpack_mtl(N.w);

	// reconstruct hemi
	gbd.hemi = gbuf_unpack_hemi(N.w);

	// reconstruct sss
	gbd.sss = gbuf_unpack_sss(N.w);

	float4 C = s_diffuse.Sample(smp_nofilter, tc);

	gbd.C = C.xyz;
	gbd.gloss = C.w;

	return gbd;
}

gbuffer_data gbuffer_load_data_offset( float2 tc : TEXCOORD, float2 OffsetTC : TEXCOORD, float2 pos2d)
{
   float2 delta = ((OffsetTC - tc) * pos_decompression_params2.xy);
   return gbuffer_load_data(OffsetTC, pos2d + delta);
}

#endif	//	common_functions_h_included
