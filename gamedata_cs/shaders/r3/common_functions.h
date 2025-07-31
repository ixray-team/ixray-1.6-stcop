#ifndef	common_functions_h_included
#define	common_functions_h_included

//	contrast function
half Contrast(half Input, half ContrastPower)
{
     //piecewise contrast function
     bool IsAboveHalf = Input > 0.5 ;
     half ToRaise = saturate(2*(IsAboveHalf ? 1-Input : Input));
     half Output = 0.5*pow(ToRaise, ContrastPower); 
     Output = IsAboveHalf ? 1-Output : Output;
     return Output;
}

void tonemap( out half4 low, out half4 high, half3 rgb, half scale)
{
	rgb		=	rgb*scale;

	const float fWhiteIntensity = 1.7;

	const float fWhiteIntensitySQR = fWhiteIntensity*fWhiteIntensity;

//	low		=	(rgb/(rgb + 1)).xyzz;
	low		=	( (rgb*(1+rgb/fWhiteIntensitySQR)) / (rgb+1) ).xyzz;

	high	=	rgb.xyzz/def_hdr;	// 8x dynamic range

/*
	rgb		=	rgb*scale;

	low		=	rgb.xyzz;
	high	=	low/def_hdr;	// 8x dynamic range
*/
}

half4 combine_bloom( half3  low, half4 high)	
{
        return half4( low + high*high.a, 1.h );
}

half calc_fogging( half4 w_pos )      
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

half3 	unpack_normal( half3 v )	{ return 2*v-1; }
half3 	unpack_bx2( half3 v )	{ return 2*v-1; }
float3 	unpack_bx4( float3 v )	{ return 4*v-2; } //!reduce the amount of stretching from 4*v-2 and increase precision
float2 	unpack_tc_lmap( half2 tc )	{ return tc*(1.f/32768.f);	} // [-1  .. +1 ] 
float4	unpack_color( float4 c ) { return c.bgra; }
float4	unpack_D3DCOLOR( float4 c ) { return c.bgra; }
float3	unpack_D3DCOLOR( float3 c ) { return c.bgr; }

half3   p_hemi( float2 tc )
{
//	half3	t_lmh = tex2D (s_hemi, tc);
//	half3	t_lmh = s_hemi.Sample( smp_rtlinear, tc);
//	return	dot(t_lmh,1.h/4.h);
	half4	t_lmh = s_hemi.Sample( smp_rtlinear, tc);
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

#endif	//	common_functions_h_included