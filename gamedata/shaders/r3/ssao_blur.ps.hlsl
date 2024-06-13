uniform	float4		screen_res;
uniform	Texture2D					s_occ;

float ssao_blur_ps(float2 centerTC)
{
  // low weight center sample - will be used on edges
	float  fSumWeight = 0.025f;
   float2 centerData = s_occ.Sample( smp_nofilter, centerTC);
   float  fOcclusion  = centerData.r * fSumWeight;
   float  centerDepth = centerData.g;

   float2 arrOffsets[4] =
   {
     float2( 1,-1),
     float2(-1,-1),
     float2( 1, 1),
     float2(-1, 1)
   };

  [unroll]
  for(int i=0; i<4; i++)
  {
     float2 sampleTC   = centerTC + pos_decompression_params2.zw * arrOffsets[i];
     float2 sampleData = s_occ.Sample( smp_nofilter, sampleTC);
		float fDepth  = sampleData.g;
		float fDiff	  = 8*abs(fDepth-centerDepth)/min(fDepth,centerDepth);
     	float fWeight = saturate(1-fDiff);

     fOcclusion += sampleData.r * fWeight;

     fSumWeight += fWeight;
  }

   fOcclusion /= fSumWeight;   

  return fOcclusion;
}
