// Stochastic SSR (Stachowiak, SIGGRAPH 2015): one level of the min-Z pyramid.
RWTexture2D<float> u_hiz_src : register(u0);
RWTexture2D<float> u_hiz_dst : register(u1);

[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID)
{
	uint2 SrcSize, DstSize;
	u_hiz_src.GetDimensions(SrcSize.x, SrcSize.y);
	u_hiz_dst.GetDimensions(DstSize.x, DstSize.y);

	if (any(DTid >= DstSize))
	{
		return;
	}

	uint2 Src = DTid * 2;

	float MinZ = min(min(u_hiz_src[Src], u_hiz_src[Src + uint2(1, 0)]), min(u_hiz_src[Src + uint2(0, 1)], u_hiz_src[Src + uint2(1, 1)]));

	// Odd source size: the last texel also covers the extra column and row
	bool ExtraX = (SrcSize.x & 1) && DTid.x == DstSize.x - 1;
	bool ExtraY = (SrcSize.y & 1) && DTid.y == DstSize.y - 1;

	if (ExtraX)
	{
		MinZ = min(MinZ, min(u_hiz_src[Src + uint2(2, 0)], u_hiz_src[Src + uint2(2, 1)]));
	}

	if (ExtraY)
	{
		MinZ = min(MinZ, min(u_hiz_src[Src + uint2(0, 2)], u_hiz_src[Src + uint2(1, 2)]));
	}

	if (ExtraX && ExtraY)
	{
		MinZ = min(MinZ, u_hiz_src[Src + uint2(2, 2)]);
	}

	u_hiz_dst[DTid] = MinZ;
}
