struct v2p_TL
{
    float2 Tex0 : TEXCOORD0;
    float4 Color : COLOR;
    float4 HPos : SV_POSITION; // Clip-space position 	(for rasterization)
};

// Pixel
float4 main(v2p_TL I) : SV_Target
{
    return I.Color;
}
