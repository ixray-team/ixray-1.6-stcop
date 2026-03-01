struct VSOutput
{
    float4 hpos : SV_Position;
    float4 hpos2d : TEXCOORD0;
};

float4x4 m_WVP;

VSOutput main(in float3 vertices : POSITION)
{
    VSOutput O;

    O.hpos = mul(m_WVP, float4(vertices, 1.0));
    O.hpos2d = O.hpos; //No transformation

    return O;
}
