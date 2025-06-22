struct VSInput
{
    float2 texcoord : TEXCOORD0;
    float4 hpos : POSITIONT;
};

struct VSOutput
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0;
};

VSOutput main(VSInput I)
{
    VSOutput O;

    O.hpos = I.hpos;
    O.texcoord = I.texcoord;

    return O;
}
