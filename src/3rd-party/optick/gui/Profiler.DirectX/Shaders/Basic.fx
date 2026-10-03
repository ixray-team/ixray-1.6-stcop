cbuffer ConstantBuffer : register(b0)
{
    matrix View;
    matrix World;
}

// Subtracted from the input position before the world transform. The CPU moves
// the world translation into this buffer, so the multiply never has to cancel
// two huge float values (which used to make the timeline snap at high zoom).
cbuffer OriginBuffer : register(b1)
{
    float4 ViewOrigin;
}

struct VS_IN
{
	float2 pos : POSITION;
	float4 col : COLOR;
};

struct PS_IN
{
	float4 pos : SV_POSITION;
	float4 col : COLOR;
};

PS_IN VS( VS_IN input )
{
	PS_IN output = (PS_IN)0;

	float4x4 wv = mul(View, World);

	output.pos = mul(wv, float4(input.pos - ViewOrigin.xy, 0.5f, 1.0f));
	output.col = input.col;
	
	return output;
}

float4 PS( PS_IN input ) : SV_Target
{
	return input.col;
}

technique10 Render
{
	pass P0
	{
		SetGeometryShader( 0 );
		SetVertexShader( CompileShader( vs_4_0, VS() ) );
		SetPixelShader( CompileShader( ps_4_0, PS() ) );
	}
}