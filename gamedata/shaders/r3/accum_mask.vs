#include "common.hlsli"

float4 main ( float4 P: POSITION ) : SV_Position
{
	return 	mul ( m_WVP, P );
}


