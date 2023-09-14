#include "common.h"

//////////////////////////////////////////////////////////////////////////////////////////
// Vertex
v2p_dumb main ( v_dumb I )
{
	v2p_dumb O;

	O.HPos = mul( m_WVPClean, I.P );

 	return O;
}
FXVS;
