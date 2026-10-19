#ifndef	_XRLIGHT_VERTEX_H_
#define	_XRLIGHT_VERTEX_H_
#include "../Mesh/xrFace.h"

extern XRLC_LIGHT_API void LightVertex	();

void g_trans_register(Vertex* V);
bool GetTranslucency(const Vertex* V, float& v_trans);
#endif
