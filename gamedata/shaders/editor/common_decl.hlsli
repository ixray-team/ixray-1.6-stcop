#ifndef COMMON_DECL_H
#define COMMON_DECL_H

#include "d3d11\shared\fixed_cb.hlsli"

FX_CBUFFER(CBFrame, cb_frame, b0)
CB_FRAME_FIELDS
FX_CBUFFER_END()

FX_CBUFFER(CBView, cb_view, b1)
CB_VIEW_FIELDS
FX_CBUFFER_END()

FX_CBUFFER(CBObject, cb_object, b2)
CB_OBJECT_XFORM
FX_CBUFFER_END()

FX_CBUFFER(CBPass, cb_pass, b5)
CB_PASS_FIELDS
FX_CBUFFER_END()

FX_CBUFFER(CBMaterial, cb_material, b3)
CB_MATERIAL_FIELDS
FX_CBUFFER_END()

FX_CBUFFER(CBLight, cb_light, b4)
CB_LIGHT_FIELDS
FX_CBUFFER_END()

#undef FX_CBUFFER
#undef FX_CBUFFER_END
#undef FX_F4
#undef FX_F4A
#undef FX_F3
#undef FX_F3X4
#undef FX_F4X4
#undef FX_F4X4A
#undef FX_INT
#undef FX_F1
#undef FX_PAD2
#undef FX_PAD3
#undef CB_FRAME_FIELDS
#undef CB_FRAME_HUD_RAIN
#undef CB_VIEW_FIELDS
#undef CB_OBJECT_XFORM
#undef CB_OBJECT_PROPS
#undef CB_PASS_FIELDS
#undef CB_PASS_REFLECTION
#undef CB_MATERIAL_FIELDS
#undef CB_LIGHT_FIELDS
#undef CB_LIGHT_XFORM

#endif
