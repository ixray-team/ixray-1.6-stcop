#ifndef EDITOR_INSTANCING_H
#define EDITOR_INSTANCING_H

// World transforms of a vertex shader. The editors draw repeated scene objects as instances
// (USE_EDITOR_INSTANCING): the transform comes from a structured buffer indexed by the instance.
struct object_xforms
{
    float3x4 W;
    float3x4 WV;
    float4x4 WVP;
};

object_xforms object_default_xforms()
{
    object_xforms X;
    X.W = m_W;
    X.WV = m_WV;
    X.WVP = m_WVP;
    return X;
}

#ifdef USE_EDITOR_INSTANCING
struct editor_instance
{
    float4 W0;
    float4 W1;
    float4 W2;
};

StructuredBuffer<editor_instance> editor_instances;
uniform float4 editor_instance_params; // x: first instance of the draw

float4x4 editor_affine(float3x4 M)
{
    return float4x4(M[0], M[1], M[2], float4(0.0f, 0.0f, 0.0f, 1.0f));
}

object_xforms object_get_xforms(uint InstanceID)
{
    editor_instance Inst = editor_instances[InstanceID + (uint)editor_instance_params.x];

    object_xforms X;
    X.W = float3x4(Inst.W0, Inst.W1, Inst.W2);
    X.WV = (float3x4)mul(editor_affine(m_V), editor_affine(X.W));
    X.WVP = mul(m_VP, editor_affine(X.W));
    return X;
}
#else
object_xforms object_get_xforms(uint InstanceID)
{
    return object_default_xforms();
}
#endif

#endif // EDITOR_INSTANCING_H
