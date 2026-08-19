#include "MaterialPassCommon.hlsl"

float3 ReconstructDecalWorldPosition(
    const float2 PixelPosition,
    const float DeviceDepth)
{
    const float2 ViewSize = max(SceneView.zw, 1.0f.xx);
    const float2 Uv = PixelPosition / ViewSize;
    const float4 ClipPosition = float4(
        Uv.x * 2.0f - 1.0f,
        1.0f - Uv.y * 2.0f,
        DeviceDepth,
        1.0f);
    const float4 WorldPosition = mul(
        InverseViewProjectionWorldMatrix,
        ClipPosition);
    const float SafeW = abs(WorldPosition.w) > 1.0e-6f
        ? WorldPosition.w
        : (WorldPosition.w < 0.0f ? -1.0e-6f : 1.0e-6f);
    return WorldPosition.xyz / SafeW;
}

float4 Main(MaterialPassPixelInput Input) : SV_Target0
{
    const MaterialDrawGpuData DrawData =
        LoadMaterialDrawGpuData(Input.MaterialDrawIndex);
    Texture2D<float> SceneDepth =
        ResourceDescriptorHeap[DrawData.SkinningPaletteOffset];
    const float DeviceDepth = SceneDepth.Load(
        int3(int2(Input.Position.xy), 0));
    const float3 WorldPosition = ReconstructDecalWorldPosition(
        Input.Position.xy,
        DeviceDepth);
    const float4 LocalPosition4 = mul(
        DrawData.PreviousLocalToWorld,
        float4(WorldPosition, 1.0f));
    const float SafeLocalW = abs(LocalPosition4.w) > 1.0e-6f
        ? LocalPosition4.w
        : (LocalPosition4.w < 0.0f ? -1.0e-6f : 1.0e-6f);
    const float3 LocalPosition = LocalPosition4.xyz / SafeLocalW;

    // Канонический projector volume занимает [-0.5, 0.5] по трём осям.
    clip(0.5f - abs(LocalPosition));

    const float3 WorldDerivativeX = ddx(WorldPosition);
    const float3 WorldDerivativeY = ddy(WorldPosition);
    const float3 SurfaceNormal = normalize(cross(
        WorldDerivativeX,
        WorldDerivativeY));
    const float3 ProjectorNormal = normalize(mul(
        (float3x3)DrawData.LocalToWorld,
        float3(0.0f, 0.0f, 1.0f)));
    // Знак normal зависит от winding исходной поверхности, поэтому проверяется
    // абсолютное совпадение осей. Fade не даёт декали растягиваться на стену,
    // почти перпендикулярную направлению проектора.
    const float ProjectionAlignment = abs(dot(
        SurfaceNormal,
        ProjectorNormal));
    const float ProjectionAngleFade = smoothstep(
        0.2f,
        0.5f,
        ProjectionAlignment);
    clip(ProjectionAngleFade - 1.0e-4f);

    MaterialContext Context;
    Context.TexCoord0 = LocalPosition.xy + 0.5f;
    Context.TexCoord1 = Context.TexCoord0;
    Context.VertexColor = 1.0f.xxxx;
    Context.WorldNormal = SurfaceNormal;
    Context.WorldPosition = WorldPosition;
    Context.CameraPosition = CameraPositionAndTime.xyz;
    const float3 CameraDelta = Context.CameraPosition - WorldPosition;
    Context.CameraVector = CameraDelta *
        rsqrt(max(dot(CameraDelta, CameraDelta), 1.0e-8f));
    Context.Time = CameraPositionAndTime.w;

    MaterialInputs Inputs;
    const MaterialParameters Parameters =
        LoadMaterialParametersForInstance(DrawData.MaterialInstanceIndex);
    EvaluateMaterial(Context, Parameters, Inputs);
    ApplyMaterialOpacityMask(Inputs);
    const float3 DecalColor = max(
        Inputs.BaseColor + Inputs.Emissive,
        0.0f);
    const float DecalOpacity = saturate(Inputs.Opacity) *
        ProjectionAngleFade;
#if MATERIAL_BLEND_MODULATE
    // Modulate использует source color напрямую, поэтому прозрачная часть
    // должна стремиться к нейтральному белому, а не к нулю.
    return float4(lerp(1.0f.xxx, DecalColor, DecalOpacity), 1.0f);
#else
    return float4(DecalColor, DecalOpacity);
#endif
}
