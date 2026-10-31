//--------------------------------------------------------------------------------------
// Defines
//--------------------------------------------------------------------------------------

#define LEFTCELL float3(input.LR.x, input.texcoords.y, input.texcoords.z)
#define RIGHTCELL float3(input.LR.y, input.texcoords.y, input.texcoords.z)
#define BOTTOMCELL float3(input.texcoords.x, input.BT.x, input.texcoords.z)
#define TOPCELL float3(input.texcoords.x, input.BT.y, input.texcoords.z)
#define DOWNCELL float3(input.texcoords.x, input.texcoords.y, input.DU.x)
#define UPCELL float3(input.texcoords.x, input.texcoords.y, input.DU.y)

//	1.73 - voxel diagonal length
#define BOX_EXPANSION 1.73

//--------------------------------------------------------------------------------------
// Textures
//--------------------------------------------------------------------------------------

Texture3D Texture_velocity0 : register(t0);
Texture3D Texture_velocity1 : register(t1);
Texture3D Texture_color : register(t2);
Texture3D Texture_obstacles : register(t3);
Texture3D Texture_obstvelocity : register(t4);
Texture3D Texture_pressure : register(t5);
Texture3D Texture_tempscalar : register(t6);
Texture3D Texture_tempvector : register(t7);

//--------------------------------------------------------------------------------------
// Samplers
//--------------------------------------------------------------------------------------

sampler samPointClamp : register(s8);
sampler samLinear : register(s9);

//--------------------------------------------------------------------------------------
// Variables
//--------------------------------------------------------------------------------------

#include "shared\fluid_cb.hlsli"

//	For fire simulation. Fuel extinction speed.
static const float k = 0.006;

/////////////////////////////////////////////////////////////////
//	Structs

/////////////////////////////////////////////////////////////////
//	fluidsim
struct v_fluidsim
{
    float3 position;
    float3 textureCoords0;
};

v_fluidsim FluidSliceVertex(uint VertexId)
{
    uint Corner = VertexId % 6;
    float2 Cell = float2((0x16 >> Corner) & 1 ? textureWidth - 1.0 : 1.0, (0x34 >> Corner) & 1 ? textureHeight - 1.0 : 1.0);
    v_fluidsim Vertex;
    Vertex.position = float3(Cell.x * 2.0 / textureWidth - 1.0, -Cell.y * 2.0 / textureHeight + 1.0, 0.0);
    Vertex.textureCoords0 = float3(Cell, VertexId / 6 + 1);
    return Vertex;
}

struct v2g_fluidsim
{
    float3 cell0 : TEXCOORD0;
    float3 texcoords : TEXCOORD1;
    float2 LR : TEXCOORD2;
    float2 BT : TEXCOORD3;
    float2 DU : TEXCOORD4;
    float4 pos : SV_POSITION;
};

struct g2p_fluidsim
{
    float3 cell0 : TEXCOORD0; // 3D cell coordinates (x,y,z in 0-dimension range)
    float3 texcoords : TEXCOORD1; // 3D cell texcoords (x,y,z in 0-1 range)
    float2 LR : TEXCOORD2; // 3D cell texcoords for the Left and Right neighbors
    float2 BT : TEXCOORD3; // 3D cell texcoords for the Bottom and Top neighbors
    float2 DU : TEXCOORD4; // 3D cell texcoords for the Down and Up neighbors
    float4 pos : SV_POSITION; // 2D slice vertex coordinates in homogenous clip space
    uint RTIndex : SV_RenderTargetArrayIndex; // used to choose the destination slice
};

struct p_fluidsim
{
    float3 cell0 : TEXCOORD0; // 3D cell coordinates (x,y,z in 0-dimension range)
    float3 texcoords : TEXCOORD1; // 3D cell texcoords (x,y,z in 0-1 range)
    float2 LR : TEXCOORD2; // 3D cell texcoords for the Left and Right neighbors
    float2 BT : TEXCOORD3; // 3D cell texcoords for the Bottom and Top neighbors
    float2 DU : TEXCOORD4; // 3D cell texcoords for the Down and Up neighbors
};

/////////////////////////////////////////////////////////////////
//	fluidsim_clip
//	TODO: DX10: These structures are used for obstacle rendering. Remove unused fields.

struct v2g_fluidsim_clip
{
    float3 cell0 : TEXCOORD0;
    float3 texcoords : TEXCOORD1;
    float2 LR : TEXCOORD2;
    float2 BT : TEXCOORD3;
    float2 DU : TEXCOORD4;
    float3 clip0 : SV_ClipDistance0;
    float3 clip1 : SV_ClipDistance1;
    float4 pos : SV_POSITION;
};

struct g2p_fluidsim_clip
{
    float3 cell0 : TEXCOORD0; // 3D cell coordinates (x,y,z in 0-dimension range)
    float3 texcoords : TEXCOORD1; // 3D cell texcoords (x,y,z in 0-1 range)
    float2 LR : TEXCOORD2; // 3D cell texcoords for the Left and Right neighbors
    float2 BT : TEXCOORD3; // 3D cell texcoords for the Bottom and Top neighbors
    float2 DU : TEXCOORD4; // 3D cell texcoords for the Down and Up neighbors
    float3 clip0 : SV_ClipDistance0;
    float3 clip1 : SV_ClipDistance1;
    float4 pos : SV_POSITION; // 2D slice vertex coordinates in homogenous clip space
    uint RTIndex : SV_RenderTargetArrayIndex; // used to choose the destination slice
};

struct p_fluidsim_clip
{
    float3 cell0 : TEXCOORD0; // 3D cell coordinates (x,y,z in 0-dimension range)
    float3 texcoords : TEXCOORD1; // 3D cell texcoords (x,y,z in 0-1 range)
    float2 LR : TEXCOORD2; // 3D cell texcoords for the Left and Right neighbors
    float2 BT : TEXCOORD3; // 3D cell texcoords for the Bottom and Top neighbors
    float2 DU : TEXCOORD4; // 3D cell texcoords for the Down and Up neighbors
    float3 clip0 : SV_ClipDistance0;
    float3 clip1 : SV_ClipDistance1;
};

/////////////////////////////////////////////////////////////////
//	fluidsim_clip
//	TODO: DX10: These structures are used for obstacle rendering. Remove unused fields.

struct v2g_fluidsim_dyn_aabb
{
    float3 cell0 : TEXCOORD0;
    float3 velocity : TEXCOORD1;
    float3 clip0 : SV_ClipDistance0;
    float3 clip1 : SV_ClipDistance1;
    float4 pos : SV_POSITION;
};

struct g2p_fluidsim_dyn_aabb
{
    float3 cell0 : TEXCOORD0; // 3D cell coordinates (x,y,z in 0-dimension range)
    float3 velocity : TEXCOORD1; // speed of the point in local space
    float3 clip0 : SV_ClipDistance0;
    float3 clip1 : SV_ClipDistance1;
    float4 pos : SV_POSITION; // 2D slice vertex coordinates in homogenous clip space
    uint RTIndex : SV_RenderTargetArrayIndex; // used to choose the destination slice
};

struct p_fluidsim_dyn_aabb
{
    float3 cell0 : TEXCOORD0; // 3D cell coordinates (x,y,z in 0-dimension range)
    float3 velocity : TEXCOORD1;
    float3 clip0 : SV_ClipDistance0;
    float3 clip1 : SV_ClipDistance1;
};

//--------------------------------------------------------------------------------------
// Helper functions
//--------------------------------------------------------------------------------------

float4 GetObstVelocity(float3 cellTexCoords)
{
    return Texture_obstvelocity.SampleLevel(samPointClamp, cellTexCoords, 0);
}

bool IsNonEmptyCell(float3 cellTexCoords)
{
    return (Texture_obstacles.SampleLevel(samPointClamp, cellTexCoords, 0).r > 0.0);
}

bool IsBoundaryCell(float3 cellTexCoords)
{
    return (Texture_obstacles.SampleLevel(samPointClamp, cellTexCoords, 0).r > 0.9);
}

float3 GetAdvectedPosTexCoords(p_fluidsim input)
{
    float3 pos = input.cell0;

    pos -= timestep * forward *
        Texture_velocity0.SampleLevel(samPointClamp, input.texcoords, 0).xyz;

    return float3(pos.x / textureWidth, pos.y / textureHeight, (pos.z + 0.5) / textureDepth);
}
