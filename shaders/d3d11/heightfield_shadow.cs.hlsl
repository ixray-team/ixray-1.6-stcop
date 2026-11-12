#include "shared\fixed_cb.hlsli"

// The original heightfield sun scan: share each upstream maximum across a row.
// All variants use SM5 group memory; no per-receiver ray marching.
#define HEIGHTFIELD_BLOCK_SIZE 64
#define HEIGHTFIELD_EMPTY (-1.0e20f)

Texture2D<float> s_heightfield_input : register(t27);
RWTexture2D<float> HeightfieldOutput : register(u0);

int HeightfieldRowOffset(uint T)
{
	return int(floor(HeightfieldScan.z * T - HeightfieldScan.w + 0.5f));
}

#if HEIGHTFIELD_PASS < 2
groupshared float HeightfieldShared[HEIGHTFIELD_BLOCK_SIZE];

void HeightfieldPrefixMax(uint Lane, float Value)
{
	HeightfieldShared[Lane] = Value;
	GroupMemoryBarrierWithGroupSync();
	[unroll]
	for (uint Offset = 1; Offset < HEIGHTFIELD_BLOCK_SIZE; Offset <<= 1)
	{
		float Previous = Lane >= Offset ? HeightfieldShared[Lane - Offset] : HEIGHTFIELD_EMPTY;
		GroupMemoryBarrierWithGroupSync();
		HeightfieldShared[Lane] = max(HeightfieldShared[Lane], Previous);
		GroupMemoryBarrierWithGroupSync();
	}
}
#endif

#if HEIGHTFIELD_PASS == 0
RWTexture2D<float> HeightfieldBlocks : register(u1);

[numthreads(HEIGHTFIELD_BLOCK_SIZE, 1, 1)]
void main(uint3 Group : SV_GroupID, uint Lane : SV_GroupIndex)
{
	uint T = Group.x * HEIGHTFIELD_BLOCK_SIZE + Lane;
	int Major = HeightfieldScan.y > 0.0f ? int(T) : int(HeightfieldGrid.z) - 1 - int(T);
	int Minor = int(Group.y) + HeightfieldRowOffset(T);
	int2 Pixel = HeightfieldScan.x < 0.5f ? int2(Major, Minor) : int2(Minor, Major);
	float Height = HEIGHTFIELD_EMPTY;
	if (all(Pixel >= 0) && all(Pixel < int2(HeightfieldGrid.xy)))
	{
		float Encoded = s_heightfield_input.Load(int3(Pixel, 0));
		if (Encoded > 0.0f)
		{
			Height = Encoded * HeightfieldDecode.x + HeightfieldDecode.y;
		}
	}
	// Padded lanes must also participate in every group barrier.
	HeightfieldPrefixMax(Lane, Height + T * HeightfieldParams.y);
	HeightfieldOutput[uint2(T, Group.y)] = HeightfieldShared[Lane];
	if (Lane == HEIGHTFIELD_BLOCK_SIZE - 1)
	{
		HeightfieldBlocks[Group.xy] = HeightfieldShared[Lane];
	}
}
#elif HEIGHTFIELD_PASS == 1
[numthreads(HEIGHTFIELD_BLOCK_SIZE, 1, 1)]
void main(uint3 Group : SV_GroupID, uint Lane : SV_GroupIndex)
{
	uint Blocks = (uint(HeightfieldGrid.z) + HEIGHTFIELD_BLOCK_SIZE - 1) / HEIGHTFIELD_BLOCK_SIZE;
	float Height = Lane < Blocks ? s_heightfield_input.Load(int3(Lane, Group.y, 0)) : HEIGHTFIELD_EMPTY;
	HeightfieldPrefixMax(Lane, Height);
	if (Lane < Blocks)
	{
		HeightfieldOutput[uint2(Lane, Group.y)] = HeightfieldShared[Lane];
	}
}
#else
Texture2D<float> s_heightfield_blocks : register(t28);

[numthreads(8, 8, 1)]
void main(uint2 Pixel : SV_DispatchThreadID)
{
	if (any(Pixel >= uint2(HeightfieldGrid.xy)))
	{
		return;
	}
	int Major = HeightfieldScan.x < 0.5f ? Pixel.x : Pixel.y;
	int Minor = HeightfieldScan.x < 0.5f ? Pixel.y : Pixel.x;
	int T = HeightfieldScan.y > 0.0f ? Major : int(HeightfieldGrid.z) - 1 - Major;
	// Invert the same integer shift, including exact half-texel shear offsets.
	int Row = Minor - HeightfieldRowOffset(T);
	float Height = HEIGHTFIELD_EMPTY;
	if (Row >= 0 && Row < int(HeightfieldGrid.w))
	{
		Height = s_heightfield_input.Load(int3(T, Row, 0));
		int Block = T / HEIGHTFIELD_BLOCK_SIZE;
		if (Block > 0)
		{
			Height = max(Height, s_heightfield_blocks.Load(int3(Block - 1, Row, 0)));
		}
		Height -= T * HeightfieldParams.y;
	}
	// Undo the shear with nearest sampling. Filter comparisons, not ceiling heights.
	// Negated world height lets the existing LESS_EQUAL comparison sampler test visibility.
	HeightfieldOutput[Pixel] = -Height;
}
#endif
