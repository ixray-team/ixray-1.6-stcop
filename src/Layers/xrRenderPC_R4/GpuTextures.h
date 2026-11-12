#pragma once

namespace nri
{
struct Texture;
struct Descriptor;
} // namespace nri

class CTexture;
class IRHISurface;

// A surface of the old backend as an NRI texture; Format is the typed DXGI format of a typeless one
nri::Texture* WrapNriSurface(IRHISurface* Surface, u32 Format = 0);

// Material textures as slices of square Texture2DArrays, read from their files. Every texture has
// a tail slice: its mips from 512 down. The larger mips are a head slice in a bucket of that size.
// A texture that is not square, or smaller than its slice, is stored repeated and sampled with a
// scale. So far heads are loaded with the tails: nothing is streamed.
class GpuTextures
{
public:
	static constexpr u32 MaxBuckets = 16;
	static constexpr u32 Absent = u32(-1);

	// Tail and Head are bucket << 16 | slice; Info is the repeat shift along x, << 4 along y, and
	// << 8 the mip count of the head. Tail is Absent for a texture that has no bucket: not a plain
	// block-compressed 2D texture with mips, or no bucket left.
	struct Ref
	{
		u32 Tail = Absent;
		u32 Head = Absent;
		u32 Info = 0;
	};

	void Build(const xr_vector<CTexture*>& Textures, xr_vector<Ref>& Refs);
	void Destroy();
	// The array of a bucket; a bucket that is not in use gives the array of another
	nri::Descriptor* View(u32 Bucket) const;
	u32 Count() const { return BucketCount; }

private:
	struct Bucket
	{
		nri::Texture* Texture = nullptr;
		nri::Descriptor* View = nullptr;
		u32 Format = 0;
		u32 Size = 0;
		u32 Mips = 0;
		u32 Slices = 0;
	};

	u32 Place(u32 Format, u32 Size, u32 Mips);

	Bucket Buckets[MaxBuckets];
	u32 BucketCount = 0;
};
