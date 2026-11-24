/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once

#include "OneJobThread.h"

namespace nri
{
struct Texture;
struct Descriptor;
struct CommandBuffer;
struct Streamer;
} // namespace nri

class CTexture;
class IRHISurface;

// A surface of the old backend as an NRI texture; Format is the typed DXGI format of a typeless one
nri::Texture* WrapNriSurface(IRHISurface* Surface, u32 Format = 0);

// Material textures as slices of square Texture2DArrays, read from their files. Every texture has
// a tail slice: its mips from 512 down. The larger mips are a head slice in a bucket of that size.
// A texture that is not square, or smaller than its slice, is stored repeated and sampled with a
// scale. A head is loaded with the tail where it is asked for at once; any other is streamed into the
// pool of its bucket when the resolve reports it needed, and gives its place away when it is not.
class GpuTextures
{
public:
	static constexpr u32 MaxBuckets = 16;
	static constexpr u32 Absent = u32(-1);

	// Tail and Head are bucket << 16 | slice; Info is the repeat shift along x, << 4 along y,
	// << 8 the mip count of the head, << 12 log2 of the tail's size, << 16 its mip count and, for a
	// texture with a streamed head, << 20 its index plus one. Tail is Absent for a texture that has no bucket: not a plain
	// block-compressed 2D texture with mips, or no bucket left.
	struct Ref
	{
		u32 Tail = Absent;
		u32 Head = Absent;
		u32 Info = 0;
	};

	// A texture gets its head only where WantsHead is set
	void Build(const xr_vector<CTexture*>& Textures, const xr_vector<u8>& WantsHead, xr_vector<Ref>& Refs);
	// A texture first seen after Build: its tail takes a spare slice of its tail bucket, read from its file here and
	// copied with Commands, and its head is streamed into the pool of its kind where there is one. Tail is Absent when
	// it has no place
	Ref Add(const CTexture& Texture, nri::CommandBuffer& Commands, nri::Streamer& Streamer);
	// The blocks of every mip of a plain block-compressed 2D file, as its texture takes them, with its DXGI format
	// and size. Any thread. False for any other file
	static bool ReadWhole(const char* Name, xr_vector<xr_vector<u8>>& Levels, u32& Format, u32& Width, u32& Height, u32& BlockBytes);
	// Whether a texture's file can go into buckets, with its DXGI format and size; nothing is loaded. A missing file is
	// the engine's placeholder. Why names the reason when it cannot, or that the placeholder stands in
	static bool Probe(const CTexture& Texture, u32& Format, u32& Width, u32& Height, const char*& Why);
	// Whether the engine reads a texture as one 2D texture of any format and size, which a draw can bind as it is; Why
	// names the reason when it does not
	static bool IsPlain(const CTexture& Texture, const char*& Why);
	// The DXGI format of the bucket a reference names, 0 for none
	u32 Format(u32 Slot) const { return (Slot >> 16) < BucketCount ? Buckets[Slot >> 16].Format : 0; }
	void Destroy();
	// The array of a bucket; a bucket that is not in use gives the array of another
	nri::Descriptor* View(u32 Bucket) const;
	u32 Count() const { return BucketCount; }
	// Seen: per texture the frame its head was last asked for << 8, and in the low byte how much it was needed
	// then (255: its finest level). A head is read by a thread of its own and copied here when it is ready, one at
	// a time, with the copies recorded into Commands
	void Update(const u32* Seen, u32 Frame, nri::CommandBuffer& Commands, nri::Streamer& Streamer);
	// Per texture the slice of its streamed head, Absent while it is not loaded
	const xr_vector<u32>& GetHeads() const { return Heads; }
	// Per pool of streamed heads: its places, the heads in them, those asked for now and those that wait, to the log
	void LogPools() const;

private:
	struct Bucket
	{
		nri::Texture* Texture = nullptr;
		nri::Descriptor* View = nullptr;
		u32 Format = 0;
		u32 Size = 0;
		u32 Mips = 0;
		u32 Slices = 0;
		u32 Spare = 0; // the first slice no texture has: those of a tail bucket from it to PoolFirst wait for Add
		// The slices streamed heads share: Wanted textures could use them
		u32 PoolFirst = 0;
		u32 Wanted = 0;
		xr_vector<u32> Owners; // per slice of the pool the texture in it, or Absent
		u32 Starved = 0; // the frames in a row a head of it waited with no place to take
	};

	// An array a pool has grown or shrunk out of: the frames in flight still read it
	struct Retired
	{
		nri::Texture* Texture;
		nri::Descriptor* View;
		u32 Frame;
	};

	struct Stream
	{
		shared_str Name;
		u32 Bucket = Absent; // Absent: no head to stream
		u32 Slice = Absent;
		u32 LastSeen = 0;
		float Heat = 0.0f; // how much it was needed over the frames it was asked for, the older ones counting less
	};

	// The bucket of a kind, BucketCount for none
	u32 Find(u32 Format, u32 Size, u32 Mips) const;
	u32 Place(u32 Format, u32 Size, u32 Mips);
	static u64 SliceBytes(const Bucket& Item);
	void CreateArray(Bucket& Item);
	// Gives the pool another count of places: the slices kept stay where they are, the heads of those cut off are
	// let go. The array comes made: creating one stops the thread that asks for a frame or two
	void Resize(u32 Slot, u32 Places, nri::Texture* Texture, nri::Descriptor* View, u32 Frame, nri::CommandBuffer& Commands);
	// Takes places nothing asks for from the end of another pool and gives their bytes to this one: the new arrays
	// are asked of the loading thread, and the pools change when they are made
	bool Rebalance(u32 Slot, float Heat, u32 Frame);
	// The place a head would take in its pool now: a free one, else that of the coldest head it may push out
	u32 FindPlace(const Stream& Item, u32 Frame) const;
	// What the loading thread left: a head to copy into its place, or arrays for pools to move into
	void TakeLoaded(u32 Frame, nri::CommandBuffer& Commands, nri::Streamer& Streamer);
	// Waits for the loading thread and throws away what it made
	void DropLoading();

	// One level of a head as its slice takes it, read by the loading thread
	struct Level
	{
		xr_vector<u8> Data;
		u32 RowBytes = 0;
	};

	// First: the level of the file the slice starts at
	static bool ReadHead(const char* Name, u32 Size, u32 Mips, xr_vector<Level>& Levels, u32 First = 0);
	static void CopySlice(const Bucket& Pool, u32 Slice, const xr_vector<Level>& Levels, nri::CommandBuffer& Commands, nri::Streamer& Streamer);

	// A pool that is to move into another array, made by the loading thread
	struct Moving
	{
		u32 Slot = Absent;
		u32 Places = 0;
		Bucket Made;
	};

	OneJobThread Loader;
	u32 LoadingHead = Absent; // the texture whose head is being read
	bool IsHeadRead = false;
	xr_vector<Level> LoadingLevels;
	Moving MovingPools[2];

	Bucket Buckets[MaxBuckets];
	u32 BucketCount = 0;
	xr_vector<Stream> Streams;
	xr_vector<u32> Heads;
	u32 LastFrame = 0;
	u32 ResizeFrame = 0;
	u64 PoolBudget = 0;
	xr_vector<Retired> RetiredArrays;
};
