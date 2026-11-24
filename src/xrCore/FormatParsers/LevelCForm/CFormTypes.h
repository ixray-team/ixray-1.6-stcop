#pragma once

enum class CFormVersions : u32
{
	Vanilla = 4,
	VanillaChunked,
	VanillaChunkedData,
	Instanced,
	InstancedChunked,
	InstancedChunkedData,
	// Keep existing version numbers stable. Streamed masters contain metadata only.
	Streamed,
	StreamedInstanced
};

#pragma pack(push, 8)
struct hdrCFORM
{
	CFormVersions version;
	u32 vertcount;
	u32 facecount;
	Fbox aabb;
};
#pragma pack(pop)
