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
#include <atomic>

// nri::Device is not named here: the engine defines Device as a macro
namespace nri
{
struct Texture;
struct Descriptor;
struct TextureDesc;
struct TextureSubresourceUploadDesc;
struct AccessLayoutStage;
} // namespace nri

class IRHISurface;
struct RHITextureDesc;
struct RHISubResource;

// A texture the renderer makes, fills, views and destroys through NRI. Once filled it is read by shaders for the rest of
// its life; it is destroyed once the GPU has run the frame that let it go. The old backend draws it through a surface that
// adopts its native texture
class GpuResource
{
public:
	enum class EState : u8
	{
		Undefined,
		Read
	};

	GpuResource() = default;
	GpuResource(GpuResource&& Other) noexcept;
	GpuResource& operator=(GpuResource&& Other) noexcept;
	~GpuResource();

	// Levels: every subresource, the mips of a layer after those of the one before. D3D11 fills a texture made off the
	// primary thread at the primary's next Ready
	bool Create(const nri::TextureDesc& Desc, const nri::TextureSubresourceUploadDesc* Levels, bool IsCubeMap, const char* Name);
	// The renderer's own small uncompressed textures, as the old backend described them
	bool Create(const RHITextureDesc& Desc, const RHISubResource* Levels, const char* Name);
	// A DDS file, without the mips the texture quality leaves out; Bytes: what was uploaded
	bool Load(const char* Name, u32& Bytes);
	// A texture filled elsewhere and left read by shaders
	void Take(nri::Texture& Made);
	void Reset();
	// Filled; on the primary thread a fill that waits is done now
	bool Ready();
	// The surface the old backend draws it through, a new reference
	IRHISurface* Adopt() const;

	static nri::AccessLayoutStage ReadState();

	nri::Texture* Texture = nullptr;
	nri::Descriptor* View = nullptr;
	EState State = EState::Undefined;
	bool IsCube = false;

private:
	struct Waiting;

	bool MakeView();
	bool Fill(const nri::TextureSubresourceUploadDesc* Levels);
	bool Record(const nri::TextureSubresourceUploadDesc* Levels);

	std::atomic<Waiting*> Late = nullptr;
};
