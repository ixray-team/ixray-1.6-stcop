#pragma once

// nri::Device is not named here: the engine defines Device as a macro
namespace nri
{
struct CommandBuffer;
struct Texture;
struct Descriptor;
struct Pipeline;
struct PipelineLayout;
struct DescriptorPool;
struct Streamer;
} // namespace nri

class GfxCmdList;
class CShaderConstants;

// NRI commands recorded into the old backend's command list or context. The surfaces are the ones
// the commands read and write: the old backend still tracks their state.
nri::CommandBuffer* BeginNriCommands(IRHISurface* const* Reads, u32 ReadCount, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth);
void EndNriCommands(nri::CommandBuffer* Commands);
// The typed DXGI format a depth surface is written through, 0 when it is not a depth surface
u32 NriDepthFormat(ERHI_FORMAT Format);

// Records a pass through NRI while the old backend still owns the resources it reads and writes.
// So far: vertex + pixel shader, no vertex input, colour and depth-stencil targets, textures in the
// pixel stage only.
class NriPass
{
public:
	// Draws with the pass, state, textures and targets set on Cmd. False when the pass needs
	// something this path lacks: the caller then draws through the old backend.
	bool Draw(GfxCmdList& Cmd, u32 VertexCount);
	void EndFrame();
	void Destroy();

private:
	static constexpr u32 FrameCount = 4;
	static constexpr u32 TextureSlots = 64;
	static constexpr u32 BlockSlots = 16;

	static constexpr u32 TargetSlots = 4;

	enum class EView : u8
	{
		Read,
		Color,
		Depth
	};

	struct Wrapped
	{
		IRHISurface* Surface;
		EView Kind;
		nri::Texture* Texture;
		nri::Descriptor* View;
	};

	// Old surfaces die on resize and their addresses get reused, so a wrapper lives for one frame
	// and is destroyed once the GPU cannot be using it any more
	struct Frame
	{
		nri::DescriptorPool* Pool = nullptr;
		xr_vector<Wrapped> Textures;
	};

	// Compared bytewise: only fixed-size fields without padding between them
	struct PipelineKey
	{
		RHIObject* Vertex;
		RHIObject* Pixel;
		u32 Formats[TargetSlots + 1];	// the last one is the depth-stencil format, 0 without one
		u8 Blend[TargetSlots][8];
		u8 Depth[13];					// compare, write, stencil on, read mask, write mask, front and back {fail, depth fail, pass, compare}
		u8 TargetCount;
		u8 Cull;
		u8 Wireframe;
		u8 FrontCounterClockwise;
		u8 AlphaToCoverage;
		u8 Unused[2];
	};

	struct CachedPipeline
	{
		PipelineKey Key;
		nri::Pipeline* Pipeline;
	};

	void Create(const CShaderConstants& Constants);
	void ReleaseFrame(Frame& Item);
	nri::Descriptor* Wrap(IRHISurface* Surface, EView Kind, u32 TypedFormat = 0);
	nri::Pipeline* FindPipeline(const PipelineKey& Key);

	bool IsCreated = false;
	nri::PipelineLayout* Layout = nullptr;
	nri::Streamer* Streamer = nullptr;
	nri::Descriptor* BlockViews[BlockSlots] = {};
	u32 BlockCount = 0;
	Frame Frames[FrameCount];
	u32 FrameIndex = 0;
	xr_vector<CachedPipeline> Pipelines;
};
