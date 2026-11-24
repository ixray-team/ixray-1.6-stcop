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

// nri::Device is not named here: the engine defines Device as a macro
namespace nri
{
struct CommandBuffer;
struct Buffer;
struct Texture;
struct Descriptor;
struct Pipeline;
struct PipelineLayout;
struct DescriptorPool;
struct DescriptorSet;
struct DescriptorRangeDesc;
struct AttachmentDesc;
struct Streamer;
struct SamplerDesc;
} // namespace nri

class GfxCmdList;
class CShaderConstants;

// A register of a compute pass its caller binds by hand. A read: a surface of the old backend, or a view made through
// NRI. A write: one mip of a surface, a buffer through its storage view, or none where the shader's writes to a texture
// are dropped. A buffer written is handed over read by shaders and taken back so; IsNew: made and not used yet
struct NriBinding
{
	u32 Slot;
	IRHISurface* Surface;
	u32 Mip = 0;
	nri::Descriptor* View = nullptr;
	nri::Buffer* Buffer = nullptr;
	bool IsNew = false;
};

// Why a draw is not drawn: each is an error of its caller, told once
enum class ENriDecline : u8
{
	None,
	Pass,
	Stages,
	Bindings,
	Depth,
	Target,
	Samples,
	Read,
	EmptyRead,
	Blend,
	Wrap,
	Reads,
	Pipeline,
	Sets,
	Input,
	Topology,
	Size,
	Count
};

// A draw of the dynamic geometry stream: the vertex shader pulls its records through Chunk at g_RingVertices
struct NriDynamicDraw
{
	nri::Descriptor* Chunk;
	ERHI_PRIMITIVE_TOPOLOGY Topology;
	u32 VertexCount;
	u32 InstanceCount;
};

// What a frame put through NRI; rs_stats shows the frame before
struct NriStats
{
	u32 Dynamic = 0;
	u32 Dropped = 0;
	u32 Runs = 0;
	u32 Submits = 0;
};

// NRI commands recorded into the old backend's command list or context. The surfaces are the ones
// the commands read and write: the old backend still tracks their state.
nri::CommandBuffer* BeginNriCommands(IRHISurface* const* Reads, u32 ReadCount, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth);
void EndNriCommands(nri::CommandBuffer* Commands);
// D3D11 records consecutive dynamic draws into one run and replays it before anything else reaches the context. True
// when a run was replayed: the context is left with NRI's shaders, viewport and scissor
RENDER_API bool FlushNriRun();
// Multisampled surfaces of the old backend into its plain targets, a pair an entry
void ResolveNri(IRHIRenderTargetView* const* Targets, IRHISurface* const* Sources, u32 Count);
// A whole surface into one of its size and format, resolved when only the source is multisampled. A typeless colour or
// a multisampled depth is not copied
void CopyNri(IRHISurface* Dest, IRHISurface* Source);
// A typeless colour target or a slice view of an array the renderer does not know is not cleared
void ClearNri(IRHIRenderTargetView* Target, const float* Color);
void ClearNri(IRHIDepthStencilView* Target, ERHI_CLEAR_TARGET Flags, float Depth, u8 Stencil);
// The typed DXGI format a depth surface is written through, 0 when it is not a depth surface
u32 NriDepthFormat(ERHI_FORMAT Format);
// A sampler state of the old backend as NRI describes it
nri::SamplerDesc NriSampler(const RHISampleDesc& Source);

// Records a pass through NRI while the old backend still owns the resources it reads and writes: vertex and pixel
// shader, colour and depth-stencil targets, textures in the pixel stage and buffers NRI has views of, with no vertex
// input (fullscreen passes) or the dynamic geometry stream's; compute shaders that read textures and structured
// buffers and write flat textures and structured buffers. A register read with nothing bound reads a zero view of its kind.
class NriPass
{
public:
	// Draws with the pass, state, textures and targets set on Cmd
	void Draw(GfxCmdList& Cmd, u32 VertexCount);
	// Fullscreen triangles of the pass set on Cmd into its targets, in one pass: each reads its own surface where the pass
	// reads its texture, with the scissor and constants set on Cmd when it is added. Reads are every surface the tiles
	// read. False when none is drawn
	bool BeginTiles(GfxCmdList& Cmd, IRHISurface* const* Reads, u32 Count);
	void AddTile(GfxCmdList& Cmd, IRHISurface* Read);
	void EndTiles(GfxCmdList& Cmd);
	// A draw of the dynamic geometry stream with the pass, state, textures, targets and constants set on Cmd. False
	// when it is not drawn
	bool DrawDynamic(GfxCmdList& Cmd, const NriDynamicDraw& Item);
	// The same draw resolved now and kept for the frame: its constants streamed, its views, set and pipeline made. Its
	// number among the frame's batches, ~0u when it is not drawn
	u32 RecordDynamic(GfxCmdList& Cmd, const NriDynamicDraw& Item);
	// A batch of this frame, in any order with the others; one recorded before a surface was forgotten is not drawn
	void SubmitDynamic(GfxCmdList& Cmd, u32 Batch);
	void SubmitDynamic(GfxCmdList& Cmd, u32 First, u32 Count);
	// A draw that is not drawn, told once a reason; Register is the stage (0 pixel, 1 vertex) times 256 and the register
	void Drop(const GfxCmdList& Cmd, ENriDecline Reason, u32 Register = ~0u);
	// The compute twin of Draw: the shader with the compute textures and constants set on Cmd, and Reads and Writes. The
	// compute textures are let go after it; the graphics shaders set on Cmd stay bound
	RENDER_API void Dispatch(GfxCmdList& Cmd, SCS* Shader, const NriBinding* Reads, u32 ReadCount, const NriBinding* Writes, u32 WriteCount, u32 GroupsX, u32 GroupsY, u32 GroupsZ,
		nri::Buffer* Arguments = nullptr, u64 ArgumentsOffset = 0);
	// Every mip of a colour target below its first from the one above it, in the layers set in Layers
	void GenerateMips(GfxCmdList& Cmd, IRHISurface* Surface, u64 Layers);
	// Records the CPU wrote for this frame's passes, as a structured buffer that lives as long as the frame's wrappers
	nri::Descriptor* StreamRecords(const void* Data, u32 Bytes, u32 Stride);
	void EndFrame();
	void Destroy();
	// Lets go of every frame's wrappers once the GPU is done with them; pipelines and layout stay
	void ReleaseTextures();
	// A target destroyed between resets: its wrappers of the last frames would hold its memory. D3D12 keeps them until
	// their frame slot comes round, as the GPU may still read through them
	void Forget(IRHISurface* Surface);
	// A view for reading a surface of the old backend in a pass recorded elsewhere; it lives as long as the
	// frame's other wrappers. Null before the first Draw
	nri::Descriptor* WrapRead(IRHISurface* Surface);
	// A structured view of a buffer of the old backend, handed over readable now; it lives as the one above
	nri::Descriptor* WrapRead(IRHIBuffer* Buffer, u32 Stride);

	NriStats Counting;
	NriStats Shown;

private:
	static constexpr u32 FrameCount = 4;
	static constexpr u32 TextureSlots = 64;
	static constexpr u32 BlockSlots = 16;
	static constexpr u32 SetSlots = 512;

	static constexpr u32 TargetSlots = 4;
	static constexpr u32 StorageSlots = 8;
	// The ring, the skinned vertices and the geometry pages, from g_RingVertices on
	static constexpr u32 VertexSlots = 7;
	static constexpr u32 DynamicReads = 32;
	static constexpr u32 DynamicSets = 512;

	template <class T>
	struct BytesHash
	{
		size_t operator()(const T& Key) const { return std::hash<std::string_view>{}(std::string_view(reinterpret_cast<const char*>(&Key), sizeof(T))); }
	};

	template <class T>
	struct BytesEqual
	{
		bool operator()(const T& Left, const T& Right) const { return !std::memcmp(&Left, &Right, sizeof(T)); }
	};

	// The keys are compared and hashed bytewise: fixed-size fields without padding between them
	template <class K, class V>
	using BytesMap = std::unordered_map<K, V, BytesHash<K>, BytesEqual<K>, xalloc<std::pair<const K, V>>>;

	enum class EView : u8
	{
		Read,
		Color,
		Depth,
		Storage
	};

	// By the native texture: every wrapper holds a reference to it, so no other texture takes its address while the
	// wrapper lives, as another surface object of the old backend takes a released one's
	struct WrapKey
	{
		void* Native;
		u32 Format;
		u32 Kind;
		u32 Slice;
		u32 Mip;
	};

	struct Wrapped
	{
		nri::Texture* Texture;
		nri::Descriptor* View;
	};

	// The views a dynamic draw reads by register: of the pixel stage, then of the vertex stage, the chunk of records it
	// pulls among them
	struct SetKey
	{
		u64 PixelSlots;
		u64 VertexSlots;
		nri::Descriptor* Views[DynamicReads];
	};

	struct DynamicSet
	{
		nri::DescriptorSet* Set;
		nri::DescriptorPool* Pool;
	};

	struct Records
	{
		nri::Buffer* Buffer;
		nri::Descriptor* View;
		u32 Bytes;
		u32 Stride;
	};

	struct BufferWrap
	{
		void* Native;
		nri::Buffer* Buffer;
		nri::Descriptor* View;
	};

	// Pools of one kind of a frame, one added when the last is full
	struct Pools
	{
		xr_vector<nri::DescriptorPool*> Items;
		u32 Count = 0;
	};

	// Old surfaces die on resize and their addresses get reused, so a wrapper lives for one frame
	// and is destroyed once the GPU cannot be using it any more. Record buffers stay for the frame slot's next turn
	struct Frame
	{
		Pools Passes;
		BytesMap<WrapKey, Wrapped> Textures;
		xr_vector<BufferWrap> Borrowed;
		xr_vector<Records> Buffers;
		u32 BufferCount = 0;
		// A UI frame draws hundreds of textures: dynamic draws share a set where they hold the same
		Pools Dynamic;
		BytesMap<SetKey, DynamicSet> Sets;
	};

	struct ComputePipeline
	{
		RHIObject* Shader;
		nri::PipelineLayout* Layout;
		nri::Pipeline* Pipeline;
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
		// ERHI_PRIMITIVE_TOPOLOGY of a dynamic draw, 0 for a fullscreen pass
		u8 Topology;
		u8 Samples;
	};

	// What a draw binds, as set on the command list
	struct Collected
	{
		IRHIRenderTargetView* Targets[TargetSlots];
		u32 TargetSlices[TargetSlots];
		u32 TargetCount;
		IRHIDepthStencilView* Depth;
		u32 DepthFormat;
		u32 DepthSlice;
		IRHISurface* Reads[TextureSlots];
		u32 ReadFormats[TextureSlots];
		u32 ReadSlots[TextureSlots];
		u32 ReadCount;
		// The registers the pixel shader reads, and those of them it is given a zero view at, of the kind it reads
		u64 Slots;
		u64 Empty;
		const ESlotKind* Kinds;
		// Views NRI made of what the old backend binds by hand, by register: of the pixel and the vertex stage; the
		// vertex shader's reads of nothing
		u64 PixelGiven;
		u64 VertexGiven;
		u64 VertexEmpty;
		nri::Descriptor* Given[2][TextureSlots];
		// Of a draw not drawn: the stage (0 pixel, 1 vertex) times 256 and the register
		u32 Declined;
		PipelineKey Key;
	};

	// The pass BeginTiles opened
	struct TileRun
	{
		nri::CommandBuffer* Commands;
		nri::DescriptorPool* Pool;
		Collected Bound;
		nri::Descriptor* Views[TextureSlots];
	};

	// What a run of dynamic draws has bound so far; a new run binds all of it
	struct RunState
	{
		u32 Epoch = ~0u;
		nri::Pipeline* Pipeline;
		nri::DescriptorPool* Pool;
		nri::DescriptorSet* Set;
		u32 Offsets[BlockSlots];
		nri::Descriptor* Colors[TargetSlots];
		nri::Descriptor* Depth;
		u32 ColorCount;
		RHIViewport Viewport;
		Irect Scissor;
		u32 StencilRef;
	};

	// A dynamic draw with all it binds resolved; the surfaces are for the old backend's tracking
	struct DynamicBatch
	{
		nri::Pipeline* Pipeline;
		nri::DescriptorPool* Pool;
		nri::DescriptorSet* Set;
		u32 Offsets[BlockSlots];
		nri::Descriptor* Colors[TargetSlots];
		nri::Descriptor* Depth;
		IRHIRenderTargetView* Targets[TargetSlots];
		IRHIDepthStencilView* DepthTarget;
		u32 ColorCount;
		RHIViewport Viewport;
		Irect Scissor;
		bool HasStencil;
		u32 StencilRef;
		u32 VertexCount;
		u32 InstanceCount;
		u32 FirstRead;
		u32 ReadCount;
	};

	void Create(const CShaderConstants& Constants);
	void CreateZeros();
	void CreateDynamic();
	void ReleaseFrame(Frame& Item);
	void DropBatches();
	// The pool the sets were made from, null when NRI cannot make another pool
	nri::DescriptorPool* AllocateSets(Pools& From, bool IsDynamic, const nri::PipelineLayout& SetLayout, nri::DescriptorSet** Sets, u32 Count);
	ENriDecline Collect(GfxCmdList& Cmd, Collected& Out, bool IsDynamic) const;
	ENriDecline MakeViews(Collected& Bound, nri::Descriptor** Views);
	const DynamicSet* FindSet(const SetKey& Key, nri::Descriptor* const* PixelViews, nri::Descriptor* const* VertexViews);
	void Attach(const Collected& Bound, nri::AttachmentDesc* Colors, nri::AttachmentDesc& Depth);
	void Rects(const GfxCmdList& Cmd, const Collected& Bound, RHIViewport& Viewport, Irect& Scissor) const;
	bool ResolveDynamic(GfxCmdList& Cmd, const NriDynamicDraw& Item, Collected& Bound, DynamicBatch& Out);
	void IssueDynamic(GfxCmdList& Cmd, const DynamicBatch& Item, IRHISurface* const* Reads);
	nri::Descriptor* Wrap(IRHISurface* Surface, EView Kind, u32 TypedFormat = 0, u32 Slice = 0, u32 Mip = 0);
	nri::Pipeline* FindPipeline(const PipelineKey& Key);
	nri::Pipeline* CreatePipeline(const PipelineKey& Key);
	nri::PipelineLayout* CreateLayout(const nri::DescriptorRangeDesc* Ranges, u32 RangeCount, u32 Stages) const;
	const ComputePipeline* FindCompute(const SCS& Shader);
	nri::Pipeline* FindMipPipeline(u32 Format);

	bool IsCreated = false;
	nri::PipelineLayout* Layout = nullptr;
	nri::Streamer* Streamer = nullptr;
	nri::Descriptor* BlockViews[BlockSlots] = {};
	u32 BlockRegisters[BlockSlots] = {};
	u32 BlockCount = 0;
	// The constants dynamic draws take: a block is streamed again only where it changed since the last one
	xr_vector<u8> LastBlocks;
	u32 LastBlockOffsets[BlockSlots] = {};
	u32 DynamicOffsets[BlockSlots] = {};
	bool HasDynamicBlocks = false;
	nri::PipelineLayout* DynamicLayout = nullptr;
	// The DRAWID stream of pulled vertex shaders: the instance's own number
	nri::Buffer* Iota = nullptr;
	RunState Run;
	TileRun Tiles = {};
	xr_vector<DynamicBatch> Batches;
	xr_vector<IRHISurface*> BatchReads;
	// Read by a pass where the old backend binds nothing, by the kind the register reads: it read zero there. A flat
	// texture, one of six layers for the arrays and cubes, a volume; a buffer of zeros
	nri::Texture* Zeros[3] = {};
	nri::Buffer* ZeroBuffer = nullptr;
	nri::Descriptor* ZeroViews[u32(ESlotKind::Count)] = {};
	// Written by a compute pass where the old backend binds nothing to a register: a texel outside it is never stored
	nri::Texture* Dropped = nullptr;
	nri::Descriptor* DroppedView = nullptr;
	Frame Frames[FrameCount];
	u32 FrameIndex = 0;
	BytesMap<PipelineKey, nri::Pipeline*> Pipelines;
	PipelineKey LastKey = {};
	nri::Pipeline* LastPipeline = nullptr;
	xr_vector<ComputePipeline> ComputePipelines;
	nri::PipelineLayout* MipLayout = nullptr;
	ref_vs MipVertex;
	ref_ps MipPixel;
	xr_vector<std::pair<u32, nri::Pipeline*>> MipPipelines;
};
