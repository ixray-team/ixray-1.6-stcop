#pragma once

#include "../../../xrCore/OffsetAllocator.h"
#include "../../../xrEngine/Render.h"
#include "../../../xrRHI/RHI.h"

namespace ImUtils::Viewer
{
	// Dedicated, budget-capped GPU storage for the viewer.
	//
	// The viewer owns a budget split into vertices, indices, and constants.
	// All three storage classes use shared backing buffers suballocated by byte
	// offset. Full level assets have separate budgets. Exhaustion returns an
	// invalid buffer instead of failing a render or a game update.
	class CViewerGpuPool final : public IRenderRuntimeBufferAllocator
	{
	public:
		struct SBuffer
		{
			IRHIBuffer* Buffer = nullptr;
			u32         Offset = COffsetAllocator::InvalidOffset;
			u32         Size = 0;
			bool        IsValid() const { return Buffer != nullptr && Offset != COffsetAllocator::InvalidOffset; }
		};

		~CViewerGpuPool() { Shutdown(); }
		bool	Init(u32 budgetBytes);
		void	Shutdown();
		bool	IsReady() const { return VertexAllocator.IsInitialized() && VertexBuffer != nullptr && IndexBuffer != nullptr && ConstantBuffer != nullptr; }

		// All three return an invalid SBuffer on budget exhaustion (and log it).
		SBuffer	CreateVertexBuffer(const void* data, u32 bytes);
		SBuffer	CreateIndexBuffer(const void* data, u32 bytes);
		SBuffer	CreateConstantBuffer(u32 bytes);
		void	Destroy(SBuffer& buffer, bool indexBuffer = false, bool constantBuffer = false);

		SRuntimeBufferSlice	Allocate(const void* data, u32 bytes, bool isIndex) override;
		void				Release(const SRuntimeBufferSlice& slice) override;

		u32							BudgetBytes() const { return Budget; }
		COffsetAllocator::SStats	VertexStats() const { return VertexAllocator.GetStats(); }
		COffsetAllocator::SStats	IndexStats() const { return IndexAllocator.GetStats(); }
		COffsetAllocator::SStats	ConstantStats() const { return ConstantAllocator.GetStats(); }

		static u32	DefaultBudgetBytes();	// from the viewer_gpu_budget_kb cvar

	private:
		SBuffer	CreateBuffer(COffsetAllocator& allocator, const void* data, u32 bytes, bool index, bool constant);

		COffsetAllocator	VertexAllocator;
		COffsetAllocator	IndexAllocator;
		COffsetAllocator	ConstantAllocator;
		IRHIBuffer*         VertexBuffer = nullptr;
		IRHIBuffer*         IndexBuffer = nullptr;
		IRHIBuffer*         ConstantBuffer = nullptr;
		u32					Budget = 0;
	};

	// Registers the viewer_gpu_budget_kb console variable. Called from
	// RegisterViewerConsoleCommand().
	void	RegisterViewerCvars();
}
