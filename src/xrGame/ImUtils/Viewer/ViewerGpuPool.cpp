#include "StdAfx.h"
#include "ViewerGpuPool.h"

#include "../../xrEngine/xr_ioc_cmd.h"

namespace ImUtils::Viewer
{
namespace ViewerGpuPoolDetail
{
	// Configurable GPU budget for the viewer, in KiB (default 4096 = 4 MB).
	int g_ViewerGpuBudgetKb = 4096;

	class CCC_ViewerGpuBudget : public IConsole_Command
	{
	public:
		CCC_ViewerGpuBudget(const char* N) : IConsole_Command(N) {}

		virtual void Execute(const char* args) override
		{
			if (args == nullptr || args[0] == 0)
			{
				Msg("* viewer_gpu_budget_kb = %d KiB", g_ViewerGpuBudgetKb);
				return;
			}
			int value = atoi(args);
			clamp(value, 64, 1024 * 1024);
			g_ViewerGpuBudgetKb = value;
			Msg("* viewer_gpu_budget_kb = %d KiB (applies to the next viewer session)", g_ViewerGpuBudgetKb);
		}
		virtual void Info(TInfo& I) override { xr_strcpy(I, "<budget in KiB> - GPU memory budget for the level viewer (default 4096 = 4 MB)"); }
		virtual void Status(TStatus& S) override { xr_sprintf(S, "%d", g_ViewerGpuBudgetKb); }
	};
}

using ViewerGpuPoolDetail::g_ViewerGpuBudgetKb;
using ViewerGpuPoolDetail::CCC_ViewerGpuBudget;

u32 CViewerGpuPool::DefaultBudgetBytes()
{
	return u32(g_ViewerGpuBudgetKb) * 1024u;
}

void RegisterViewerCvars()
{
	CMD1(CCC_ViewerGpuBudget, "viewer_gpu_budget_kb");
}

bool CViewerGpuPool::Init(u32 budgetBytes)
{
	Shutdown();
	if (budgetBytes < 64 * 1024)
	{
		Msg("! ViewerGpuPool: budget %u bytes is too small, need at least 64 KiB", budgetBytes);
		return false;
	}

	Budget = budgetBytes;
	const u32 constantBytes = (u32(Budget * 0.15f) + 255u) & ~255u;
	const u32 indexBytes = u32(Budget * 0.30f);
	VertexAllocator.Init(Budget - indexBytes - constantBytes);
	IndexAllocator.Init(indexBytes);
	ConstantAllocator.Init(constantBytes);
	if (!RHIUtils::CreateVertexBuffer(&VertexBuffer, nullptr, VertexAllocator.GetStats().Capacity, false) ||
		!RHIUtils::CreateIndexBuffer(&IndexBuffer, nullptr, IndexAllocator.GetStats().Capacity, false) ||
		!RHIUtils::CreateConstantBuffer(&ConstantBuffer, ConstantAllocator.GetStats().Capacity))
	{
		Shutdown();
		return false;
	}
	Msg("* ViewerGpuPool: budget %.2f MB (VB %.2f / IB %.2f / CB %.2f MB)",
		float(Budget) / 1048576.f,
		float(VertexAllocator.GetStats().Capacity) / 1048576.f,
		float(IndexAllocator.GetStats().Capacity) / 1048576.f,
		float(ConstantAllocator.GetStats().Capacity) / 1048576.f);
	return true;
}

void CViewerGpuPool::Shutdown()
{
	_RELEASE(VertexBuffer);
	_RELEASE(IndexBuffer);
	_RELEASE(ConstantBuffer);
	VertexAllocator.Reset();
	IndexAllocator.Reset();
	ConstantAllocator.Reset();
	Budget = 0;
}

CViewerGpuPool::SBuffer CViewerGpuPool::CreateBuffer(COffsetAllocator& allocator, const void* data, u32 bytes, bool index, bool constant)
{
	SBuffer result;
	if (!allocator.IsInitialized())
	{
		Msg("! ViewerGpuPool: pool not initialized");
		return result;
	}

	result.Offset = allocator.Alloc(bytes, constant ? 256u : 16u);
	if (result.Offset == COffsetAllocator::InvalidOffset)
	{
		Msg("! ViewerGpuPool: %s pool exhausted (%u bytes requested, %u free)",
			constant ? "constant" : (index ? "index" : "vertex"), bytes, allocator.GetStats().Free);
		return result;
	}

	if (!constant)
	{
		result.Buffer = index ? IndexBuffer : VertexBuffer;
		RHIMappedSubresource mapped = {};
		if (result.Buffer == nullptr || data == nullptr || !result.Buffer->Map(ERHI_BUFFER_MAP::WRITE_NO_OVERWRITE, 0, &mapped))
		{
			allocator.Free(result.Offset);
			result.Offset = COffsetAllocator::InvalidOffset;
			result.Buffer = nullptr;
			return result;
		}
		Memory.mem_copy(static_cast<u8*>(mapped.pData) + result.Offset, data, bytes);
		result.Buffer->Unmap();
		result.Size = bytes;
		return result;
	}

	result.Buffer = ConstantBuffer;
	result.Size = bytes;
	return result;
}

CViewerGpuPool::SBuffer CViewerGpuPool::CreateVertexBuffer(const void* data, u32 bytes)
{
	return CreateBuffer(VertexAllocator, data, bytes, false, false);
}

CViewerGpuPool::SBuffer CViewerGpuPool::CreateIndexBuffer(const void* data, u32 bytes)
{
	return CreateBuffer(IndexAllocator, data, bytes, true, false);
}

CViewerGpuPool::SBuffer CViewerGpuPool::CreateConstantBuffer(u32 bytes)
{
	return CreateBuffer(ConstantAllocator, nullptr, bytes, false, true);
}

void CViewerGpuPool::Destroy(SBuffer& buffer, bool indexBuffer, bool constantBuffer)
{
	buffer.Buffer = nullptr;
	if (buffer.Offset != COffsetAllocator::InvalidOffset)
	{
		COffsetAllocator& allocator = constantBuffer ? ConstantAllocator : (indexBuffer ? IndexAllocator : VertexAllocator);
		allocator.Free(buffer.Offset);
		buffer.Offset = COffsetAllocator::InvalidOffset;
	}
	buffer.Size = 0;
}

SRuntimeBufferSlice CViewerGpuPool::Allocate(const void* data, u32 bytes, bool isIndex)
{
	const SBuffer buffer = CreateBuffer(isIndex ? IndexAllocator : VertexAllocator, data, bytes, isIndex, false);
	return { buffer.Buffer, buffer.Offset, isIndex };
}

void CViewerGpuPool::Release(const SRuntimeBufferSlice& slice)
{
	if (slice.Buffer == nullptr || slice.Offset == COffsetAllocator::InvalidOffset)
		return;
	(slice.IsIndex ? IndexAllocator : VertexAllocator).Free(slice.Offset);
}

} // namespace ImUtils::Viewer
