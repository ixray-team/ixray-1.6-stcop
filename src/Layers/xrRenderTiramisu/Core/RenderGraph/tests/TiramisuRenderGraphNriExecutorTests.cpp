#include "Core/RenderGraph/TiramisuRenderGraphNriExecutor.h"

#include <cstdlib>
#include <iostream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

using namespace Tiramisu::RenderGraph;

namespace
{
int Failures = 0;

void Check(const bool Condition, const char* Expression, const int Line)
{
	if (Condition)
	{
		return;
	}
	++Failures;
	std::cerr << "line " << Line << ": check failed: " << Expression << '\n';
}

#define EXECUTOR_CHECK(Expression) Check((Expression), #Expression, __LINE__)

template <typename T>
T* MakeHandle()
{
	static std::uintptr_t Next = 0x10000;
	Next += 0x100;
	return reinterpret_cast<T*>(Next);
}

struct FSubmitRecord
{
	nri::Queue* Queue = nullptr;
	xr_vector<xr_pair<nri::Fence*, u64>> Waits;
	xr_vector<xr_pair<nri::Fence*, u64>> Signals;
};

struct FFakeNriState
{
	xr_vector<FSubmitRecord> Submits;
	xr_vector<xr_pair<nri::Fence*, u64>> HostWaits;
	xr_vector<nri::Queue*> IdleQueues;
	u32 AllocatorResetCount = 0;
	u32 BarrierCount = 0;
	u32 BeginCount = 0;
	u32 EndCount = 0;
	u32 AnnotationBeginCount = 0;
	u32 AnnotationEndCount = 0;
};

FFakeNriState Fake;

void ResetFakeState()
{
	Fake = {};
}

nri::Result NRI_CALL FakeCreateCommandAllocator(
	nri::Queue&, nri::CommandAllocator*& Allocator
)
{
	Allocator = MakeHandle<nri::CommandAllocator>();
	return nri::Result::SUCCESS;
}

nri::Result NRI_CALL FakeCreateCommandBuffer(
	nri::CommandAllocator&, nri::CommandBuffer*& CommandBuffer
)
{
	CommandBuffer = MakeHandle<nri::CommandBuffer>();
	return nri::Result::SUCCESS;
}

nri::Result NRI_CALL FakeCreateFence(
	nri::Device&, const u64, nri::Fence*& Fence
)
{
	Fence = MakeHandle<nri::Fence>();
	return nri::Result::SUCCESS;
}

void NRI_CALL FakeDestroyCommandAllocator(nri::CommandAllocator*) {}
void NRI_CALL FakeDestroyCommandBuffer(nri::CommandBuffer*) {}
void NRI_CALL FakeDestroyFence(nri::Fence*) {}

nri::Result NRI_CALL FakeBeginCommandBuffer(
	nri::CommandBuffer&, const nri::DescriptorPool*
)
{
	++Fake.BeginCount;
	return nri::Result::SUCCESS;
}

void NRI_CALL FakeCmdBarrier(nri::CommandBuffer&, const nri::BarrierDesc&)
{
	++Fake.BarrierCount;
}

void NRI_CALL FakeCmdBeginAnnotation(
	nri::CommandBuffer&, const char*, const u32
)
{
	++Fake.AnnotationBeginCount;
}

void NRI_CALL FakeCmdEndAnnotation(nri::CommandBuffer&)
{
	++Fake.AnnotationEndCount;
}

nri::Result NRI_CALL FakeEndCommandBuffer(nri::CommandBuffer&)
{
	++Fake.EndCount;
	return nri::Result::SUCCESS;
}

nri::Result NRI_CALL FakeQueueSubmit(
	nri::Queue& Queue, const nri::QueueSubmitDesc& Desc
)
{
	FSubmitRecord Record;
	Record.Queue = &Queue;
	for (u32 Index = 0; Index < Desc.waitFenceNum; ++Index)
	{
		Record.Waits.emplace_back(
			Desc.waitFences[Index].fence, Desc.waitFences[Index].value
		);
	}
	for (u32 Index = 0; Index < Desc.signalFenceNum; ++Index)
	{
		Record.Signals.emplace_back(
			Desc.signalFences[Index].fence, Desc.signalFences[Index].value
		);
	}
	Fake.Submits.push_back(std::move(Record));
	return nri::Result::SUCCESS;
}

nri::Result NRI_CALL FakeQueueWaitIdle(nri::Queue* Queue)
{
	Fake.IdleQueues.push_back(Queue);
	return nri::Result::SUCCESS;
}

void NRI_CALL FakeWait(nri::Fence& Fence, const u64 Value)
{
	Fake.HostWaits.emplace_back(&Fence, Value);
}

void NRI_CALL FakeResetCommandAllocator(nri::CommandAllocator&)
{
	++Fake.AllocatorResetCount;
}

nri::CoreInterface MakeCoreInterface()
{
	nri::CoreInterface Core = {};
	Core.CreateCommandAllocator = FakeCreateCommandAllocator;
	Core.CreateCommandBuffer = FakeCreateCommandBuffer;
	Core.CreateFence = FakeCreateFence;
	Core.DestroyCommandAllocator = FakeDestroyCommandAllocator;
	Core.DestroyCommandBuffer = FakeDestroyCommandBuffer;
	Core.DestroyFence = FakeDestroyFence;
	Core.BeginCommandBuffer = FakeBeginCommandBuffer;
	Core.CmdBarrier = FakeCmdBarrier;
	Core.CmdBeginAnnotation = FakeCmdBeginAnnotation;
	Core.CmdEndAnnotation = FakeCmdEndAnnotation;
	Core.EndCommandBuffer = FakeEndCommandBuffer;
	Core.QueueSubmit = FakeQueueSubmit;
	Core.QueueWaitIdle = FakeQueueWaitIdle;
	Core.Wait = FakeWait;
	Core.ResetCommandAllocator = FakeResetCommandAllocator;
	return Core;
}

struct FTestGraph
{
	FCompiledGraph Graph;
	FResourceHandle Depth;
	FResourceHandle Visibility;
};

FTestGraph BuildAsyncGraph()
{
	TiramisuRenderGraphBuilder Builder;
	const FResourceHandle Depth = Builder.CreateResource({"Depth", "d32"});
	const FResourceHandle Visibility =
		Builder.CreateResource({"Visibility", "u32_buffer"});
	(void)Builder.AddPass({"Depth", EQueue::Graphics, {{Depth, EAccess::DepthStencilWrite}}});
	(void)Builder.AddPass({"Cull", EQueue::Compute, {{Depth, EAccess::ShaderRead}, {Visibility, EAccess::StorageWrite}}});
	(void)Builder.AddPass({"Draw", EQueue::Graphics, {{Visibility, EAccess::IndirectArgument}}});

	FCompileResult Compiled = Builder.Compile();
	EXECUTOR_CHECK(Compiled.Succeeded());
	if (!Compiled.Succeeded())
	{
		return {};
	}
	return {std::move(*Compiled.Value), Depth, Visibility};
}

bool HasDiagnostic(const FNriGraphExecuteResult& Result, const xr_string_view Text)
{
	for (const xr_string& Diagnostic : Result.Diagnostics)
	{
		if (Diagnostic.find(Text) != xr_string::npos)
		{
			return true;
		}
	}
	return false;
}

xr_vector<FNriPassCallback> MakeCallbacks(
	const FCompiledGraph& Graph, xr_vector<xr_string>& ExecutedPasses
)
{
	xr_vector<FNriPassCallback> Callbacks;
	for (const FCompiledPass& Pass : Graph.Passes)
	{
		Callbacks.push_back({Pass.Handle, [&ExecutedPasses](nri::CommandBuffer&, const FCompiledPass& Executed)
							 {
								 ExecutedPasses.push_back(Executed.Name);
							 }});
	}
	return Callbacks;
}

void TestAsyncExecutionAndFrameReuse()
{
	ResetFakeState();
	FTestGraph Test = BuildAsyncGraph();
	if (Test.Graph.Passes.empty())
	{
		return;
	}

	nri::Device* Device = MakeHandle<nri::Device>();
	nri::Queue* Graphics = MakeHandle<nri::Queue>();
	nri::Queue* Compute = MakeHandle<nri::Queue>();
	const FNriQueueBindings Queues{Graphics, Compute, nullptr};
	const FNriResourceBinding Bindings[] = {
		{.Resource = Test.Depth, .Texture = MakeHandle<nri::Texture>()},
		{.Resource = Test.Visibility, .Buffer = MakeHandle<nri::Buffer>()},
	};

	TiramisuNriRenderGraphExecutor Executor;
	xr_string Diagnostic;
	EXECUTOR_CHECK(Executor.Initialize(
		*Device, MakeCoreInterface(), Queues, 2, Diagnostic
	));
	EXECUTOR_CHECK(Diagnostic.empty());

	xr_vector<xr_string> ExecutedPasses;
	const xr_vector<FNriPassCallback> Callbacks =
		MakeCallbacks(Test.Graph, ExecutedPasses);
	const FNriGraphExecuteResult First = Executor.Execute(
		0, Test.Graph, Bindings, Callbacks
	);
	EXECUTOR_CHECK(First.Succeeded());
	EXECUTOR_CHECK(First.RecordedPassCount == 3);
	EXECUTOR_CHECK(First.SubmittedPassCount == 3);
	EXECUTOR_CHECK(Fake.Submits.size() == 3);
	if (Fake.Submits.size() == 3)
	{
		EXECUTOR_CHECK(Fake.Submits[0].Queue == Graphics);
		EXECUTOR_CHECK(Fake.Submits[0].Waits.empty());
		EXECUTOR_CHECK(Fake.Submits[1].Queue == Compute);
		EXECUTOR_CHECK(Fake.Submits[1].Waits.size() == 1);
		EXECUTOR_CHECK(Fake.Submits[2].Queue == Graphics);
		EXECUTOR_CHECK(Fake.Submits[2].Waits.size() == 1);
	}
	EXECUTOR_CHECK(ExecutedPasses == xr_vector<xr_string>({"Depth", "Cull", "Draw"}));
	EXECUTOR_CHECK(Fake.BeginCount == 3);
	EXECUTOR_CHECK(Fake.EndCount == 3);
	EXECUTOR_CHECK(Fake.AnnotationBeginCount == 3);
	EXECUTOR_CHECK(Fake.AnnotationEndCount == 3);

	nri::Fence* ExternalWaitFence = MakeHandle<nri::Fence>();
	nri::Fence* ExternalSignalFence = MakeHandle<nri::Fence>();
	const nri::FenceSubmitDesc InitialWaits[] = {
		{ExternalWaitFence, 7, nri::StageBits::ALL}
	};
	const nri::FenceSubmitDesc FinalSignals[] = {
		{ExternalSignalFence, 9, nri::StageBits::ALL}
	};
	const FNriExternalQueueSync External[] = {{
		.Queue = EQueue::Graphics,
		.InitialWaits = InitialWaits,
		.FinalSignals = FinalSignals,
	}};

	const FNriGraphExecuteResult Reused = Executor.Execute(
		2, Test.Graph, Bindings, Callbacks, nullptr, External
	);
	EXECUTOR_CHECK(Reused.Succeeded());
	EXECUTOR_CHECK(Fake.HostWaits.size() == 2);
	EXECUTOR_CHECK(Fake.Submits.size() == 6);
	if (Fake.Submits.size() == 6)
	{
		EXECUTOR_CHECK(Fake.Submits[3].Waits.size() == 1);
		EXECUTOR_CHECK(Fake.Submits[3].Waits[0].first == ExternalWaitFence);
		EXECUTOR_CHECK(Fake.Submits[5].Signals.size() == 2);
		EXECUTOR_CHECK(Fake.Submits[5].Signals[1].first == ExternalSignalFence);
		EXECUTOR_CHECK(Fake.Submits[5].Signals[1].second == 9);
	}
	EXECUTOR_CHECK(Fake.AllocatorResetCount == 4);

	Executor.Destroy();
	EXECUTOR_CHECK(Fake.IdleQueues.size() == 2);
}

void TestPreflightValidationDoesNotSubmit()
{
	ResetFakeState();
	FTestGraph Test = BuildAsyncGraph();
	if (Test.Graph.Passes.empty())
	{
		return;
	}

	nri::Device* Device = MakeHandle<nri::Device>();
	nri::Queue* Graphics = MakeHandle<nri::Queue>();
	const FNriResourceBinding Bindings[] = {
		{.Resource = Test.Depth, .Texture = MakeHandle<nri::Texture>()},
		{.Resource = Test.Visibility, .Buffer = MakeHandle<nri::Buffer>()},
	};
	xr_vector<xr_string> ExecutedPasses;
	const xr_vector<FNriPassCallback> Callbacks =
		MakeCallbacks(Test.Graph, ExecutedPasses);

	TiramisuNriRenderGraphExecutor Executor;
	xr_string Diagnostic;
	EXECUTOR_CHECK(Executor.Initialize(*Device, MakeCoreInterface(), {Graphics, nullptr, nullptr}, 1, Diagnostic));
	const FNriGraphExecuteResult MissingQueue = Executor.Execute(
		0, Test.Graph, Bindings, Callbacks
	);
	EXECUTOR_CHECK(!MissingQueue.Succeeded());
	EXECUTOR_CHECK(HasDiagnostic(MissingQueue, "unavailable compute queue"));
	EXECUTOR_CHECK(Fake.Submits.empty());
	EXECUTOR_CHECK(ExecutedPasses.empty());
	Executor.Destroy();

	ResetFakeState();
	nri::Queue* Compute = MakeHandle<nri::Queue>();
	EXECUTOR_CHECK(Executor.Initialize(*Device, MakeCoreInterface(), {Graphics, Compute, nullptr}, 1, Diagnostic));
	const xr_vector<FNriPassCallback> IncompleteCallbacks = {
		Callbacks.front()
	};
	const FNriGraphExecuteResult MissingCallback = Executor.Execute(
		0, Test.Graph, Bindings, IncompleteCallbacks
	);
	EXECUTOR_CHECK(!MissingCallback.Succeeded());
	EXECUTOR_CHECK(HasDiagnostic(MissingCallback, "no executable callback"));
	EXECUTOR_CHECK(Fake.Submits.empty());
	Executor.Destroy();
}

void TestExclusiveOwnershipRequiresPairedBarriers()
{
	ResetFakeState();
	FTestGraph Test = BuildAsyncGraph();
	if (Test.Graph.Passes.empty())
	{
		return;
	}

	nri::Device* Device = MakeHandle<nri::Device>();
	nri::Queue* Graphics = MakeHandle<nri::Queue>();
	nri::Queue* Compute = MakeHandle<nri::Queue>();
	const FNriResourceBinding Bindings[] = {
		{.Resource = Test.Depth, .Texture = MakeHandle<nri::Texture>(), .ExclusiveQueueOwnership = true},
		{.Resource = Test.Visibility, .Buffer = MakeHandle<nri::Buffer>()},
	};
	xr_vector<xr_string> ExecutedPasses;
	const xr_vector<FNriPassCallback> Callbacks =
		MakeCallbacks(Test.Graph, ExecutedPasses);

	TiramisuNriRenderGraphExecutor Executor;
	xr_string Diagnostic;
	EXECUTOR_CHECK(Executor.Initialize(*Device, MakeCoreInterface(), {Graphics, Compute, nullptr}, 1, Diagnostic));
	const FNriGraphExecuteResult Result = Executor.Execute(
		0, Test.Graph, Bindings, Callbacks
	);
	EXECUTOR_CHECK(!Result.Succeeded());
	EXECUTOR_CHECK(HasDiagnostic(Result, "paired release/acquire barriers"));
	EXECUTOR_CHECK(Fake.Submits.empty());
	Executor.Destroy();
}

void TestIncompleteCoreInterfaceIsRejected()
{
	TiramisuNriRenderGraphExecutor Executor;
	xr_string Diagnostic;
	EXECUTOR_CHECK(!Executor.Initialize(*MakeHandle<nri::Device>(), {}, {MakeHandle<nri::Queue>(), nullptr, nullptr}, 1, Diagnostic));
	EXECUTOR_CHECK(Diagnostic.find("incomplete core interface") != xr_string::npos);
}
} // namespace

int main()
{
	TestAsyncExecutionAndFrameReuse();
	TestPreflightValidationDoesNotSubmit();
	TestExclusiveOwnershipRequiresPairedBarriers();
	TestIncompleteCoreInterfaceIsRejected();
	if (Failures != 0)
	{
		return EXIT_FAILURE;
	}
	std::cout << "Tiramisu NRI render-graph executor tests passed.\n";
	return EXIT_SUCCESS;
}
