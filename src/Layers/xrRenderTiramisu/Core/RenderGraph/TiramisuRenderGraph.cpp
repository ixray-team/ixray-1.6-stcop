#include "TiramisuRenderGraph.h"

#include <algorithm>
#include <functional>
#include <queue>
#include <set>
#include <unordered_set>

namespace Tiramisu::RenderGraph
{
namespace
{
void AddDiagnostic(FCompileResult& Result, xr_string Code, xr_string Message)
{
	Result.Diagnostics.push_back({std::move(Code), std::move(Message)});
}

bool NeedsBarrier(const EAccess Before, const EAccess After, const bool QueueTransfer)
{
	if (After == EAccess::None)
	{
		return false;
	}
	// A first use still needs an undefined/unknown -> requested-state transition.
	if (Before == EAccess::None)
	{
		return true;
	}
	// Different read layouts and exclusive queue ownership also need an explicit
	// transition even when neither side writes.
	return Before != After || IsWriteAccess(Before) || IsWriteAccess(After) ||
		   QueueTransfer;
}
} // namespace

bool IsReadAccess(const EAccess Access) noexcept
{
	switch (Access)
	{
		case EAccess::ShaderRead:
		case EAccess::StorageRead:
		case EAccess::DepthStencilRead:
		case EAccess::CopySource:
		case EAccess::IndirectArgument:
		case EAccess::Present:
			return true;
		default:
			return false;
	}
}

bool IsWriteAccess(const EAccess Access) noexcept
{
	switch (Access)
	{
		case EAccess::StorageWrite:
		case EAccess::ColorAttachmentWrite:
		case EAccess::DepthStencilWrite:
		case EAccess::CopyDestination:
			return true;
		default:
			return false;
	}
}

bool IsAccessSupportedByQueue(const EAccess Access, const EQueue Queue) noexcept
{
	switch (Access)
	{
		case EAccess::ShaderRead:
		case EAccess::StorageRead:
		case EAccess::StorageWrite:
		case EAccess::IndirectArgument:
			return Queue == EQueue::Graphics || Queue == EQueue::Compute;
		case EAccess::ColorAttachmentWrite:
		case EAccess::DepthStencilRead:
		case EAccess::DepthStencilWrite:
		case EAccess::Present:
			return Queue == EQueue::Graphics;
		case EAccess::CopySource:
		case EAccess::CopyDestination:
			return true;
		case EAccess::None:
			return false;
	}
	return false;
}

xr_string_view ToString(const EQueue Queue) noexcept
{
	switch (Queue)
	{
		case EQueue::Graphics:
			return "graphics";
		case EQueue::Compute:
			return "compute";
		case EQueue::Copy:
			return "copy";
	}
	return "unknown";
}

xr_string_view ToString(const EAccess Access) noexcept
{
	switch (Access)
	{
		case EAccess::None:
			return "none";
		case EAccess::ShaderRead:
			return "shader_read";
		case EAccess::StorageRead:
			return "storage_read";
		case EAccess::StorageWrite:
			return "storage_write";
		case EAccess::ColorAttachmentWrite:
			return "color_attachment_write";
		case EAccess::DepthStencilRead:
			return "depth_stencil_read";
		case EAccess::DepthStencilWrite:
			return "depth_stencil_write";
		case EAccess::CopySource:
			return "copy_source";
		case EAccess::CopyDestination:
			return "copy_destination";
		case EAccess::IndirectArgument:
			return "indirect_argument";
		case EAccess::Present:
			return "present";
	}
	return "unknown";
}

FResourceHandle TiramisuRenderGraphBuilder::CreateResource(FResourceDesc Desc)
{
	Desc.Imported = false;
	const FResourceHandle Handle{static_cast<u32>(Resources.size()), Generation};
	Resources.push_back(std::move(Desc));
	return Handle;
}

FResourceHandle TiramisuRenderGraphBuilder::ImportResource(FResourceDesc Desc)
{
	Desc.Imported = true;
	Desc.Transient = false;
	const FResourceHandle Handle{static_cast<u32>(Resources.size()), Generation};
	Resources.push_back(std::move(Desc));
	return Handle;
}

FPassHandle TiramisuRenderGraphBuilder::AddPass(FPassDesc Desc)
{
	const FPassHandle Handle{static_cast<u32>(Passes.size()), Generation};
	Passes.push_back(std::move(Desc));
	return Handle;
}

void TiramisuRenderGraphBuilder::AddDependency(const FPassHandle Before, const FPassHandle After)
{
	Dependencies.push_back({Before, After});
}

FCompileResult TiramisuRenderGraphBuilder::Compile() const
{
	FCompileResult Result;
	if (Passes.empty())
	{
		AddDiagnostic(Result, "render_graph.empty", "Render graph contains no passes.");
		return Result;
	}

	auto ValidResource = [this](const FResourceHandle Handle)
	{
		return Handle.Generation == Generation && Handle.Index < Resources.size();
	};
	auto ValidPass = [this](const FPassHandle Handle)
	{
		return Handle.Generation == Generation && Handle.Index < Passes.size();
	};

	xr_set<xr_string> PassNames;
	for (u32 PassIndex = 0; PassIndex < Passes.size(); ++PassIndex)
	{
		const FPassDesc& Pass = Passes[PassIndex];
		if (Pass.Name.empty())
		{
			AddDiagnostic(Result, "render_graph.empty_pass_name", "Render graph pass has no name.");
		}
		else if (!PassNames.insert(Pass.Name).second)
		{
			AddDiagnostic(Result, "render_graph.duplicate_pass_name", "Render graph contains duplicate pass name '" + Pass.Name + "'.");
		}
		if (Pass.Uses.empty())
		{
			AddDiagnostic(Result, "render_graph.empty_pass", "Pass '" + Pass.Name + "' has no resource uses.");
		}

		xr_set<u32> UsedResources;
		for (const FResourceUse& Use : Pass.Uses)
		{
			if (!ValidResource(Use.Resource))
			{
				AddDiagnostic(Result, "render_graph.invalid_resource", "Pass '" + Pass.Name + "' references an invalid or stale resource handle.");
				continue;
			}
			if (Use.Access == EAccess::None ||
				(!IsReadAccess(Use.Access) && !IsWriteAccess(Use.Access)))
			{
				AddDiagnostic(Result, "render_graph.invalid_access", "Pass '" + Pass.Name + "' has an invalid access declaration.");
			}
			else if (!IsAccessSupportedByQueue(Use.Access, Pass.Queue))
			{
				AddDiagnostic(Result, "render_graph.queue_access_mismatch", "Pass '" + Pass.Name + "' cannot use access '" + xr_string(ToString(Use.Access)) + "' on the " + xr_string(ToString(Pass.Queue)) + " queue.");
			}
			if (!UsedResources.insert(Use.Resource.Index).second)
			{
				AddDiagnostic(Result, "render_graph.duplicate_use", "Pass '" + Pass.Name + "' declares the same resource more than once.");
			}
		}
	}

	for (const FResourceDesc& Resource : Resources)
	{
		if (Resource.Name.empty())
		{
			AddDiagnostic(Result, "render_graph.empty_resource_name", "Render graph resource has no name.");
		}
	}
	if (!Result.Diagnostics.empty())
	{
		return Result;
	}

	xr_vector<xr_set<u32>> Edges(Passes.size());
	auto AddEdge = [&Edges](const u32 Before, const u32 After)
	{
		Edges[Before].insert(After);
	};
	for (const FDependency& Dependency : Dependencies)
	{
		if (!ValidPass(Dependency.Before) || !ValidPass(Dependency.After))
		{
			AddDiagnostic(Result, "render_graph.invalid_dependency", "Render graph contains an invalid or stale explicit dependency.");
			continue;
		}
		AddEdge(Dependency.Before.Index, Dependency.After.Index);
	}
	if (!Result.Diagnostics.empty())
	{
		return Result;
	}

	struct FResourceHazards
	{
		xr_optional<u32> LastWriter;
		xr_set<u32> Readers;
		bool Initialized = false;
	};
	xr_vector<FResourceHazards> Hazards(Resources.size());
	for (u32 ResourceIndex = 0; ResourceIndex < Resources.size(); ++ResourceIndex)
	{
		Hazards[ResourceIndex].Initialized = Resources[ResourceIndex].Imported;
	}

	for (u32 PassIndex = 0; PassIndex < Passes.size(); ++PassIndex)
	{
		for (const FResourceUse& Use : Passes[PassIndex].Uses)
		{
			FResourceHazards& Hazard = Hazards[Use.Resource.Index];
			if (IsReadAccess(Use.Access))
			{
				if (!Hazard.Initialized)
				{
					AddDiagnostic(Result, "render_graph.read_before_write", "Transient resource '" + Resources[Use.Resource.Index].Name + "' is read before its first write.");
				}
				if (Hazard.LastWriter)
				{
					AddEdge(*Hazard.LastWriter, PassIndex);
				}
				Hazard.Readers.insert(PassIndex);
			}
			else
			{
				if (Hazard.LastWriter)
				{
					AddEdge(*Hazard.LastWriter, PassIndex);
				}
				for (const u32 Reader : Hazard.Readers)
				{
					AddEdge(Reader, PassIndex);
				}
				Hazard.Readers.clear();
				Hazard.LastWriter = PassIndex;
				Hazard.Initialized = true;
			}
		}
	}
	if (!Result.Diagnostics.empty())
	{
		return Result;
	}

	xr_vector<u32> Indegree(Passes.size(), 0);
	for (const auto& Destinations : Edges)
	{
		for (const u32 Destination : Destinations)
		{
			++Indegree[Destination];
		}
	}

	std::priority_queue<u32, xr_vector<u32>, std::greater<>> Ready;
	for (u32 PassIndex = 0; PassIndex < Passes.size(); ++PassIndex)
	{
		if (Indegree[PassIndex] == 0)
		{
			Ready.push(PassIndex);
		}
	}

	xr_vector<u32> Order;
	Order.reserve(Passes.size());
	while (!Ready.empty())
	{
		const u32 PassIndex = Ready.top();
		Ready.pop();
		Order.push_back(PassIndex);
		for (const u32 Destination : Edges[PassIndex])
		{
			if (--Indegree[Destination] == 0)
			{
				Ready.push(Destination);
			}
		}
	}
	if (Order.size() != Passes.size())
	{
		AddDiagnostic(Result, "render_graph.cycle", "Render graph pass dependencies contain a cycle.");
		return Result;
	}

	FCompiledGraph Graph;
	Graph.Passes.reserve(Passes.size());
	xr_vector<u32> Ordinal(Passes.size());
	for (u32 Position = 0; Position < Order.size(); ++Position)
	{
		const u32 PassIndex = Order[Position];
		Ordinal[PassIndex] = Position;
		FCompiledPass Compiled;
		Compiled.Handle = {PassIndex, Generation};
		Compiled.Name = Passes[PassIndex].Name;
		Compiled.Queue = Passes[PassIndex].Queue;
		for (u32 Source = 0; Source < Edges.size(); ++Source)
		{
			if (Edges[Source].contains(PassIndex))
			{
				Compiled.Dependencies.push_back({Source, Generation});
			}
		}
		Graph.Passes.push_back(std::move(Compiled));
	}

	Graph.Resources.reserve(Resources.size());
	for (u32 ResourceIndex = 0; ResourceIndex < Resources.size(); ++ResourceIndex)
	{
		xr_optional<u32> First;
		u32 Last = 0;
		for (u32 PassIndex = 0; PassIndex < Passes.size(); ++PassIndex)
		{
			const auto Use = std::find_if(Passes[PassIndex].Uses.begin(), Passes[PassIndex].Uses.end(), [ResourceIndex](const FResourceUse& Candidate)
										  { return Candidate.Resource.Index == ResourceIndex; });
			if (Use == Passes[PassIndex].Uses.end())
			{
				continue;
			}
			const u32 Position = Ordinal[PassIndex];
			First = First ? std::min(*First, Position) : Position;
			Last = std::max(Last, Position);
		}
		if (!First)
		{
			AddDiagnostic(Result, "render_graph.unused_resource", "Resource '" + Resources[ResourceIndex].Name + "' is never used.");
			continue;
		}
		Graph.Resources.push_back({{ResourceIndex, Generation}, *First, Last, FCompiledResource::NoAliasSlot});
	}
	if (!Result.Diagnostics.empty())
	{
		return Result;
	}

	struct FAliasSlot
	{
		xr_string CompatibilityClass;
		u32 LastPass = 0;
	};
	xr_vector<FAliasSlot> AliasSlots;
	xr_vector<FCompiledResource*> AliasCandidates;
	for (FCompiledResource& Resource : Graph.Resources)
	{
		const FResourceDesc& Desc = Resources[Resource.Handle.Index];
		if (Desc.Transient && !Desc.Imported && !Desc.CompatibilityClass.empty())
		{
			AliasCandidates.push_back(&Resource);
		}
	}
	std::ranges::sort(AliasCandidates, [](const FCompiledResource* Left, const FCompiledResource* Right)
					  {
        if (Left->FirstPass != Right->FirstPass){ return Left->FirstPass < Right->FirstPass;
}
        return Left->Handle.Index < Right->Handle.Index; });
	for (FCompiledResource* Resource : AliasCandidates)
	{
		const xr_string& CompatibilityClass = Resources[Resource->Handle.Index].CompatibilityClass;
		xr_optional<u32> SlotIndex;
		for (u32 Index = 0; Index < AliasSlots.size(); ++Index)
		{
			if (AliasSlots[Index].CompatibilityClass == CompatibilityClass &&
				AliasSlots[Index].LastPass < Resource->FirstPass)
			{
				SlotIndex = Index;
				break;
			}
		}
		if (!SlotIndex)
		{
			SlotIndex = static_cast<u32>(AliasSlots.size());
			AliasSlots.push_back({CompatibilityClass, Resource->LastPass});
		}
		else
		{
			AliasSlots[*SlotIndex].LastPass = Resource->LastPass;
		}
		Resource->AliasSlot = *SlotIndex;
	}
	Graph.AliasSlotCount = static_cast<u32>(AliasSlots.size());

	struct FLastState
	{
		EAccess Access = EAccess::None;
		FPassHandle Pass;
		EQueue Queue = EQueue::Graphics;
	};
	xr_vector<FLastState> LastStates(Resources.size());
	for (const FCompiledPass& CompiledPass : Graph.Passes)
	{
		const FPassDesc& Pass = Passes[CompiledPass.Handle.Index];
		for (const FResourceUse& Use : Pass.Uses)
		{
			FLastState& Last = LastStates[Use.Resource.Index];
			const bool QueueTransfer = Last.Access != EAccess::None &&
									   Last.Queue != Pass.Queue;
			if (NeedsBarrier(Last.Access, Use.Access, QueueTransfer))
			{
				Graph.Barriers.push_back({Use.Resource, Last.Access, Use.Access, Last.Pass, CompiledPass.Handle, QueueTransfer});
			}
			Last.Access = Use.Access;
			Last.Pass = CompiledPass.Handle;
			Last.Queue = Pass.Queue;
		}
	}

	Graph.Submissions.reserve(Graph.Passes.size());
	for (const FCompiledPass& Pass : Graph.Passes)
	{
		FQueueSubmission Submission;
		Submission.Pass = Pass.Handle;
		Submission.Queue = Pass.Queue;

		auto AddWait = [&Submission](const FPassHandle SourcePass, const EQueue SourceQueue)
		{
			const auto Existing = std::find_if(Submission.Waits.begin(), Submission.Waits.end(), [SourcePass](const FQueueWait& Wait)
											   { return Wait.SourcePass == SourcePass; });
			if (Existing == Submission.Waits.end())
			{
				Submission.Waits.push_back({SourcePass, SourceQueue});
			}
		};

		for (const FPassHandle Dependency : Pass.Dependencies)
		{
			const auto Source = std::find_if(Graph.Passes.begin(), Graph.Passes.end(), [Dependency](const FCompiledPass& Candidate)
											 { return Candidate.Handle == Dependency; });
			if (Source != Graph.Passes.end() && Source->Queue != Pass.Queue)
			{
				AddWait(Source->Handle, Source->Queue);
			}
		}
		for (const FBarrier& Barrier : Graph.Barriers)
		{
			if (Barrier.DestinationPass == Pass.Handle)
			{
				Submission.Barriers.push_back(Barrier);
				if (Barrier.QueueTransfer && Barrier.SourcePass.IsValid())
				{
					const auto Source = std::find_if(Graph.Passes.begin(), Graph.Passes.end(), [&Barrier](const FCompiledPass& Candidate)
													 { return Candidate.Handle == Barrier.SourcePass; });
					if (Source != Graph.Passes.end())
					{
						AddWait(Source->Handle, Source->Queue);
					}
				}
			}
		}

		Graph.Submissions.push_back(std::move(Submission));
	}

	Result.Value.emplace(std::move(Graph));
	return Result;
}

void TiramisuRenderGraphBuilder::Reset() noexcept
{
	Resources.clear();
	Passes.clear();
	Dependencies.clear();
	Generation = Generation == std::numeric_limits<u32>::max() ? 1 : Generation + 1;
}
} // namespace Tiramisu::RenderGraph
