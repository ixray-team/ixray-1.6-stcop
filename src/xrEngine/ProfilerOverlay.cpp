#include "stdafx.h"

#ifdef DEBUG_DRAW
class CProfilerOverlay
{
	static constexpr u32 None = CFrameProfiler::None;
	static constexpr u32 Columns = CFrameProfiler::HistorySize - 1;

	enum ESource : u32
	{
		Instant,
		Smooth,
		Worst,
		SourceCount
	};

	float NodeMs[CFrameProfiler::MaxNodes] = {};
	float NodeCalls[CFrameProfiler::MaxNodes] = {};
	ImDrawList* List = nullptr;
	float RowHeight = 0.0f;
	float SelectedMs = 0.0f;
	float Peak = 1.0f;
	u64 Selected = 0;
	u32 Averaged = 0;
	u32 Source = Instant;
	u32 Zoom = None;
	u32 GpuTrack = None;
	bool Frozen = false;
	bool Interactive = false;

public:
	void Draw();

private:
	void RecordGpu();
	void Collect(const u64* Frames, u32 Count);
	void DrawHistory(u64 Current);
	void DrawFlame();
	u32 DrawNode(u32 Node, float X, float Scale, float Top);
	void DrawBlock(u32 Node, float X, float Width, float Y, bool Ancestor);
	ImU32 BlockColor(u32 Node, float Alpha) const;
};

static CProfilerOverlay Overlay;

static float ToMs(u64 Ticks)
{
	return float(double(Ticks) * 1000.0 / double(CPU::qpc_freq));
}

static float FrameMs(u64 Index)
{
	const u32 Slot = u32(Index % CFrameProfiler::HistorySize);
	const u64 Begin = g_FrameProfiler.FrameBegin[Slot];
	const u64 End = g_FrameProfiler.FrameEnd[Slot];
	return Begin != 0 && End > Begin ? ToMs(End - Begin) : 0.0f;
}

void CProfilerOverlay::Draw()
{
	CFrameProfiler& Profiler = g_FrameProfiler;
	const bool Visible = Engine.External.EditorStates[static_cast<u8>(EditorUI::Profiler)];
	Profiler.SetEnabled(Visible);
	if (!Visible)
	{
		return;
	}

	RecordGpu();

	const u64 Current = Profiler.FrameIndex.load(std::memory_order_acquire);
	const u64 Second = CPU::qpc_freq;
	u64 Recent[Columns];
	u32 RecentCount = 0;
	u64 Newest = 0;
	u64 NewestEnd = 0;
	u64 Slowest = 0;
	float SlowestMs = 0.0f;
	float TotalMs = 0.0f;
	u32 Counted = 0;
	for (u64 Age = 1; Age <= Columns && Age <= Current; Age++)
	{
		const u64 Index = Current - Age;
		const u64 End = Profiler.FrameEnd[Index % CFrameProfiler::HistorySize];
		const float Ms = FrameMs(Index);
		if (Ms == 0.0f)
		{
			continue;
		}

		if (NewestEnd == 0)
		{
			NewestEnd = End;
			Newest = Index;
		}

		if (End + Second < NewestEnd)
		{
			break;
		}

		if (End + Second / 2 >= NewestEnd)
		{
			Recent[RecentCount++] = Index;
		}

		TotalMs += Ms;
		Counted++;
		if (Ms > SlowestMs)
		{
			SlowestMs = Ms;
			Slowest = Index;
		}
	}

	if (!Frozen && RecentCount != 0)
	{
		Selected = Source == Worst ? Slowest : Newest;
		Collect(Source == Smooth ? Recent : &Selected, Source == Smooth ? RecentCount : 1);
	}

	Interactive = CImGuiManager::Instance().IsCapturingInputs();
	const ImGuiViewport* Viewport = ImGui::GetMainViewport();
	ImGui::SetNextWindowPos(Viewport->WorkPos);
	ImGui::SetNextWindowSizeConstraints(ImVec2(Viewport->WorkSize.x, 0.0f), Viewport->WorkSize);
	ImGui::SetNextWindowBgAlpha(0.8f);
	constexpr ImGuiWindowFlags Flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav | ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_AlwaysAutoResize;
	if (ImGui::Begin("##Profiler", nullptr, Flags))
	{
		static const char* const Sources[SourceCount] = { "Instant", "Smooth", "Worst" };
		for (u32 Index = 0; Index < SourceCount; Index++)
		{
			if (Index != 0)
			{
				ImGui::SameLine();
			}

			if (ImGui::Selectable(Sources[Index], Source == Index, 0, ImGui::CalcTextSize(Sources[Index])))
			{
				Source = Index;
				Frozen = false;
			}
		}

		const float AverageMs = TotalMs / float(std::max(Counted, 1u));
		ImGui::SameLine(0.0f, 24.0f);
		ImGui::Checkbox("Freeze", &Frozen);
		ImGui::SameLine(0.0f, 24.0f);
		ImGui::Text("%.2f ms avg   %.2f ms worst   %.0f fps", AverageMs, SlowestMs, 1000.0f / std::max(AverageMs, 0.001f));
		ImGui::SameLine(0.0f, 24.0f);
		if (Averaged > 1)
		{
			ImGui::TextDisabled("average of %u frames", Averaged);
		}
		else
		{
			ImGui::TextDisabled("frame %llu", Selected);
		}

		ImGui::SameLine(0.0f, 24.0f);
		ImGui::TextDisabled("Alt to interact: click a frame to freeze it, a block to zoom; the gpu is a few frames late");
		List = ImGui::GetWindowDrawList();
		RowHeight = ImGui::GetTextLineHeight() + 2.0f;
		DrawHistory(Current);
		DrawFlame();
	}

	ImGui::End();
}

void CProfilerOverlay::RecordGpu()
{
	if (GpuTrack == None)
	{
		GpuTrack = g_FrameProfiler.AddTrack("GPU");
	}

	GRHI->GPUStatsEnable = true;
	const RHI_GPU_EVENT& Events = GRHI->GPUStats();
	for (u64 Index = 0; Index < Events.count; Index++)
	{
		const RHI_GPU_EVENT_STATS& Event = Events.events[Index];
		if (Event.name.c_str() != nullptr)
		{
			const u64 Duration = Event.freq != 0 && Event.end > Event.begin ? u64(double(Event.end - Event.begin) * double(CPU::qpc_freq) / double(Event.freq)) : 0;
			g_FrameProfiler.Record(GpuTrack, u32(Event.stack), Event.name.c_str(), Duration);
		}
	}
}

void CProfilerOverlay::Collect(const u64* Frames, u32 Count)
{
	CFrameProfiler& Profiler = g_FrameProfiler;
	const u32 NodeCount = Profiler.NodeCount();
	const double Scale = 1000.0 / double(CPU::qpc_freq) / double(Count);
	memset(NodeMs, 0, NodeCount * sizeof(float));
	memset(NodeCalls, 0, NodeCount * sizeof(float));
	SelectedMs = 0.0f;
	for (u32 Frame = 0; Frame < Count; Frame++)
	{
		const u32 Slot = u32(Frames[Frame] % CFrameProfiler::HistorySize);
		SelectedMs += FrameMs(Frames[Frame]) / float(Count);
		for (u32 Node = 0; Node < NodeCount; Node++)
		{
			NodeMs[Node] += float(double(Profiler.Ticks[Slot][Node]) * Scale);
			NodeCalls[Node] += float(Profiler.Calls[Slot][Node]) / float(Count);
		}
	}

	for (u32 TrackId = 0; TrackId < Profiler.TrackCount(); TrackId++)
	{
		const u32 Root = Profiler.Tracks[TrackId].Root;
		for (u32 Child = Profiler.Nodes[Root].FirstChild; Child != None; Child = Profiler.Nodes[Child].NextSibling)
		{
			NodeMs[Root] += NodeMs[Child];
		}
	}

	Averaged = Count;
}

void CProfilerOverlay::DrawHistory(u64 Current)
{
	const ImVec2 Corner = ImGui::GetCursorScreenPos();
	const float Width = ImGui::GetContentRegionAvail().x;
	const float Height = 48.0f;
	const float Step = Width / float(Columns);
	const float Bottom = Corner.y + Height;
	float Highest = 1.0f;
	for (u32 Column = 0; Column < Columns; Column++)
	{
		Highest = std::max(Highest, Current + Column >= Columns ? FrameMs(Current + Column - Columns) : 0.0f);
	}

	Peak += (Highest - Peak) * std::min(ImGui::GetIO().DeltaTime * 6.0f, 1.0f);
	const float Scale = Height / Peak;
	for (const float Target : { 1000.0f / 60.0f, 1000.0f / 30.0f })
	{
		if (Target < Peak)
		{
			List->AddLine(ImVec2(Corner.x, Bottom - Target * Scale), ImVec2(Corner.x + Width, Bottom - Target * Scale), IM_COL32(255, 255, 255, 64));
		}
	}

	for (u32 Column = 0; Column < Columns; Column++)
	{
		if (Current + Column < Columns)
		{
			continue;
		}

		const u64 Index = Current + Column - Columns;
		const float X = Corner.x + float(Column) * Step;
		const float Right = X + std::max(Step - 1.0f, 1.0f);
		List->AddRectFilled(ImVec2(X, Bottom - std::min(FrameMs(Index) * Scale, Height)), ImVec2(Right, Bottom), Index == Selected && Averaged == 1 ? IM_COL32_WHITE : IM_COL32(144, 144, 144, 255));
		if (Zoom != None)
		{
			const float ZoomMs = ToMs(g_FrameProfiler.Ticks[Index % CFrameProfiler::HistorySize][Zoom]);
			List->AddRectFilled(ImVec2(X, Bottom - std::min(ZoomMs * Scale, Height)), ImVec2(Right, Bottom), BlockColor(Zoom, 1.0f));
		}
	}

	if (Interactive && ImGui::IsMouseHoveringRect(Corner, ImVec2(Corner.x + Width, Bottom)))
	{
		const u32 Column = std::min(u32((ImGui::GetIO().MousePos.x - Corner.x) / Step), Columns - 1);
		const u64 Index = Current + Column - Columns;
		if (Current + Column >= Columns && FrameMs(Index) != 0.0f)
		{
			ImGui::SetTooltip("frame %llu   %.2f ms", Index, FrameMs(Index));
			if (ImGui::IsMouseClicked(ImGuiMouseButton_Left))
			{
				Selected = Index;
				Collect(&Selected, 1);
				Frozen = true;
			}
		}
	}

	ImGui::Dummy(ImVec2(Width, Height));
}

void CProfilerOverlay::DrawFlame()
{
	CFrameProfiler& Profiler = g_FrameProfiler;
	const ImVec2 Corner = ImGui::GetCursorScreenPos();
	const float Width = ImGui::GetContentRegionAvail().x;
	const u32 View = Zoom;
	float Top = Corner.y;
	if (View != None)
	{
		for (u32 Node = Profiler.Nodes[View].Parent; Node != None; Node = Profiler.Nodes[Node].Parent)
		{
			DrawBlock(Node, Corner.x, Width, Top + float(Profiler.Nodes[Node].Depth) * RowHeight, true);
		}

		Top += float(DrawNode(View, Corner.x, Width / std::max(NodeMs[View], 0.001f), Top) + 1) * RowHeight;
	}
	else
	{
		float Span = SelectedMs;
		for (u32 TrackId = 0; TrackId < Profiler.TrackCount(); TrackId++)
		{
			Span = std::max(Span, NodeMs[Profiler.Tracks[TrackId].Root]);
		}

		for (u32 TrackId = 0; TrackId < Profiler.TrackCount(); TrackId++)
		{
			const u32 Root = Profiler.Tracks[TrackId].Root;
			if (NodeMs[Root] > 0.0f)
			{
				Top += float(DrawNode(Root, Corner.x, Width / std::max(Span, 0.001f), Top) + 1) * RowHeight + 4.0f;
			}
		}
	}

	ImGui::Dummy(ImVec2(Width, Top - Corner.y));
}

u32 CProfilerOverlay::DrawNode(u32 Node, float X, float Scale, float Top)
{
	const CFrameProfiler::Node& Item = g_FrameProfiler.Nodes[Node];
	const float Width = NodeMs[Node] * Scale;
	DrawBlock(Node, X, Width, Top + float(Item.Depth) * RowHeight, false);
	u32 Deepest = Item.Depth;
	if (Width < 2.0f)
	{
		return Deepest;
	}

	for (u32 Child = Item.FirstChild; Child != None; Child = g_FrameProfiler.Nodes[Child].NextSibling)
	{
		if (NodeMs[Child] > 0.0f)
		{
			Deepest = std::max(Deepest, DrawNode(Child, X, Scale, Top));
			X += NodeMs[Child] * Scale;
		}
	}

	return Deepest;
}

void CProfilerOverlay::DrawBlock(u32 Node, float X, float Width, float Y, bool Ancestor)
{
	CFrameProfiler& Profiler = g_FrameProfiler;
	const CFrameProfiler::Node& Item = Profiler.Nodes[Node];
	const ImVec2 Min(X, Y);
	const ImVec2 Max(X + std::max(Width, 1.0f), Y + RowHeight - 1.0f);
	List->AddRectFilled(Min, Max, BlockColor(Node, Ancestor ? 0.4f : 1.0f));
	if (Max.x - Min.x > 16.0f)
	{
		string256 Label;
		xr_sprintf(Label, "%s  %.2f ms", Item.Name, NodeMs[Node]);
		List->PushClipRect(Min, Max, true);
		List->AddText(ImVec2(X + 2.0f, Y + 1.0f), IM_COL32_BLACK, Label);
		List->PopClipRect();
	}

	if (!Interactive || !ImGui::IsMouseHoveringRect(Min, Max))
	{
		return;
	}

	float Children = 0.0f;
	for (u32 Child = Item.FirstChild; Child != None; Child = Profiler.Nodes[Child].NextSibling)
	{
		Children += NodeMs[Child];
	}

	List->AddRect(Min, Max, IM_COL32_WHITE);
	ImGui::BeginTooltip();
	ImGui::TextUnformatted(Item.Name);
	ImGui::TextDisabled("%s", Profiler.Tracks[Item.Track].Name);
	ImGui::Text("%.3f ms   self %.3f ms   %.1f calls", NodeMs[Node], NodeMs[Node] - Children, NodeCalls[Node]);
	if (Item.Parent != None)
	{
		ImGui::Text("%.1f%% of %s   %.1f%% of frame", 100.0f * NodeMs[Node] / std::max(NodeMs[Item.Parent], 0.001f), Profiler.Nodes[Item.Parent].Name, 100.0f * NodeMs[Node] / std::max(SelectedMs, 0.001f));

		const u64 Current = Profiler.FrameIndex.load(std::memory_order_relaxed);
		float Total = 0.0f;
		float Highest = 0.0f;
		u32 Samples = 0;
		for (u64 Age = 1; Age <= Columns && Age <= Current; Age++)
		{
			const u64 Index = Current - Age;
			const float Ms = ToMs(Profiler.Ticks[Index % CFrameProfiler::HistorySize][Node]);
			if (FrameMs(Index) != 0.0f)
			{
				Total += Ms;
				Highest = std::max(Highest, Ms);
				Samples++;
			}
		}

		ImGui::Text("avg %.3f ms   peak %.3f ms   over %u frames", Total / float(std::max(Samples, 1u)), Highest, Samples);
	}

	ImGui::TextDisabled("%s", Node == Zoom ? "click to zoom out" : Item.Depth == 0 ? "click to show every track" : "click to zoom");
	ImGui::EndTooltip();
	if (ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		const u32 Target = Node == Zoom ? Item.Parent : Node;
		Zoom = Target != None && Profiler.Nodes[Target].Depth != 0 ? Target : None;
	}
}

ImU32 CProfilerOverlay::BlockColor(u32 Node, float Alpha) const
{
	const CFrameProfiler::Node& Item = g_FrameProfiler.Nodes[Node];
	ImVec4 Color(0.7f, 0.7f, 0.7f, Alpha);
	if (Item.Depth != 0)
	{
		const u32 Hash = u32(std::hash<xr_string_view>()(Item.Name));
		ImGui::ColorConvertHSVtoRGB((Item.Track == GpuTrack ? 0.52f : 0.0f) + float(Hash & 0xFF) / 255.0f * 0.12f, 0.5f + float((Hash >> 8) & 0xFF) / 255.0f * 0.25f, 0.95f, Color.x, Color.y, Color.z);
	}

	return ImGui::ColorConvertFloat4ToU32(Color);
}

void DrawProfilerOverlay()
{
	Overlay.Draw();
}
#endif
