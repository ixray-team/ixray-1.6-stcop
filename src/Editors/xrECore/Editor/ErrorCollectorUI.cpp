#include "stdafx.h"

class ErrorCollector :
	public IEditorWnd
{
private:
	struct ErrorEntry
	{
		xr_string FullMessage;
		xr_string FirstTime;
		xr_string LastTime;
		xr_string Summary;
		xr_string FileLine;
		xr_string Function;
		bool bFatal = false;
		u32 Count = 1;
	};

	xr_vector<ErrorEntry> Entries;
	u32 TotalCount = 0;
	ImGuiTextFilter Filter;
	bool AutoScroll = true;
	bool bScrollToBottom = false;
	xrCriticalSection Lock;

	static void AddErrorToCollector(const char* FullErrorMessage);

	static xr_string GetField(const xr_string& Message, const char* Label)
	{
		const size_t Pos = Message.find(Label);
		if (Pos == xr_string::npos)
		{
			return {};
		}

		const size_t Start = Pos + xr_strlen(Label);
		size_t End = Message.find('\n', Start);

		if (End == xr_string::npos)
		{
			End = Message.size();
		}

		xr_string Result = Message.substr(Start, End - Start);

		while (!Result.empty() && (Result.back() == '\r' || Result.back() == ' ' || Result.back() == '\t'))
		{
			Result.pop_back();
		}

		return Result;
	}

	static xr_string BuildSummary(const xr_string& Message)
	{
		xr_string Description = GetField(Message, "Description   : ");
		xr_string Expression = GetField(Message, "Expression    : ");
		xr_string Arguments = GetField(Message, "Arguments     : ");
		xr_string Function = GetField(Message, "Function      : ");

		if (Description == "<no expression>")
		{
			Description.clear();
		}

		if (!Description.empty() && !Arguments.empty())
		{
			return Description + " " + Arguments;
		}

		if (!Description.empty())
		{
			return Description;
		}

		if (!Arguments.empty())
		{
			return Arguments;
		}

		if (!Expression.empty() && Expression != "fatal error")
		{
			return Expression;
		}

		if (!Function.empty())
		{
			return Function;
		}

		return "Unknown error";
	}

	static xr_string FormatEntry(const ErrorEntry& Entry)
	{
		xr_string Result = "[" + Entry.LastTime + "] ";

		if (Entry.Count > 1)
		{
			Result += "(x" + xr_string::ToString(Entry.Count) + ") ";
		}

		return Result + Entry.Summary + " | " + Entry.FileLine;
	}

	bool PassFilter(const ErrorEntry& Entry) const
	{
		if (!Filter.IsActive())
		{
			return true;
		}

		return Filter.PassFilter(Entry.Summary.c_str()) || Filter.PassFilter(Entry.FileLine.c_str()) || Filter.PassFilter(Entry.FullMessage.c_str());
	}

	void DrawToolbar()
	{
		if (ImGui::Button("Clear"))
		{
			Entries.clear();
			TotalCount = 0;
		}

		ImGui::SameLine();

		ImGui::BeginDisabled(Entries.empty());
		if (ImGui::Button("Copy All"))
		{
			xr_string All;

			for (const ErrorEntry& Entry : Entries)
			{
				if (PassFilter(Entry))
				{
					All += FormatEntry(Entry) + "\n";
				}
			}

			ImGui::SetClipboardText(All.c_str());
		}
		ImGui::EndDisabled();

		ImGui::SameLine();
		XRay::ImGui::ToggleButton("Auto Scroll", &AutoScroll, { 0, 0 });

		ImGui::SameLine();
		ImGui::AlignTextToFramePadding();
		if (TotalCount != Entries.size())
		{
			ImGui::TextDisabled("%u errors (%u unique)", TotalCount, (u32)Entries.size());
		}
		else
		{
			ImGui::TextDisabled("%u errors", TotalCount);
		}

		ImGui::SameLine();
		Filter.Draw("##filter", -1.0f);

		if (!Filter.IsActive())
		{
			ImVec2 Min = ImGui::GetItemRectMin();
			ImGui::GetWindowDrawList()->AddText(ImVec2(Min.x + ImGui::GetStyle().FramePadding.x, Min.y + ImGui::GetStyle().FramePadding.y), ImGui::GetColorU32(ImGuiCol_TextDisabled), "Search... (inc,-exc)");
		}

		if (GUIManager->SearchIcon)
		{
			ImVec2 IconSize = { 14, 14 };
			ImVec2 Max = ImGui::GetItemRectMax();
			ImVec2 Min = ImGui::GetItemRectMin();
			ImVec2 Pos = { Max.x - IconSize.x - 6.0f, Min.y + (Max.y - Min.y - IconSize.y) * 0.5f };
			ImGui::GetWindowDrawList()->AddImage(GUIManager->SearchIcon, Pos, ImVec2(Pos.x + IconSize.x, Pos.y + IconSize.y));
		}
	}

	void DrawDetails(const ErrorEntry& Entry)
	{
		if (!ImGui::BeginTable("##details", 2, ImGuiTableFlags_SizingFixedFit))
		{
			return;
		}

		ImGui::TableSetupColumn("Name", ImGuiTableColumnFlags_WidthFixed);
		ImGui::TableSetupColumn("Value", ImGuiTableColumnFlags_WidthStretch);

		auto PrintRow = [&](const char* Name, const char* Label)
		{
			xr_string Value = GetField(Entry.FullMessage, Label);
			if (Value.empty())
			{
				return;
			}

			ImGui::TableNextRow();
			ImGui::TableSetColumnIndex(0);
			ImGui::TextDisabled("%s", Name);
			ImGui::TableSetColumnIndex(1);
			ImGui::TextWrapped("%s", Value.c_str());
		};

		PrintRow("Expression", "Expression    : ");
		PrintRow("Description", "Description   : ");
		PrintRow("Arguments", "Arguments     : ");
		PrintRow("Function", "Function      : ");
		PrintRow("File", "File          : ");
		PrintRow("Line", "Line          : ");

		if (Entry.Count > 1)
		{
			ImGui::TableNextRow();
			ImGui::TableSetColumnIndex(0);
			ImGui::TextDisabled("Occurrences");
			ImGui::TableSetColumnIndex(1);
			ImGui::Text("%u (first %s, last %s)", Entry.Count, Entry.FirstTime.c_str(), Entry.LastTime.c_str());
		}

		ImGui::EndTable();

		if (ImGui::SmallButton("Copy Full"))
		{
			ImGui::SetClipboardText(Entry.FullMessage.c_str());
		}

		ImGui::Spacing();
	}

	void DrawContextMenu(const ErrorEntry& Entry)
	{
		if (!ImGui::BeginPopupContextItem("##ctx"))
		{
			return;
		}

		if (ImGui::MenuItem("Copy Message"))
		{
			ImGui::SetClipboardText(Entry.Summary.c_str());
		}

		if (ImGui::MenuItem("Copy Location"))
		{
			ImGui::SetClipboardText(Entry.FileLine.c_str());
		}

		if (ImGui::MenuItem("Copy Full"))
		{
			ImGui::SetClipboardText(Entry.FullMessage.c_str());
		}

		ImGui::EndPopup();
	}

	void DrawTable()
	{
		const ImGuiTableFlags Flags = ImGuiTableFlags_BordersOuter | ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_RowBg |
			ImGuiTableFlags_Resizable | ImGuiTableFlags_ScrollY | ImGuiTableFlags_SizingFixedFit;

		if (!ImGui::BeginTable("ErrorsTable", 3, Flags))
		{
			return;
		}

		const float TimeWidth = ImGui::CalcTextSize("00:00:00").x + ImGui::GetStyle().CellPadding.x * 2.0f;

		ImGui::TableSetupScrollFreeze(0, 1);
		ImGui::TableSetupColumn("Time", ImGuiTableColumnFlags_WidthFixed | ImGuiTableColumnFlags_NoResize, TimeWidth);
		ImGui::TableSetupColumn("Error", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableSetupColumn("Location", ImGuiTableColumnFlags_WidthFixed, 260.0f);
		ImGui::TableHeadersRow();

		for (int Index = 0; Index < (int)Entries.size(); ++Index)
		{
			ErrorEntry& Entry = Entries[Index];

			if (!PassFilter(Entry))
			{
				continue;
			}

			ImGui::PushID(Index);
			ImGui::TableNextRow();

			ImGui::TableSetColumnIndex(0);
			ImGui::AlignTextToFramePadding();
			ImGui::TextDisabled("%s", Entry.LastTime.c_str());

			ImGui::TableSetColumnIndex(1);

			const ImVec4 Color = Entry.bFatal ? ImVec4(1.0f, 0.45f, 0.45f, 1.0f) : ImVec4(1.0f, 0.85f, 0.35f, 1.0f);
			ImGui::PushStyleColor(ImGuiCol_Text, Color);
			const ImGuiTreeNodeFlags NodeFlags = ImGuiTreeNodeFlags_SpanAllColumns | ImGuiTreeNodeFlags_FramePadding;
			const bool bOpenDetails = Entry.Count > 1
				? ImGui::TreeNodeEx("##entry", NodeFlags, "%s  [x%u]", Entry.Summary.c_str(), Entry.Count)
				: ImGui::TreeNodeEx("##entry", NodeFlags, "%s", Entry.Summary.c_str());
			ImGui::PopStyleColor();

			if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayNormal))
			{
				ImGui::SetTooltip("%s", Entry.Summary.c_str());
			}

			DrawContextMenu(Entry);

			ImGui::TableSetColumnIndex(2);
			ImGui::TextColored(ImVec4(0.55f, 0.75f, 1.0f, 1.0f), "%s", Entry.FileLine.c_str());

			if (!Entry.Function.empty() && ImGui::IsItemHovered())
			{
				ImGui::SetTooltip("%s", Entry.Function.c_str());
			}

			if (bOpenDetails)
			{
				ImGui::TableNextRow();
				ImGui::TableSetColumnIndex(1);
				DrawDetails(Entry);
				ImGui::TreePop();
			}

			ImGui::PopID();
		}

		if (bScrollToBottom && AutoScroll)
		{
			ImGui::SetScrollHereY(1.0f);
		}
		bScrollToBottom = false;

		ImGui::EndTable();
	}

public:
	ErrorCollector()
	{
		Debug.SilentErrorMode = true;
		Debug.SendErrorCallback = AddErrorToCollector;
	}

	virtual void Draw() override
	{
		if (!bOpen)
		{
			return;
		}

		ImGui::SetNextWindowSize(ImVec2(900, 550), ImGuiCond_FirstUseEver);

		if (!ImGui::Begin("Error Collector", &bOpen))
		{
			ImGui::End();
			return;
		}

		xrCriticalSectionGuard Guard(Lock);

		DrawToolbar();
		ImGui::Separator();

		if (Entries.empty())
		{
			const char* Text = "No errors";
			const ImVec2 Avail = ImGui::GetContentRegionAvail();
			const ImVec2 Size = ImGui::CalcTextSize(Text);
			ImGui::SetCursorPos(ImVec2(ImGui::GetCursorPosX() + (Avail.x - Size.x) * 0.5f, ImGui::GetCursorPosY() + (Avail.y - Size.y) * 0.5f));
			ImGui::TextDisabled("%s", Text);
		}
		else
		{
			DrawTable();
		}

		ImGui::End();
	}

	void Add(const char* FullErrorMessage);
};

static ErrorCollector Collector;

void ErrorCollector::AddErrorToCollector(const char* FullErrorMessage)
{
	static bool RegisteredWnd = false;

	if (!RegisteredWnd)
	{
		EContext.UI->Push(&Collector, false);
		RegisteredWnd = true;
	}

	Collector.bOpen = true;
	Collector.Add(FullErrorMessage);
}

void ErrorCollector::Add(const char* FullErrorMessage)
{
	auto Now = std::chrono::system_clock::now();
	auto TimeNow = xr_chrono_to_time_t(Now);

	char Buffer[64]{};
	std::tm LocalTm{};
	localtime_s(&LocalTm, &TimeNow);
	strftime(Buffer, sizeof(Buffer), "%H:%M:%S", &LocalTm);

	xrCriticalSectionGuard Guard(Lock);

	++TotalCount;

	for (ErrorEntry& Existing : Entries)
	{
		if (Existing.FullMessage == FullErrorMessage)
		{
			++Existing.Count;
			Existing.LastTime = Buffer;
			return;
		}
	}

	ErrorEntry Entry;
	Entry.FullMessage = FullErrorMessage;
	Entry.FirstTime = Buffer;
	Entry.LastTime = Buffer;

	const xr_string& Message = Entry.FullMessage;
	Entry.Summary = BuildSummary(Message);

	const xr_string Expression = GetField(Message, "Expression    : ");
	Entry.bFatal = Expression == "fatal error" || Entry.Summary.Contains("fatal") || Entry.Summary.Contains("not found");

	xr_string File = GetField(Message, "File          : ");
	xr_string Line = GetField(Message, "Line          : ");
	Entry.Function = GetField(Message, "Function      : ");

	if (!File.empty())
	{
		File = xr_path(File).xfilename();
	}

	Entry.FileLine = (!File.empty() && !Line.empty()) ? File + ":" + Line : xr_string("Unknown location");

	Entries.push_back(std::move(Entry));
	bScrollToBottom = true;
}
