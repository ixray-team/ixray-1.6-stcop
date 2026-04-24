#include "stdafx.h"

class ErrorCollector :
	public IEditorWnd
{
private:
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

	static xr_string BuildShortDescription(const xr_string& Message)
	{
		xr_string Description = GetField(Message, "Description   : ");
		xr_string Expression = GetField(Message, "Expression    : ");
		xr_string Arguments = GetField(Message, "Arguments     : ");
		xr_string Function = GetField(Message, "Function      : ");

		if (!Arguments.empty())
		{
			return Arguments;
		}

		if (!Description.empty() && Description != "<no expression>")
		{
			return Description;
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

public:
	struct ErrorEntry
	{
		xr_string FullMessage;
		xr_string TimeStr;
		xr_string ShortDesc;
		xr_string FileLine;
		xr_string Function;
		bool Expanded = false;
	};

	xr_vector<ErrorEntry> Entries;
	string_path Filter = {};

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

		if (ImGui::Button("Clear"))
		{
			Entries.clear();
		}

		ImGui::SameLine();

		if (ImGui::Button("Copy All"))
		{
			xr_string All;

			for (const auto& Entry : Entries)
			{
				All += Entry.TimeStr + " | " + Entry.ShortDesc + " | " + Entry.FileLine + "\n";
			}

			ImGui::SetClipboardText(All.c_str());
		}

		ImGui::SameLine();
		XRay::ImGui::ToggleButton("Auto Scroll", &AutoScroll, { 0, 0 });

		ImGui::SameLine();
		ImGui::SetNextItemWidth(-1);

		ImGui::InputTextWithHint("##value", "Search...", Filter, sizeof(Filter));

		if (GUIManager->SearchIcon)
		{
			ImVec2 IconSize = { 14, 14 };

			ImGui::SameLine();
			ImVec2 cursorPos = ImGui::GetCursorPos();
			ImGui::SetCursorPos(ImVec2(cursorPos.x - IconSize.x - 10.f, 1 + cursorPos.y + (IconSize.y / 3)));

			ImGui::Image(GUIManager->SearchIcon, IconSize);
		}

		ImGui::Separator();

		if (ImGui::BeginTable("ErrorsTable", 3, ImGuiTableFlags_BordersOuter | ImGuiTableFlags_RowBg | ImGuiTableFlags_Resizable | ImGuiTableFlags_ScrollY))
		{
			ImGui::TableSetupColumn("Time", ImGuiTableColumnFlags_WidthFixed, 150.0f);
			ImGui::TableSetupColumn("Error", ImGuiTableColumnFlags_WidthStretch);
			ImGui::TableSetupColumn("Location", ImGuiTableColumnFlags_WidthFixed, 220.0f);
			ImGui::TableHeadersRow();

			int Index = 0;

			for (ErrorEntry& Entry : Entries)
			{
				xr_string FilterStr = Filter;
				if (!FilterStr.empty() && !Entry.ShortDesc.Contains(FilterStr) && !Entry.FileLine.Contains(FilterStr))
				{
					continue;
				}

				ImGui::TableNextRow();

				ImGui::TableSetColumnIndex(0);
				ImGui::TextDisabled("%s", Entry.TimeStr.c_str());

				ImGui::TableSetColumnIndex(1);

				ImVec4 Color = ImVec4(1.0f, 0.85f, 0.35f, 1.0f);

				if (Entry.ShortDesc.find("fatal") != xr_string::npos || Entry.ShortDesc.find("not found") != xr_string::npos)
				{
					Color = ImVec4(1.0f, 0.45f, 0.45f, 1.0f);
				}

				ImGui::PushStyleColor(ImGuiCol_Text, Color);
				ImGui::PushID(Index);

				if (ImGui::Selectable(Entry.ShortDesc.c_str(), Entry.Expanded, ImGuiSelectableFlags_SpanAllColumns))
				{
					Entry.Expanded = !Entry.Expanded;
				}

				ImGui::PopStyleColor();

				ImGui::TableSetColumnIndex(2);
				ImGui::TextColored(ImVec4(0.55f, 0.75f, 1.0f, 1.0f), "%s", (Entry.FileLine + ":" + Entry.Function).c_str());

				if (Entry.Expanded)
				{
					ImGui::TableNextRow();
					ImGui::TableSetColumnIndex(1);

					ImGui::Indent();

					auto PrintRow = [&](const char* Name, const char* Label)
					{
						xr_string Value = GetField(Entry.FullMessage, Label);

						if (!Value.empty())
						{
							ImGui::Text("%s:", Name);
							ImGui::SameLine(120.0f);
							ImGui::TextWrapped("%s", Value.c_str());
						}
					};

					PrintRow("Expression", "Expression    : ");
					PrintRow("Function", "Function      : ");
					PrintRow("Description", "Description   : ");
					PrintRow("Arguments", "Arguments     : ");

					if (ImGui::SmallButton("Copy Full"))
					{
						ImGui::SetClipboardText(Entry.FullMessage.c_str());
					}

					ImGui::Unindent();
				}
				ImGui::PopID();

				Index++;
			}

			ImGui::EndTable();
		}

		if (AutoScroll && ImGui::GetScrollY() >= ImGui::GetScrollMaxY())
		{
			ImGui::SetScrollHereY(1.0f);
		}

		ImGui::End();
	}

private:
	bool AutoScroll = true;
};

static ErrorCollector Collector;

void ErrorCollector::AddErrorToCollector(const char* FullErrorMessage)
{
	static bool RegisteredWnd = false;

	if (!RegisteredWnd)
	{
		UI->Push(&Collector, false);
		RegisteredWnd = true;
	}

	Collector.bOpen = true;

	ErrorEntry Entry;
	Entry.FullMessage = FullErrorMessage;

	auto Now = std::chrono::system_clock::now();
	auto TimeNow = xr_chrono_to_time_t(Now);

	char Buffer[64]{};
	std::tm LocalTm{};
	localtime_s(&LocalTm, &TimeNow);

	strftime(Buffer, sizeof(Buffer), "%H:%M:%S", &LocalTm);
	Entry.TimeStr = Buffer;

	const xr_string Message = FullErrorMessage;

	Entry.ShortDesc = BuildShortDescription(Message);

	if (Entry.ShortDesc.size() > 100)
	{
		Entry.ShortDesc = Entry.ShortDesc.substr(0, 97) + "...";
	}

	xr_string File = GetField(Message, "File          : ");
	xr_string Line = GetField(Message, "Line          : ");
	Entry.Function = GetField(Message, "Function      : ");

	if (!File.empty())
	{
		File = xr_path(File).xfilename();
	}

	if (!File.empty() && !Line.empty())
	{
		Entry.FileLine = File + ":" + Line;
	}
	else
	{
		Entry.FileLine = "Unknown location";
	}

	Collector.Entries.push_back(std::move(Entry));
}