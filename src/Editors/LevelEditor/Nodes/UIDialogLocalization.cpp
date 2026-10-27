#include "stdafx.h"
#include "UIDialogLocalization.h"
#include "../../../xrEngine/string_table.h"
#include "../../xrEUI/IconsFontAwesome6.h"

#include <fstream>

static constexpr const char* EmptyStringTable = "<?xml version=\"1.0\" encoding=\"windows-1251\" ?>\r\n<string_table>\r\n</string_table>\r\n";

CUIDialogLocalization::CUIDialogLocalization()
{
	bOpen = false;
}

void CUIDialogLocalization::Open(const char* NewStringId, const char* NewNodeName, const char* NewFileName, RegisteredCallback NewOnRegistered)
{
	OnRegistered = std::move(NewOnRegistered);
	xr_strcpy(StringId, NewStringId);
	xr_strcpy(FileName, NewFileName);
	NodeName = NewNodeName;
	Error.clear();

	Texts.clear();
	const xr_string CurrentLanguage = CStringTable::LangName();
	for (const xr_token& Token : CStringTable::languages_token)
	{
		if (Token.name == nullptr)
		{
			continue;
		}

		LanguageText& Entry = Texts.emplace_back();
		Entry.Language = Token.name;
		Entry.IsCurrent = Entry.Language == CurrentLanguage;
	}

	// The current language goes first: it is the one the editor shows
	std::stable_sort(Texts.begin(), Texts.end(), [](const LanguageText& L, const LanguageText& R)
	{
		return L.IsCurrent && !R.IsCurrent;
	});

	bOpen = true;
	FocusText = true;
}

bool CUIDialogLocalization::IsValidId(const char* Id)
{
	if (Id == nullptr || Id[0] == '\0')
	{
		return false;
	}

	for (const char* Char = Id; *Char != '\0'; ++Char)
	{
		const bool IsAllowed = (*Char >= 'a' && *Char <= 'z') || (*Char >= 'A' && *Char <= 'Z') || (*Char >= '0' && *Char <= '9') || *Char == '_' || *Char == '.';
		if (!IsAllowed)
		{
			return false;
		}
	}

	return true;
}

xr_string CUIDialogLocalization::EscapeText(const char* Text)
{
	xr_string Result;
	for (const char* Char = Text; *Char != '\0'; ++Char)
	{
		switch (*Char)
		{
		case '&': Result += "&amp;"; break;
		case '<': Result += "&lt;"; break;
		case '>': Result += "&gt;"; break;
		// The game breaks lines on the "\n" sequence
		case '\n': Result += "\\n"; break;
		case '\r': break;
		default: Result += *Char; break;
		}
	}

	return Result;
}

bool CUIDialogLocalization::CanRegister(xr_string& Reason) const
{
	if (!IsValidId(StringId))
	{
		Reason = "The string ID may contain only latin letters, digits, '_' and '.'";
		return false;
	}

	if (g_pStringTable->HasString(StringId))
	{
		Reason = "This string ID is already registered";
		return false;
	}

	if (!IsValidId(FileName) || strstr(FileName, ".xml") == nullptr)
	{
		Reason = "The file must be an .xml name without folders";
		return false;
	}

	if (Texts.empty() || Texts.front().Text[0] == '\0')
	{
		Reason = "Enter the text for the current language";
		return false;
	}

	return true;
}

bool CUIDialogLocalization::AppendString(const char* Language, const char* Utf8Text)
{
	string_path RelativePath = {};
	xr_sprintf(RelativePath, "text\\%s\\%s", Language, FileName);

	string_path FullPath = {};
	FS.update_path(FullPath, _game_config_, RelativePath);

	xr_string Content;
	if (std::filesystem::exists(FullPath))
	{
		std::ifstream Input(FullPath, std::ios::binary);
		if (!Input.is_open())
		{
			Error = make_string<xr_string>("Cannot read %s", FullPath);
			return false;
		}

		Content.assign(std::istreambuf_iterator<char>(Input), std::istreambuf_iterator<char>());
	}
	else
	{
		Content = EmptyStringTable;
		std::filesystem::create_directories(std::filesystem::path(FullPath).parent_path());
	}

	const size_t End = Content.rfind("</string_table>");
	if (End == xr_string::npos)
	{
		Error = make_string<xr_string>("%s has no </string_table>", FullPath);
		return false;
	}

	// String tables are stored in the ANSI code page
	const xr_string Text = Platform::UTF8_to_CP1251(EscapeText(Utf8Text));
	const xr_string Entry = make_string<xr_string>("\t<string id=\"%s\">\r\n\t\t<text>%s</text>\r\n\t</string>\r\n", StringId, Text.c_str());
	Content.insert(End, Entry);

	std::ofstream Output(FullPath, std::ios::binary | std::ios::trunc);
	if (!Output.is_open())
	{
		Error = make_string<xr_string>("Cannot write %s", FullPath);
		return false;
	}

	Output.write(Content.data(), Content.size());
	Msg("* Dialog Editor: '%s' added to %s", StringId, FullPath);
	return true;
}

bool CUIDialogLocalization::Register()
{
	Error.clear();
	if (!CanRegister(Error))
	{
		return false;
	}

	for (const LanguageText& Entry : Texts)
	{
		if (Entry.Text[0] != '\0' && !AppendString(Entry.Language.c_str(), Entry.Text))
		{
			return false;
		}
	}

	g_pStringTable->SetString(StringId, Platform::UTF8_to_CP1251(EscapeText(Texts.front().Text)).c_str());

	if (OnRegistered)
	{
		OnRegistered(StringId);
	}

	return true;
}

void CUIDialogLocalization::Draw()
{
	if (!bOpen)
	{
		return;
	}

	ImGui::SetNextWindowSize({ 560.f, 0.f }, ImGuiCond_Appearing);
	ImGui::SetNextWindowPos(ImGui::GetMainViewport()->GetCenter(), ImGuiCond_Appearing, { 0.5f, 0.5f });
	if (!ImGui::Begin(ICON_FA_LANGUAGE " Localize Text###DialogLocalization", &bOpen, ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::End();
		return;
	}

	if (!NodeName.empty())
	{
		ImGui::TextDisabled("%s", NodeName.c_str());
	}

	const float LabelWidth = ImGui::CalcTextSize("String ID").x + ImGui::GetStyle().ItemSpacing.x * 2.f;
	const float FieldWidth = 520.f - LabelWidth;

	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted("String ID");
	ImGui::SameLine(LabelWidth);
	ImGui::SetNextItemWidth(FieldWidth);
	ImGui::InputText("##StringId", StringId, sizeof(StringId), ImGuiInputTextFlags_CharsNoBlank);

	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted("File");
	ImGui::SameLine(LabelWidth);
	ImGui::SetNextItemWidth(FieldWidth);
	ImGui::InputText("##File", FileName, sizeof(FileName), ImGuiInputTextFlags_CharsNoBlank);

	string_path Probe = {};
	xr_sprintf(Probe, "text\\%s\\%s", CStringTable::LangName().c_str(), FileName);
	string_path ProbePath = {};
	FS.update_path(ProbePath, _game_config_, Probe);
	ImGui::SetCursorPosX(LabelWidth);
	if (std::filesystem::exists(ProbePath))
	{
		ImGui::TextDisabled("configs\\text\\<language>\\%s", FileName);
	}
	else
	{
		ImGui::TextColored({ 0.9f, 0.75f, 0.35f, 1.f }, ICON_FA_FILE_CIRCLE_PLUS " configs\\text\\<language>\\%s will be created", FileName);
	}

	for (LanguageText& Entry : Texts)
	{
		ImGui::PushID(Entry.Language.c_str());
		ImGui::SeparatorText(Entry.IsCurrent ? make_string<xr_string>("%s (current)", Entry.Language.c_str()).c_str() : Entry.Language.c_str());

		if (Entry.IsCurrent && FocusText)
		{
			ImGui::SetKeyboardFocusHere();
			FocusText = false;
		}

		ImGui::InputTextMultiline("##Text", Entry.Text, sizeof(Entry.Text), { 520.f, ImGui::GetTextLineHeight() * (Entry.IsCurrent ? 4.f : 2.5f) });
		ImGui::PopID();
	}

	ImGui::TextDisabled("Empty languages are skipped. Line breaks are saved as \\n");
	ImGui::Spacing();

	xr_string Reason;
	const bool IsReady = CanRegister(Reason);

	ImGui::BeginDisabled(!IsReady);
	const bool IsSubmit = ImGui::Button(ICON_FA_CHECK " Register", { 120.f, 0.f }) || (IsReady && ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && ImGui::GetIO().KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_Enter));
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Ctrl+Enter");

	ImGui::SameLine();
	if (ImGui::Button("Cancel", { 120.f, 0.f }) || (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && ImGui::IsKeyPressed(ImGuiKey_Escape)))
	{
		bOpen = false;
	}

	if (IsSubmit && Register())
	{
		bOpen = false;
	}

	if (!Error.empty())
	{
		ImGui::TextColored({ 0.95f, 0.4f, 0.4f, 1.f }, "%s", Error.c_str());
	}
	else if (!IsReady)
	{
		ImGui::TextDisabled("%s", Reason.c_str());
	}

	ImGui::End();
}
