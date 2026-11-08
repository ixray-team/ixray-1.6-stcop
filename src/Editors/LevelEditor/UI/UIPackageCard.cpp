#include "stdafx.h"
#include "UIPackageManager.h"
#include "UIPackageCard.h"
#include "../Packages/PackageMarkdown.h"
#include <SDL3/SDL_misc.h>

namespace PackageCardUI
{
void DrawMetadata(const SdkPackages::PackageRow& Row, bool HasLicenseText, xr_string& Error)
{
	const auto& Info = Row.Package;
	if (Row.IsMetadataAvailable)
	{
		ImGui::TextWrapped("Author: %s", Info.Author.empty() ? "Not specified" : Info.Author.c_str());
		ImGui::TextWrapped("License: %s", Info.Card.License.empty() ? (HasLicenseText ? "See License tab" : "Not specified") : Info.Card.License.c_str());
	}
	xr_string Repository = Info.Card.SourceUrl;
	if (Repository.empty())
	{
		Repository = Info.Source == SdkPackages::PackageSource::GitHub ? Info.Url : xr_string{};
		if (Info.Source == SdkPackages::PackageSource::GitHub && Repository.ends_with(".git"))
		{
			Repository.resize(Repository.size() - 4);
		}
	}
	const float Available = ImGui::GetContentRegionAvail().x;
	const float Spacing = std::max(8.0f, ImGui::GetStyle().ItemSpacing.x);
	float Used = 0;
	const auto LinkLambda = [&Used, Available, Spacing, &Error](const char* Label, const xr_string& Url)
	{
		if (!SdkPackages::Presentation::IsWebUrl(Url))
		{
			return;
		}
		const float Width = ImGui::CalcTextSize(Label).x;
		if (Used > 0 && Used + Width < Available)
		{
			ImGui::SameLine(0, Spacing);
		}
		else
		{
			Used = 0;
		}
		if (ImGui::TextLink(Label) && !SDL_OpenURL(Url.c_str()))
		{
			Error = xr_string("Cannot open link: ") + SDL_GetError();
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("%s", Url.c_str());
		}
		Used += Width + Spacing;
	};
	LinkLambda("Source", Repository);
	LinkLambda("Documentation", Info.Card.DocumentationUrl);
	LinkLambda("Website", Info.Card.WebsiteUrl);
	LinkLambda("Release notes", Info.Card.ChangelogUrl);
}

// Keep wrapped inline fragments aligned to the paragraph, including after links.
static void DrawInline(const xr_string& Line)
{
	const float Left = ImGui::GetCursorPosX();
	const float Right = Left + ImGui::GetContentRegionAvail().x;
	const float SpaceWidth = ImGui::CalcTextSize(" ").x;
	float X = Left;
	bool HasWord = false, HasSpace = false;
	u32 Item = 0;
	for (const auto& Span : SdkPackages::Markdown::ParseInline(Line))
	{
		for (size_t Position = 0; Position < Span.Text.size();)
		{
			if (Span.Text[Position] == ' ' || Span.Text[Position] == '\t')
			{
				HasSpace = true;
				++Position;
				continue;
			}
			const auto End = Span.Text.find_first_of(" \t", Position);
			xr_string Word = Span.Text.substr(Position, End == xr_string::npos ? xr_string::npos : End - Position).c_str();
			// Long identifiers and URLs must fit narrow panels without splitting UTF-8.
			if (ImGui::CalcTextSize(Word.c_str()).x > Right - Left)
			{
				size_t Fit = 0;
				for (size_t Next = 0; Next < Word.size();)
				{
					++Next;
					while (Next < Word.size() && (static_cast<unsigned char>(Word[Next]) & 0xc0) == 0x80)
					{
						++Next;
					}
					if (Fit && ImGui::CalcTextSize(Word.c_str(), Word.c_str() + Next).x > Right - Left)
					{
						break;
					}
					Fit = Next;
				}
				Word.resize(Fit);
			}
			Position += Word.size();
			const float Width = ImGui::CalcTextSize(Word.c_str()).x;
			const float Gap = HasSpace ? SpaceWidth : 0;
			if (HasWord && X + Gap + Width <= Right)
			{
				ImGui::SameLine(0, Gap);
				X += Gap;
			}
			else
			{
				ImGui::SetCursorPosX(Left);
				X = Left;
			}
			ImGui::PushID(++Item);
			if (SdkPackages::Presentation::IsWebUrl(Span.Url))
			{
				if (ImGui::TextLink(Word.c_str()))
				{
					SDL_OpenURL(Span.Url.c_str());
				}
			}
			else
			{
				const bool ShouldHighlight = Span.IsStrong || Span.IsCode;
				if (ShouldHighlight)
				{
					ImGui::PushStyleColor(ImGuiCol_Text, Span.IsCode ? ImVec4(0.78f, 0.87f, 1, 1) : ImVec4(1, 1, 1, 1));
				}
				ImGui::TextUnformatted(Word.c_str());
				if (ShouldHighlight)
				{
					ImGui::PopStyleColor();
				}
			}
			if (!Span.Url.empty() && ImGui::IsItemHovered())
			{
				ImGui::SetTooltip("%s", Span.Url.c_str());
			}
			ImGui::PopID();
			X += Width;
			HasWord = true;
			HasSpace = false;
		}
	}
}

void DrawDocument(const xr_string& Text, const char* EmptyMessage, bool IsMarkdown)
{
	if (Text.empty())
	{
		ImGui::TextWrapped("%s", EmptyMessage);
		return;
	}
	if (!IsMarkdown)
	{
		ImGui::TextWrapped("%s", Text.c_str());
		return;
	}
	bool IsInCode = false;
	xr_string Code;
	u32 Block = 0;
	const auto FlushCodeLambda = [&Code, &Block]()
	{
		if (Code.empty())
		{
			return;
		}
		const auto Lines = std::count(Code.begin(), Code.end(), '\n') + 1;
		ImGui::PushID(++Block);
		ImGui::BeginChild("Code", {0, std::min(200.0f, ImGui::GetTextLineHeightWithSpacing() * Lines + ImGui::GetStyle().WindowPadding.y * 2)}, ImGuiChildFlags_Borders, ImGuiWindowFlags_HorizontalScrollbar);
		ImGui::TextUnformatted(Code.c_str());
		ImGui::EndChild();
		ImGui::PopID();
		Code.clear();
	};
	size_t Position = 0;
	size_t Lines = 0;
	while (Position < Text.size() && Lines++ < 1024)
	{
		const auto Newline = Text.find('\n', Position);
		const auto End = Newline == xr_string::npos ? Text.size() : Newline;
		auto Line = Text.substr(Position, End - Position);
		if (Line.ends_with('\r'))
		{
			Line.pop_back();
		}
		Position = Newline == xr_string::npos ? Text.size() : End + 1;
		if (Line.starts_with("```"))
		{
			if (IsInCode)
			{
				FlushCodeLambda();
			}
			IsInCode = !IsInCode;
			continue;
		}
		if (IsInCode)
		{
			Code += Line + "\n";
			continue;
		}
		if (Line.empty())
		{
			ImGui::Spacing();
			continue;
		}
		ImGui::PushID(static_cast<int>(Lines));
		const auto Heading = Line.find_first_not_of('#');
		if (Line.front() == '#' && Heading != xr_string::npos && Heading <= 6 && Line[Heading] == ' ')
		{
			ImGui::Spacing();
			ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.78f, 0.87f, 1.0f, 1.0f));
			DrawInline(Line.substr(Heading + 1).c_str());
			ImGui::PopStyleColor();
			ImGui::Separator();
		}
		else if (Line.starts_with("- ") || Line.starts_with("* ") || Line.starts_with("+ "))
		{
			ImGui::Bullet();
			ImGui::SameLine(0, 4.0f);
			DrawInline(Line.substr(2).c_str());
		}
		else if (Line == "---" || Line == "***")
		{
			ImGui::Separator();
		}
		else
		{
			DrawInline(Line.c_str());
		}
		ImGui::PopID();
	}
	FlushCodeLambda();
	if (Position < Text.size())
	{
		ImGui::TextDisabled("Preview limited to 1024 lines.");
	}
}
} // namespace PackageCardUI

void UIPackageManager::LoadCardDocuments(const SdkPackages::PackageRow& Row)
{
	if (CardDocumentsKey == Row.Key)
	{
		return;
	}
	CardDocumentsKey = Row.Key;
	CardDocuments = {};
	CardDocumentsError.clear();
	CardDocumentsSource.clear();
	if (Row.IsInstalled)
	{
		if (!Store.ReadDocuments(Row.Package.Id, CardDocuments, CardDocumentsError))
		{
			return;
		}
		if (!CardDocuments.Readme.empty() || !CardDocuments.Changelog.empty() || !CardDocuments.License.empty())
		{
			CardDocumentsSource = "Documentation saved with the installed package.";
			return;
		}
	}
	const auto InstalledNotice = CardDocuments.Notice;
	CardDocuments = Row.Documents;
	CardDocuments.Notice.insert(0, InstalledNotice);
	if (!Row.BundledDocumentsPath.empty())
	{
		const auto Path = SdkPackages::FromUtf8(Row.BundledDocumentsPath);
		SdkPackages::PackageDocuments Included;
		SdkPackages::Presentation::Discover(Path, Path, Included);
		if (CardDocuments.Readme.empty())
		{
			CardDocuments.Readme = Included.Readme;
		}
		if (CardDocuments.Changelog.empty())
		{
			CardDocuments.Changelog = Included.Changelog;
		}
		if (CardDocuments.License.empty())
		{
			CardDocuments.License = Included.License;
		}
		CardDocuments.Notice += Included.Notice;
	}
	if (!CardDocuments.Readme.empty() || !CardDocuments.Changelog.empty() || !CardDocuments.License.empty())
	{
		CardDocumentsSource = Row.IsInstalled ? "Documentation from the package for this installed version." : "Documentation provided by the package.";
	}
}
