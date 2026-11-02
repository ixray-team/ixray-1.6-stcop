#pragma once

#include "HttpSource.h"

namespace SdkPackages::RemoteDocuments
{
template <typename TReader>
bool Fetch(PackageDocuments& Documents, TReader&& Read, std::stop_token Stop = {}, const nlohmann::json* Cached = nullptr, const xr_string& Commit = {})
{
	PackageDocuments Previous;
	xr_string CachedCommit, CacheError;
	if (Cached && GitSource::IsCommit(Commit) && Presentation::Text(*Cached, "commit", CachedCommit, 64) && Details::Lower(CachedCommit) == Details::Lower(Commit) && Cached->contains("documents"))
	{
		Presentation::ParseDocuments((*Cached)["documents"], Previous, CacheError);
	}
	const auto DocumentLambda = [&](const std::initializer_list<const char*>& Names, xr_string& Text, const xr_string& CachedText)
	{
		for (const auto* Name : Names)
		{
			if (Stop.stop_requested())
			{
				return false;
			}
			xr_string Bytes;
			bool IsMissing = false;
			const bool IsSuccess = Read(Name, Bytes, IsMissing);
			if (Stop.stop_requested())
			{
				return false;
			}
			if (!IsSuccess)
			{
				if (IsMissing)
				{
					continue;
				}
				if (!CachedText.empty())
				{
					Text = CachedText;
				}
				Documents.Notice += "A document preview could not be downloaded. Sync the catalog to retry.\n";
				return true;
			}
			if (Bytes.starts_with("\xef\xbb\xbf"))
			{
				Bytes.erase(0, 3);
			}
			if (Bytes.size() > Presentation::DocumentLimit)
			{
				size_t End = Presentation::DocumentLimit;
				while (End && (static_cast<u8>(Bytes[End]) & 0xc0) == 0x80)
				{
					--End;
				}
				Bytes.resize(End);
				Documents.Notice += "A document preview was truncated to 32 KiB.\n";
			}
			if (!Presentation::IsUtf8(Bytes))
			{
				if (!CachedText.empty())
				{
					Text = CachedText;
				}
				Documents.Notice += "A documentation file is not UTF-8 text and was skipped.\n";
				return true;
			}
			Text = std::move(Bytes);
			return true;
		}
		return true;
	};
	const bool IsSuccess = DocumentLambda({"README.md", "readme.md", "README.txt", "README"}, Documents.Readme, Previous.Readme) &&
						   DocumentLambda({"CHANGELOG.md", "changelog.md", "CHANGES.md", "CHANGELOG"}, Documents.Changelog, Previous.Changelog) &&
						   DocumentLambda({"LICENSE.md", "LICENSE", "LICENSE.txt", "license.md", "COPYING"}, Documents.License, Previous.License);
	Documents.Notice.resize(std::min(Documents.Notice.size(), size_t(4096)));
	return IsSuccess;
}
} // namespace SdkPackages::RemoteDocuments
