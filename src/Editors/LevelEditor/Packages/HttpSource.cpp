#include "stdafx.h"
#include "HttpSource.h"
#include "PackageProcess.h"

namespace SdkPackages::HttpSource
{
bool RelativeFile(const xr_string& File)
{
	if (File.empty() || File.find('\\') != xr_string::npos || File.find(':') != xr_string::npos || File.front() == '/' || File.back() == '/')
	{
		return false;
	}
	size_t Start = 0;
	for (;;)
	{
		const auto End = File.find('/', Start);
		if (!Details::IsId(Details::Lower(File.substr(Start, End == xr_string::npos ? xr_string::npos : End - Start))))
		{
			return false;
		}
		if (End == xr_string::npos)
		{
			return true;
		}
		Start = End + 1;
	}
}

bool RawUrl(const xr_string& Repository, const xr_string& Commit, const xr_string& File, xr_string& Url, xr_string& Error)
{
	xr_string Canonical, Id;
	Url.clear();
	if (!PackageStore::ParseGitHubUrl(Repository, Canonical, Id, Error) || !GitSource::IsCommit(Commit) || !RelativeFile(File))
	{
		return Details::Fail(Error, "Raw package requests require a GitHub repository, an exact commit and a safe relative file.");
	}
	auto Path = Canonical.substr(xr_string("https://github.com/").size());
	Path.resize(Path.size() - 4);
	Url = "https://raw.githubusercontent.com/" + Path + "/" + Commit + "/" + File;
	return true;
}

bool ReadRaw(const xr_string& Repository, const xr_string& Commit, const xr_string& File, xr_string& Bytes, xr_string& Error, std::stop_token Stop, bool* IsMissing)
{
	if (IsMissing)
	{
		*IsMissing = false;
	}
	xr_string Url;
	Bytes.clear();
	if (!RawUrl(Repository, Commit, File, Url, Error))
	{
		return false;
	}
	const bool IsSuccess = PackageProcess::Run({"curl", "--disable", "--fail", "--silent", "--show-error", "--globoff", "--proto", "=https", "--proto-redir", "=https", "--location", "--max-redirs", "3", "--connect-timeout", "5", "--max-time", "15", "--max-filesize", "65536", "--write-out", "\n%{http_code}", "--url", Url}, Bytes, Stop, 20000, 65540);
	if (IsMissing && !Stop.stop_requested())
	{
		*IsMissing = Bytes.ends_with("\n404") || Bytes.ends_with("\n410");
	}
	if (!IsSuccess || !Bytes.ends_with("\n200"))
	{
		Error = Bytes;
		Bytes.clear();
		return false;
	}
	Bytes.resize(Bytes.size() - 4);
	Error.clear();
	return true;
}
} // namespace SdkPackages::HttpSource
