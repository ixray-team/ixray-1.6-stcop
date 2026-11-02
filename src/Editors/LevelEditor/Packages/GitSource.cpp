#include "stdafx.h"
#include "GitSource.h"
#include "PackageProcess.h"

namespace SdkPackages::GitSource
{
static bool Run(const xr_vector<xr_string>& Arguments, xr_string& Output, std::stop_token Stop, u64 Timeout = 180000)
{
	xr_vector<xr_string> Command = {"git", "-c", "credential.helper=", "-c", "core.askPass=", "-c", "credential.interactive=false", "-c", "core.hooksPath=/dev/null"};
	Command.insert(Command.end(), Arguments.begin(), Arguments.end());
	return PackageProcess::Run(Command, Output, Stop, Timeout);
}

bool IsCommit(const xr_string& Commit)
{
	return (Commit.size() == 40 || Commit.size() == 64) && std::all_of(Commit.begin(), Commit.end(), [](char Letter)
																	   { return (Letter >= '0' && Letter <= '9') || (Letter >= 'a' && Letter <= 'f') || (Letter >= 'A' && Letter <= 'F'); });
}

bool ResolveRevision(const xr_string& Url, const xr_string& Ref, xr_string& Commit, xr_string& Error, std::stop_token Stop)
{
	Commit.clear();
	xr_string Canonical, Id;
	if (!PackageStore::ParseGitHubUrl(Url, Canonical, Id, Error) || !Details::IsRef(Ref))
	{
		return Details::Fail(Error, "Invalid package repository or revision.");
	}
	xr_vector<xr_string> Arguments = {"ls-remote", "--", Canonical};
	if (Ref.empty())
	{
		Arguments.emplace_back("HEAD");
	}
	else
	{
		Arguments.push_back("refs/heads/" + Ref);
		Arguments.push_back("refs/tags/" + Ref);
		Arguments.push_back("refs/tags/" + Ref + "^{}");
	}
	xr_string Output;
	if (!Run(Arguments, Output, Stop, 20000))
	{
		Error = Output;
		return false;
	}
	xr_string Branch, Tag, Peeled;
	for (size_t Start = 0; Start < Output.size();)
	{
		const auto End = Output.find('\n', Start);
		xr_string Line = Output.substr(Start, End == xr_string::npos ? xr_string::npos : End - Start);
		Line = Line.Trim();
		const auto Tab = Line.find('\t');
		if (Tab != xr_string::npos && IsCommit(Line.substr(0, Tab)))
		{
			const auto Name = Line.substr(Tab + 1);
			if ((Ref.empty() && Name == "HEAD") || (!Ref.empty() && Name == "refs/heads/" + Ref))
			{
				Branch = Line.substr(0, Tab);
			}
			else if (Name == "refs/tags/" + Ref + "^{}")
			{
				Peeled = Line.substr(0, Tab);
			}
			else if (Name == "refs/tags/" + Ref)
			{
				Tag = Line.substr(0, Tab);
			}
		}
		Start = End == xr_string::npos ? Output.size() : End + 1;
	}
	Commit = !Branch.empty() ? Branch : !Peeled.empty() ? Peeled
														: Tag;
	return IsCommit(Commit) || Details::Fail(Error, "Package branch or tag was not found.");
}

bool Checkout(const xr_string& Url, const xr_string& Ref, const xr_path& Destination, xr_string& Commit, xr_string& Error, std::stop_token Stop, const xr_string& ExpectedCommit)
{
	xr_string CanonicalUrl, Id;
	if (!PackageStore::ParseGitHubUrl(Url, CanonicalUrl, Id, Error) || !Details::IsRef(Ref) || (!ExpectedCommit.empty() && !IsCommit(ExpectedCommit)))
	{
		return Details::Fail(Error, "Use a GitHub repository URL and a valid branch/tag.");
	}
	if (!ExpectedCommit.empty())
	{
		if (!Run({"init", "--quiet", "--", Utf8(Destination)}, Error, Stop) ||
			!Run({"-C", Utf8(Destination), "fetch", "--depth", "1", "--no-tags", "--", CanonicalUrl, ExpectedCommit}, Error, Stop) ||
			!Run({"-C", Utf8(Destination), "checkout", "--quiet", "--detach", "FETCH_HEAD"}, Error, Stop))
		{
			return false;
		}
	}
	else
	{
		xr_vector<xr_string> Arguments = {"clone", "--depth", "1", "--single-branch"};
		if (!Ref.empty())
		{
			Arguments.emplace_back("--branch");
			Arguments.push_back(Ref);
		}
		Arguments.emplace_back("--");
		Arguments.push_back(CanonicalUrl);
		Arguments.push_back(Utf8(Destination));
		if (!Run(Arguments, Error, Stop))
		{
			return false;
		}
	}
	xr_string Output;
	if (!Run({"-C", Utf8(Destination), "rev-parse", "HEAD"}, Output, Stop))
	{
		Error = Output;
		return false;
	}
	Commit = Output.Trim();
	return (IsCommit(Commit) && (ExpectedCommit.empty() || Commit == ExpectedCommit)) || Details::Fail(Error, "Git returned an unexpected package revision.");
}
} // namespace SdkPackages::GitSource
