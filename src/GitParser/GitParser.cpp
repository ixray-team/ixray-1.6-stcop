////////////////////////////////////////////////////
// Author: ForserX								  //
// Task  : Parsing current branch and commit hash //
////////////////////////////////////////////////////

#include <string>
#include <string_view>
#include <fstream>
#include <istream>
#include <sstream>
#include <deque> 
#include <utility>
#include <iostream>

// https://www.appveyor.com/docs/environment-variables/
#define NewStr(str) #str

std::deque<std::string> dqSplit(std::string_view Str, char splitCh) noexcept
{
	std::deque<std::string> Result;

	size_t SubStrBeginCursor = 0;
	size_t Len = 0;
	for (size_t StrCursor = 0; StrCursor < Str.size(); ++StrCursor)
	{
		if (Str[StrCursor] == splitCh)
		{
			//Don't create empty string
			if ((StrCursor - 1 - SubStrBeginCursor) > 0)
			{
				Len = StrCursor - 1 - SubStrBeginCursor;
				Result.emplace_back(std::move(Str.substr(SubStrBeginCursor, Len)));
				SubStrBeginCursor = StrCursor + 1;
			}
		}
	}

	Result.emplace_back(std::move(Str.substr(SubStrBeginCursor, Str.length() - Len)));
	return Result;
}

std::string_view GetValueFromEnvironmentVariable(const std::string& Variable)
{
	size_t DelimIter = Variable.find_first_of('=', 0);
	if (DelimIter != std::string::npos)
	{
		// skip '=' symbol
		return std::string_view { &Variable[DelimIter + 1], Variable.size() - DelimIter - 1 };
	}

	return {};
}

int main(int argc, char** argv, char** envp)
{
	bool bIsCI = false;
	std::string GithubRepoCommitAuthor;
	std::string GithubRepoBranch;
	std::string GithubRepoCommit;

	char** EnvironmentVariableIter = envp;
	char* EnvVar = *EnvironmentVariableIter;
	while (EnvVar != nullptr)
	{
		std::string EnvVarStr{ EnvVar };

		if (EnvVarStr.starts_with("CI"))
		{
			std::string_view AppVeyorBool = GetValueFromEnvironmentVariable(EnvVarStr);
			if (AppVeyorBool == "true")
			{
				bIsCI = true;
			}
		}
		else if (EnvVarStr.starts_with("GITHUB_ACTOR"))
		{
			std::string_view RepoCommitAuthor = GetValueFromEnvironmentVariable(EnvVarStr);
			GithubRepoCommitAuthor = RepoCommitAuthor;
		}
		else if (EnvVarStr.starts_with("GITHUB_REF_NAME"))
		{
			std::string_view RepoBranch = GetValueFromEnvironmentVariable(EnvVarStr);
			GithubRepoBranch = RepoBranch;
		}
		else if (EnvVarStr.starts_with("GITHUB_SHA"))
		{
			std::string_view RepoCommit = GetValueFromEnvironmentVariable(EnvVarStr);
			GithubRepoCommit = RepoCommit;
		}

		EnvironmentVariableIter++;
		EnvVar = *EnvironmentVariableIter;
	}

	// Make oxy_version.h
	std::stringstream HeaderString;
	HeaderString << "#pragma once" << std::endl;

	if (bIsCI)
	{
		HeaderString << "#define _AUTHOR " << "\"" << GithubRepoCommitAuthor << "\"" << std::endl;
		HeaderString << "#define _BRANCH " << "\"" << GithubRepoBranch << "\"" << std::endl;
		HeaderString << "#define _HASH " << "\"" << GithubRepoCommit << "\"" << std::endl;
		HeaderString << "#define CI_BUILD 1 " << std::endl;
	}
	else
	{
		std::string PathFile = "../../.git/";

		// Get repo data 
		std::string BranchName = "";
		std::ifstream Reader(PathFile + "HEAD");
		Reader >> BranchName >> BranchName;
		BranchName = dqSplit(BranchName, '/').back();
		Reader.close();

		// Get author nickname (#HACK (dirty))
		Reader.open(PathFile + "logs/HEAD");
		std::string AuthorName = "";
		/* File struct: Head Hash !Nick! EMail Message */
		Reader >> AuthorName >> AuthorName >> AuthorName;

		if (AuthorName.empty())
			AuthorName = "Unknown";

		Reader.close();

		// Get current branch 
		PathFile += "refs/heads/" + BranchName;
		Reader.open(PathFile);

		// Get current hash commit code 
		std::string hash = "";
		Reader >> hash;
		Reader.close();

		HeaderString << "#define _AUTHOR " << "\"" << AuthorName << "\"" << std::endl;
		HeaderString << "#define _BRANCH " << "\"" << BranchName << "\"" << std::endl;
		HeaderString << "#define _HASH " << "\"" << hash << "\"" << std::endl;
		HeaderString << "#define CI_BUILD 0" << std::endl;
	}

	std::ofstream Writter("git_version.h");
	Writter.write(HeaderString.str().c_str(), HeaderString.str().size());
	Writter.close();

    return 0;
}
