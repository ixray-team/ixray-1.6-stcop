#include "stdafx.h"
#include "PackageProcess.h"
#include "../../../xrCore/Platform/Process.h"

namespace SdkPackages::PackageProcess
{
bool Run(const xr_vector<xr_string>& Command, xr_string& Output, std::stop_token Stop, u64 Timeout, size_t Limit)
{
	Platform::ProcessOptions Options;
	Options.Environment = {{"GIT_TERMINAL_PROMPT", "0"}, {"GIT_LFS_SKIP_SMUDGE", "1"}};
	Options.Timeout = Timeout;
	Options.OutputLimit = Limit;
	return Platform::RunProcess(Command, Options, Output, Stop);
}
} // namespace SdkPackages::PackageProcess
