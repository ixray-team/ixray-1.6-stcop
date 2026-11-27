#pragma once

// In-engine self-tests for the sandbox level builder and the
// resource-masked level loading. No external test framework: each test is a
// plain function returning true on success, logging the failing check.
namespace SelfTest
{
	using TestFn = bool (*)();	// return true = pass; use SELFTEST_CHECK to Msg and return false

	void	Register(const char* name, TestFn fn, bool needsDeviceReady);
	// Logs each result, returns the failure count. Tests flagged
	// needsDeviceReady run only when includeDeviceTests is true.
	int		RunAll(bool includeDeviceTests, const char* onlyName = nullptr);
	// True when the command line contains -level_selftest.
	bool	StartupFlagPresent();
	// Called from xrGameInitialize; runs includeDeviceTests=false unless
	// Device.b_is_Ready. Silent when the flag is absent.
	void	RunStartupTests();
	// Registers the level_selftest, sandbox_build and sandbox_remove console
	// commands. Call after CCC_RegisterCommands().
	void	RegisterConsoleCommands();
}
