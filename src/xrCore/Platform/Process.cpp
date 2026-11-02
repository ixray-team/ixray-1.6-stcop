#include "stdafx.h"
#include "Process.h"
#include <chrono>
#include <thread>
#ifndef IXR_WINDOWS
#include <cerrno>
#include <csignal>
#include <dirent.h>
#include <spawn.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

namespace Platform::ProcessDetails
{
static bool Fail(xr_string& Output, const xr_string& Message)
{
	Output = Message;
	return false;
}

static bool SystemError(xr_string& Output, const char* Operation, int Code)
{
	return Fail(Output, xr_string(Operation) + ": " + std::system_category().message(Code).c_str());
}

#ifdef IXR_WINDOWS
static bool Wide(const xr_string_view& Text, std::wstring& Result, xr_string& Error);
#endif

class Environment final
{
public:
	Environment(const ProcessOptions& Options, xr_string& Error)
		: Snapshot(SDL_CreateEnvironment(true))
	{
		if (!Snapshot)
		{
			Error = SDL_GetError();
			return;
		}
		for (const auto& [Name, Value] : Options.Environment)
		{
#ifdef IXR_WINDOWS
			std::wstring RequestedName;
			if (!Wide(Name, RequestedName, Error))
			{
				return;
			}
			Variables = SDL_GetEnvironmentVariables(Snapshot);
			if (!Variables)
			{
				Error = SDL_GetError();
				return;
			}
			for (char** Entry = Variables; *Entry; ++Entry)
			{
				const char* Separator = strchr(*Entry, '=');
				if (!Separator || Separator == *Entry)
				{
					continue;
				}
				const xr_string ExistingName(*Entry, static_cast<size_t>(Separator - *Entry));
				std::wstring ConvertedName;
				if (!Wide(ExistingName, ConvertedName, Error))
				{
					return;
				}
				if (::CompareStringOrdinal(RequestedName.c_str(), -1, ConvertedName.c_str(), -1, true) == CSTR_EQUAL && !SDL_UnsetEnvironmentVariable(Snapshot, ExistingName.c_str()))
				{
					Error = SDL_GetError();
					return;
				}
			}
			SDL_free(Variables);
			Variables = nullptr;
#endif
			if (!SDL_SetEnvironmentVariable(Snapshot, Name.c_str(), Value.c_str(), true))
			{
				Error = SDL_GetError();
				return;
			}
		}
		Variables = SDL_GetEnvironmentVariables(Snapshot);
		if (!Variables)
		{
			Error = SDL_GetError();
		}
		IsReady = Variables != nullptr;
	}
	~Environment()
	{
		SDL_free(Variables);
		SDL_DestroyEnvironment(Snapshot);
	}
	Environment(const Environment&) = delete;
	Environment& operator=(const Environment&) = delete;
	char** Get() const { return IsReady ? Variables : nullptr; }

private:
	SDL_Environment* Snapshot = nullptr;
	char** Variables = nullptr;
	bool IsReady = false;
};

#ifdef IXR_WINDOWS
class NativeHandle final
{
public:
	explicit NativeHandle(HANDLE Value = nullptr)
		: Value(Value) {}
	~NativeHandle() { Reset(); }
	NativeHandle(const NativeHandle&) = delete;
	NativeHandle& operator=(const NativeHandle&) = delete;
	void Reset(HANDLE NewValue = nullptr)
	{
		if (Value && Value != INVALID_HANDLE_VALUE)
		{
			::CloseHandle(Value);
		}
		Value = NewValue;
	}
	HANDLE Get() const { return Value; }

private:
	HANDLE Value;
};

class HandleList final
{
public:
	HandleList(HANDLE Output, HANDLE Input, xr_string& Error)
	{
		size_t Size = 0;
		::InitializeProcThreadAttributeList(nullptr, 1, 0, &Size);
		Storage.resize(Size);
		List = reinterpret_cast<LPPROC_THREAD_ATTRIBUTE_LIST>(Storage.data());
		IsInitialized = ::InitializeProcThreadAttributeList(List, 1, 0, &Size);
		Handles = {Output, Input};
		if (!IsInitialized || !::UpdateProcThreadAttribute(List, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST, Handles.data(), sizeof(Handles), nullptr, nullptr))
		{
			SystemError(Error, "Cannot restrict process handle inheritance", ::GetLastError());
			return;
		}
		IsReady = true;
	}
	~HandleList()
	{
		if (IsInitialized)
		{
			::DeleteProcThreadAttributeList(List);
		}
	}
	HandleList(const HandleList&) = delete;
	HandleList& operator=(const HandleList&) = delete;
	LPPROC_THREAD_ATTRIBUTE_LIST Get() const { return IsReady ? List : nullptr; }

private:
	xr_vector<std::byte> Storage;
	xr_array<HANDLE, 2> Handles{};
	LPPROC_THREAD_ATTRIBUTE_LIST List = nullptr;
	bool IsInitialized = false;
	bool IsReady = false;
};

static bool Wide(const xr_string_view& Text, std::wstring& Result, xr_string& Error)
{
	if (Text.size() > static_cast<size_t>(std::numeric_limits<int>::max()))
	{
		return Fail(Error, "Process argument is too long.");
	}
	if (Text.empty())
	{
		Result.clear();
		return true;
	}
	const int Size = ::MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, Text.data(), static_cast<int>(Text.size()), nullptr, 0);
	if (!Size)
	{
		return SystemError(Error, "Invalid UTF-8 process argument", ::GetLastError());
	}
	Result.resize(Size);
	return ::MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, Text.data(), static_cast<int>(Text.size()), Result.data(), Size) != 0 || SystemError(Error, "Cannot convert process argument", ::GetLastError());
}

static void Quote(const std::wstring& Argument, std::wstring& CommandLine)
{
	CommandLine += L'"';
	size_t Slashes = 0;
	for (const wchar_t Letter : Argument)
	{
		if (Letter == L'\\')
		{
			++Slashes;
			continue;
		}
		CommandLine.append(Letter == L'"' ? Slashes * 2 + 1 : Slashes, L'\\');
		CommandLine += Letter;
		Slashes = 0;
	}
	CommandLine.append(Slashes * 2, L'\\');
	CommandLine += L'"';
}

class Child final
{
public:
	Child(const xr_vector<xr_string>& Command, char** Environment, xr_string& Error)
	{
		std::wstring CommandLine;
		for (const auto& Argument : Command)
		{
			std::wstring Converted;
			if (!Wide(Argument, Converted, Error))
			{
				return;
			}
			if (!CommandLine.empty())
			{
				CommandLine += L' ';
			}
			Quote(Converted, CommandLine);
		}
		if (CommandLine.size() >= 32767)
		{
			Fail(Error, "Process command line exceeds the Windows limit.");
			return;
		}
		xr_vector<std::wstring> Variables;
		for (char** Entry = Environment; *Entry; ++Entry)
		{
			std::wstring Converted;
			if (!Wide(*Entry, Converted, Error))
			{
				return;
			}
			Variables.push_back(std::move(Converted));
		}
		const auto LessLambda = [](const std::wstring& Left, const std::wstring& Right)
		{ return ::CompareStringOrdinal(Left.c_str(), -1, Right.c_str(), -1, true) == CSTR_LESS_THAN; };
		std::sort(Variables.begin(), Variables.end(), LessLambda);
		xr_vector<wchar_t> EnvironmentBlock;
		for (const auto& Entry : Variables)
		{
			EnvironmentBlock.insert(EnvironmentBlock.end(), Entry.begin(), Entry.end());
			EnvironmentBlock.push_back(L'\0');
		}
		EnvironmentBlock.push_back(L'\0');
		if (Variables.empty())
		{
			EnvironmentBlock.push_back(L'\0');
		}
		SECURITY_ATTRIBUTES Security{sizeof(Security), nullptr, true};
		HANDLE Read = nullptr, Write = nullptr;
		if (!::CreatePipe(&Read, &Write, &Security, 0))
		{
			SystemError(Error, "Cannot create process output pipe", ::GetLastError());
			return;
		}
		Output.Reset(Read);
		NativeHandle OutputWrite(Write);
		if (!::SetHandleInformation(Output.Get(), HANDLE_FLAG_INHERIT, 0))
		{
			SystemError(Error, "Cannot configure process output pipe", ::GetLastError());
			return;
		}
		NativeHandle Input(::CreateFileW(L"NUL", GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, &Security, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr));
		if (Input.Get() == INVALID_HANDLE_VALUE)
		{
			SystemError(Error, "Cannot open process input", ::GetLastError());
			return;
		}
		Job.Reset(::CreateJobObjectW(nullptr, nullptr));
		JOBOBJECT_EXTENDED_LIMIT_INFORMATION Limits{};
		Limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
		if (!Job.Get() || !::SetInformationJobObject(Job.Get(), JobObjectExtendedLimitInformation, &Limits, sizeof(Limits)))
		{
			SystemError(Error, "Cannot create process job", ::GetLastError());
			return;
		}
		HandleList InheritedHandles(OutputWrite.Get(), Input.Get(), Error);
		if (!InheritedHandles.Get())
		{
			return;
		}
		STARTUPINFOEXW Startup{};
		Startup.StartupInfo.cb = sizeof(Startup);
		Startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
		Startup.StartupInfo.hStdOutput = OutputWrite.Get();
		Startup.StartupInfo.hStdError = OutputWrite.Get();
		Startup.StartupInfo.hStdInput = Input.Get();
		Startup.lpAttributeList = InheritedHandles.Get();
		PROCESS_INFORMATION Info{};
		if (!::CreateProcessW(nullptr, CommandLine.data(), nullptr, nullptr, true, CREATE_NO_WINDOW | CREATE_SUSPENDED | CREATE_UNICODE_ENVIRONMENT | EXTENDED_STARTUPINFO_PRESENT, EnvironmentBlock.data(), nullptr, &Startup.StartupInfo, &Info))
		{
			SystemError(Error, "Cannot start process", ::GetLastError());
			return;
		}
		Process.Reset(Info.hProcess);
		NativeHandle Thread(Info.hThread);
		if (!::AssignProcessToJobObject(Job.Get(), Process.Get()) || ::ResumeThread(Thread.Get()) == static_cast<DWORD>(-1))
		{
			SystemError(Error, "Cannot start owned process tree", ::GetLastError());
			return;
		}
		IsReady = true;
	}
	~Child()
	{
		if (Process.Get())
		{
			::TerminateJobObject(Job.Get(), 1);
			// Assignment can fail while the newly created process is still suspended.
			::TerminateProcess(Process.Get(), 1);
			::WaitForSingleObject(Process.Get(), 5000);
			const auto Start = std::chrono::steady_clock::now();
			JOBOBJECT_BASIC_ACCOUNTING_INFORMATION Accounting{};
			while (::QueryInformationJobObject(Job.Get(), JobObjectBasicAccountingInformation, &Accounting, sizeof(Accounting), nullptr) && Accounting.ActiveProcesses)
			{
				if (std::chrono::steady_clock::now() - Start >= std::chrono::seconds(5))
				{
					break;
				}
				std::this_thread::sleep_for(std::chrono::milliseconds(1));
			}
		}
	}
	Child(const Child&) = delete;
	Child& operator=(const Child&) = delete;
	bool HasStarted() const { return IsReady; }
	bool Poll(bool& HasExited, int& ExitCode, xr_string& Error) const
	{
		const auto State = ::WaitForSingleObject(Process.Get(), 0);
		HasExited = State == WAIT_OBJECT_0;
		if (State == WAIT_FAILED)
		{
			return SystemError(Error, "Cannot inspect process", ::GetLastError());
		}
		DWORD Code = 0;
		if (HasExited && !::GetExitCodeProcess(Process.Get(), &Code))
		{
			return SystemError(Error, "Cannot read process exit code", ::GetLastError());
		}
		ExitCode = static_cast<int>(Code);
		return true;
	}
	bool Read(char* Bytes, size_t Capacity, size_t& Count, xr_string& Error) const
	{
		Count = 0;
		DWORD Available = 0, Received = 0;
		if (!::PeekNamedPipe(Output.Get(), nullptr, 0, nullptr, &Available, nullptr))
		{
			const auto Code = ::GetLastError();
			return Code == ERROR_BROKEN_PIPE || SystemError(Error, "Cannot inspect process output", Code);
		}
		if (Available && !::ReadFile(Output.Get(), Bytes, static_cast<DWORD>(std::min(Capacity, static_cast<size_t>(Available))), &Received, nullptr))
		{
			return SystemError(Error, "Cannot read process output", ::GetLastError());
		}
		Count = Received;
		return true;
	}

private:
	NativeHandle Job;
	NativeHandle Process;
	NativeHandle Output;
	bool IsReady = false;
};
#else
class Descriptor final
{
public:
	explicit Descriptor(int Value = -1)
		: Value(Value) {}
	~Descriptor() { Reset(); }
	Descriptor(const Descriptor&) = delete;
	Descriptor& operator=(const Descriptor&) = delete;
	void Reset(int NewValue = -1)
	{
		if (Value >= 0)
		{
			close(Value);
		}
		Value = NewValue;
	}
	int Get() const { return Value; }

private:
	int Value;
};

class SpawnSetup final
{
public:
	SpawnSetup()
	{
		Code = posix_spawn_file_actions_init(&Actions);
		HasActions = !Code;
		if (!Code)
		{
			Code = posix_spawnattr_init(&Attributes);
			HasAttributes = !Code;
		}
	}
	~SpawnSetup()
	{
		if (HasAttributes)
		{
			posix_spawnattr_destroy(&Attributes);
		}
		if (HasActions)
		{
			posix_spawn_file_actions_destroy(&Actions);
		}
	}
	SpawnSetup(const SpawnSetup&) = delete;
	SpawnSetup& operator=(const SpawnSetup&) = delete;
	posix_spawn_file_actions_t Actions{};
	posix_spawnattr_t Attributes{};
	int Code = 0;

private:
	bool HasActions = false;
	bool HasAttributes = false;
};

static int CloseOtherDescriptors(SpawnSetup& Setup, int Input, int Output)
{
#ifdef POSIX_SPAWN_CLOEXEC_DEFAULT
	return 0;
#else
#ifdef __GLIBC__
#if __GLIBC_PREREQ(2, 34)
	return posix_spawn_file_actions_addclosefrom_np(&Setup.Actions, 3);
#endif
#endif
	class Directory final
	{
	public:
		Directory()
#ifdef IXR_LINUX
			: Handle(opendir("/proc/self/fd")){}
#else
			: Handle(opendir("/dev/fd"))
		{
		}
#endif
			~Directory()
		{
			if (Handle)
			{
				closedir(Handle);
			}
		}
		Directory(const Directory&) = delete;
		Directory& operator=(const Directory&) = delete;
		DIR* Handle;
	} Directory;
	if (!Directory.Handle)
	{
		return errno;
	}
	for (;;)
	{
		errno = 0;
		const auto Entry = readdir(Directory.Handle);
		if (!Entry)
		{
			return errno;
		}
		char* End = nullptr;
		const auto Number = strtol(Entry->d_name, &End, 10);
		if (*End || Number < 3 || Number > std::numeric_limits<int>::max() || Number == dirfd(Directory.Handle) || Number == Input || Number == Output)
		{
			continue;
		}
		const int Code = posix_spawn_file_actions_addclose(&Setup.Actions, static_cast<int>(Number));
		if (Code)
		{
			return Code;
		}
	}
#endif
}

class Child final
{
public:
	Child(const xr_vector<xr_string>& Command, char** Environment, xr_string& Error)
	{
		int Pipe[2];
#ifdef IXR_LINUX
		const int PipeResult = pipe2(Pipe, O_CLOEXEC);
#else
		const int PipeResult = pipe(Pipe);
#endif
		if (PipeResult != 0)
		{
			SystemError(Error, "Cannot create process output pipe", errno);
			return;
		}
		Descriptor Read(Pipe[0]), Write(Pipe[1]);
		// Keep pipe descriptors separate from stdin/stdout/stderr, even in headless tools.
		Output.Reset(fcntl(Read.Get(), F_DUPFD_CLOEXEC, 3));
		Descriptor OutputWrite(fcntl(Write.Get(), F_DUPFD_CLOEXEC, 3));
		if (Output.Get() < 0 || OutputWrite.Get() < 0 || fcntl(Read.Get(), F_SETFD, FD_CLOEXEC) < 0 || fcntl(Write.Get(), F_SETFD, FD_CLOEXEC) < 0 || fcntl(Output.Get(), F_SETFL, O_NONBLOCK) < 0)
		{
			SystemError(Error, "Cannot configure process output pipe", errno);
			return;
		}
		Read.Reset();
		Write.Reset();
		SpawnSetup Setup;
		int Code = Setup.Code;
		if (!Code)
		{
			Code = posix_spawn_file_actions_addopen(&Setup.Actions, STDIN_FILENO, "/dev/null", O_RDONLY, 0);
		}
		if (!Code)
		{
			Code = posix_spawn_file_actions_adddup2(&Setup.Actions, OutputWrite.Get(), STDOUT_FILENO);
		}
		if (!Code)
		{
			Code = posix_spawn_file_actions_adddup2(&Setup.Actions, OutputWrite.Get(), STDERR_FILENO);
		}
		if (!Code)
		{
			Code = posix_spawn_file_actions_addclose(&Setup.Actions, Output.Get());
		}
		if (!Code)
		{
			Code = posix_spawn_file_actions_addclose(&Setup.Actions, OutputWrite.Get());
		}
		if (!Code)
		{
			Code = CloseOtherDescriptors(Setup, Output.Get(), OutputWrite.Get());
		}
		if (!Code)
		{
			Code = posix_spawnattr_setpgroup(&Setup.Attributes, 0);
		}
		sigset_t Mask, Defaults;
		sigemptyset(&Mask);
		sigemptyset(&Defaults);
		sigaddset(&Defaults, SIGPIPE);
		sigaddset(&Defaults, SIGINT);
		sigaddset(&Defaults, SIGTERM);
		if (!Code)
		{
			Code = posix_spawnattr_setsigmask(&Setup.Attributes, &Mask);
		}
		if (!Code)
		{
			Code = posix_spawnattr_setsigdefault(&Setup.Attributes, &Defaults);
		}
		if (!Code)
		{
			short Flags = POSIX_SPAWN_SETPGROUP | POSIX_SPAWN_SETSIGMASK | POSIX_SPAWN_SETSIGDEF;
#ifdef POSIX_SPAWN_CLOEXEC_DEFAULT
			Flags |= POSIX_SPAWN_CLOEXEC_DEFAULT;
#endif
			Code = posix_spawnattr_setflags(&Setup.Attributes, Flags);
		}
		xr_vector<char*> Arguments;
		for (const auto& Argument : Command)
		{
			Arguments.push_back(const_cast<char*>(Argument.c_str()));
		}
		Arguments.push_back(nullptr);
		if (!Code)
		{
			Code = posix_spawnp(&Pid, Arguments.front(), &Setup.Actions, &Setup.Attributes, Arguments.data(), Environment);
		}
		if (Code)
		{
			Pid = -1;
			SystemError(Error, "Cannot start process", Code);
		}
	}
	~Child()
	{
		if (Pid > 0)
		{
			// Poll keeps the leader unreaped, so its process-group ID cannot be reused.
			kill(-Pid, SIGKILL);
			while (waitpid(Pid, nullptr, 0) < 0 && errno == EINTR)
			{
			}
		}
	}
	Child(const Child&) = delete;
	Child& operator=(const Child&) = delete;
	bool HasStarted() const { return Pid > 0; }
	bool Poll(bool& HasExited, int& ExitCode, xr_string& Error) const
	{
		siginfo_t Info{};
		if (waitid(P_PID, static_cast<id_t>(Pid), &Info, WEXITED | WNOHANG | WNOWAIT) != 0)
		{
			if (errno != EINTR)
			{
				return SystemError(Error, "Cannot inspect process", errno);
			}
		}
		HasExited = Info.si_pid != 0;
		ExitCode = Info.si_code == CLD_EXITED ? Info.si_status : 128 + Info.si_status;
		return true;
	}
	bool Read(char* Bytes, size_t Capacity, size_t& Count, xr_string& Error) const
	{
		const auto Received = read(Output.Get(), Bytes, Capacity);
		Count = Received > 0 ? static_cast<size_t>(Received) : 0;
		return Received >= 0 || errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR || SystemError(Error, "Cannot read process output", errno);
	}

private:
	Descriptor Output;
	pid_t Pid = -1;
};
#endif
} // namespace Platform::ProcessDetails

bool Platform::RunProcess(const xr_vector<xr_string>& Command, const ProcessOptions& Options, xr_string& Output, std::stop_token Stop)
{
	Output.clear();
	if (Stop.stop_requested())
	{
		return ProcessDetails::Fail(Output, "Process operation cancelled.");
	}
	if (Command.empty() || Command.front().empty())
	{
		return ProcessDetails::Fail(Output, "Process command is empty.");
	}
	for (const auto& Argument : Command)
	{
		if (Argument.find('\0') != xr_string::npos)
		{
			return ProcessDetails::Fail(Output, "Process argument contains a null character.");
		}
	}
	for (const auto& [Name, Value] : Options.Environment)
	{
		if (Name.empty() || Name.find_first_of("=\0", 0, 2) != xr_string::npos || Value.find('\0') != xr_string::npos)
		{
			return ProcessDetails::Fail(Output, "Invalid process environment variable.");
		}
	}
	ProcessDetails::Environment Environment(Options, Output);
	if (!Environment.Get())
	{
		return false;
	}
	ProcessDetails::Child Child(Command, Environment.Get(), Output);
	if (!Child.HasStarted())
	{
		return false;
	}
	const auto Start = std::chrono::steady_clock::now();
	for (;;)
	{
		if (Stop.stop_requested())
		{
			return ProcessDetails::Fail(Output, "Process operation cancelled.");
		}
		if (static_cast<u64>(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - Start).count()) >= Options.Timeout)
		{
			return ProcessDetails::Fail(Output, "Process operation timed out.");
		}
		bool HasExited = false;
		int ExitCode = 0;
		char Bytes[4096];
		size_t Count = 0;
		if (!Child.Poll(HasExited, ExitCode, Output) || !Child.Read(Bytes, sizeof(Bytes), Count, Output))
		{
			return false;
		}
		if (Count > Options.OutputLimit - Output.size())
		{
			return ProcessDetails::Fail(Output, "Process output exceeds its size limit.");
		}
		Output.append(Bytes, Count);
		if (HasExited && !Count)
		{
			return ExitCode == 0 || (!Output.empty() ? false : ProcessDetails::Fail(Output, "Process exited with code " + xr_string(std::to_string(ExitCode).c_str()) + "."));
		}
		if (!Count)
		{
			std::this_thread::sleep_for(std::chrono::milliseconds(15));
		}
	}
}
