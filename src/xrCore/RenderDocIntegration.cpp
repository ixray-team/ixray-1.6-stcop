#include "stdafx.h"
#include "RenderDocIntegration.h"

#ifdef IXR_WINDOWS
#include "../3rd-party/renderdoc/renderdoc/api/app/renderdoc_app.h"

namespace
{
HMODULE RenderDocModule = nullptr;
RENDERDOC_API_1_6_0* RenderDocApi = nullptr;
xr_string CapturePathTemplate;
bool InitializationAttempted = false;
void* ActiveCaptureDevice = nullptr;
void* ActiveCaptureWindow = nullptr;
bool ActiveCapturePairValid = false;

xr_string WideToUtf8(const wchar_t* Value)
{
	if (!Value || !Value[0])
	{
		return {};
	}

	const int Length = WideCharToMultiByte(
		CP_UTF8, 0, Value, -1, nullptr, 0, nullptr, nullptr
	);
	if (Length <= 1)
	{
		return {};
	}

	xr_string Result;
	Result.resize(static_cast<size_t>(Length));
	WideCharToMultiByte(
		CP_UTF8, 0, Value, -1, Result.data(), Length, nullptr, nullptr
	);
	Result.pop_back();
	return Result;
}

std::wstring GetEnvironmentPath(const wchar_t* Name)
{
	const DWORD Required = GetEnvironmentVariableW(Name, nullptr, 0);
	if (Required <= 1)
	{
		return {};
	}

	std::wstring Value;
	Value.resize(Required);
	const DWORD Written =
		GetEnvironmentVariableW(Name, Value.data(), Required);
	if (Written == 0 || Written >= Required)
	{
		return {};
	}

	Value.resize(Written);
	return Value;
}

std::wstring GetExecutableDirectory()
{
	std::wstring Path;
	Path.resize(32768);
	const DWORD Length = GetModuleFileNameW(
		nullptr, Path.data(), static_cast<DWORD>(Path.size())
	);
	if (Length == 0 || Length >= Path.size())
	{
		return {};
	}

	Path.resize(Length);
	const size_t Separator = Path.find_last_of(L"\\/");
	if (Separator == std::wstring::npos)
	{
		return {};
	}

	Path.resize(Separator);
	return Path;
}

HMODULE LoadRenderDocModule(xr_string& LoadedPath)
{
	if (HMODULE Existing = GetModuleHandleW(L"renderdoc.dll"))
	{
		wchar_t ModulePath[32768] = {};
		GetModuleFileNameW(
			Existing, ModulePath, static_cast<DWORD>(std::size(ModulePath))
		);
		LoadedPath = WideToUtf8(ModulePath);
		return Existing;
	}

	xr_vector<std::wstring> Candidates;
	if (std::wstring ExplicitPath = GetEnvironmentPath(L"RENDERDOC_DLL");
		!ExplicitPath.empty())
	{
		Candidates.push_back(std::move(ExplicitPath));
	}

	if (std::wstring ExecutableDirectory = GetExecutableDirectory();
		!ExecutableDirectory.empty())
	{
		Candidates.push_back(
			ExecutableDirectory + L"\\renderdoc.dll"
		);
	}

	// Allows deployments which place RenderDoc on PATH.
	Candidates.emplace_back(L"renderdoc.dll");

	if (std::wstring ProgramFilesPath = GetEnvironmentPath(L"ProgramFiles");
		!ProgramFilesPath.empty())
	{
		Candidates.push_back(
			ProgramFilesPath + L"\\RenderDoc\\renderdoc.dll"
		);
	}

	for (const std::wstring& Candidate : Candidates)
	{
		HMODULE Module = LoadLibraryW(Candidate.c_str());
		if (!Module)
		{
			continue;
		}

		wchar_t ModulePath[32768] = {};
		GetModuleFileNameW(
			Module, ModulePath, static_cast<DWORD>(std::size(ModulePath))
		);
		LoadedPath = WideToUtf8(ModulePath);
		return Module;
	}

	return nullptr;
}
} // namespace
#endif

bool xrRenderDoc::Initialize()
{
#ifdef IXR_WINDOWS
	if (InitializationAttempted)
	{
		return RenderDocApi != nullptr;
	}

	InitializationAttempted = true;

	xr_string LoadedPath;
	RenderDocModule = LoadRenderDocModule(LoadedPath);
	if (!RenderDocModule)
	{
		Msg("! RenderDoc: -renderdoc requested, but renderdoc.dll was not found. "
			"Install RenderDoc or set RENDERDOC_DLL to its full path.");
		return false;
	}

	const auto GetApi = reinterpret_cast<pRENDERDOC_GetAPI>(
		GetProcAddress(RenderDocModule, "RENDERDOC_GetAPI")
	);
	if (!GetApi)
	{
		Msg("! RenderDoc: RENDERDOC_GetAPI is missing in %s",
			LoadedPath.c_str());
		return false;
	}

	void* Api = nullptr;
	if (GetApi(eRENDERDOC_API_Version_1_6_0, &Api) != 1 || !Api)
	{
		Msg("! RenderDoc: API 1.6.0 is not available in %s",
			LoadedPath.c_str());
		return false;
	}
	RenderDocApi = static_cast<RENDERDOC_API_1_6_0*>(Api);

	int Major = 0;
	int Minor = 0;
	int Patch = 0;
	RenderDocApi->GetAPIVersion(&Major, &Minor, &Patch);

	// xrCore owns the process crash handler and native debuggers must receive
	// exceptions directly. RenderDoc's Breakpad handler cannot launch while
	// Rider/LLDB is attached and can otherwise compete with xrDebug while a
	// capture is finalized from Present().
	RenderDocApi->UnloadCrashHandler();
	RenderDocApi->SetCaptureOptionU32(
		eRENDERDOC_Option_DebugOutputMute, 1
	);

	RENDERDOC_InputButton CaptureKey = eRENDERDOC_Key_F12;
	RenderDocApi->SetCaptureKeys(&CaptureKey, 1);

	std::error_code Error;
	std::filesystem::path WorkingDirectory =
		std::filesystem::current_path(Error);
	if (Error)
	{
		Error.clear();
		WorkingDirectory = std::filesystem::path(GetExecutableDirectory());
	}
	std::filesystem::path CaptureDirectory =
		WorkingDirectory / "logs" / "renderdoc";
	std::filesystem::create_directories(CaptureDirectory, Error);
	const char* GraphicsApi =
		Core.Params && strstr(Core.Params, "-dx12")
			? "d3d12"
			: "vulkan";
	const xr_string CaptureName =
		xr_string(Core.ApplicationName) + "_" + GraphicsApi + "_" +
		xr_string::ToString(static_cast<u32>(GetCurrentProcessId()));
	const std::filesystem::path CaptureTemplate =
		CaptureDirectory / CaptureName.c_str();
	CapturePathTemplate = CaptureTemplate.generic_string().c_str();
	RenderDocApi->SetCaptureFilePathTemplate(CapturePathTemplate.c_str());

	Msg("* RenderDoc: API %d.%d.%d loaded before graphics device creation",
		Major,
		Minor,
		Patch);
	Msg("* RenderDoc: module=%s", LoadedPath.c_str());
	Msg("* RenderDoc: internal crash handler disabled; xrCore/debugger owns "
		"exception handling");
	Msg("* RenderDoc: capture key=F12, output=%s*.rdc",
		CapturePathTemplate.c_str());
	if (Error)
	{
		Msg("! RenderDoc: failed to create capture directory: %s",
			Error.message().c_str());
	}
	return true;
#else
	Msg("! RenderDoc: -renderdoc is supported only on Windows");
	return false;
#endif
}

bool xrRenderDoc::IsAvailable()
{
#ifdef IXR_WINDOWS
	return RenderDocApi != nullptr;
#else
	return false;
#endif
}

bool xrRenderDoc::IsLoaded()
{
#ifdef IXR_WINDOWS
	return RenderDocModule != nullptr ||
		   GetModuleHandleW(L"renderdoc.dll") != nullptr;
#else
	return false;
#endif
}

RENDERDOC_API_1_6_0* xrRenderDoc::GetApi()
{
#ifdef IXR_WINDOWS
	return RenderDocApi;
#else
	return nullptr;
#endif
}

const char* xrRenderDoc::GetCapturePathTemplate()
{
#ifdef IXR_WINDOWS
	return CapturePathTemplate.c_str();
#else
	return "";
#endif
}

bool xrRenderDoc::TriggerCapture()
{
#ifdef IXR_WINDOWS
	if (!RenderDocApi)
	{
		return false;
	}

	RenderDocApi->TriggerCapture();
	return true;
#else
	return false;
#endif
}

bool xrRenderDoc::BeginCapture(void* WindowHandle, void* DeviceHandle)
{
#ifdef IXR_WINDOWS
	if (!RenderDocApi || RenderDocApi->IsFrameCapturing())
	{
		return false;
	}

	ActiveCaptureDevice = nullptr;
	ActiveCaptureWindow = nullptr;
	ActiveCapturePairValid = false;
	if (DeviceHandle && WindowHandle)
	{
		// Иначе TriggerCapture может выбрать другую API/window pair или не
		// найти активную D3D12 swapchain при скрытом smoke-окне.
		RenderDocApi->SetActiveWindow(DeviceHandle, WindowHandle);
	}

	const auto TryBegin = [&](void* CandidateDevice, void* CandidateWindow)
	{
		RenderDocApi->StartFrameCapture(CandidateDevice, CandidateWindow);
		if (!RenderDocApi->IsFrameCapturing())
		{
			return false;
		}

		ActiveCaptureDevice = CandidateDevice;
		ActiveCaptureWindow = CandidateWindow;
		ActiveCapturePairValid = true;
		return true;
	};

	// RenderDoc сопоставляет D3D12 device по COM identity, а отдельные drivers
	// по-разному обрабатывают пару с HWND. Проверяем разрешённые API wildcard
	// комбинации и сохраняем ту же пару для EndFrameCapture.
	if (TryBegin(DeviceHandle, WindowHandle))
	{
		return true;
	}
	if (DeviceHandle && TryBegin(DeviceHandle, nullptr))
	{
		return true;
	}
	if (WindowHandle && TryBegin(nullptr, WindowHandle))
	{
		return true;
	}
	return TryBegin(nullptr, nullptr);
#else
	return false;
#endif
}

bool xrRenderDoc::EndCapture(void* WindowHandle, void* DeviceHandle)
{
#ifdef IXR_WINDOWS
	if (!RenderDocApi || !RenderDocApi->IsFrameCapturing())
	{
		return false;
	}

	void* CaptureDevice = ActiveCapturePairValid
		? ActiveCaptureDevice
		: DeviceHandle;
	void* CaptureWindow = ActiveCapturePairValid
		? ActiveCaptureWindow
		: WindowHandle;
	const bool Succeeded =
		RenderDocApi->EndFrameCapture(CaptureDevice, CaptureWindow) != 0;
	ActiveCaptureDevice = nullptr;
	ActiveCaptureWindow = nullptr;
	ActiveCapturePairValid = false;
	return Succeeded;
#else
	return false;
#endif
}
