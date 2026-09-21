// FX: платформенный код, не лезем в кастомные контейнеры xray. 
// Пишем максимально околостандарта

#include "../xrCore/xrCore.h"
#include "ToastNotify.h"

#include "cl_log.h"

#ifdef IXR_WINDOWS
#	include <shlobj.h>
#	include <propvarutil.h>   // InitPropVariantFromString
#	include <propkey.h>       // PKEY_AppUserModel_ID

// WinRT
#	include <winrt/base.h>
#	include <winrt/Windows.Data.Xml.Dom.h>
#	include <winrt/Windows.UI.Notifications.h>
#	include <winrt/Windows.Foundation.h>

#	pragma comment(lib, "shell32.lib")
#	pragma comment(lib, "runtimeobject.lib")
#	pragma comment(lib, "windowsapp.lib")
#	pragma comment(lib, "propsys.lib")

static bool CreateStartMenuShortcut(const std::wstring& aumi)
{
	wchar_t AppDataPath[MAX_PATH] = {0};
	if (FAILED(SHGetFolderPathW(nullptr, CSIDL_APPDATA, nullptr, 0, AppDataPath)))
	{
		return false;
	}

	std::wstring ShortcutPath = std::wstring(AppDataPath) + L"\\Microsoft\\Windows\\Start Menu\\Programs\\IX-Ray Level Builder.lnk";

	if (GetFileAttributesW(ShortcutPath.c_str()) != INVALID_FILE_ATTRIBUTES)
	{
		return true;
	}

	wchar_t ExePath[MAX_PATH] = {0};
	if (!GetModuleFileNameW(nullptr, ExePath, MAX_PATH))
	{
		return false;
	}

	std::wstring ExeDir(ExePath);
	const size_t LastSlash = ExeDir.find_last_of(L"\\/");
	if (LastSlash != std::wstring::npos)
	{
		ExeDir.erase(LastSlash);
	}

	IShellLinkW* ShellLink = nullptr;
	IPersistFile* PersistFile = nullptr;
	HRESULT HandleResult = CoCreateInstance(CLSID_ShellLink, nullptr, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&ShellLink));
	if (FAILED(HandleResult))
	{
		return false;
	}

	ShellLink->SetPath(ExePath);
	ShellLink->SetWorkingDirectory(ExeDir.c_str());

	IPropertyStore* PropertyStore = nullptr;
	HandleResult = ShellLink->QueryInterface(IID_PPV_ARGS(&PropertyStore));
	if (SUCCEEDED(HandleResult))
	{
		PROPVARIANT PropVar;
		PropVariantInit(&PropVar);
		HandleResult = InitPropVariantFromString(aumi.c_str(), &PropVar);
		if (SUCCEEDED(HandleResult))
		{
			PropertyStore->SetValue(PKEY_AppUserModel_ID, PropVar);
		}
		PropVariantClear(&PropVar);
		PropertyStore->Release();
	}

	HandleResult = ShellLink->QueryInterface(IID_PPV_ARGS(&PersistFile));
	if (SUCCEEDED(HandleResult))
	{
		HandleResult = PersistFile->Save(ShortcutPath.c_str(), TRUE);
		PersistFile->Release();
	}
	ShellLink->Release();

	return SUCCEEDED(HandleResult);
}
#endif

CToastNotify& CToastNotify::Instance()
{
	static CToastNotify Instance;
	return Instance;
}

CToastNotify::~CToastNotify()
{
	Shutdown();
}

bool CToastNotify::Initialize()
{
	clMsg("* ToastNotify: Initialize called, already initialized = %d", Initialized);

	if (Initialized)
	{
		return true;
	}

#ifdef IXR_WINDOWS
	// Инициализация WinRT (одноразовая на процесс).
	try
	{
		winrt::init_apartment(winrt::apartment_type::multi_threaded);
	}
	catch (const winrt::hresult_error& e)
	{
		// RPC_E_CHANGED_MODE означает, что апартамент уже инициализирован — это ок.
		if (e.code() != RPC_E_CHANGED_MODE)
		{
			clMsg("! ToastNotify: init_apartment failed (0x%08X) - using SDL fallback", (unsigned)e.code());
		}
	}

	Aumi = L"IX-Ray Team!IX-Ray Level Builder!Compilers!1.6";

	{
		char aumi_ansi[256] = {0};
		WideCharToMultiByte(CP_ACP, 0, Aumi.c_str(), -1, aumi_ansi, sizeof(aumi_ansi), nullptr, nullptr);
		clMsg("* ToastNotify: AUMI = '%s'", aumi_ansi);
	}

	// Создаём/проверяем ярлык в Start Menu (нужен для unpackaged Win32).
	if (CreateStartMenuShortcut(Aumi))
	{
		clMsg("* ToastNotify: Start Menu shortcut OK");
	}
	else
	{
		clMsg("! ToastNotify: failed to create Start Menu shortcut - native toasts may not work");
	}
#else
	clMsg("* ToastNotify: initialized in SDL fallback mode (non-Windows)");
#endif

	Initialized = true;
	return true;
}

void CToastNotify::Shutdown()
{
	Initialized = false;
}

bool CToastNotify::ShowFallback(const std::wstring& title, const std::wstring& message)
{
	// SDL3 требует UTF-8.
	auto ToUtf8 = [](const std::wstring& w) -> std::string
	{
		if (w.empty())
		{
			return {};
		}
		const int size = WideCharToMultiByte(CP_UTF8, 0, w.c_str(), (int)w.size(), nullptr, 0, nullptr, nullptr);
		if (size <= 0)
		{
			return {};
		}
		std::string out(size, '\0');
		WideCharToMultiByte(CP_UTF8, 0, w.c_str(), (int)w.size(), out.data(), size, nullptr, nullptr);
		return out;
	};

	const std::string TitleU8 = ToUtf8(title);
	const std::string MsgU8 = ToUtf8(message);

	clMsg("* ToastNotify: SDL fallback message box ('%s', '%s')", TitleU8.c_str(), MsgU8.c_str());

	SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_INFORMATION, TitleU8.c_str(), MsgU8.c_str(), nullptr);
	return true;
}

#ifdef IXR_WINDOWS
static std::wstring XmlEscape(const std::wstring& in)
{
	std::wstring out;
	out.reserve(in.size());
	for (wchar_t c : in)
	{
		switch (c)
		{
			case L'&': out += L"&amp;"; break;
			case L'<': out += L"&lt;"; break;
			case L'>': out += L"&gt;"; break;
			case L'"': out += L"&quot;"; break;
			case L'\'':out += L"&apos;"; break;
			default: out += c; break;
		}
	}
	return out;
}

bool CToastNotify::ShowWinRT(const std::wstring& title, const std::wstring& message, bool errorStyle)
{
	using namespace winrt::Windows::Data::Xml::Dom;
	using namespace winrt::Windows::UI::Notifications;

	try
	{
		// audio: default для обычных, alarm для ошибок.
		// Scenario: reminder для ошибок (не уходит в Action Center тихо).
		// Duration: short/long.
		const wchar_t* AudioSrc = errorStyle ? L"ms-winsoundevent:Notification.Looping.Alarm"
											 : L"ms-winsoundevent:Notification.Default";
		const wchar_t* Scenario = errorStyle ? L" scenario=\"reminder\"" : L"";
		const wchar_t* Duration = errorStyle ? L"long" : L"short";

		std::wstring ShitXML =
			L"<toast activationType=\"foreground\""
			L" launch=\"default\""
			L" duration=\"" +
			std::wstring(Duration) + L"\"" +
			std::wstring(Scenario) + L">"
									 L"<visual><binding template=\"ToastGeneric\">"
									 L"<text>" +
			XmlEscape(title) + L"</text>"
							   L"<text>" +
			XmlEscape(message) + L"</text>"
								 L"</binding></visual>"
								 L"<audio src=\"" +
			std::wstring(AudioSrc) + L"\" loop=\"" +
			(errorStyle ? L"true" : L"false") + L"\"/>"
												L"</toast>";

		XmlDocument Doc;
		Doc.LoadXml(winrt::hstring(ShitXML));

		ToastNotification Toast(Doc);
		ToastNotificationManager::CreateToastNotifier(winrt::hstring(Aumi)).Show(Toast);

		clMsg("* ToastNotify: WinRT toast shown");
		return true;
	}
	catch (const winrt::hresult_error& ResultError)
	{
		clMsg("! ToastNotify: WinRT toast failed (0x%08X: %ls)", (unsigned)ResultError.code(), ResultError.message().c_str());
		return false;
	}
}
#endif

void CToastNotify::Show(const std::wstring& title, const std::wstring& message)
{
	clMsg("* ToastNotify: Show('%ls', '%ls')", title.c_str(), message.c_str());

	if (!Initialized && !Initialize())
	{
		clMsg("! ToastNotify: Show skipped - not initialized");
		return;
	}

#ifdef IXR_WINDOWS
	if (!ShowWinRT(title, message, false))
	{
		ShowFallback(title, message);
	}
#else
	ShowFallback(title, message);
#endif
}

void CToastNotify::ShowInfo(const std::wstring& title, const std::wstring& message)
{
	Show(title, message);
}

void CToastNotify::ShowSuccess(const std::wstring& title, const std::wstring& message)
{
	Show(title, message);
}

void CToastNotify::ShowError(const std::wstring& title, const std::wstring& message)
{
	clMsg("* ToastNotify: ShowError('%ls', '%ls')", title.c_str(), message.c_str());

	if (!Initialized && !Initialize())
	{
		return;
	}

#ifdef IXR_WINDOWS
	if (!ShowWinRT(title, message, true))
	{
		ShowFallback(title, message);
	}
#else
	ShowFallback(title, message);
#endif
}