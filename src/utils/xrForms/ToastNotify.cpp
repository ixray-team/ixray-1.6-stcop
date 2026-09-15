#include "ToastNotify.h"

#include "../../xrCore/xrCore.h"
#include "cl_log.h" // >>> clMsg

using namespace WinToastLib;

CToastNotify& CToastNotify::Instance()
{
	static CToastNotify instance;
	return instance;
}

CToastNotify::~CToastNotify()
{
	Shutdown();
}

bool CToastNotify::Initialize()
{
	clMsg("* ToastNotify: Initialize called, already initialized = %d", m_initialized);

	if (m_initialized && m_pWinToast)
	{
		return true;
	}

	// Включаем внутреннее отладочное логирование WinToast.
	setDebugOutputEnabled(true);

	// >>> ВАЖНО: берём указатель ОДИН раз и сохраняем.
	m_pWinToast = WinToast::instance();
	clMsg("* ToastNotify: WinToast::instance() = 0x%p", m_pWinToast);

	if (!m_pWinToast)
	{
		clMsg("! ToastNotify: WinToast::instance() returned NULL");
		return false;
	}

	if (!WinToast::isCompatible())
	{
		clMsg("! ToastNotify: system is not compatible (requires Win8+)");
		return false;
	}

	clMsg("* ToastNotify: system IS compatible");

	m_aumi = WinToast::configureAUMI(L"IX-Ray Team", L"IX-Ray Level Builder", L"Compilers", L"1.6");

	{
		char aumi_ansi[256] = {0};
		WideCharToMultiByte(CP_ACP, 0, m_aumi.c_str(), -1, aumi_ansi, sizeof(aumi_ansi), nullptr, nullptr);
		clMsg("* ToastNotify: AUMI = '%s'", aumi_ansi);
	}

	m_pWinToast->setAppName(L"IX-Ray Level Builder");
	m_pWinToast->setAppUserModelId(m_aumi);

	WinToast::WinToastError error = WinToast::NoError;
	if (!m_pWinToast->initialize(&error))
	{
		clMsg("! ToastNotify: initialize FAILED (%d)", (int)error);
		m_pWinToast = nullptr;
		return false;
	}

	clMsg("* ToastNotify: initialize OK, error = %d, isInitialized = %d", (int)error, m_pWinToast->isInitialized() ? 1 : 0);

	// Проверяем, создан ли ярлык для AUMI (обязателен для unpackaged Win32).
	{
		wchar_t shortcutPath[MAX_PATH] = {0};
		HRESULT hr = SHGetFolderPathW(nullptr, CSIDL_APPDATA, nullptr, 0, shortcutPath);
		if (SUCCEEDED(hr))
		{
			wcscat_s(shortcutPath, L"\\Microsoft\\Windows\\Start Menu\\Programs\\IX-Ray Level Builder.lnk");
			DWORD attrs = GetFileAttributesW(shortcutPath);
			if (attrs == INVALID_FILE_ATTRIBUTES)
			{
				clMsg("! ToastNotify: shortcut NOT found at '%ls' - toasts may not work", shortcutPath);
			}
			else
			{
				clMsg("* ToastNotify: shortcut found at '%ls'", shortcutPath);
			}
		}
	}

	m_initialized = true;
	return true;
}

void CToastNotify::Shutdown()
{
	if (m_initialized && m_pWinToast)
	{
		m_pWinToast->clear();
		m_initialized = false;
		m_pWinToast = nullptr;
	}
}

void CToastNotify::Show(const std::wstring& title, const std::wstring& message)
{
	clMsg("* ToastNotify: Show('%ls', '%ls')", title.c_str(), message.c_str());

	if (!m_initialized && !Initialize())
	{
		clMsg("! ToastNotify: Show skipped - not initialized");
		return;
	}

	if (!m_pWinToast)
	{
		clMsg("! ToastNotify: Show skipped - m_pWinToast is NULL");
		return;
	}

	// >>> Явная проверка перед showToast.
	if (!m_pWinToast->isInitialized())
	{
		clMsg("! ToastNotify: m_pWinToast->isInitialized() == false, re-initializing...");
		m_initialized = false;
		if (!Initialize())
		{
			clMsg("! ToastNotify: re-initialize FAILED, show skipped");
			return;
		}
	}

	WinToastTemplate templ(WinToastTemplate::Text02);
	templ.setTextField(title, WinToastTemplate::FirstLine);
	templ.setTextField(message, WinToastTemplate::SecondLine);
	templ.setAudioOption(WinToastTemplate::AudioOption::Default);
	templ.setDuration(WinToastTemplate::Duration::Short);

	WinToast::WinToastError error = WinToast::NoError;
	const INT64 id = m_pWinToast->showToast(templ, new CToastHandler(), &error);
	clMsg("* ToastNotify: showToast returned id = %lld, error = %d", id, (int)error);

	if (id < 0)
	{
		clMsg("! ToastNotify: showToast FAILED (%d)", (int)error);
	}
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
	if (!m_initialized && !Initialize())
	{
		return;
	}

	if (!m_pWinToast)
	{
		return;
	}

	if (!m_pWinToast->isInitialized())
	{
		m_initialized = false;
		if (!Initialize())
		{
			return;
		}
	}

	WinToastTemplate templ(WinToastTemplate::Text02);
	templ.setTextField(title, WinToastTemplate::FirstLine);
	templ.setTextField(message, WinToastTemplate::SecondLine);
	templ.setAudioPath(WinToastTemplate::AudioSystemFile::Alarm);
	templ.setScenario(WinToastTemplate::Scenario::Reminder);
	templ.setDuration(WinToastTemplate::Duration::Long);

	WinToast::WinToastError error = WinToast::NoError;
	const INT64 id = m_pWinToast->showToast(templ, new CToastHandler(), &error);
	clMsg("* ToastNotify: ShowError showToast returned id = %lld, error = %d", id, (int)error);
}

// --- CToastHandler implementations ---
void CToastHandler::toastActivated() const
{
	clMsg("* ToastHandler: toastActivated()");
}

void CToastHandler::toastActivated(int actionIndex) const
{
	clMsg("* ToastHandler: toastActivated(actionIndex=%d)", actionIndex);
}

void CToastHandler::toastActivated(std::wstring /*response*/) const
{
	clMsg("* ToastHandler: toastActivated(response)");
}

void CToastHandler::toastDismissed(WinToastDismissalReason state) const
{
	clMsg("* ToastHandler: toastDismissed(state=%d)", (int)state);
}

void CToastHandler::toastFailed() const
{
	clMsg("! ToastHandler: toastFailed()");
}