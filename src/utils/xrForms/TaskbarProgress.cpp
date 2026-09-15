#include "TaskbarProgress.h"

#include "../../xrCore/xrCore.h"
#include "cl_log.h" // >>> clMsg

CTaskbarProgress& CTaskbarProgress::Instance()
{
	static CTaskbarProgress instance;
	return instance;
}

CTaskbarProgress::~CTaskbarProgress()
{
	Release();
}

bool CTaskbarProgress::Initialize(HWND hwnd)
{
	clMsg("* TaskbarProgress: Initialize called, hwnd = 0x%p", hwnd);

	if (!hwnd)
	{
		clMsg("! TaskbarProgress: hwnd is NULL - progress bar will not work");
		return false;
	}

	if (m_pTaskbar && m_hwnd == hwnd)
	{
		return true;
	}

	Release();

	HRESULT hrCo = CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE);
	clMsg("* TaskbarProgress: CoInitializeEx returned 0x%08X", hrCo);

	m_needCoUninit = SUCCEEDED(hrCo);
	if (FAILED(hrCo) && hrCo != RPC_E_CHANGED_MODE)
	{
		clMsg("! TaskbarProgress: CoInitializeEx failed (0x%08X)", hrCo);
		return false;
	}

	HRESULT hr = CoCreateInstance(CLSID_TaskbarList, nullptr, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&m_pTaskbar));
	clMsg("* TaskbarProgress: CoCreateInstance returned 0x%08X, ptr = 0x%p", hr, m_pTaskbar);

	if (FAILED(hr) || !m_pTaskbar)
	{
		clMsg("! TaskbarProgress: CoCreateInstance failed (0x%08X)", hr);
		m_pTaskbar = nullptr;
		return false;
	}

	hr = m_pTaskbar->HrInit();
	clMsg("* TaskbarProgress: HrInit returned 0x%08X", hr);

	if (FAILED(hr))
	{
		clMsg("! TaskbarProgress: HrInit failed (0x%08X)", hr);
		m_pTaskbar->Release();
		m_pTaskbar = nullptr;
		return false;
	}

	m_hwnd = hwnd;

	// >>> UX-PROGRESS: подгружаем иконки сразу
	if (!LoadOverlayIcons())
	{
		clMsg("! TaskbarProgress: some overlay icons are missing - overlays may be skipped");
	}
	// <<< UX-PROGRESS

	clMsg("* TaskbarProgress: initialized successfully for hwnd = 0x%p", m_hwnd);
	return true;
}

void CTaskbarProgress::Release()
{
	if (m_pTaskbar)
	{
		if (m_hwnd)
		{
			HRESULT hr = m_pTaskbar->SetProgressState(m_hwnd, TBPF_NOPROGRESS);
			clMsg("* TaskbarProgress: Release - SetProgressState(NOPROGRESS) = 0x%08X", hr);
		}

		// >>> UX-PROGRESS: снимаем оверлей и освобождаем ресурсы
		if (m_hwnd)
		{
			m_pTaskbar->SetOverlayIcon(m_hwnd, nullptr, L"");
		}

		ReleaseOverlayIcons();
		// <<< UX-PROGRESS

		m_pTaskbar->Release();
		m_pTaskbar = nullptr;
	}

	m_hwnd = nullptr;

	if (m_needCoUninit)
	{
		CoUninitialize();
		m_needCoUninit = false;
	}
}

void CTaskbarProgress::SetState(TBPFLAG state)
{
	if (!m_pTaskbar || !m_hwnd)
	{
		return;
	}

	HRESULT hr = m_pTaskbar->SetProgressState(m_hwnd, state);
	clMsg("* TaskbarProgress: SetProgressState(0x%X) = 0x%08X", (unsigned)state, hr);
}

void CTaskbarProgress::SetProgress(ULONGLONG completed, ULONGLONG total)
{
	if (!m_pTaskbar || !m_hwnd || total == 0)
	{
		return;
	}

	HRESULT hr = m_pTaskbar->SetProgressValue(m_hwnd, completed, total);
	clMsg("* TaskbarProgress: SetProgressValue(%llu/%llu) = 0x%08X", completed, total, hr);
}

void CTaskbarProgress::SetMarquee(bool enable)
{
	SetState(enable ? TBPF_INDETERMINATE : TBPF_NOPROGRESS);
}

void CTaskbarProgress::Reset()
{
	SetState(TBPF_NOPROGRESS);
}

void CTaskbarProgress::SetTooltip(const std::wstring& text)
{
	if (!m_pTaskbar || !m_hwnd)
	{
		return;
	}

	HRESULT hr = m_pTaskbar->SetThumbnailTooltip(m_hwnd, text.c_str());
	clMsg("* TaskbarProgress: SetThumbnailTooltip('%ls') = 0x%08X", text.c_str(), hr);
}


// =========================================================================
// >>> UX-PROGRESS: оверлей-иконки
// =========================================================================

bool CTaskbarProgress::LoadOverlayIcons()
{
	// >>> Здесь предполагается, что иконки добавлены в .rc проекта
	//     как ICON-ресурсы с идентификаторами:
	//         IDI_OVERLAY_XRLC
	//         IDI_OVERLAY_XRAI
	//         IDI_OVERLAY_XRDO
	//         IDI_OVERLAY_SUCCESS
	//         IDI_OVERLAY_ERROR
	//
	//     Как их создать / подключить — см. раздел "Ресурсы" ниже.
	//
	//     LoadImage с размером 16x16 гарантирует, что мы получим
	//     маленький вариант иконки для оверлея (требование SetOverlayIcon:
	//     16×16 при 96 DPI).
	HINSTANCE hInst = GetModuleHandle(nullptr);

	auto load = [hInst](int id) -> HICON
	{
		return (HICON)LoadImageW(hInst, MAKEINTRESOURCEW(id), IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
	};

	if (!m_hIconXrLC)
	{
		m_hIconXrLC = load(IDI_OVERLAY_XRLC);
	}
	if (!m_hIconXrAI)
	{
		m_hIconXrAI = load(IDI_OVERLAY_XRAI);
	}
	if (!m_hIconXrDO)
	{
		m_hIconXrDO = load(IDI_OVERLAY_XRDO);
	}
	if (!m_hIconSuccess)
	{
		m_hIconSuccess = load(IDI_OVERLAY_SUCCESS);
	}
	if (!m_hIconError)
	{
		m_hIconError = load(IDI_OVERLAY_ERROR);
	}

	bool ok = m_hIconXrLC && m_hIconXrAI && m_hIconXrDO && m_hIconSuccess && m_hIconError;

	clMsg("* TaskbarProgress: overlay icons loaded - XrLC=%p XrAI=%p XrDO=%p Success=%p Error=%p", m_hIconXrLC, m_hIconXrAI, m_hIconXrDO, m_hIconSuccess, m_hIconError);

	return ok;
}

void CTaskbarProgress::ReleaseOverlayIcons()
{
	auto destroy = [](HICON& h)
	{
		if (h)
		{
			DestroyIcon(h);
			h = nullptr;
		}
	};

	destroy(m_hIconXrLC);
	destroy(m_hIconXrAI);
	destroy(m_hIconXrDO);
	destroy(m_hIconSuccess);
	destroy(m_hIconError);
}

HICON CTaskbarProgress::GetIconForOverlay(TaskbarOverlay overlay) const
{
	switch (overlay)
	{
		case TaskbarOverlay::XrLC:
			return m_hIconXrLC;
		case TaskbarOverlay::XrAI:
			return m_hIconXrAI;
		case TaskbarOverlay::XrDO:
			return m_hIconXrDO;
		case TaskbarOverlay::Success:
			return m_hIconSuccess;
		case TaskbarOverlay::Error:
			return m_hIconError;
		case TaskbarOverlay::None:
		default:
			return nullptr;
	}
}

void CTaskbarProgress::SetOverlayIcon(HICON hIcon, const std::wstring& description)
{
	if (!m_pTaskbar || !m_hwnd)
	{
		clMsg("! TaskbarProgress: SetOverlayIcon skipped - not initialized");
		return;
	}

	// SetOverlayIcon копирует иконку себе, но ответственность за освобождение
	// HICON остаётся на приложении[reference:1]. Мы не освобождаем загруженные
	// при Initialize() иконки до Release() — они переиспользуются между стадиями.
	HRESULT hr = m_pTaskbar->SetOverlayIcon(m_hwnd, hIcon, description.c_str());
	clMsg("* TaskbarProgress: SetOverlayIcon(ptr=0x%p, desc='%ls') = 0x%08X", hIcon, description.c_str(), hr);
}

void CTaskbarProgress::SetOverlayIcon(TaskbarOverlay overlay, const std::wstring& description)
{
	HICON hIcon = GetIconForOverlay(overlay);
	SetOverlayIcon(hIcon, description);
}

// <<< UX-PROGRESS