#pragma once

#include <string>
#include "wintoastlib.h"

class CToastNotify
{
public:
	static CToastNotify& Instance();

	bool Initialize();
	void Shutdown();
	bool IsInitialized() const { return m_initialized; }
	const std::wstring& GetAumi() const { return m_aumi; }

	void Show(const std::wstring& title, const std::wstring& message);
	void ShowInfo(const std::wstring& title, const std::wstring& message);
	void ShowSuccess(const std::wstring& title, const std::wstring& message);
	void ShowError(const std::wstring& title, const std::wstring& message);

private:
	CToastNotify() = default;
	~CToastNotify();
	CToastNotify(const CToastNotify&) = delete;
	CToastNotify& operator=(const CToastNotify&) = delete;

	bool m_initialized = false;
	std::wstring m_aumi;
	WinToastLib::WinToast* m_pWinToast = nullptr; // сохранённый экземпляр
};

class CToastHandler : public WinToastLib::IWinToastHandler
{
public:
	void toastActivated() const override;
	void toastActivated(int actionIndex) const override;
	void toastActivated(std::wstring response) const override;
	void toastDismissed(WinToastDismissalReason state) const override;
	void toastFailed() const override;
};