#pragma once

#include <Windows.h>
#include <ShObjIdl.h>
#include <string>
#include "resource.h" // >>> IDI_OVERLAY_*

// >>> UX-PROGRESS
// Идентификаторы оверлей-иконок, которые могут быть показаны на иконке
// приложения в панели задач.
enum class TaskbarOverlay : int
{
	None = 0, // снять оверлей
	XrLC,	  // стадия xrLC
	XrAI,	  // стадия xrAI
	XrDO,	  // стадия xrDO
	Success,  // успешное завершение
	Error	  // ошибка
};
// <<< UX-PROGRESS


class CTaskbarProgress
{
public:
	static CTaskbarProgress& Instance();

	bool Initialize(HWND hwnd);
	void Release();

	void SetState(TBPFLAG state);
	void SetProgress(ULONGLONG completed, ULONGLONG total);
	void SetMarquee(bool enable);
	void Reset();

	// Подсказка над иконкой в панели задач.
	void SetTooltip(const std::wstring& text);

	// >>> UX-PROGRESS: оверлей-иконка
	// Устанавливает оверлей. Если hIcon == nullptr, оверлей снимается.
	void SetOverlayIcon(HICON hIcon, const std::wstring& description);
	void SetOverlayIcon(TaskbarOverlay overlay, const std::wstring& description);

	// Загрузить иконки из ресурсов приложения. Возвращает false, если
	// хотя бы одна иконка не загружена.
	bool LoadOverlayIcons();

	// Освободить загруженные иконки.
	void ReleaseOverlayIcons();

	bool IsInitialized() const { return m_pTaskbar != nullptr && m_hwnd != nullptr; }
	HWND GetHwnd() const { return m_hwnd; }
	// <<< UX-PROGRESS

private:
	CTaskbarProgress() = default;
	~CTaskbarProgress();
	CTaskbarProgress(const CTaskbarProgress&) = delete;
	CTaskbarProgress& operator=(const CTaskbarProgress&) = delete;

	ITaskbarList3* m_pTaskbar = nullptr;
	HWND m_hwnd = nullptr;
	bool m_needCoUninit = false;

	// >>> UX-PROGRESS
	HICON m_hIconXrLC = nullptr;
	HICON m_hIconXrAI = nullptr;
	HICON m_hIconXrDO = nullptr;
	HICON m_hIconSuccess = nullptr;
	HICON m_hIconError = nullptr;

	HICON GetIconForOverlay(TaskbarOverlay overlay) const;
	// <<< UX-PROGRESS
};