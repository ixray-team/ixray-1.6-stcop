#include "stdafx.h"

#ifdef INGAME_EDITOR
#	include "../include/editor/ide.hpp"
#	include "engine_impl.hpp"
#endif // #ifdef INGAME_EDITOR

extern LRESULT CALLBACK WndProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam );

#ifdef INGAME_EDITOR
void CRenderDevice::initialize_editor	()
{
	m_editor_module		= LoadLibraryA("editor.dll");
	if (!m_editor_module) {
		Msg				("! cannot load library \"editor.dll\"");
		return;
	}

	m_editor_initialize	= (initialize_function_ptr)GetProcAddress(m_editor_module, "initialize");
	VERIFY				(m_editor_initialize);

	m_editor_finalize	= (finalize_function_ptr)GetProcAddress(m_editor_module, "finalize");
	VERIFY				(m_editor_finalize);

	m_engine			= xr_new<engine_impl>();
	m_editor_initialize	(m_editor, m_engine);
	VERIFY				(m_editor);

	m_hWnd				= m_editor->view_handle();
	VERIFY				(m_hWnd != INVALID_HANDLE_VALUE);
}
#endif // #ifdef INGAME_EDITOR

void CRenderDevice::Initialize			()
{
	Log("Initializing Engine...");
	TimerGlobal.Start			();
	TimerMM.Start				();

#ifdef INGAME_EDITOR
	if (strstr(Core.Params,"-editor"))
		initialize_editor		();
#endif // #ifdef INGAME_EDITOR

    // Save window properties
    m_dwWindowStyle = GetWindowLong( m_hWnd, GWL_STYLE );
    GetWindowRect	( m_hWnd, &m_rcWindowBounds );
    GetClientRect	( m_hWnd, &m_rcWindowClient );
}

