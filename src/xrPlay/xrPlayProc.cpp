//-----------------------------------------------------------------------------
// Name: logDlgProc()
// Desc: Static msg handler which passes messages to the logo wnd.
//-----------------------------------------------------------------------------
INT_PTR CALLBACK logDlgProc(HWND hw, UINT msg, WPARAM wp, LPARAM lp) {
	switch (msg) 
	{
	case WM_DESTROY:
		break;
	case WM_CLOSE:
		DestroyWindow(hw);
		break;
	case WM_COMMAND:
		if (LOWORD(wp) == IDCANCEL)
			DestroyWindow(hw);
		break;
	default:
		return FALSE;
	}
	return TRUE;
}
