#include "stdafx.h"

bool Platform::OpenFileWnd(char* buffer, size_t sz_buf, FS_Path* P, int start_flt_ext, string1024 flt, LPCSTR offset, bool bMulti)
{
	OPENFILENAME ofn;
	Memory.mem_fill(&ofn, 0, sizeof(ofn));

	if (xr_strlen(buffer))
	{
		string_path		dr;
		if (!(buffer[0] == '\\' && buffer[1] == '\\')) { // if !network
			_splitpath(buffer, dr, 0, 0, 0);

			if (0 == dr[0])
			{
				string_path		bb;
				P->_update(bb, buffer);
				xr_strcpy(buffer, sz_buf, bb);
			}
		}
	}

	ofn.lStructSize = sizeof(OPENFILENAME);
	ofn.hwndOwner = GetForegroundWindow();
	ofn.lpstrDefExt = xr_strdup(ANSI_TO_TCHAR(P->m_DefExt));
	ofn.lpstrFile = xr_strdup(ANSI_TO_TCHAR(buffer));
	ofn.nMaxFile = (DWORD)sz_buf;
	ofn.lpstrFilter = xr_strdup(ANSI_TO_TCHAR(flt));
	ofn.nFilterIndex = start_flt_ext + 2;
	ofn.lpstrTitle = L"Open a File";

	string512 path;
	xr_strcpy(path, (offset && offset[0]) ? offset : P->m_Path);
	ofn.lpstrInitialDir = xr_strdup(ANSI_TO_TCHAR(path));
	ofn.Flags = OFN_PATHMUSTEXIST |
		OFN_FILEMUSTEXIST |
		OFN_HIDEREADONLY |
		OFN_FILEMUSTEXIST |
		OFN_NOCHANGEDIR |
		(bMulti ? OFN_ALLOWMULTISELECT | OFN_EXPLORER : 0);

	ofn.FlagsEx = OFN_EX_NOPLACESBAR;

	bool bRes = !!GetOpenFileName(&ofn);
	if (!bRes)
	{
		u32 err = CommDlgExtendedError();
		switch (err)
		{
		case FNERR_BUFFERTOOSMALL:
			Log("Too many files selected.");
			break;
		}
	}
	if (bRes && bMulti)
	{
		Msg("buff=%s", buffer);
		int cnt = _GetItemCount(buffer, 0x0);
		if (cnt > 1)
		{
			char 		dir[255 * 255];
			char 		buf[255 * 255];
			char 		fns[255 * 255];

			xr_strcpy(dir, buffer);
			xr_strcpy(fns, dir);
			xr_strcat(fns, "\\");
			xr_strcat(fns, _GetItem(buffer, 1, buf, 0x0));

			for (int i = 2; i < cnt; i++)
			{
				xr_strcat(fns, ",");
				xr_strcat(fns, dir);
				xr_strcat(fns, "\\");
				xr_strcat(fns, _GetItem(buffer, i, buf, 0x0));
			}
			xr_strcpy(buffer, sz_buf, fns);
		}
	}
	_strlwr(buffer);

	return bRes;
}