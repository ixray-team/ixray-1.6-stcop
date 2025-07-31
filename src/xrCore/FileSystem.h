//----------------------------------------------------
// file: FileSystem.h
//----------------------------------------------------
#pragma once
#define BACKUP_FILE_LEVEL 5

class XRCORE_API EFS_Utils
{
protected:
	bool 		GetOpenNameInternal(LPCSTR initial, LPSTR buffer, int sz_buf, bool bMulti = false, LPCSTR offset = 0, int start_flt_ext = -1, const char* ext = nullptr);

public:
	EFS_Utils() = default;
	virtual 	~EFS_Utils() = default;
	void 		_initialize() {}
	void 		_destroy() {}

	LPCSTR		GenerateName(LPCSTR base_path, LPCSTR base_name, LPCSTR def_ext, LPSTR out_name, u32 const out_name_size);

	bool 		GetOpenName(LPCSTR initial, xr_string& buf, bool bMulti = false, LPCSTR offset = 0, int start_flt_ext = -1, const char* ext = nullptr);


	template<xr_ssnt_t Size>
	IC bool GetOpenName(xr_stack_tstring<Size>& path_to_file, const xr_char_t* mask, bool many_picking = false)
	{
#ifdef IXR_WINDOWS
		OPENFILENAME ofn;       // common dialog box structure

		// Initialize OPENFILENAME
		ZeroMemory(&ofn, sizeof(ofn));
		ofn.lStructSize = sizeof(ofn);
		ofn.hwndOwner = nullptr;
		ofn.lpstrFile = path_to_file.data();
		ofn.nMaxFile = sizeof(path_to_file);
		ofn.lpstrFilter = mask;
		ofn.nFilterIndex = 1;
		ofn.lpstrFileTitle = NULL;
		ofn.nMaxFileTitle = 0;
		ofn.lpstrInitialDir = NULL;
		ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;

		bool result = GetOpenFileName(&ofn) == TRUE;

		return result;
#else
		return false;
#endif
	}

	template<xr_ssnt_t Size>
	IC bool CopyTextToClipboard(xr_stack_string<Size>& buffer)
	{
		if (buffer.empty())
			return false;

#ifdef IXR_WINDOWS
		HGLOBAL hMem = GlobalAlloc(GMEM_MOVEABLE, buffer.size());

		std::memcpy(GlobalLock(hMem), buffer.c_str(), buffer.size());
		GlobalUnlock(hMem);

		BOOL nStatus = OpenClipboard(NULL);

		if (!nStatus)
		{
			Msg("[win32]: Failed to OpenClipboard!");
			return false;
		}
			
		nStatus = EmptyClipboard();
		if (!nStatus)
		{
			Msg("[win32]: Failed to EmptyClipboard!");
			return false;
		}

		HANDLE hStatus = SetClipboardData(CF_TEXT, hMem);

		if (!hStatus)
		{
			Msg("[win32]: Failed to SetClipboardData");
			return false;
		}

		nStatus = CloseClipboard();
		if (!nStatus)
		{
			Msg("[win32]: Failed to CloseClipboard!");
			return false;
		}

		return true;
#else
		return false;
#endif
	}

	bool 		GetSaveName(LPCSTR initial, string_path& buffer, LPCSTR offset = 0, int start_flt_ext = -1, const char* ext = nullptr);
	bool 		GetSaveName(LPCSTR initial, xr_string& buf, LPCSTR offset = 0, int start_flt_ext = -1, const char* ext = nullptr);

	void 		MarkFile(LPCSTR fn, bool bDeleteSource);

	xr_string 	AppendFolderToName(xr_string& tex_name, int depth, BOOL full_name);

	LPCSTR		AppendFolderToName(LPSTR tex_name, u32 const tex_name_size, int depth, BOOL full_name);
	LPCSTR		AppendFolderToName(LPCSTR src_name, LPSTR dest_name, u32 const dest_name_size, int depth, BOOL full_name);

	xr_string	ChangeFileExt(LPCSTR src, LPCSTR ext);
	xr_string	ChangeFileExt(const xr_string& src, LPCSTR ext);

	xr_string	ExtractFileName(LPCSTR src);
	xr_string	ExtractFilePath(LPCSTR src);
	xr_string	ExtractFileExt(LPCSTR src);
	xr_string	ExcludeBasePath(LPCSTR full_path, LPCSTR excl_path);
};

extern XRCORE_API EFS_Utils* xr_EFS;
#define EFS (*xr_EFS)