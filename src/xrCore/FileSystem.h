//----------------------------------------------------
// file: FileSystem.h
//----------------------------------------------------
#pragma once
#define BACKUP_FILE_LEVEL 5

class XRCORE_API EFS_Utils
{
protected:
	bool 		GetOpenNameInternal(const char* initial, LPSTR buffer, int sz_buf, bool bMulti = false, const char* offset = nullptr, int start_flt_ext = -1, const char* ext = nullptr);

public:
	EFS_Utils() = default;
	virtual 	~EFS_Utils() = default;
	void 		_initialize() {}
	void 		_destroy() {}

	const char*		GenerateName(const char* base_path, const char* base_name, const char* def_ext, LPSTR out_name, u32 const out_name_size);

	bool 		GetOpenName(const char* initial, xr_string& buf, bool bMulti = false, const char* offset = nullptr, int start_flt_ext = -1, const char* ext = nullptr);


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
		ofn.lpstrFileTitle = nullptr;
		ofn.nMaxFileTitle = 0;
		ofn.lpstrInitialDir = nullptr;
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

		bool nStatus = OpenClipboard(nullptr);

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

	bool 		GetSaveName(const char* initial, string_path& buffer, const char* offset = nullptr, int start_flt_ext = -1, const char* ext = nullptr);
	bool 		GetSaveName(const char* initial, xr_string& buf, const char* offset = nullptr, int start_flt_ext = -1, const char* ext = nullptr);

	void 		MarkFile(const char* fn, bool bDeleteSource);

	xr_string 	AppendFolderToName(xr_string& tex_name, int depth, bool full_name);

	const char*		AppendFolderToName(LPSTR tex_name, u32 const tex_name_size, int depth, bool full_name);
	const char*		AppendFolderToName(const char* src_name, LPSTR dest_name, u32 const dest_name_size, int depth, bool full_name);

	xr_string	ChangeFileExt(const char* src, const char* ext);
	xr_string	ChangeFileExt(const xr_string& src, const char* ext);

	xr_string	ExtractFileName(const char* src);
	xr_string	ExtractFilePath(const char* src);
	xr_string	ExtractFileExt(const char* src);
	xr_string	ExcludeBasePath(const char* full_path, const char* excl_path);
};

extern XRCORE_API EFS_Utils* xr_EFS;
#define EFS (*xr_EFS)