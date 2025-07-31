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
	IC bool GetOpenName(xr_stack_wstring<Size>& path_to_file, LPCWSTR mask, bool many_picking = false)
	{
#ifdef IXR_WINDOWS
		OPENFILENAMEW ofn;       // common dialog box structure

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

		bool result = GetOpenFileNameW(&ofn) == TRUE;

		return result;
#else
		return false;
#endif
	}

	template<xr_ssnt_t Size>
	IC bool GetOpenName(xr_stack_string<Size>& path_to_file, LPCSTR mask, bool many_picking = false)
	{
		assert(false && "not implemented!");
		return false;
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