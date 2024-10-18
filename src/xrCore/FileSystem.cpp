//----------------------------------------------------
// file: FileSystem.cpp
//----------------------------------------------------

#include "stdafx.h"
#pragma hdrstop

EFS_Utils* xr_EFS = nullptr;
//----------------------------------------------------
xr_string	EFS_Utils::ExtractFileName(LPCSTR src)
{
	string_path name;
	_splitpath	(src,0,0,name,0);
	return xr_string(name);
}

xr_string	EFS_Utils::ExtractFileExt(LPCSTR src)
{
	string_path ext;
	_splitpath	(src,0,0,0,ext);
	return xr_string(ext);
}

xr_string	EFS_Utils::ExtractFilePath(LPCSTR src)
{
	string_path drive,dir;
	_splitpath	(src,drive,dir,0,0);
	return xr_string(drive)+dir;
}

xr_string	EFS_Utils::ExcludeBasePath(LPCSTR full_path, LPCSTR excl_path)
{
	LPCSTR sub		= strstr(full_path,excl_path);
	if (0!=sub) 	return xr_string(sub+xr_strlen(excl_path));
	else	   		return xr_string(full_path);
}

xr_string	EFS_Utils::ChangeFileExt(LPCSTR src, LPCSTR ext)
{
	xr_string	tmp;
	LPSTR src_ext	= strext(src);
	if (src_ext){
		size_t		ext_pos	= src_ext-src;
		tmp.assign	(src,0,ext_pos);
	} else {
		tmp			= src;
	}
	tmp				+= ext;
	return tmp;
}

xr_string	EFS_Utils::ChangeFileExt(const xr_string& src, LPCSTR ext)
{
	return ChangeFileExt(src.c_str(),ext);
}

//----------------------------------------------------
void MakeFilter(string1024& dest, LPCSTR info, LPCSTR ext)
{
	std::string res;

	if (ext)
	{
		res += info;
		res	+= "(";
		res	+= ext;
		res	+= ")|";
		res	+= ext;
		res	+= "|";
		int icnt		= _GetItemCount(ext,';');
		if(icnt>1)
		{
		for(int idx=0; idx<icnt; ++idx)
		{
		  string64		buf;
		  _GetItem		(ext, idx, buf, ';');
	
		  res += info;
		  res += "(";
		  res += buf;
		  res += ")|";
		  res += buf;
		  res += "|";
		}
	  }
		res += "|";
	}else
	{
		res = "All files(*.*)|*.*||";
	}
	xr_strcpy(dest, res.c_str());
	
	for(u32 i=0; i<res.size(); ++i)           
	{
		if(res[i]=='|')
			dest[i]='\0';
	}
  

}

//------------------------------------------------------------------------------
// start_flt_ext = -1-all 0..n-indices
//------------------------------------------------------------------------------
  
// Vista uses this hook for old-style save dialog
UINT_PTR CALLBACK OFNHookProcOldStyle(HWND , UINT , WPARAM , LPARAM )
{
	// let default hook work on this message
	return 0;
}

bool EFS_Utils::GetOpenNameInternal(LPCSTR initial, LPSTR buffer, int sz_buf, bool bMulti, LPCSTR offset, int start_flt_ext, const char* ext)
{
#ifdef IXR_WINDOWS
	VERIFY(buffer && (sz_buf > 0));
	FS_Path& P = *FS.get_path(initial);
	string1024 			flt;
	MakeFilter(flt, P.m_FilterCaption ? P.m_FilterCaption : "", ext ? ext : P.m_DefExt);

	OPENFILENAMEA 		ofn;
	memset(&ofn, 0, sizeof(ofn));

	if (xr_strlen(buffer))
	{
		string_path		dr;
		if (!(buffer[0] == '\\' && buffer[1] == '\\')) { // if !network
			_splitpath(buffer, dr, 0, 0, 0);

			if (0 == dr[0])
			{
				string_path		bb;
				P._update(bb, buffer);
				xr_strcpy(buffer, sz_buf, bb);
			}
		}
	}
	ofn.lStructSize = sizeof(OPENFILENAMEA);
	ofn.hwndOwner = GetForegroundWindow();
	ofn.lpstrDefExt = P.m_DefExt;
	ofn.lpstrFile = buffer;
	ofn.nMaxFile = sz_buf;
	ofn.lpstrFilter = flt;
	ofn.nFilterIndex = start_flt_ext + 2;
	ofn.lpstrTitle = "Open a File";
	string512 path;
	xr_strcpy(path, (offset && offset[0]) ? offset : P.m_Path);
	ofn.lpstrInitialDir = path;

	ofn.Flags = OFN_PATHMUSTEXIST |
		OFN_FILEMUSTEXIST |
		OFN_HIDEREADONLY |
		OFN_FILEMUSTEXIST |
		OFN_NOCHANGEDIR |
		(bMulti ? OFN_ALLOWMULTISELECT | OFN_EXPLORER : 0);

	ofn.FlagsEx = 0;

	bool bRes = !!GetOpenFileNameA(&ofn);
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
		int cnt = _GetItemCount(ofn.lpstrFile, 0x0);
		if (cnt > 1)
		{
			char 		dir[255 * 255];
			char 		buf[255 * 255];
			char 		fns[255 * 255];

			xr_strcpy(dir, buffer);
			xr_strcpy(fns, dir);
			xr_strcat(fns, "\\");
			xr_strcat(fns, _GetItem(ofn.lpstrFile, 1, buf, sizeof(buf), 0x0));

			for (int i = 2; i < cnt; i++)
			{
				xr_strcat(fns, ",");
				xr_strcat(fns, dir);
				xr_strcat(fns, "\\");
				xr_strcat(fns, _GetItem(ofn.lpstrFile, i, buf, sizeof(buf), 0x0));
			}
			xr_strcpy(buffer, sz_buf, fns);
		}
	}
	strlwr(buffer);
	return bRes;
#else
	return false;
#endif
}


bool EFS_Utils::GetOpenName(LPCSTR initial, xr_string& buffer, bool bMulti, LPCSTR offset, int start_flt_ext, const char* ext)
{
	char			buf	[255*255]; //max files to select
	xr_strcpy			(buf, buffer.c_str());

	bool bRes		= GetOpenNameInternal(initial, buf, sizeof(buf), bMulti, offset, start_flt_ext, ext);

	if (bRes)
		buffer=(char*)buf;

	return bRes;
}

bool EFS_Utils::GetSaveName(LPCSTR initial, string_path& buffer, LPCSTR offset, int start_flt_ext, const char* ext)
{
#ifdef IXR_WINDOWS
	FS_Path& P = *FS.get_path(initial);
	string1024 			flt;

	LPCSTR def_ext = P.m_DefExt;

	MakeFilter(flt, P.m_FilterCaption ? P.m_FilterCaption : "", ext ? ext : def_ext);
	OPENFILENAMEA ofn;
	memset(&ofn, 0, sizeof(ofn));
	if (xr_strlen(buffer)) {
		string_path		dr;
		if (!(buffer[0] == '\\' && buffer[1] == '\\')) { // if !network
			_splitpath(buffer, dr, 0, 0, 0);
			if (0 == dr[0])	P._update(buffer, buffer);
		}
	}
	ofn.hwndOwner = GetForegroundWindow();
	ofn.lpstrDefExt = def_ext;
	ofn.lpstrFile = buffer;
	ofn.lpstrFilter = flt;
	ofn.lStructSize = sizeof(ofn);
	ofn.nMaxFile = sizeof(buffer);
	ofn.nFilterIndex = start_flt_ext + 2;
	ofn.lpstrTitle = "Save a File";
	string512 path; xr_strcpy(path, (offset && offset[0]) ? offset : P.m_Path);
	ofn.lpstrInitialDir = path;
	ofn.Flags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT | OFN_NOCHANGEDIR | OFN_EXPLORER;
	ofn.FlagsEx = 0;


	bool bRes = !!GetSaveFileNameA(&ofn);
	if (!bRes) {
		u32 err = CommDlgExtendedError();
		switch (err) {
		case FNERR_BUFFERTOOSMALL: 	Log("Too many file selected."); break;
		}
}
	strlwr(buffer);
	return bRes;
#else
	return false;
#endif
}


bool EFS_Utils::GetSaveName( LPCSTR initial, xr_string& buffer, LPCSTR offset, int start_flt_ext, const char* ext)
{
	string_path				buf;
	xr_strcpy				(buf,sizeof(buf), buffer.c_str());
	bool bRes				= GetSaveName(initial, buf ,offset, start_flt_ext, ext);
	if (bRes) 
		buffer				= buf;

	return bRes;
}

xr_string EFS_Utils::AppendFolderToName(xr_string& tex_name, int depth, BOOL full_name)
{
	string1024 nm;
	xr_strcpy(nm, tex_name.c_str());
	tex_name = AppendFolderToName(nm, sizeof(nm), depth, full_name);
	return tex_name;
}

void EFS_Utils::MarkFile(LPCSTR fn, bool bDeleteSource)
{
	xr_string ext = strext(fn);
	ext.insert(1, "~");
	xr_string backup_fn = EFS.ChangeFileExt(fn, ext.c_str());
	if (bDeleteSource) {
		FS.file_rename(fn, backup_fn.c_str(), true);
	}
	else {
		FS.file_copy(fn, backup_fn.c_str());
	}
}

//----------------------------------------------------
LPCSTR EFS_Utils::AppendFolderToName(LPSTR tex_name, u32 const tex_name_size, int depth, BOOL full_name)
{
	string256 _fn;
	xr_strcpy(tex_name,tex_name_size,AppendFolderToName(tex_name, _fn, sizeof(_fn), depth, full_name));
	return tex_name;
}

LPCSTR EFS_Utils::AppendFolderToName(LPCSTR src_name, LPSTR dest_name, u32 const dest_name_size, int depth, BOOL full_name)
{
	shared_str tmp = src_name;
	LPCSTR s 	= src_name;
	LPSTR d 	= dest_name;
	int sv_depth= depth;
	for (; *s&&depth; s++, d++){
		if (*s=='_'){depth--; *d='\\';}else{*d=*s;}
	}
	if (full_name){
		*d			= 0;
		if (depth<sv_depth)	xr_strcat(dest_name,dest_name_size,*tmp);
	}else{
		for (; *s; s++, d++) *d=*s;
		*d			= 0;
	}
	return dest_name;
}

LPCSTR EFS_Utils::GenerateName(LPCSTR base_path, LPCSTR base_name, LPCSTR def_ext, LPSTR out_name, u32 const out_name_size)
{
	int cnt = 0;
	string_path fn;
	if (base_name)	
		xr_strconcat(fn, base_path,base_name,def_ext);
	else 			
		xr_sprintf		(fn, sizeof(fn), "%s%02d%s",base_path,cnt++,def_ext);

	while (FS.exist(fn))
		if (base_name)	
			xr_sprintf	(fn, sizeof(fn),"%s%s%02d%s",base_path,base_name,cnt++,def_ext);
		else 			
			xr_sprintf	(fn, sizeof(fn), "%s%02d%s",base_path,cnt++,def_ext);
	xr_strcpy(out_name,out_name_size,fn);
	return out_name;
}

//#endif
