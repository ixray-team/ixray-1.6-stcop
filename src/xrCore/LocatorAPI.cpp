// LocatorAPI.cpp: implementation of the CLocatorAPI class.
//
// Fast LocatorAPI init authors: mnelenpridumivat and v2v3v4
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include <fstream>


#include "FS_internal.h"
#include "stream_reader.h"
#include "file_stream_reader.h"
#include "Crypto/trivial_encryptor.h"

#include "xrAddons.h"

constexpr u32 BIG_FILE_READER_WINDOW_SIZE = 1024*1024;

CLocatorAPI* xr_FS = nullptr;

#define FSLTX "fsgame.ltx"

void CLocatorAPI::ParseIgnoreList()
{
	std::fstream IgnoreFile(".xrignore");

	xr_string Data = "";

	while (std::getline(IgnoreFile, Data))
	{
		if (Data.starts_with('#'))
			continue;
		
		if (Data.size() < 3)
			continue;

		IgnoreData.push_back(Data);
	}
}

bool CLocatorAPI::CheckSkip(const xr_path& Path) const
{
	auto UnixPath = Path.native();
	std::ranges::replace(UnixPath, '\\', '/');

	for (const auto& SkipPath : IgnoreData)
	{
		if(UnixPath.contains(SkipPath.native()))
		{
			return true;
		}
	}

	return false;
}

CLocatorAPI::CLocatorAPI()
{
	m_Flags.zero();
	// get page size
	dwAllocGranularity	= (u32)Platform::GetPageSize();
	m_iLockRescan = 0;
	dwOpenCounter = 0;
}

CLocatorAPI::~CLocatorAPI()
{
	VERIFY				(0==m_iLockRescan);
}

void CLocatorAPI::Register(const char* name, u32 vfs, u32 crc, u32 ptr, u32 size_real, u32 size_compressed, time_t modif)
{	
	xr_string TempPath = name;
	xr_strlwr(TempPath);

	file desc;

	if (IsAddonPhase && !IsArchivePhase)
	{
		if (!GAddonsManager->CanApply(TempPath, desc))
		{
			return;
		}
	}

	// Register file
	desc.name			= xr_strdup(TempPath.data());
	desc.vfs			= vfs;
	desc.crc			= crc;
	desc.ptr			= ptr;
	desc.size_real		= size_real;
	desc.size_compressed= size_compressed;
	desc.modif			= modif;// &(~u32(0x3));

	{
		xrSRWLockGuard g(m_files_lock, true);
	files_it			I = m_files.find(desc);

	if (I != m_files.end()) 
	{
		desc.name		= I->name;

		// sad but true, performance option
		// correct way is to erase and then insert new record:
		const_cast<file&>(*I)	= desc;
		return;
	}
	else
	{
		desc.name		= xr_strdup(desc.name);
	}
	}

	// otherwise insert file
	{
		xrSRWLockGuard g(m_files_lock);
	m_files.insert		(desc); 
	}
	
	// Try to register folder(s)
	string_path			temp;	
	xr_strcpy			(temp,sizeof(temp),desc.name);
	string_path			path;
	string_path			folder;
	while (temp[0]) 
	{
		_splitpath		(temp, path, folder, nullptr, nullptr );
		xr_strcat			(path,folder);
		if (!exist(path))	
		{
			desc.name			= xr_strdup(path);
			desc.vfs			= 0xffffffff;
			desc.ptr			= 0;
			desc.size_real		= 0;
			desc.size_compressed= 0;
			desc.modif			= u32(-1);
			
			xrSRWLockGuard g(m_files_lock);
			std::pair<files_it,bool> I_ = m_files.insert(desc); 

			R_ASSERT(I_.second);
		}
		xr_strcpy					(temp,sizeof(temp),folder);
		if (xr_strlen(temp))		temp[xr_strlen(temp)-1]=0;
	}
}

IReader* open_chunk(FileHandle ptr, u32 ID, const char* archiveName, u32 archiveSize, bool shouldDecrypt = false)
{
	u32 dwType;
	u32 dwSize = 0;
	size_t read_byte;

	if (Platform::SetFilePointer(ptr, 0, FILE_BEGIN) == INVALID_SET_FILE_POINTER)
	{
		return nullptr;
	}

	while (true)
	{
		read_byte = Platform::ReadFile(ptr, &dwType, 4);
		if (read_byte == 0 || read_byte == Platform::INVALID_READ_SIZE)
		{
			return nullptr;
		}

		read_byte = Platform::ReadFile(ptr, &dwSize, 4);
		if (read_byte == 0 || read_byte == Platform::INVALID_READ_SIZE)
		{
			return nullptr;
		}

		if ((dwType & (~CFS_CompressMark)) == ID)
		{
			u8* src_data = xr_alloc<u8>(dwSize);
			read_byte = Platform::ReadFile(ptr, src_data, dwSize);

			if (read_byte != dwSize)
			{
				xr_free(src_data);
				return nullptr;
			}

			if (dwType & CFS_CompressMark)
			{
				u8* dest = nullptr;
				u32 dest_sz = 0;

				if (shouldDecrypt)
				{
					// Try WW key first
					g_trivial_encryptor.decode(src_data, dwSize, src_data);
				}

				bool result = _decompressLZ(&dest, &dest_sz, src_data, dwSize, archiveSize);

				if (!result && shouldDecrypt)
				{
					// Let's try to decode with RU key
					g_trivial_encryptor.encode(src_data, dwSize, src_data); // rollback
					g_trivial_encryptor.decode(src_data, dwSize, src_data, trivial_encryptor::key_flag::russian);
					result = _decompressLZ(&dest, &dest_sz, src_data, dwSize, archiveSize);
				}

				R_ASSERT3(result, "Can't decompress archive", archiveName);
				xr_free(src_data);
				return new CTempReader(dest, dest_sz, 0);
			}
			else
			{
				return new CTempReader(src_data, dwSize, 0);
			}
		}
		else
		{
			if (Platform::SetFilePointer(ptr, dwSize, FILE_CURRENT) == INVALID_SET_FILE_POINTER)
			{
				return nullptr;
			}
		}
	}

	return nullptr;
};

void CLocatorAPI::LoadArchive(archive& A, const char* entrypoint)
{
	// Create base path
	string_path fs_entry_point;
	bool shouldDecrypt = false;
	fs_entry_point[0] = 0;

	if(A.header)
	{
		shared_str read_path	= A.header->r_string("header","entry_point");
		if(0==_stricmp(read_path.c_str(),"gamedata"))
		{
			read_path				= "$fs_root$";
			PathPairIt P			= pathes.find(read_path.c_str()); 
			if(P!=pathes.end())
			{
				FS_Path* root			= P->second;
//				R_ASSERT3				(root, "path not found ", read_path.c_str());
				xr_strcpy				(fs_entry_point, sizeof(fs_entry_point), root->m_Path);
			}
			xr_strcat					(fs_entry_point,"gamedata\\");
		}else
		{
			string256			alias_name;
			alias_name[0]		= 0;
			R_ASSERT2			(*read_path.c_str()=='$', read_path.c_str());

			int count			= sscanf(read_path.c_str(),"%[^\\]s", alias_name);
			R_ASSERT2			(count==1,read_path.c_str());

			PathPairIt P		= pathes.find(alias_name); 

			if(P!=pathes.end())
			{
				FS_Path* root		= P->second;
	//			R_ASSERT3			(root, "path not found ", alias_name);
				xr_strcpy			(fs_entry_point, sizeof(fs_entry_point), root->m_Path);
			}
			xr_strcat			(fs_entry_point, sizeof(fs_entry_point), read_path.c_str()+xr_strlen(alias_name)+1);
		}

	}else
	{
		Msg("~ Found archive without ini header: %s", A.path.c_str());

		if (!strstr(A.path.c_str(), ".xdb"))
		{
			Msg("Assuming that [%s] is encrypted SoC archive", A.path.c_str());
			shouldDecrypt = true;
		}

		auto P = pathes.find("$fs_root$");
		if (P != pathes.end())
		{
			FS_Path* root = P->second;
			// R_ASSERT3 (root, "path not found ", read_path.c_str());
			xr_strcpy(fs_entry_point, sizeof fs_entry_point, root->m_Path);
		}
		xr_strcat(fs_entry_point, "gamedata\\");
	}
	if(entrypoint)
		xr_strcpy				(fs_entry_point, sizeof(fs_entry_point), entrypoint);

	// Read FileSystem
	A.open				();
	IReader* hdr		= open_chunk(A.hSrcFile,1, A.path.c_str(), A.size, shouldDecrypt); 
	R_ASSERT			(hdr);

	while (!hdr->eof())
	{
		string_path		name,full;
		string1024		buffer_start;
		u16				buffer_size	= hdr->r_u16();
		VERIFY			(buffer_size < sizeof(name) + 4*sizeof(u32));
		VERIFY			(buffer_size < sizeof(buffer_start));
		u8				*buffer = (u8*)&*buffer_start;
		hdr->r			(buffer,buffer_size);

		u32 size_real	= *(u32*)buffer;
		buffer			+= sizeof(size_real);

		u32 size_compr	= *(u32*)buffer;
		buffer			+= sizeof(size_compr);

		u32 crc			= *(u32*)buffer;
		buffer			+= sizeof(crc);

		u32				name_length = buffer_size - 4*sizeof(u32);
		Memory.mem_copy	(name,buffer,name_length);
		name[name_length] = 0;
		buffer			+= buffer_size - 4*sizeof(u32);

		u32 ptr			= *(u32*)buffer;
		buffer			+= sizeof(ptr);

		xr_strconcat(full, fs_entry_point, name);

		Register		(full,A.vfs_idx,crc,ptr,size_real,size_compr,0);
	}
	hdr->close			();
}

void CLocatorAPI::archive::open()
{
	// Open the file
	if (hSrcFile)
		return;

#ifdef IXR_WINDOWS
	if (hSrcMap != nullptr)
		return;
#endif

	hSrcFile = Platform::CreateFile(*path, false);

#ifdef IXR_WINDOWS
	hSrcMap			= CreateFileMapping	(hSrcFile, nullptr, PAGE_READONLY, 0, 0, nullptr);
	R_ASSERT		(hSrcMap!=INVALID_HANDLE_VALUE);
#endif

	size			= Platform::GetFileSize(hSrcFile);
	R_ASSERT		(size>0);
}

void CLocatorAPI::archive::close()
{
#ifdef IXR_WINDOWS
	CloseHandle		(hSrcMap);
	hSrcMap			= nullptr;
#endif
	Platform::CloseFile(hSrcFile);
	hSrcFile = 0;
}

void CLocatorAPI::ProcessArchive(const char* _path, const char* base_path)
{
	// find existing archive
	shared_str path = Platform::ANSI_TO_UTF8(_path).c_str();

	for (archives_it it=m_archives.begin(); it!=m_archives.end(); ++it)
		if (it->path==path)	
				return;

	archive& A					= m_archives.emplace_back();
	A.vfs_idx					= (u32)m_archives.size()-1;
	A.path						= _path;

	A.open						();

	// Read header
	bool bProcessArchiveLoading = true;

	IReader* hdr				= open_chunk(A.hSrcFile, CFS_HeaderChunkID, A.path.c_str(), A.size);
	if(hdr)
	{
		A.header				= new CInifile(hdr,"archive_header");
		hdr->close				();
		bProcessArchiveLoading	= A.header->r_bool("header","auto_load");
	}
	
	if(bProcessArchiveLoading || Core.ParamsData.test(ECoreParams::auto_load_arch))
		LoadArchive				(A, base_path);
	else
		A.close					();
}

void CLocatorAPI::unload_archive(CLocatorAPI::archive& A)
{
	xrSRWLockGuard g(m_files_lock);
	files_it	I 	= m_files.begin();
	for (; I!=m_files.end(); ++I)
	{
		const file& entry = *I;
		if(entry.vfs==A.vfs_idx)
		{
			char* str		= LPSTR(I->name);
			xr_free			(str);
			m_files.erase	(I);
			break;
		}
	}	
	A.close();
}

bool CLocatorAPI::load_all_unloaded_archives()
{
	archives_it it		= m_archives.begin();
	archives_it it_e	= m_archives.end();
	bool res = false;
	for(;it!=it_e;++it)
	{
		archive& A = *it;
		if(!A.hSrcFile)
		{
			LoadArchive(A);
			res = true;
		}
	}
	return res;
}


void CLocatorAPI::ProcessOne(const char* path, system_file* F)
{
	xr_string NormalPath = F->name;

	if (!NormalPath.StartWith(path))
	{
		NormalPath = path + NormalPath;
	}

	string_path N = {};
	xr_strcpy(N, NormalPath.data());
	xr_strlwr(N);

	if (F->attrib & _A_HIDDEN)		
		return;

	if (F->attrib & _A_SUBDIR) 
	{
		if (bNoRecurse)				
			return;

		if (0 == xr_strcmp(F->name, "."))	
			return;

		if (0 == xr_strcmp(F->name, ".."))	
			return;

		xr_strcat(N, Platform::kPreferredSeparator);
		Register(N, 0xffffffff, 0, 0, F->size, F->size, F->time_write);
		Recurse(N);
	}
	else 
	{
		if (strext(N) && (0 == strncmp(strext(N), ".db", 3) || 0 == strncmp(strext(N), ".xdb", 4)))
		{
			IsArchivePhase = true;
			ProcessArchive(N);
			IsArchivePhase = false;
		}
		else
			Register(N, 0xffffffff, 0, 0, F->size, F->size, F->time_write);
	}
}

IC bool pred_str_ff(const system_file& x, const system_file& y)
{	
	return xr_strcmp(x.name,y.name)<0;	
}

// we need to check for file existance
// because Unicode file names can 
// be interpolated by FindNextFile()

bool ignore_path(const char* _path)
{
	return !std::filesystem::exists(_path);
}

namespace Platform
{
	XRCORE_API xr_string TCHAR_TO_ANSI_U8(const xr_special_char* C);
}

xr_unique_ptr<xr_hash_set<shared_str>>& GetScannedDirsBuffer()
{
	static xr_unique_ptr<xr_hash_set<shared_str>> buff;
	if (!buff)
	{
		buff = xr_make_unique<xr_hash_set<shared_str>>();
	}
	return buff;
}


struct scan_chache
{
	bool dir;
	xr_string fileName;
	u64 fsize;
	time_t ftime;
};

xr_unique_ptr<xr_vector<scan_chache>>& GetScanCacheBuffer()
{
	static xr_unique_ptr<xr_vector<scan_chache>> buff;
	if (!buff)
	{
		buff = xr_make_unique<xr_vector<scan_chache>>();
	}
	return buff;
}

bool CLocatorAPI::Recurse(const char* path)
{
	string_path scanPath;
	xr_strcpy(scanPath, sizeof(scanPath), path);
	
	size_t oldSize = GetScanCacheBuffer()->size();
	GetScanCacheBuffer()->reserve(oldSize + 256);

	shared_str str = Platform::ValidPath(path);
	path = str.c_str();

	if (!std::filesystem::exists(path))
	{
		return false;
	}

	for (xr_dir_entry elem : xr_dir_iter(path))
	{
		xr_path elem_path = elem;

		if (CheckSkip(elem_path))
		{
			continue;
		}

		GetScanCacheBuffer()->emplace_back();
		scan_chache& chache = GetScanCacheBuffer()->back();
		chache.dir = elem.is_directory();
		chache.ftime = xr_chrono_to_time_t(elem.last_write_time());
		chache.fsize = (chache.dir ? 0 : elem.file_size());
		chache.fileName = elem_path.xfilename();
	}

	size_t newSize = GetScanCacheBuffer()->size();
	if (newSize > oldSize)
	{
		for (size_t i = oldSize; i < newSize; i++)
		{
			auto& chache = GetScanCacheBuffer()->at(i);
			
			string_path N;
			
			VERIFY(path[xr_strlen(path)-1] == Platform::kPreferredSeparator[0]);
			VERIFY(path[xr_strlen(path)-2] != Platform::kPreferredSeparator[0]);
			
			xr_strcpy(N, sizeof(N), path);
			xr_strcat(N, chache.fileName.c_str());
			xr_strlwr(N);

			if (chache.dir)
			{
				if (bNoRecurse || chache.fileName == "." || chache.fileName == "..")
				{
					continue;
				}

				if (!m_Flags.test(flReady))
				{
					if (GetScannedDirsBuffer()->find(N) == GetScannedDirsBuffer()->end())
					{
						GetScannedDirsBuffer()->insert(N);
						xr_strcat(N, Platform::kPreferredSeparator);
						Recurse(N);
					}
				} else
				{
					xr_strcat(N, Platform::kPreferredSeparator);
					Recurse(N);
				}
			}
			else
			{
				if (strext(N) && (0 == strncmp(strext(N), ".db", 3) || 0 == strncmp(strext(N), ".xdb", 4)))
				{
					ProcessArchive(N);
				}
				else
				{
					u32 fsize = chache.fsize;
					Register(N, 0xffffffff, 0, 0, fsize, fsize, chache.ftime);
					//Register(N, 0xffffffff, 0, 0, fsize, fsize, u32(chache.ftime / 10000000 - 11644473600LL));
				}
			}
		}
		GetScanCacheBuffer()->erase(GetScanCacheBuffer()->begin() + oldSize, GetScanCacheBuffer()->end());
	}
	
	if (path && path[0] != 0)
	{
		Register(path, 0xffffffff, 0, 0, 0, 0, 0);
	}

	return true;
}

bool file_handle_internal	(const char* file_name, intptr_t&size, int &file_handle);
void *FileDownload			(const char* file_name, const int &file_handle, intptr_t&file_size);

void CLocatorAPI::setup_fs_path		(const char* fs_name, string_path &fs_path)
{
	xr_strcpy			(fs_path,fs_name ? fs_name : "");
	LPSTR				slash = strrchr(fs_path,Platform::kPreferredSeparator[0]);
	if (!slash)
		slash			= strrchr(fs_path,'/');
	if (!slash) {
		xr_strcpy		(fs_path,"");
		return;
	}

	*(slash+1)			= 0;
}

void CLocatorAPI::setup_fs_path		(const char* fs_name)
{
	string_path			fs_path;
	setup_fs_path		(fs_name, fs_path);

	string_path full_current_directory;

	Platform::GetAbsolutePath(full_current_directory, fs_path, sizeof(full_current_directory));

	xr_string TestPath = full_current_directory;
	if (fs_name != nullptr && !std::filesystem::exists(TestPath + "/" + fs_name))
	{
		auto TryTestPath = [&TestPath, fs_name](auto Path)
		{
			xr_path TryPath = Path;
			xr_string StrPath = TryPath.parent_path().generic_string().c_str();

			if (std::filesystem::exists(StrPath + "/" + fs_name))
			{
				TestPath = Platform::RestorePath(StrPath.c_str());
				std::filesystem::current_path(TryPath.parent_path());
				return true;
			}

			return false;
		};

		if (!TryTestPath(full_current_directory))
		{
			TryTestPath(Platform::GetBinaryFolderPath());
		}
	}

	FS_Path *path = new FS_Path(TestPath.c_str(), "", "", "", 0);
	pathes.insert(std::make_pair(xr_strdup("$fs_root$"), path));
}

IReader *CLocatorAPI::setup_fs_ltx	(const char* fs_name)
{
	setup_fs_path	(fs_name);

	const char*			fs_file_name = FSLTX;
	if (fs_name && *fs_name)
		fs_file_name= fs_name;
				
	Msg("using fs-ltx %s", fs_file_name);

	int				file_handle;
	intptr_t		file_size;
	IReader			*result = nullptr;
	CHECK_OR_EXIT	(
		file_handle_internal(fs_file_name, file_size, file_handle),
		make_string<const char*>("Cannot open file \"%s\".\nCheck your working folder.",fs_file_name)
	);

	void			*buffer = FileDownload(fs_file_name, file_handle, file_size);
	result			= new CTempReader(buffer,file_size,0);

#ifdef DEBUG
	if (result && m_Flags.is(flBuildCopy|flReady))
		copy_file_to_build	(result, fs_file_name);
#endif // DEBUG

	return			(result);
}

void CLocatorAPI::_initialize(u32 flags, const char* target_folder, const char* fs_name)
{	
	char _delimiter = '|'; //','
	if (m_Flags.is(flReady))return;
	CTimer t;
	t.Start();
	Log("Initializing File System...");
	//u32	M1 = Memory.mem_usage();

	m_Flags.set(flags, true);

	// scan root directory
	bNoRecurse = true;
	string4096		buf;

	// Load ignore list
	ParseIgnoreList();

	// append application path
	if (m_Flags.is(flScanAppRoot))
		append_path("$app_root$", Core.ApplicationPath, nullptr, false);


	//-----------------------------------------------------------
	// append application data path
	// target folder 
	if (m_Flags.is(flTargetFolderOnly))
	{
		append_path("$target_folder$", target_folder, nullptr, true);
	}
	else
	{
		IReader* pFSltx = setup_fs_ltx(fs_name);
		// append all pathes    
		string_path		id, root, add, def, capt;
		const char*			lp_add, *lp_def, *lp_capt;
		string16		b_v;
		string4096		temp;

		Msg("pFSltx: %s", fs_name);

		GetScannedDirsBuffer()->clear();
		while (!pFSltx->eof())
		{
			pFSltx->r_string(buf, sizeof(buf));
			if (buf[0] == ';')		continue;

			_GetItem(buf, 0, id, '=');

			if (!m_Flags.is(flBuildCopy) && (0 == xr_strcmp(id, "$build_copy$")))
				continue;

			_GetItem(buf, 1, temp, '=');
			int cnt = _GetItemCount(temp, _delimiter);
		
			R_ASSERT2(cnt >= 3, temp);

			u32 fl = 0;
			_GetItem(temp, 0, b_v, _delimiter);

			if (CInifile::IsBOOL(b_v))
				fl |= FS_Path::flRecurse;

			_GetItem(temp, 1, b_v, _delimiter);
			if (CInifile::IsBOOL(b_v))
				fl |= FS_Path::flNotif;

			_GetItem(temp, 2, root, _delimiter);
			_GetItem(temp, 3, add, _delimiter);
			_GetItem(temp, 4, def, _delimiter);
			_GetItem(temp, 5, capt, _delimiter);
			xr_strlwr(id);


			xr_strlwr(root);
			lp_add = (cnt >= 4) ? xr_strlwr(add) : nullptr;
			lp_def = (cnt >= 5) ? def : nullptr;
			lp_capt = (cnt >= 6) ? capt : nullptr;

			PathPairIt p_it = pathes.find(root);

			std::pair<PathPairIt, bool> I;
			FS_Path* P = new FS_Path((p_it != pathes.end()) ? p_it->second->m_Path : root, lp_add, lp_def, lp_capt, fl);
			bNoRecurse = !(fl & FS_Path::flRecurse);
			Recurse(P->m_Path);
			I = pathes.insert(std::make_pair(xr_strdup(id), P));
#ifndef DEBUG
			m_Flags.set(flCacheFiles, false);
#endif // DEBUG

			CHECK_OR_EXIT(I.second, "The file 'fsgame.ltx' is corrupted (it contains duplicated lines).\nPlease reinstall the game or fix the problem manually.");
		}
		r_close(pFSltx);
		R_ASSERT(path_exist("$app_data_root$"));
	};

	// Load addons
	if (FS.path_exist("$arch_dir_addons$"))
	{
		GAddonsManager = new CAddonManager;
		GetScannedDirsBuffer()->clear();
		GAddonsManager->Initialize();
	}

	// u32	M2 = Memory.mem_usage();
	// Msg("FS: %d files cached %d archives, %dKb memory used.", m_files.size(), m_archives.size(), (M2 - M1) / 1024);

	m_Flags.set(flReady, true);

	Msg("Init FileSystem %f sec", t.GetElapsed_sec());
	//-----------------------------------------------------------
	if (Core.ParamsData.test(ECoreParams::overlaypath))
	{
		string1024				c_newAppPathRoot;
		sscanf(strstr(Core.Params, "-overlaypath ") + 13, "%[^ ] ", c_newAppPathRoot);
		FS_Path* pLogsPath = FS.get_path("$logs$");
		FS_Path* pAppdataPath = FS.get_path("$app_data_root$");


		if (pLogsPath) pLogsPath->_set_root(c_newAppPathRoot);
		if (pAppdataPath)
		{
			GetScannedDirsBuffer()->clear();
			pAppdataPath->_set_root(c_newAppPathRoot);
			rescan_path(pAppdataPath->m_Path, pAppdataPath->m_Flags.is(FS_Path::flRecurse));
		}
	}

	GetScanCacheBuffer().reset();
	GetScannedDirsBuffer().reset();
	rec_files.clear();
	//-----------------------------------------------------------

	if (!Core.ParamsData.test(ECoreParams::nolog))
	{
		xrLogger::OpenLogFile();
	}
}

void CLocatorAPI::_destroy()
{
	xrLogger::CloseLog();

	xrSRWLockGuard g(m_files_lock);

	for (files_it I = m_files.begin(); I != m_files.end(); I++)
	{
		char* str = LPSTR(I->name);
		xr_free(str);
	}
	m_files.clear();
	for (PathPairIt p_it = pathes.begin(); p_it != pathes.end(); p_it++)
	{
		char* str = LPSTR(p_it->first);
		xr_free(str);
		xr_delete(p_it->second);
	}
	pathes.clear();
	for (archives_it a_it = m_archives.begin(); a_it != m_archives.end(); a_it++)
	{

		xr_delete(a_it->header);
		a_it->close();
	}
	m_archives.clear();
}

const CLocatorAPI::file* CLocatorAPI::exist			(const char* fn)
{
	xrSRWLockGuard g(m_files_lock, true);
	files_it it = file_find_it(fn);

	if (it == m_files.end()) {
		xr_string temp_path = fn;
		std::replace(temp_path.begin(), temp_path.end(), '/', '\\'); 
		it = file_find_it(temp_path.data());
	}

	return (it != m_files.end()) ? &(*it) : nullptr;
}

const CLocatorAPI::file* CLocatorAPI::exist			(const char* path, const char* name)
{
	string_path		temp;       
	update_path		(temp,path,name);
	return			exist(temp);
}

const CLocatorAPI::file* CLocatorAPI::exist			(string_path& fn, const char* path, const char* name)
{
	update_path		(fn,path,name);
	return			exist(fn);
}

const CLocatorAPI::file* CLocatorAPI::exist			(string_path& fn, const char* path, const char* name, const char* ext)
{
	string_path		nm;
	xr_strconcat(nm,name,ext);
	update_path		(fn,path,nm);
	return			exist(fn);
}

xr_vector<char*>* CLocatorAPI::file_list_open			(const char* initial, const char* folder, u32 flags)
{
	string_path		N = {};
	R_ASSERT		(initial&&initial[0]);
	update_path		(N,initial,folder);
	return			file_list_open(N,flags);
}

xr_vector<char*>* CLocatorAPI::file_list_open			(const char* _path, u32 flags)
{
	R_ASSERT		(_path);
	VERIFY			(flags);
	// проверить нужно ли пересканировать пути
	check_pathes	();

	string_path		N;

	if (path_exist(_path))	
	{
		update_path	(N,_path,"");
	}
	else					
	{
		xr_strcpy(N,sizeof(N), _path);
	}

	xr_strcpy(N, Platform::ValidPath(N));

	xrSRWLockGuard g(m_files_lock, true);
	
	file			desc;
	desc.name		= N;
	files_it	I 	= m_files.find(desc);
	if (I==m_files.end())	{
		for (int i = 0; i < strlen(N); ++i) {
			if (N[i] == '/') N[i] = '\\';
		}

		I = m_files.find(desc);

		if (I==m_files.end())	{
			return nullptr;
		}
	}
	
	xr_vector<char*>*	dest	= new xr_vector<char*>();

	size_t base_len		= xr_strlen(N);
	for (++I; I!=m_files.end(); I++)
	{
		const file& entry = *I;
		if (0!=strncmp(entry.name,N,base_len))
		{
			break;	// end of list
		}

		const char* end_symbol = entry.name+xr_strlen(entry.name)-1;
		if ((*end_symbol) != Platform::kPreferredSeparator[0])
		{
			// file
			if ((flags&FS_ListFiles) == 0)
			{
				continue;
			}

			const char* entry_begin = entry.name+base_len;
			if ((flags&FS_RootOnly) && strchr(entry_begin, Platform::kPreferredSeparator[0]))
			{
				// folder in folder
				continue;
			}

			dest->push_back(xr_strdup(entry_begin));
			LPSTR fname = dest->back();
			if (flags&FS_ClampExt)
			{
				if (nullptr != strext(fname))
				{
					*strext(fname) = 0;
				}
			}
		}
		else
		{
			// folder
			if ((flags&FS_ListFolders) == 0)
			{
				continue;
			}

			const char* entry_begin = entry.name + base_len;
			
			if ((flags&FS_RootOnly) && (strchr(entry_begin, Platform::kPreferredSeparator[0]) != end_symbol))
			{
				// folder in folder
				continue;
			}

			dest->push_back	(xr_strdup(entry_begin));
		}
	}

	return dest;
}

void CLocatorAPI::file_list_close(xr_vector<char*>* &lst)
{
	if (lst) 
	{
		for (xr_vector<char*>::iterator I=lst->begin(); I!=lst->end(); I++)
			xr_free	(*I);
		xr_delete	(lst);
	}
}

int CLocatorAPI::file_list(FS_FileSet& dest, const char* path, u32 flags, const char* InputMask)
{
	R_ASSERT(path);
	VERIFY(flags);
	// проверить нужно ли пересканировать пути
	check_pathes();

	string_path		N;
	if (path_exist(path))
		update_path(N, path, "");
	else
		xr_strcpy(N, sizeof(N), path);

	xrSRWLockGuard	g(m_files_lock, true);
	
	file			desc;
	desc.name = N;
	files_it	I = m_files.find(desc);
	if (I == m_files.end())	return 0;

	string128 mask;
	if (InputMask != nullptr)
	{
		xr_strcpy(mask, Platform::ValidPath(InputMask));
	}

	SStringVec 		masks;
	_SequenceToList(masks, mask);
	bool b_mask = !masks.empty();

	size_t base_len = xr_strlen(N);
	for (++I; I != m_files.end(); ++I)
	{
		const file& entry = *I;
		if (0 != strncmp(entry.name, N, base_len))	break;	// end of list
		const char* end_symbol = entry.name + xr_strlen(entry.name) - 1;
		if ((*end_symbol) != Platform::kPreferredSeparator[0])
		{
			// file
			if ((flags & FS_ListFiles) == 0)	
				continue;

			const char* entry_begin = entry.name + base_len;
			if ((flags & FS_RootOnly) && strchr(entry_begin, Platform::kPreferredSeparator[0]))	
				continue;	// folder in folder

			// check extension
			if (b_mask)
			{
				bool bOK = false;
				for (SStringVecIt it = masks.begin(); it != masks.end(); it++)
				{
					if (PatternMatch(entry_begin, it->c_str()))
					{
						bOK = true;
						break;
					}
				}
				if (!bOK)			
					continue;
			}

			FS_File file;

			if (flags & FS_ClampExt)
				file.name = EFS.ChangeFileExt(entry_begin, "");
			else
				file.name = entry_begin;


			u32 fl = (entry.vfs != 0xffffffff ? FS_File::flVFS : 0);
			file.size = entry.size_real;
			file.time_write = entry.modif;
			file.attrib = fl;

			dest.insert(std::move(file));
		} else {
			// folder
			if ((flags & FS_ListFolders) == 0) {
				continue;
			}

			const char* entry_begin = entry.name + base_len;

			if ((flags & FS_RootOnly) && (strchr(entry_begin, Platform::kPreferredSeparator[0]) != end_symbol)) {
				continue;	// folder in folder
			}

			u32 fl = FS_File::flSubDir | (entry.vfs ? FS_File::flVFS : 0);
			dest.emplace(FS_File(entry_begin, entry.size_real, entry.modif, fl));
		}
	}
	return (u32) dest.size();
}

void CLocatorAPI::check_cached_files	(LPSTR fname, const u32 &fname_size, const file &desc, const char* &source_name)
{
	string_path		fname_copy;
	if (pathes.size() <= 1)
		return;
	
	if (!path_exist(_server_root_))
		return;

	const char*			path_base = get_path(_server_root_)->m_Path;
	u32				len_base = xr_strlen(path_base);
	const char*			path_file = fname;
	u32				len_file = xr_strlen(path_file);
	if (len_file <= len_base)
		return;

	if ((len_base == 1) && (*path_base == Platform::kPreferredSeparator[0]))
		len_base	= 0;

	if (0!=memcmp(path_base,fname,len_base))
		return;

	bool		bCopy	= false;

	string_path	fname_in_cache	;
	update_path	(fname_in_cache,"$cache$",path_file+len_base);
	xrSRWLockGuard	g(m_files_lock);
	files_it	fit	= file_find_it(fname_in_cache);
	if (fit!=m_files.end())	
	{
		// use
		const file&	fc	= *fit;
		if ((fc.size_real == desc.size_real)&&(fc.modif==desc.modif))	{
			// use
		} else {
			// copy & use
			Msg			("copy: db[%X],cache[%X] - '%s', ",desc.modif,fc.modif,fname);
			bCopy		= true;
		}
	} else {
		// copy & use
		bCopy	= true;
	}

	// copy if need
	if (bCopy) {
		IReader		*_src;
		if (desc.size_real<256*1024)	_src = new CFileReader			(fname);
		else							_src = new CVirtualFileReader	(fname);
		IWriter*	_dst	= new CFileWriter			(fname_in_cache,false);
		_dst->w				(_src->pointer(),_src->length());
		xr_delete			(_dst);
		xr_delete			(_src);
		set_file_age		(fname_in_cache,desc.modif);
		Register			(fname_in_cache,0xffffffff,0,0,desc.size_real,desc.size_real,desc.modif);
	}

	// Use
	source_name		= &fname_copy[0];
	xr_strcpy		(fname_copy,sizeof(fname_copy),fname);
	xr_strcpy		(fname,fname_size,fname_in_cache);
}

void CLocatorAPI::file_from_cache_impl(IReader*& R, LPSTR fname, const file& desc)
{
	const char* RealFileName = desc.wrap == nullptr ? fname : desc.wrap;
	if (desc.size_real < 16 * 1024)
	{
		R = new CFileReader(RealFileName);
		return;
	}

	R = new CVirtualFileReader(RealFileName);
}

void CLocatorAPI::file_from_cache_impl(CStreamReader*& R, LPSTR fname, const file& desc)
{
	const char* RealFileName = desc.wrap == nullptr ? fname : desc.wrap;
	CFileStreamReader* r = new CFileStreamReader();
	r->construct(RealFileName, BIG_FILE_READER_WINDOW_SIZE);
	R = r;
}

template <typename T>
void CLocatorAPI::file_from_cache	(T *&R, LPSTR fname, const u32 &fname_size, const file &desc, const char* &source_name)
{
#ifdef DEBUG
	if (m_Flags.is(flCacheFiles))
		check_cached_files		(fname,fname_size,desc,source_name);
#endif // DEBUG
	
	file_from_cache_impl		(R,fname,desc);
}

void CLocatorAPI::file_from_archive	(IReader *&R, const char* fname, const file &desc)
{
	// Archived one
	archive& A					= m_archives[desc.vfs];
	u32 start					= (desc.ptr/dwAllocGranularity)*dwAllocGranularity;
	u32 end						= (desc.ptr+desc.size_compressed)/dwAllocGranularity;
	if ((desc.ptr+desc.size_compressed)%dwAllocGranularity)	end+=1;
	end							*= dwAllocGranularity;
	if (end>A.size)				end = A.size;
	u32 sz						= (end-start);

	FileHandle SrcMapFile = A.hSrcFile;
#ifdef IXR_WINDOWS
	SrcMapFile = A.hSrcMap;
#endif

	u8* ptr = (u8*)Platform::MapFile(SrcMapFile, sz, true, start);
	if (ptr == nullptr)
	{
		auto ErrorCode = GetLastError();
		xr_string ErrorMsg = "cannot create file mapping on file ";
		ErrorMsg += fname;
		ErrorMsg += "! Error Code: ";
		ErrorMsg += std::to_string(ErrorCode);
		VERIFY2(ptr, ErrorMsg.c_str());
	}

	string512 temp;
	xr_sprintf(temp, sizeof(temp),"%s:%s",*A.path,fname);

	u32 ptr_offs = desc.ptr-start;
	if (desc.size_real == desc.size_compressed)
	{
		R = new CPackReader(ptr,ptr+ptr_offs,desc.size_real);
		return;
	}

	// Compressed
	u8* dest = xr_alloc<u8>(desc.size_real);
	rtc_decompress(dest,desc.size_real,ptr+ptr_offs,desc.size_compressed);
	R = new CTempReader(dest,desc.size_real,0);

	Platform::UnmapFile(ptr, sz);
}

void CLocatorAPI::file_from_archive	(CStreamReader *&R, const char* fname, const file &desc)
{
	archive						&A = m_archives[desc.vfs];
	R_ASSERT2					(
		desc.size_compressed == desc.size_real,
		make_string<const char*>(
			"cannot use stream reading for compressed data %s, do not compress data to be streamed",
			fname
		)
	);

	R							= new CStreamReader();
	R->construct				(
#ifdef IXR_WINDOWS
		A.hSrcMap,
#else
		A.hSrcFile,
#endif
		desc.ptr,
		desc.size_compressed,
		A.size,
		BIG_FILE_READER_WINDOW_SIZE
	);
}

void CLocatorAPI::copy_file_to_build	(IWriter *W, IReader *r)
{
	W->w				(r->pointer(),r->length());
}

void CLocatorAPI::copy_file_to_build	(IWriter *W, CStreamReader *r)
{
	u32					buffer_size = r->length();
	u8					*buffer = xr_alloc<u8>(buffer_size);
	r->r				(buffer,buffer_size);
	W->w				(buffer,buffer_size);
	xr_free				(buffer);
	r->seek				(0);
}


template <typename T>
void CLocatorAPI::copy_file_to_build	(T *&r, const char* source_name)
{
	string_path	cpy_name;
	string_path	e_cpy_name;
	FS_Path* 	P; 

	string_path				fs_root;
	update_path				(fs_root,"$fs_root$","");
	const char* const position	= strstr(source_name, fs_root);
	if ( position == source_name )
		update_path			(cpy_name,"$build_copy$",source_name + xr_strlen(fs_root));
	else
		update_path			(cpy_name,"$build_copy$",source_name);

	IWriter* W = w_open		(cpy_name);
	if (!W) {
		Msg("!Can't build: %s", source_name);
		return;
	}

	copy_file_to_build	(W,r);
	w_close				(W);
	set_file_age(cpy_name,get_file_age(source_name));
	if (!m_Flags.is(flEBuildCopy))
		return;

	const char* ext		= strext(cpy_name);
	if (!ext)
		return;

	IReader* R		= nullptr;
	if (0==xr_strcmp(ext,".dds")){
		P			= get_path(_game_textures_);               
		update_path	(e_cpy_name,_textures_,source_name+xr_strlen(P->m_Path));
		// tga
		*strext		(e_cpy_name) = 0;
		xr_strcat		(e_cpy_name,".tga");
		r_close		(R=r_open(e_cpy_name));
		// thm
		*strext		(e_cpy_name) = 0;
		xr_strcat		(e_cpy_name,".thm");
		r_close		(R=r_open(e_cpy_name));
		return;
	}
	
	if (0==xr_strcmp(ext,".ogg")){
		P			= get_path(_game_sounds_);                               
		update_path	(e_cpy_name,_sounds_,source_name+xr_strlen(P->m_Path));
		// wav
		*strext		(e_cpy_name) = 0;
		xr_strcat		(e_cpy_name,".wav");
		r_close		(R=r_open(e_cpy_name));
		// thm
		*strext		(e_cpy_name) = 0;
		xr_strcat		(e_cpy_name,".thm");
		r_close		(R=r_open(e_cpy_name));
		return;
	}
	
	if (0==xr_strcmp(ext,".object")){
		xr_strcpy		(e_cpy_name,sizeof(e_cpy_name),source_name);
		// object thm
		*strext		(e_cpy_name) = 0;
		xr_strcat		(e_cpy_name,".thm");
		R			= r_open(e_cpy_name);
		if (R)		r_close	(R);
	}
}

bool CLocatorAPI::check_for_file	(const char* path, const char* _fname, string_path& fname, const file *&desc)
{
	// проверить нужно ли пересканировать пути
	check_pathes();

	// correct path
	xr_strcpy(fname,_fname);
	xr_strlwr(fname);
	if (path&&path[0])
		update_path(fname,path,fname);

	// Search entry
	file desc_f;
	desc_f.name	= fname;

	files_it I = m_files.find(desc_f);

	if (I == m_files.end()) {
#ifndef IXR_WINDOWS
		xr_string temp_path = fname;
		std::replace(temp_path.begin(), temp_path.end(), '/', '\\'); 
		desc_f.name = temp_path.data();
		
		I = m_files.find(desc_f);
		if (I == m_files.end()) {
			return false;
		}
#else
		return false;
#endif
	}

	++dwOpenCounter;
	desc = &*I;
	return true;
}

template <typename T>
T *CLocatorAPI::r_open_impl	(const char* path, const char* _fname)
{
	PROF_EVENT("r_open_impl");
	T						*R = nullptr;
	string_path				fname;
	const file				*desc = nullptr;
	const char*					source_name = &fname[0];

#ifdef IXR_WINDOWS
	if (!check_for_file(path,_fname,fname,desc)) {
		return nullptr;
	}
#else
	if (!check_for_file(path,Platform::ValidPath(_fname),fname,desc)) {
		memset(fname, 0, sizeof(fname));
		xr_string temp_path = Platform::ValidPath(_fname);
		std::replace(temp_path.begin(), temp_path.end(), '/', '\\'); 
		if (!check_for_file(path,temp_path.data(),fname,desc)) {
			if (!check_for_file(path,_fname,fname,desc)) {
				return nullptr;
			}
		}
	}
#endif

/*

*/

	// OK, analyse
	if (0xffffffff == desc->vfs)
	{
		file_from_cache(R, fname, sizeof(fname), *desc, source_name);
	}
	else
		file_from_archive	(R,fname,*desc);

#ifdef DEBUG
	if (R && m_Flags.is(flBuildCopy|flReady))
		copy_file_to_build	(R,source_name);
#endif // DEBUG

	return (R);
}

CStreamReader* CLocatorAPI::rs_open(const char* path, const char* _fname)
{
	return (r_open_impl<CStreamReader>(path, _fname));
}

IReader* CLocatorAPI::r_open(const char* path, const char* _fname)
{
	return (r_open_impl<IReader>(path, _fname));
}

void CLocatorAPI::r_close(IReader*& fs)
{
	xr_delete(fs);
}

void CLocatorAPI::r_close(CStreamReader*& fs)
{
	fs->close();
}

CReaderGuarded CLocatorAPI::rg_open(const char* initial, const char* N)
{
	return CReaderGuarded(r_open(initial, N));
}

CReaderGuarded CLocatorAPI::rg_open(const char* N)
{
	return CReaderGuarded(r_open(N));
}

void CLocatorAPI::get_all_files_in_dir(xr_set<xr_string>& out, const char* dir)
{
	xrSRWLockGuard g(m_files_lock, true);
	for (auto& elem : m_files)
	{
		if (xr_strlen(elem.name) > xr_strlen(dir) && std::isalpha(elem.name[xr_strlen(elem.name)-1]) &&  !xr_strncmp(elem.name, dir, xr_strlen(dir)-1))
		{
			out.emplace(elem.name);
		}
	}
}

void CLocatorAPI::get_all_files_in_dir(xr_vector<const char*>& out, const char* dir)
{
	out.clear();
	out.reserve(m_files.size());
	for (auto& elem : m_files)
	{
		if (xr_strlen(elem.name) > xr_strlen(dir) && std::isalpha(elem.name[xr_strlen(elem.name) - 1]) && !xr_strncmp(elem.name, dir, xr_strlen(dir) - 1))
		{
			out.push_back(elem.name);
		}
	}
}

void CLocatorAPI::get_all_files_in_dir(xr_vector<xr_string>& out, const char* dir)
{
	out.clear();
	out.reserve(m_files.size());
	for (auto& elem : m_files)
	{
		if (xr_strlen(elem.name) > xr_strlen(dir) && std::isalpha(elem.name[xr_strlen(elem.name) - 1]) && !xr_strncmp(elem.name, dir, xr_strlen(dir) - 1))
		{
			out.push_back(elem.name);
		}
	}
}

IWriter* CLocatorAPI::w_open	(const char* path, const char* _fname)
{
	string_path	fname;
	xr_strcpy(fname,_fname);
	xr_strlwr(fname);//,".$");
	if (path&&path[0]) update_path(fname,path,fname);
	CFileWriter* W 	= new CFileWriter(fname,false); 

	return W;
}

IWriter* CLocatorAPI::w_open_ex	(const char* path, const char* _fname)
{
	string_path	fname;
	xr_strcpy(fname,_fname);
	xr_strlwr(fname);//,".$");
	if (path&&path[0]) update_path(fname,path,fname);
	CFileWriter* W 	= new CFileWriter(fname,true); 
	return W;
}

void CLocatorAPI::w_close(IWriter*& S)
{
	if (S)
		{
		R_ASSERT(S->fName.size());
		string_path	fname;
		xr_strcpy(fname, sizeof(fname), *S->fName);
		bool bReg = S->valid();
		xr_delete(S);

		if (bReg)
		{
			time_t Time = 0;
			size_t StSize = Platform::Stat(fname, Time);
			Register(fname, 0xffffffff, 0, 0, StSize, StSize, Time);
		}
	}
}

CWriterGuarded CLocatorAPI::wg_open(const char* initial, const char* N)
{
	return CWriterGuarded(w_open(initial, N));
}

CWriterGuarded CLocatorAPI::wg_open(const char* N)
{
	return CWriterGuarded(w_open(N));
}

xr_string CLocatorAPI::fix_path(const xr_string& file)
{
	xr_string TempPath = file;
	if (!exist(file.c_str()))
	{
		xr_string FSPath = get_path("$fs_root$")->m_Path;

		if (TempPath.Contains(FSPath))
		{
			TempPath = TempPath.substr(FSPath.length());
		}
	}

	xr_strlwr(TempPath);
	return TempPath;
}

CLocatorAPI::files_it CLocatorAPI::file_find_it(const char* InputPath)
{
	// проверить нужно ли пересканировать пути
	check_pathes	();

	string_path file_name;

	VERIFY(xr_strlen(InputPath)*sizeof(char) < sizeof(file_name));
	xr_strcpy(file_name,sizeof(file_name),InputPath);
	xr_strcpy(file_name, Platform::ValidPath(file_name));
	
	file desc_f;
	desc_f.name = file_name;
	files_it I = m_files.find(desc_f);
	return (I);
}

bool CLocatorAPI::TryLoad(const xr_string& File)
{
	bool Found = FS.exist(File.c_str());

	if (!Found)
	{
		Found = std::filesystem::exists(File.c_str());

		if (Found)
		{
			size_t FileSize = std::filesystem::file_size(File.c_str());
			size_t FileModif = xr_chrono_to_time_t(std::filesystem::last_write_time(File.c_str()));
			FS.Register(File.c_str(), 0xffffffff, 0, 0, FileSize, FileSize, FileModif);
		}
	}

	return Found;
}

bool CLocatorAPI::dir_delete(const char* path,const char* nm,bool remove_files)
{
	string_path	fpath = {};

	if (path && path[0])
	{
		update_path(fpath, path, nm);
	}
	else 
	{
		xr_strcpy(fpath, sizeof(fpath), nm);
	}

	files_set 	folders;
	files_it I;
	// remove files
	xrSRWLockGuard g(m_files_lock);
	I					= file_find_it(fpath);
	if (I != m_files.end())
	{
		size_t base_len			= xr_strlen(fpath);
		for (; I != m_files.end(); )
		{
			files_it cur_item	= I;
			const file& entry 	= *cur_item;
			I					= cur_item; I++;
			if (0 != strncmp(entry.name, fpath, base_len))	break;	// end of list
			const char* end_symbol = entry.name + xr_strlen(entry.name) - 1;

			if ((*end_symbol) != Platform::kPreferredSeparator[0])
			{
				if (!remove_files)
					return false;

				Platform::Unlink(entry.name);
				m_files.erase(cur_item);
			}
			else
			{
				folders.insert(entry);
			}
		}
	}

	// remove folders
	files_set::reverse_iterator r_it = folders.rbegin();
	for (; r_it != folders.rend(); r_it++)
	{
		const char* end_symbol = r_it->name + xr_strlen(r_it->name) - 1;
		if ((*end_symbol) == Platform::kPreferredSeparator[0])
		{
			_rmdir(r_it->name);
			m_files.erase(*r_it);
		}
	}

	return true;
}

void CLocatorAPI::file_delete(const char* path, const char* nm)
{
	string_path	fname;
	if (path && path[0])
	{
		update_path(fname, path, nm);
	}
	else
	{
		xr_strcpy(fname, sizeof(fname), nm);
	}

	xrSRWLockGuard g(m_files_lock);
	const files_it I	= file_find_it(fname);
	if (I != m_files.end())
	{
		// remove file
		Platform::Unlink(I->name);
		char* str		= LPSTR(I->name);
		xr_free(str);
		m_files.erase(I);
	}
}

void CLocatorAPI::file_copy(const char* src, const char* dest)
{
	if (exist(src))
	{
		if (IReader* S = r_open(src))
		{
			if (IWriter* D = w_open(dest))
			{
				D->w(S->pointer(), S->length());
				w_close(D);
			}
			r_close(S);
		}
	}
}

void CLocatorAPI::file_rename(const char* src, const char* dest, bool bOwerwrite)
{
	xrSRWLockGuard g(m_files_lock);
	
	files_it	S		= file_find_it(src);

	if (S!=m_files.end())
	{
		files_it D		= file_find_it(dest);
		if (D != m_files.end())
		{
			if (!bOwerwrite)
				return;

			Platform::Unlink(D->name);
			char* str	= LPSTR(D->name);
			xr_free(str);

			m_files.erase(D);
		}

		file new_desc	= *S;

		// remove existing item
		char* str		= LPSTR(S->name);
		xr_free(str);
		m_files.erase(S);

		// insert updated item
		new_desc.name	= xr_strlwr(xr_strdup(dest));
		m_files.insert(new_desc); 
		
		// physically rename file
		VerifyPath(dest);
		rename(src,dest);
	}
}

int	CLocatorAPI::file_length(const char* src)
{
	xrSRWLockGuard g(m_files_lock, true);
	files_it	I		= file_find_it(src);
	return (I!=m_files.end())?I->size_real:-1;
}

bool CLocatorAPI::path_exist(const char* path)
{
	PathPairIt P 			= pathes.find(path); 
	return					(P!=pathes.end());
}

FS_Path* CLocatorAPI::append_path(const char* path_alias, const char* root, const char* add, bool recursive)
{
	VERIFY(root);
	VERIFY(!path_exist(path_alias));

	FS_Path* P = new FS_Path(root, add, nullptr, nullptr, 0);
	bNoRecurse = !recursive;
	xr_path path;
	if (!xr_strlen(P->m_Path))
	{
		path = ".\\";
	} else
	{
		path = P->m_Path;
	}
	Recurse			(path.xstring().c_str());

	pathes.insert(std::make_pair(xr_strdup(path_alias), P));
	return P;
}

FS_Path* CLocatorAPI::get_path(const char* path)
{
	PathPairIt P 			= pathes.find(path); 
	R_ASSERT2(P!=pathes.end(),path);
	return P->second;
}

const char* CLocatorAPI::update_path(string_path& dest, const char* initial, const char* src)
{
	return get_path(initial)->_update(dest,src);
}

const char* CLocatorAPI::update_path(xr_stack_string_path& dest, const char* initial, const char* src)
{
	return get_path(initial)->_update(dest,src);
}

time_t CLocatorAPI::get_file_age(const char* nm)
{
	// проверить нужно ли пересканировать пути
	check_pathes();

	xrSRWLockGuard g(m_files_lock, true);
	files_it I = file_find_it(nm);
	return (I != m_files.end()) ? I->modif : std::numeric_limits<long long>::max();
}

void CLocatorAPI::set_file_age(const char* nm, time_t age)
{
	// проверить нужно ли пересканировать пути
	check_pathes	();

	// set file
	_utimbuf	tm;
	tm.actime	= age;
	tm.modtime	= age;
	int res 	= _utime(nm,&tm);
	if (0!=res)
	{
#ifdef IXR_WINDOWS
		Msg("!Can't set file age: '%s'. Error: '%s'", nm,_sys_errlist[errno]);
#else
		Msg("!Can't set file age: '%s'", nm);
#endif
	}
	else
	{
		xrSRWLockGuard g(m_files_lock);
		// update record
		files_it I 		= file_find_it(nm);
		if (I != m_files.end())
		{
			file& F		= (file&)*I;
			F.modif		= age;
		}
	}
}

void CLocatorAPI::rescan_path(const char* full_path, bool bRecurse, bool NeedMountAddons)
{
	file desc = {};
	desc.name		= full_path;
	{
		xrSRWLockGuard g(m_files_lock);
	files_it	I 	= m_files.lower_bound(desc);
	if (I == m_files.end())
		{
		return;
		}
	
	size_t base_len			= xr_strlen(full_path);
	for (; I != m_files.end(); )
	{
		files_it cur_item	= I;
		const file& entry 	= *cur_item;
		I					= cur_item; I++;

		if (0 != strncmp(entry.name, full_path, base_len))
		{
			break;	// end of list
		}

		if (entry.vfs != 0xFFFFFFFF)
		{
				continue;
			}
	
			const char* entry_begin = entry.name + base_len;
			if (!bRecurse && strchr(entry_begin, Platform::kPreferredSeparator[0]))
			{
				continue;
			}
	
			// erase item
			char* str		= LPSTR(cur_item->name);
			xr_free(str);
			m_files.erase(cur_item);
		}
	}

	bNoRecurse	= !bRecurse;
	Recurse(full_path);
	
	GetScanCacheBuffer().reset();
	GetScannedDirsBuffer().reset();

	if (NeedMountAddons && GAddonsManager != nullptr)
	{
		GAddonsManager->MountAddons();
	}
}

void  CLocatorAPI::rescan_pathes()
{
	m_Flags.set(flNeedRescan,false);
	for (PathPairIt p_it=pathes.begin(); p_it!=pathes.end(); p_it++)
	{
		FS_Path* P	= p_it->second;
		if (P->m_Flags.is(FS_Path::flNeedRescan))
		{
			rescan_path(P->m_Path,P->m_Flags.is(FS_Path::flRecurse), false);
			P->m_Flags.set(FS_Path::flNeedRescan,false);
		}
	}

	if (GAddonsManager != nullptr)
	{
		GAddonsManager->MountAddons();
	}
}

void CLocatorAPI::lock_rescan()
{
	m_iLockRescan++;
}

void CLocatorAPI::unlock_rescan()
{
	m_iLockRescan--;  VERIFY(m_iLockRescan>=0);
	if ((0==m_iLockRescan)&&m_Flags.is(flNeedRescan)) 
		rescan_pathes();
}

void CLocatorAPI::check_pathes()
{
	if (m_Flags.is(flNeedRescan)&&(0==m_iLockRescan)){
		lock_rescan		();
		rescan_pathes	();
		unlock_rescan	();
	}
}

bool CLocatorAPI::file_find(const char* full_name, FS_File& f)
{
	xr_path Path = full_name;
	
	if (!strchr(full_name, ':'))
	{
		string_path FullPath = {};
		FS.update_path(FullPath, "$fs_root$", full_name);
		Path = FullPath;
	}

	if (std::filesystem::exists(Path))
	{
		f.name = full_name;
		f.size = std::filesystem::file_size(Path);
		f.time_write = xr_chrono_to_time_t(std::filesystem::last_write_time(Path));

		return true;
	}

	return false;
}

bool CLocatorAPI::can_write_to_folder(const char* path)
{
	if (path && path[0])
	{
		string_path		temp;       
		const char* fn		= "$!#%TEMP%#!$.$$$";
		xr_strconcat(temp, path, path[xr_strlen(path) - 1] != Platform::kPreferredSeparator[0] ? Platform::kPreferredSeparator : "", fn);

		FILE* hf;
		fopen_s(&hf, temp, "wb");

		if (hf == nullptr)
		{
			return false;
		}
		else 
		{
			fclose(hf);
			Platform::Unlink(temp);
			return 		true;
		}
	}

	return false;
}

bool CLocatorAPI::can_write_to_alias(const char* path)
{
	string_path			temp;       
	update_path(temp,path,"");
	return can_write_to_folder(temp);
}

bool CLocatorAPI::can_modify_file(const char* fname)
{
	FILE* hf;
	fopen_s(&hf, fname, "r+b");

	if (hf)
	{	
		fclose(hf);
		return 			true;
	}
	return false;
}

bool CLocatorAPI::can_modify_file(const char* path, const char* name)
{
	string_path			temp;       
	update_path			(temp,path,name);
	return can_modify_file(temp);
}

void CLocatorAPI::ProcessExternalArch()
{
	FS_FileSet		fset;
	file_list		(fset, "$mod_dir$", FS_ListFiles, "*.xdb*");

	FS_FileSetIt	it		= fset.begin();
	FS_FileSetIt	it_e	= fset.end();

	string_path		full_mod_name, _path;
	for( ;it!=it_e; ++it)
	{
		Msg					("--found external arch %s",(*it).name.c_str());
		update_path			(full_mod_name,"$mod_dir$",(*it).name.c_str());

		FS_Path* pFSRoot		= FS.get_path("$fs_root$");
		
		xr_strconcat		(_path, pFSRoot->m_Path, "gamedata");

		ProcessArchive		(full_mod_name, _path);
	}
}