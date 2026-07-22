// LocatorAPI.h: interface for the CLocatorAPI class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include "LocatorAPI_defs.h"

class XRCORE_API CStreamReader;
class XRCORE_API CReaderGuarded;
class XRCORE_API CWriterGuarded;

class XRCORE_API CLocatorAPI  
{
	friend class FS_Path;

	using PathMap = xr_map<const char*, FS_Path*, pred_str>;
	using PathPairIt = PathMap::iterator;

public:
	struct file
	{
		const char*					name;			// low-case name
		const char*					wrap = nullptr;	// addons real path
		u32						vfs;			// 0xffffffff - standart file
		u32						crc;			// contents CRC
		u32						ptr;			// pointer inside vfs
		u32						size_real;		// 
		u32						size_compressed;// if (size_real==size_compressed) - uncompressed
		time_t					modif;			// for editor
	};
	struct	archive
	{
		shared_str				path;
        FileHandle              hSrcFile;
#ifdef IXR_WINDOWS
		void					*hSrcMap = nullptr;
#endif
		CInifile*				header;
		u32						size;
		u32						vfs_idx;

		archive():hSrcFile(0),header(nullptr),size(0),vfs_idx(u32(-1)){}
		void					open();
		void					close();
	};

	using archives_vec = xr_vector<archive>;
	using archives_it = archives_vec::iterator;

    archives_vec				m_archives;
	void						LoadArchive		(archive& A, const char* entrypoint= nullptr);

	PathMap						pathes;

private:

	struct file_pred 
	{
		IC bool operator()	(const file& x, const file& y) const
		{	return xr_strcmp(x.name,y.name)<0;	}
	};

	using files_set = xr_set<file, file_pred>;
	using files_it = files_set::iterator;

	using FFVec = xr_vector<system_file>;
	using FFIt = FFVec::iterator;

	FFVec						rec_files;

    int							m_iLockRescan	; 
    void						check_pathes	();

	xrSRWLock					m_files_lock	;

public:
	files_set					m_files			;

private:
	bool						bNoRecurse		;

	xrCriticalSection			m_auth_lock		;
	u64							m_auth_code		;

	bool IsArchivePhase = false;

	void						Register		(const char* name, u32 vfs, u32 crc, u32 ptr, u32 size_real, u32 size_compressed, time_t modif);
	void						ProcessArchive	(const char* path, const char* base_path = nullptr);
	void						ProcessOne		(const char* path, system_file* F);
	bool						Recurse			(const char* path);	

	files_it					file_find_it	(const char* n);

public:
	bool IsAddonPhase = false;

	// Editor
	bool TryLoad(const xr_string& File);

public:
	enum{
		flNeedRescan			= (1<<0),
		flBuildCopy				= (1<<1),
		flReady					= (1<<2),
		flEBuildCopy			= (1<<3),
		flEventNotificator      = (1<<4),
		flTargetFolderOnly		= (1<<5),
		flCacheFiles			= (1<<6),
		flScanAppRoot			= (1<<7),
		flNeedCheck				= (1<<8),
	};    
	Flags32						m_Flags			;
	u32							dwAllocGranularity;
	u32							dwOpenCounter;

private:
			void				check_cached_files	(LPSTR fname, const u32 &fname_size, const file &desc, const char* &source_name);

			void				file_from_cache_impl(IReader *&R, LPSTR fname, const file &desc);
			void				file_from_cache_impl(CStreamReader *&R, LPSTR fname, const file &desc);
	template <typename T>
			void				file_from_cache		(T *&R, LPSTR fname, const u32 &fname_size, const file &desc, const char* &source_name);
			
			void				file_from_archive	(IReader *&R, const char* fname, const file &desc);
			void				file_from_archive	(CStreamReader *&R, const char* fname, const file &desc);

			void				copy_file_to_build	(IWriter *W, IReader *r);
			void				copy_file_to_build	(IWriter *W, CStreamReader *r);
	template <typename T>
			void				copy_file_to_build	(T *&R, const char* source_name);

			bool				check_for_file		(const char* path, const char* _fname, string_path& fname, const file *&desc);
	
	template <typename T>
	IC		T					*r_open_impl		(const char* path, const char* _fname);
			void				ProcessExternalArch	();
private:
			void				setup_fs_path		(const char* fs_name, string_path &fs_path);
			void				setup_fs_path		(const char* fs_name);
			IReader				*setup_fs_ltx		(const char* fs_name);

private:
	// IXR: .xrignore
	xr_vector<xr_path> IgnoreData;
	void ParseIgnoreList();
	bool CheckSkip(const xr_path& Path) const;

public:
								CLocatorAPI			();
								~CLocatorAPI		();
	void						_initialize			(u32 flags, const char* target_folder=nullptr, const char* fs_name=nullptr);
	void						_destroy			();

	CStreamReader*				rs_open				(const char* initial, const char* N);
	IReader*					r_open				(const char* initial, const char* N);
	IC IReader*					r_open				(const char* N){return r_open(nullptr,N);}
	void						r_close				(IReader* &S);
	void						r_close				(CStreamReader* &fs);

	CReaderGuarded rg_open(const char* initial, const char* N);
	CReaderGuarded rg_open(const char* N);

	void						get_all_files_in_dir(xr_set<xr_string>& out, const char* dir);
	void						get_all_files_in_dir(xr_vector<const char*>& out, const char* dir);
	void						get_all_files_in_dir(xr_vector<xr_string>& out, const char* dir);

	IWriter*					w_open				(const char* initial, const char* N);
	IC IWriter*					w_open				(const char* N){return w_open(nullptr,N);}
	IWriter*					w_open_ex			(const char* initial, const char* N);
	IC IWriter*					w_open_ex			(const char* N){return w_open_ex(nullptr,N);}
	void						w_close				(IWriter* &S);
	
	CWriterGuarded wg_open(const char* initial, const char* N);
	CWriterGuarded wg_open(const char* N);

	xr_string					fix_path			(const xr_string& file);

	const file*					exist				(const char* N);
	const file*					exist				(const char* path, const char* name);
	const file*					exist				(string_path& fn, const char* path, const char* name);
	const file*					exist				(string_path& fn, const char* path, const char* name, const char* ext);

	bool						file_find			(const char* full_name, FS_File& f);

    bool 						can_write_to_folder	(const char* path); 
    bool 						can_write_to_alias	(const char* path); 
    bool						can_modify_file		(const char* fname);
    bool						can_modify_file		(const char* path, const char* name);

    bool 						dir_delete			(const char* path,const char* nm,bool remove_files);
    bool 						dir_delete			(const char* full_path,bool remove_files){return dir_delete(nullptr,full_path,remove_files);}
    void 						file_delete			(const char* path,const char* nm);
    void 						file_delete			(const char* full_path){file_delete(nullptr,full_path);}
	void 						file_copy			(const char* src, const char* dest);
	void 						file_rename			(const char* src, const char* dest,bool bOwerwrite=true);
    int							file_length			(const char* src);

    time_t 						get_file_age		(const char* nm);
    void 						set_file_age		(const char* nm, time_t age);

	xr_vector<LPSTR>*			file_list_open		(const char* initial, const char* folder,	u32 flags=FS_ListFiles);
	xr_vector<LPSTR>*			file_list_open		(const char* path,					u32 flags=FS_ListFiles);
	void						file_list_close		(xr_vector<LPSTR>* &lst);
                                                     
    bool						path_exist			(const char* path);
    FS_Path*					get_path			(const char* path);
    FS_Path*					append_path			(const char* path_alias, const char* root, const char* add, bool recursive);
    const char*						update_path			(string_path& dest, const char* initial, const char* src);
	const char*						update_path			(xr_stack_string_path& dest, const char* initial, const char* src);

	int							file_list			(FS_FileSet& dest, const char* path, u32 flags=FS_ListFiles, const char* mask=nullptr);

	bool						load_all_unloaded_archives();
	void						unload_archive		(archive& A);

	void						auth_generate		(xr_vector<shared_str>&	ignore, xr_vector<shared_str>&	important);
	u64							auth_get			();
	void						auth_runtime		(void*);

	void						rescan_path			(const char* full_path, bool bRecurse, bool NeedMountAddons = true);
	// editor functions
	void						rescan_pathes		();
	void						lock_rescan			();
	void						unlock_rescan		();
};

extern XRCORE_API	CLocatorAPI*					xr_FS;
#define FS (*xr_FS)

class XRCORE_API CReaderGuarded
{
	IReader* Reader;
public:
	CReaderGuarded(IReader* Reader) : Reader(Reader) {}
	~CReaderGuarded() { if (Reader) {FS.r_close(Reader);}}

	CReaderGuarded(const CReaderGuarded&) = delete;
	CReaderGuarded& operator=(const CReaderGuarded&) = delete;
	CReaderGuarded(CReaderGuarded&&) = delete;
	CReaderGuarded& operator=(CReaderGuarded&&) = delete;

	ICF IReader& operator*() const { return *Reader; }
	ICF IReader* operator->() const { return Reader; }
	ICF operator bool() const { return Reader; }
};

class XRCORE_API CWriterGuarded
{
	IWriter* Writer;
public:
	CWriterGuarded(IWriter* Writer) : Writer(Writer) {}
	~CWriterGuarded() { if (Writer) {FS.w_close(Writer);}}

	CWriterGuarded(const CWriterGuarded&) = delete;
	CWriterGuarded& operator=(const CWriterGuarded&) = delete;
	CWriterGuarded(CWriterGuarded&&) = delete;
	CWriterGuarded& operator=(CWriterGuarded&&) = delete;

	ICF IWriter& operator*() const { return *Writer; }
	ICF IWriter* operator->() const { return Writer; }
	ICF operator bool() const { return Writer; }
};