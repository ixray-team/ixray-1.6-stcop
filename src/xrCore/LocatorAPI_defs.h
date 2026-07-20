#pragma once

// general path definition
#define _app_data_root_			"$app_data_root$"

// game path definition
#define _game_data_				"$game_data$"
#define _game_textures_			"$game_textures$"
#define _game_levels_			"$game_levels$"
#define _game_sounds_			"$game_sounds$"
#define _game_meshes_			"$game_meshes$"
#define _game_shaders_			"$game_shaders$"
#define _game_config_			"$game_config$"
#define _game_fonts_			"$game_fonts$"

// editor path definition
#define _server_root_		    "$server_root$"
#define _server_data_root_	    "$server_data_root$"
#define _local_root_		    "$local_root$"
#define _import_			    "$import$"
#define _sounds_			    "$sounds$"
#define _textures_			    "$textures$"
#define _objects_			    "$objects$"
#define _maps_				    "$maps$"
#define _temp_				    "$temp$"
#define _omotion_			    "$omotion$"
#define _omotions_			    "$omotions$"
#define _smotion_			    "$smotion$"
#define _detail_objects_	    "$detail_objects$"

struct system_file
{
	u8			attrib;
	xr_time_t	time_create;
	xr_time_t	time_write;
	size_t		size;
	string256	name;
};

enum FS_List
{
	FS_ListFiles	=(1<<0),
	FS_ListFolders	=(1<<1),
	FS_ClampExt		=(1<<2),
	FS_RootOnly		=(1<<3),
	FS_forcedword	=u32(-1)
};

class XRCORE_API FS_Path
{
public:
	enum{
    	flRecurse	= (1<<0),
    	flNotif		= (1<<1),
    	flNeedRescan= (1<<2),
    };
public:
	LPSTR		m_Path;
	LPSTR		m_Root;
	LPSTR		m_Add;
	LPSTR		m_DefExt;
	LPSTR		m_FilterCaption;
    Flags32		m_Flags;
public:
				FS_Path		(const char* _Root, const char* _Add, const char* _DefExt=nullptr, const char* _FilterString=nullptr, u32 flags=0);
				~FS_Path	();
	const char*		_update		(string_path& dest, const char* src) const;
	const char*		_update		(xr_stack_string_path& dest, const char* src) const;
	void		_set		(const char* add);
	void		_set_root	(const char* root);

    void  rescan_path_cb	();
};

struct XRCORE_API FS_File{
	enum{ 
		flSubDir= (1<<0),
		flVFS	= (1<<1),
	};
	unsigned 	attrib;
	time_t	  	time_write;
	long     	size;
	xr_string	name;			// low-case name
	void		set			(xr_string nm, long sz, time_t modif,unsigned attr);
public:
				FS_File		(){}
				FS_File		(xr_string nm);
				FS_File		(const system_file& f);
				FS_File		(xr_string nm, const system_file& f);
				FS_File		(xr_string nm, long sz, time_t modif,unsigned attr);
	bool 		operator<	(const FS_File& _X) const	{return xr_strcmp(name.c_str(),_X.name.c_str())<0; }
};

using FS_FileSet = xr_set<FS_File>;
using FS_FileSetIt = FS_FileSet::iterator;

extern bool	XRCORE_API PatternMatch(const char* s, const char* mask);