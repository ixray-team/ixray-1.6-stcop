#include "stdafx.h"


#include "LocatorAPI_defs.h"

//////////////////////////////////////////////////////////////////////
// FS_File
//////////////////////////////////////////////////////////////////////
FS_File::FS_File(xr_string nm, long sz, time_t modif,unsigned attr)	{set(nm,sz,modif,attr);}
FS_File::FS_File(xr_string nm)										{set(nm,0,0,0);}
FS_File::FS_File(const system_file& f)								{set(f.name,f.size,f.time_write,(f.attrib&_A_SUBDIR)?flSubDir:0);}
FS_File::FS_File(xr_string nm, const system_file& f)				{set(nm,f.size,f.time_write,(f.attrib&_A_SUBDIR)?flSubDir:0);}

void FS_File::set(xr_string nm, long sz, time_t modif,unsigned attr)
{
	name		= nm;		xr_strlwr	(name);
	size		= sz;
	time_write	= modif;
	attrib		= attr;
}

//////////////////////////////////////////////////////////////////////
// FS_Path
//////////////////////////////////////////////////////////////////////
FS_Path::FS_Path	(const char* _Root, const char* _Add, const char* _DefExt, const char* _FilterCaption, u32 flags)
{
//	VERIFY			(_Root&&_Root[0]);
	string_path		temp;
    xr_strcpy		(temp,sizeof(temp),_Root); 
    if (_Add) 		xr_strcat(temp,_Add);

	if (temp[0] && temp[xr_strlen(temp)-1] != Platform::kPreferredSeparator[0]) 
	{
		xr_strcat(temp,Platform::kPreferredSeparator);
	}

	m_Path			= xr_strlwr(xr_strdup(temp));
	m_DefExt		= _DefExt?xr_strlwr(xr_strdup(_DefExt)):nullptr;
	m_FilterCaption	= _FilterCaption?xr_strlwr(xr_strdup(_FilterCaption)):nullptr;
	m_Add			= _Add?xr_strlwr(xr_strdup(_Add)):nullptr;
	m_Root			= _Root?xr_strlwr(xr_strdup(_Root)):nullptr;
    m_Flags.assign	(flags);
}

FS_Path::~FS_Path	()
{
	xr_free	(m_Root);
	xr_free	(m_Path);
	xr_free	(m_Add);
	xr_free	(m_DefExt);
	xr_free	(m_FilterCaption);
}

void FS_Path::_set(const char* add)
{
	// m_Add
	R_ASSERT		(add);
	xr_free			(m_Add);
	m_Add			= xr_strlwr(xr_strdup(add));

	// m_Path
	string_path		temp;
	xr_strconcat(temp,m_Root,m_Add);
	if (temp[xr_strlen(temp)-1]!= Platform::kPreferredSeparator[0])
	{
		xr_strcat(temp, Platform::kPreferredSeparator);
	}
	
	xr_free			(m_Path);
	m_Path			= xr_strlwr(xr_strdup(temp));
}

void FS_Path::_set_root(const char* root)
{
	string_path		temp;
	xr_strcpy		( temp, root );
	if (m_Root[0] && m_Root[xr_strlen(m_Root)-1]!=Platform::kPreferredSeparator[0])
	{
		xr_strcat(temp,Platform::kPreferredSeparator);
	}
	
	xr_free			(m_Root);
	m_Root			= xr_strlwr(xr_strdup(temp));

	// m_Path
	xr_strconcat(temp,m_Root,m_Add ? m_Add : "");
	if (*temp && temp[xr_strlen(temp)-1]!=Platform::kPreferredSeparator[0])
	{
		xr_strcat(temp,Platform::kPreferredSeparator);
	}
	
	xr_free			(m_Path);
	m_Path			= xr_strlwr(xr_strdup(temp));
}

const char* FS_Path::_update(string_path& dest, const char* src)const
{
	R_ASSERT			(dest);
    R_ASSERT			(src);
	string_path			temp;
	xr_strcpy			(temp, sizeof(temp), src);
	xr_strconcat(dest, m_Path, temp);
	xr_strcpy			(dest, Platform::ValidPath( dest));
	return xr_strlwr	(dest);
}

const char* FS_Path::_update(xr_stack_string_path& dest, const char* src) const
{
    R_ASSERT(src);
	dest = "";
	dest.append(m_Path);
	dest.append(src);
	xr_strlwr(dest);
	return dest.c_str();
}

void FS_Path::rescan_path_cb	()
{
	m_Flags.set(flNeedRescan,true);
    FS.m_Flags.set(CLocatorAPI::flNeedRescan,true);
}

bool XRCORE_API PatternMatch(const char* s, const char* mask)
{
	const char* cp=nullptr;
	const char* mp=nullptr;
	for (; *s&&*mask!='*'; mask++,s++) if (*mask!=*s&&*mask!='?') return false;
	for (;;) {
		if (!*s) { while (*mask=='*') mask++; return !*mask; }
		if (*mask=='*') { if (!*++mask) return true; mp=mask; cp=s+1; continue; }
		if (*mask==*s||*mask=='?') { mask++, s++; continue; }
		mask=mp; s=cp++;
	}
}

