#include "stdafx.h"
#include "SVGStorage.h"

#include "ResourceManager.h"
#include "blenders/Blender.h"

void CResourceManager::OnDeviceDestroy(bool )
{
	WaitForTextureLoading();
	if (RDEVICE.b_is_Ready)				return;

	if (m_pStorageSVG)
	{
		m_pStorageSVG->uninit();
		xr_delete(m_pStorageSVG);
	}

	m_textures_description.UnLoad		();

	// Matrices
	for (map_Matrix::iterator m=m_matrices.begin(); m!=m_matrices.end(); m++)	{
		R_ASSERT		(1==m->second->dwReference);
		xr_delete		(m->second);
	}
	m_matrices.clear	();
    
	// Constants
	for (map_Constant::iterator c=m_constants.begin(); c!=m_constants.end(); c++)
	{
		R_ASSERT		(1==c->second->dwReference);
		xr_delete		(c->second);
	}
	m_constants.clear	();

   	// Release blenders
	for (map_BlenderIt b = m_blenders.begin(); b != m_blenders.end(); b++)
	{
		xr_free((char*&)b->first);
		xr_delete(b->second);
	}
	m_blenders.clear	();

	// destroy TD
	for (map_TDIt _t=m_td.begin(); _t!=m_td.end(); _t++)
	{
		xr_free		((char*&)_t->first);
		xr_free		((char*&)_t->second.T);
		xr_delete	(_t->second.cs);
	}
	m_td.clear		();
}

void CResourceManager::WarnLuaBlends()
{
	xr_vector<char*>* Folder = FS.file_list_open(_game_shaders_, ::Render->getShaderPath(), FS_ListFiles | FS_RootOnly);
	if (!Folder)
	{
		return;
	}

	for (char* Name : *Folder)
	{
		const char* Extension = strext(Name);
		if (Extension && 0 == xr_strcmp(Extension, ".lua"))
		{
			Msg("! Lua shader blend '%s' is not supported, convert it to an XML blend", Name);
		}
	}
	FS.file_list_close(Folder);
}

void	CResourceManager::OnDeviceCreate	(IReader* F)
{
	if (!RDEVICE.b_is_Ready) return;

	string256	name;

	WarnLuaBlends			();
	IReader*	fs			= nullptr;
	// Load constants
 	fs	 		  			= F->open_chunk	(0);
	if (fs){
		while (!fs->eof())	{
			fs->r_stringZ	(name,sizeof(name));
			CConstant*	C	= _CreateConstant	(name);
			C->Load			(fs);
		}
		fs->close			();
	}

	// Load matrices
    fs						= F->open_chunk(1);
	if (fs){
		while (!fs->eof())	{
			fs->r_stringZ	(name,sizeof(name));
			CMatrix*	M	= _CreateMatrix	(name);
			M->Load			(fs);
		}
		fs->close			();
	}

	// Load blenders
    fs						= F->open_chunk	(2);
	if (fs){
		IReader*	chunk	= nullptr;
		int			chunk_id= 0;

		while ((chunk=fs->open_chunk(chunk_id))!=nullptr){
			CBlender_DESC	desc;
			chunk->r		(&desc,sizeof(desc));
			IBlender*		B = IBlender::Create(desc.CLS);
			if	(nullptr==B)
			{
				Msg				("! Renderer doesn't support blender '%s'",desc.cName);
			}
			else
			{
				if	(B->getDescription().version != desc.version)
				{
					Msg			("! Version conflict in shader '%s'",desc.cName);
				}

				chunk->seek		(0);
				B->Load			(*chunk,desc.version);

				std::pair<map_BlenderIt, bool> I =  m_blenders.insert	(std::make_pair(xr_strdup(desc.cName),B));
				R_ASSERT2		(I.second,"shader.xr - found duplicate name!!!");
			}
			chunk->close	();
			chunk_id		+= 1;
		}
		fs->close();
	}

	m_textures_description.Load				();
}

void CResourceManager::ED_ReloadLibrary(IReader* F)
{
	string256 Name;

	// Constants and matrices are referenced by the compiled shaders: load into the existing objects
	if (IReader* Chunk = F->open_chunk(0))
	{
		while (!Chunk->eof())
		{
			Chunk->r_stringZ(Name, sizeof(Name));
			if (CConstant* Item = _CreateConstant(Name))
			{
				Item->Load(Chunk);
			}
		}
		Chunk->close();
	}

	if (IReader* Chunk = F->open_chunk(1))
	{
		while (!Chunk->eof())
		{
			Chunk->r_stringZ(Name, sizeof(Name));
			if (CMatrix* Item = _CreateMatrix(Name))
			{
				Item->Load(Chunk);
			}
		}
		Chunk->close();
	}

	if (IReader* Blenders = F->open_chunk(2))
	{
		int ChunkID = 0;
		while (IReader* Chunk = Blenders->open_chunk(ChunkID))
		{
			CBlender_DESC Desc;
			Chunk->r(&Desc, sizeof(Desc));
			IBlender* Blender = IBlender::Create(Desc.CLS);
			if (Blender == nullptr)
			{
				Msg("! Renderer doesn't support blender '%s'", Desc.cName);
			}
			else
			{
				Chunk->seek(0);
				Blender->Load(*Chunk, Desc.version);

				map_BlenderIt It = m_blenders.find(Desc.cName);
				if (It == m_blenders.end())
				{
					m_blenders.insert(std::make_pair(xr_strdup(Desc.cName), Blender));
				}
				else
				{
					xr_delete(It->second);
					It->second = Blender;
				}
			}
			Chunk->close();
			ChunkID++;
		}
		Blenders->close();
	}
}

void	CResourceManager::OnDeviceCreate	(const char* shName)
{
	if (Device.IsEditorMode() && !FS.exist(shName)) return;

	// Check if file is compressed already
	string32	ID			= "shENGINE";
	string32	id;
	IReader*	F			= FS.r_open(shName);
	R_ASSERT2	(F,shName);
	F->r		(&id,8);
	if (0==strncmp(id,ID,8))
	{
		FATAL				("Unsupported blender library. Compressed?");
	}
	OnDeviceCreate			(F);
	FS.r_close				(F);
}

void CResourceManager::StoreNecessaryTextures()
{
	if (!m_necessary.empty())
		return;
	
	map_TextureIt it			= m_textures.begin();
	map_TextureIt it_e			= m_textures.end();

	for (;it!=it_e;++it)
	{
		const char* texture_name		= it->first;
		if(strstr(texture_name,"\\levels\\"))	continue;
		if(!strchr(texture_name,'\\'))			continue;

		ref_texture				T;
		T.create				(texture_name);
		m_necessary.push_back	(T);
		
	}
}

void CResourceManager::DestroyNecessaryTextures()
{
	m_necessary.clear			();
}
