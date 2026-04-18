//----------------------------------------------------
// file: PSLibrary.cpp
//----------------------------------------------------
#include "stdafx.h"


#include "PSLibrary.h"


#include "ParticleAnimCurve.h"
#include "ParticleEffect.h"
#include "ParticleGroup.h"

#ifdef _EDITOR
#	include "../xrECore/Editor/ParticleEffectActions.h"
#   include "../xrECore/Editor/ui_main.h"
#endif

bool ped_sort_pred(const PS::CPEDef* a, const PS::CPEDef* b){	return xr_strcmp(a->Name(),b->Name())<0;}
bool pgd_sort_pred(const PS::CPGDef* a, const PS::CPGDef* b){	return xr_strcmp(a->m_Name,b->m_Name)<0;}
bool pacd_sort_pred(const PS::CPACDef* a, const PS::CPACDef* b){	return xr_strcmp(a->getName(),b->getName())<0;}

bool ped_find_pred(const PS::CPEDef* a, const char* b){ return xr_strcmp(a->Name(),b)<0;}
bool pgd_find_pred(const PS::CPGDef* a, const char* b){ return xr_strcmp(a->m_Name,b)<0;}
bool pacd_find_pred(const PS::CPACDef* a, const char* b){ return xr_strcmp(a->getName(),b)<0;}
//----------------------------------------------------
void CPSLibrary::OnCreate()
{
#ifdef _EDITOR
    if(pCreateEAction)
    {
        Load2();
    }else
#endif
    {
    	string_path		fn;
        FS.update_path	(fn,_game_data_,"particles.xr");
        Load			(fn);
    }
#ifndef _EDITOR
	Load2();
#endif
	PS::CPACLibraryWrapper::GetInstance().SetPACLibrary(this);
}
 
void CPSLibrary::OnDestroy()
{
	for (auto elem : m_PEDs)
	{
		elem->DestroyShader();
		xr_delete(elem);
	}
	m_PEDs.clear	();

	for (auto elem : m_PGDs)
	{
		xr_delete(elem);
	}
	m_PGDs.clear	();

	for (auto elem : m_PACDs)
	{
		xr_delete(elem);
	}
	m_PACDs.clear();
	
    m_all_ps.clear();
	PS::CPACLibraryWrapper::GetInstance().SetPACLibrary(nullptr);
}
//----------------------------------------------------
PS::PEDIt CPSLibrary::FindPEDIt(const char* Name)
{
	if (!Name) return m_PEDs.end();
#ifdef _EDITOR
	for (PS::PEDIt it=m_PEDs.begin(); it!=m_PEDs.end(); it++)
    	if (0==xr_strcmp((*it)->Name(),Name)) return it;
	return m_PEDs.end();
#else
	PS::PEDIt I = std::lower_bound(m_PEDs.begin(),m_PEDs.end(),Name,ped_find_pred);
	if (I==m_PEDs.end() || (0!=xr_strcmp((*I)->m_Name,Name)))	return m_PEDs.end();
	else														return I;
#endif
}

PS::CPEDef* CPSLibrary::FindPED(const char* Name)
{
	auto it = FindPEDIt(Name);
    return (it==m_PEDs.end())?0:*it;
}

PS::PGDIt CPSLibrary::FindPGDIt(const char* Name)
{
	if (!Name) return m_PGDs.end();
#ifdef _EDITOR
	for (PS::PGDIt it=m_PGDs.begin(); it!=m_PGDs.end(); it++)
    	if (0==xr_strcmp((*it)->m_Name,Name)) return it;
	return m_PGDs.end();
#else
	PS::PGDIt I = std::lower_bound(m_PGDs.begin(),m_PGDs.end(),Name,pgd_find_pred);
	if (I==m_PGDs.end() || (0!=xr_strcmp((*I)->m_Name,Name)))	return m_PGDs.end();
	else														return I;
#endif
}

PS::CPGDef* CPSLibrary::FindPGD(const char* Name)
{
	auto it = FindPGDIt(Name);
    return (it==m_PGDs.end())?0:*it;
}

void CPSLibrary::RenamePED(PS::CPEDef* src, const char* new_name)
{
	R_ASSERT(src&&new_name&&new_name[0]);
	src->SetName(new_name);
}

PS::CPACDef* CPSLibrary::FindPACD(const char* Name)
{
	auto it = FindPACDIt(Name);
	return (it==m_PACDs.end())?0:*it;
}

PS::IPAC* CPSLibrary::FindIPAC(const char* name)
{
	return FindPACD(name);
}

PS::PACDIt CPSLibrary::FindPACDIt(const char* Name)
{
	if (!Name) return m_PACDs.end();
#ifdef _EDITOR
	for (auto it=m_PACDs.begin(); it!=m_PACDs.end(); it++)
		if (0==xr_strcmp((*it)->getName(),Name)) return it;
	return m_PACDs.end();
#else
	PS::PACDIt I = std::lower_bound(m_PACDs.begin(),m_PACDs.end(),Name,pacd_find_pred);
	if (I==m_PACDs.end() || (0!=xr_strcmp((*I)->getName(),Name)))	return m_PACDs.end();
	else														return I;
#endif
}

void CPSLibrary::RenamePGD(PS::CPGDef* src, const char* new_name)
{
	R_ASSERT(src&&new_name&&new_name[0]);
	src->SetName(new_name);
}

void CPSLibrary::RenamePACD(PS::CPACDef* src, const char* new_name)
{
	R_ASSERT(src&&new_name&&new_name[0]);
	src->setName(new_name);
}

void CPSLibrary::Remove(const char* nm)
{
	{
		auto it = FindPEDIt(nm);
		if (it!=m_PEDs.end())
		{
			(*it)->DestroyShader();
			xr_delete(*it);
			m_PEDs.erase(it);
			return;
		}
	}
	{
		auto it = FindPGDIt(nm);
		if (it!=m_PGDs.end())
		{
			xr_delete(*it);
			m_PGDs.erase(it);
			return;
		}
	}
	{
		auto it = FindPACDIt(nm);
		if (it!=m_PACDs.end())
		{
			xr_delete(*it);
			m_PACDs.erase(it);
			return;
		}
	}
}
//----------------------------------------------------
bool CPSLibrary::Load2()
{
	FS_FileSet					files;
	string_path					_path;
    FS.update_path				(_path, "$game_particles$", "");

	FS.file_list				(files, _path, FS_ListFiles, "*.pe,*.pg,*.pac");

#ifdef _EDITOR
	SPBItem* pb = nullptr;
	if(UI->m_bReady)
    pb 							= UI->ProgressStart(files.size(),"Loading particles...");
#endif
	FS_FileSet::iterator it		= files.begin();
	FS_FileSet::iterator it_e	= files.end();

    string_path 				p_path, p_name, p_ext;
	for(;it!=it_e;++it)
	{
		const FS_File& f		= (*it);
	    _splitpath				(f.name.c_str(), 0, p_path, p_name, p_ext );
        FS.update_path			(_path, "$game_particles$",f.name.c_str());
        CInifile				ini (_path,TRUE,TRUE,FALSE);

#ifdef _EDITOR
        if(pb) pb->Inc					();
#endif

        xr_sprintf				(_path, sizeof(_path),"%s%s",p_path, p_name);
        if(0==_stricmp(p_ext,".pe"))
        {
            PS::CPEDef*	def		= new PS::CPEDef();
            def->m_Name			= _path;
            if (def->Load2(ini))
            {
            	auto found_elem = std::find_if(m_PEDs.begin(), m_PEDs.end(),
            		[&def](const auto& elem)
					{
						return xr_strcmp(def->Name(), elem->Name()) == 0;
					}
				);
            	if (found_elem!=m_PEDs.end())
            	{
					Msg("* Particle %s replaced by addon", (*found_elem)->m_Name.c_str());
            		xr_delete(*found_elem);
            		*found_elem = def;
            	} else
            	{
            		m_PEDs.push_back(def);
					m_all_ps.push_back(def->m_Name);
            	}
            }
            else
            {
	            xr_delete		(def);
            }
        }
		else if(0==_stricmp(p_ext,".pg"))
        {
            PS::CPGDef*	def		= new PS::CPGDef();
            def->m_Name			= _path;
            if (def->Load2(ini))
            {
				auto found_elem = std::find_if(m_PGDs.begin(), m_PGDs.end(), [&def](const auto& elem){ return xr_strcmp(def->m_Name.c_str(), *elem->m_Name) == 0; });
            	if (found_elem!=m_PGDs.end())
            	{
					Msg("* Particle %s replaced by addon", (*found_elem)->m_Name.c_str());
            		xr_delete(*found_elem);
            		*found_elem = def;
            	} else
            	{
            		m_PGDs.push_back(def);
					m_all_ps.push_back(def->m_Name);
            	}
            }
            else
            {
	            xr_delete		(def);
            }
        }
		else if(0==_stricmp(p_ext,".pac"))
		{
			auto def = new PS::CPACDef();
			if (def->Load2(ini))
			{
				auto found_elem = std::find_if(m_PACDs.begin(), m_PACDs.end(), [&def](const auto& elem){ return xr_strcmp(def->getName(), elem->getName()) == 0; });
				if (found_elem!=m_PACDs.end())
				{
					Msg("* Particle %s replaced by addon", (*found_elem)->getName());
					xr_delete(*found_elem);
					*found_elem = def;
				} else
				{
					m_PACDs.push_back(def);
				}
			}
			else
			{
				xr_delete(def);
			}
		}
		else
        {
        	R_ASSERT(0);
        }
	}

	std::ranges::sort(m_PEDs,ped_sort_pred);
	std::ranges::sort(m_PGDs,pgd_sort_pred);
	std::ranges::sort(m_PACDs,pacd_sort_pred);

	for (auto elem : m_PEDs)
	{
		elem->CreateShader();
	}

#ifdef _EDITOR
    if(pb) UI->ProgressEnd		(pb);
#endif
	Msg				("Loaded particles :%d", files.size());
	return true;
}


bool CPSLibrary::Load(const char* nm)
{
    if (!FS.TryLoad(nm))
    {
        Msg("Can't find file: '%s'",nm);
        return 				false;
    }
    
	IReader* F = FS.r_open(nm);
	bool bRes 				= true;

    bool FoundedChunk = !!F->find_chunk(PS::Chunks::VERSION);
    R_ASSERT2(FoundedChunk, "Not found chunk PS_CHUNK_VERSION");

    //u16 ver					= F->r_u16();
    //if (ver!=PS_VERSION) return false;

	auto ver = F->r_enum<PS::Version>();
	switch (ver)
	{
	case PS::Version::Original:
		{
			bRes = LoadOriginal(*F);
			break;
		}
	case PS::Version::Extended:
		{
			bRes = LoadExtended(*F);
			break;
		}
	default:
		{
			xr_string Message = "Unable to open [";
			Message.append(nm);
			Message.append("] as particle storage! (ver [");
			auto VerStr = magic_enum::enum_name(ver);
			if (VerStr.size())
			{
				Message.append(VerStr);
			} else
			{
				Message.append(std::to_string(u16(ver)));
			}
			Message.append("]");
			R_ASSERT2(false, Message.c_str());
			return false;
		}
	}

    // final
	FS.r_close			(F);

	std::ranges::sort(m_PEDs, ped_sort_pred);
	std::ranges::sort(m_PGDs, pgd_sort_pred);
	std::ranges::sort(m_PACDs, pacd_sort_pred);

	for (auto elem : m_PEDs)
	{
		elem->CreateShader();
	}

    return bRes;
}

bool CPSLibrary::LoadOriginal(IReader& F)
{
	bool bRes = true;
	// second generation
	IReader* OBJ;
	OBJ			 			= F.open_chunk(PS::Chunks::ORIGINAL_SECONDGEN);
	if (OBJ){
		IReader* O   		= OBJ->open_chunk(0);
		for (int count=1; O; count++) {
			PS::CPEDef*	def	= new PS::CPEDef();
			if (I_ASSERT(def->LoadOriginal(*O)))
			{
				m_all_ps.push_back(def->m_Name);
				m_PEDs.push_back(def);
			}
			else{ bRes = false; xr_delete(def); }
			O->close();
			if (!bRes)	break;
			O 			= OBJ->open_chunk(count);
		}
		OBJ->close();
	}
	// second generation
	OBJ 					= F.open_chunk(PS::Chunks::ORIGINAL_THIRDGEN);
	if (OBJ){
		IReader* O   		= OBJ->open_chunk(0);
		for (int count=1; O; count++) {
			PS::CPGDef*	def	= new PS::CPGDef();
			if (I_ASSERT(def->LoadOriginal(*O)))
			{
				m_all_ps.push_back(def->m_Name);
				m_PGDs.push_back(def);
			}
			else{ bRes = false; xr_delete(def); }
			O->close();
			if (!bRes) break;
			O 			= OBJ->open_chunk(count);
		}
		OBJ->close();
	}
	return bRes;
}

bool CPSLibrary::LoadExtended(IReader& F)
{
	bool bRes = true;

	IReader* OBJ;
	OBJ			 			= F.open_chunk(PS::Chunks::EXTENDED_PE);
	if (OBJ){
		IReader* O   		= OBJ->open_chunk(0);
		for (int count=1; O; count++) {
			PS::CPEDef*	def	= new PS::CPEDef();
			if (I_ASSERT(def->LoadOriginal(*O)))
			{
				m_all_ps.push_back(def->m_Name);
				m_PEDs.push_back(def);
			}
			else
			{
				bRes = false;
				xr_delete(def);
			}
			O->close();
			if (!bRes)
			{
				break;
			}
			O = OBJ->open_chunk(count);
		}
		OBJ->close();
	}

	OBJ 					= F.open_chunk(PS::Chunks::EXTENDED_PG);
	if (OBJ){
		IReader* O   		= OBJ->open_chunk(0);
		for (int count=1; O; count++) {
			PS::CPGDef*	def	= new PS::CPGDef();
			if (I_ASSERT(def->LoadOriginal(*O)))
			{
				m_all_ps.push_back(def->m_Name);
				m_PGDs.push_back(def);
			}
			else
			{
				bRes = false;
				xr_delete(def);
			}
			O->close();
			if (!bRes)
			{
				break;
			}
			O = OBJ->open_chunk(count);
		}
		OBJ->close();
	}

	OBJ 					= F.open_chunk(PS::Chunks::EXTENDED_PAC);
	if (OBJ){
		IReader* O   		= OBJ->open_chunk(0);
		for (int count=1; O; count++) {
			PS::CPACDef*	def	= new PS::CPACDef();
			if (I_ASSERT(def->Load(*O)))
			{
				m_all_ps.push_back(def->getName());
				m_PACDs.push_back(def);
			}
			else
			{
				bRes = false;
				xr_delete(def);
			}
			O->close();
			if (!bRes)
			{
				break;
			}
			O = OBJ->open_chunk(count);
		}
		OBJ->close();
	}
	return bRes;
}

//----------------------------------------------------
void CPSLibrary::Reload()
{
	OnDestroy();
    OnCreate();
	Msg( "PS Library was succesfully reloaded." );
}
//----------------------------------------------------

using PS::CPGDef;

CPGDef const* const* CPSLibrary::particles_group_begin	() const
{
	return	(m_PGDs.size() ? &*m_PGDs.begin() : 0);
}

CPGDef const* const* CPSLibrary::particles_group_end	() const
{
	return	(m_PGDs.size() ? &*m_PGDs.end() : 0);
}

void CPSLibrary::particles_group_next			(PS::CPGDef const* const*& iterator) const
{
	VERIFY	(iterator);
	VERIFY	(iterator >= particles_group_begin());
	VERIFY	(iterator <  particles_group_end());
	++iterator;
}

shared_str const& CPSLibrary::particles_group_id(CPGDef const& particles_group) const
{
	return	(particles_group.m_Name);
}
