//----------------------------------------------------
// file: PSLibrary.h
//----------------------------------------------------
#ifndef PSLibraryH
#define PSLibraryH
#include "RenderAPI.h"

#include "../../Include/xrRender/particles_systems_library_interface.hpp"
#include "../../xrEngine/ParticleAnimCurveInterface.h"

namespace PS {
	class CPEDef;
    
    using PEDVec = xr_vector<CPEDef*>;
    using PEDIt = PEDVec::iterator;

	class CPGDef;
	
    using PGDVec = xr_vector<CPGDef*>;
    using PGDIt = PGDVec::iterator;

	class CPACDef;

	using PACDVec = xr_vector<CPACDef*>;
	using PACDIt = PACDVec::iterator;
} // namespace PS

// Exported per member: the Particle Editor defines the editing members (Append*, Save) in its own module.
class CPSLibrary : public particles_systems::library_interface, public PS::IPACLibrary {
	PS::PEDVec			m_PEDs;
    PS::PGDVec			m_PGDs;
	PS::PACDVec			m_PACDs;
    xr_vector<shared_str> m_all_ps;
    xr_string			m_CurrentParticles;

public:
	// stream
    RENDER_API bool Load(const char* nm);
	RENDER_API bool LoadOriginal(IReader& F);
	RENDER_API bool LoadExtended(IReader& F);
	
    bool Save(const char* nm);

	// individuals INI
	RENDER_API bool Load2();
	bool Save2();
	
public:
						CPSLibrary		(){;}
    		 			~CPSLibrary		(){;}

    RENDER_API void		OnCreate		();
    RENDER_API void		OnDestroy		();

    RENDER_API PS::CPEDef*		FindPED			(const char* name);
    RENDER_API PS::PEDIt		FindPEDIt		(const char* name);
    RENDER_API PS::CPGDef*		FindPGD			(const char* name);
	RENDER_API PS::PGDIt		FindPGDIt		(const char* name);
	RENDER_API PS::CPACDef*		FindPACD		(const char* name);
	RENDER_API PS::PACDIt		FindPACDIt		(const char* name);

	RENDER_API virtual PS::IPAC*	FindIPAC		(const char* name) override;

    // get object properties methods
	IC const PS::PEDVec& VecPEDs		()	{ return m_PEDs; }
    IC PS::PEDIt		FirstPED		()	{return m_PEDs.begin();}
    IC PS::PEDIt		LastPED			()	{return m_PEDs.end();}
	IC const PS::PGDVec& VecPGDs		()	{ return m_PGDs; }
    IC PS::PGDIt		FirstPGD		()	{return m_PGDs.begin();}
    IC PS::PGDIt		LastPGD			()	{return m_PGDs.end();}
	IC const PS::PACDVec& VecPACDs		()	{ return m_PACDs; }
	IC PS::PACDIt		FirstPACD		()	{return m_PACDs.begin();}
	IC PS::PACDIt		LastPACD			()	{return m_PACDs.end();}

    PS::CPEDef*			AppendPED		(PS::CPEDef* src=nullptr);
    PS::CPGDef*			AppendPGD		(PS::CPGDef* src=nullptr);
    PS::CPACDef*		AppendPACD		(PS::CPACDef* src=nullptr);
    RENDER_API void		Remove			(const char* name);
    RENDER_API void		RenamePED		(PS::CPEDef* src, const char* new_name);
    RENDER_API void		RenamePGD		(PS::CPGDef* src, const char* new_name);
    RENDER_API void		RenamePACD		(PS::CPACDef* src, const char* new_name);

    RENDER_API void		Reload			();
    bool				Save			();

	RENDER_API virtual	PS::CPGDef const* const*	particles_group_begin	() const;
	RENDER_API virtual	PS::CPGDef const* const*	particles_group_end		() const;
	RENDER_API virtual	void						particles_group_next	(PS::CPGDef const* const*& iterator) const;
	RENDER_API virtual	shared_str const&			particles_group_id		(PS::CPGDef const& particles_group) const;
    virtual xr_vector<shared_str> const& vec_all_particles() const {return m_all_ps;};
};

#define PS_LIB_SIGN 			"PS_LIB"

#define PS_VERSION				0x0001

namespace PS
{
	enum class Version: u16
	{
		Original = 0x0001,
		Extended,
		MAX,
		Latest = MAX - 1,
	};

	enum class Chunks: u32
	{
		VERSION = 0x0001,
		// Original chunks
		ORIGINAL_FIRSTGEN,
		ORIGINAL_SECONDGEN,
		ORIGINAL_THIRDGEN,
		// Extended chunks
		EXTENDED_PE = VERSION+1,
		EXTENDED_PG,
		EXTENDED_PAC,
	};
}

//----------------------------------------------------

#endif /*_INCDEF_PSLibrary_H_*/

