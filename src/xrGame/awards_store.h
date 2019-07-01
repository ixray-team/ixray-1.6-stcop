#ifndef AWARD_SYSTEM_INCLUDED
#define AWARD_SYSTEM_INCLUDED

#include "../xrServerEntities/associative_vector.h"
#include <boost/noncopyable.hpp>
#include "profile_data_types.h"
#include "../xrGameSpy/GameSpy/sake/sake.h"
#include "gsc_dsigned_ltx.h"

class CGameSpy_Full;
class CGameSpy_SAKE;

namespace gamespy_profile
{

class awards_store : boost::noncopyable
{
public:
				awards_store		(CGameSpy_Full* fullgs);
				~awards_store		();

	void		load_awards					(store_operation_cb & opcb);
	void		load_awards_from_ltx		(CInifile& ini);
	bool		is_sake_equal_to_file		() const;
	void		reset_awards				();

	all_awards_t &					get_player_awards	();
private:
	all_awards_t			m_awards_result;
	all_awards_t			m_ltx_awards_result;
	store_operation_cb		m_award_operation_cb;
	
	CGameSpy_SAKE*			m_sake_obj;
	CGameSpy_Full*			m_fullgs_obj;

	static int const		fields_count = at_awards_count * ap_award_params_count;
	char*					m_field_names_store[fields_count];
	SAKEGetMyRecordsInput	m_get_records_input;
	void					init_field_names();
		
	void				process_aw_out_response	(SAKEGetMyRecordsOutput* tmp_out);
	void				process_award			(SAKEField* ap);


	static void __cdecl	get_my_awards_cb				(SAKE sake,
														 SAKERequest request,
														 SAKERequestResult result,
														 void * inputData,
														 void * outputData,
														 void * userData);
}; //class award_system

} //namespace gamespy_profile

#endif //#ifndef AWARD_SYSTEM_INCLUDED