#include "stdafx.h"
#include "global_slots_data.h"

void	global_slots_data::	Load			( )
{	
	Phase("global_slots_data :: Loading (build.details)");

	IReader*	R		= FS.r_open	( "$level$", "build.details" );

	R_ASSERT2(R != nullptr, "build.details not found!");

	R->r_chunk			( 0, &dtH );
	R->seek				( 0 );
	u32 check_sum		= crc32( R-> pointer(), R->length());

	recalculation_data.load( check_sum );


	if( !recalculation_data.recalculating() )
	{
		IWriter*	W		= FS.w_open	( "$level$", "level.details" );
		W->w				( R->pointer(), R->length() );
		FS.w_close			( W );
	}

	FS.r_close			( R );
	
	// re-open
	string_path			N;
	FS.update_path		( N, "$level$", "level.details" );
	dtFS				= new CVirtualFileRW ( N );

	R_ASSERT			( dtH.version()==DETAIL_VERSION );

	dtFS->find_chunk	( 2 );
	dtS		= (DetailSlot*)dtFS->pointer();
}

void	global_slots_data::Free			()
{
	if ( dtFS )	
		xr_delete( dtFS );
	recalculation_data.close();
}
  
void	global_slots_data::FreeOnAgent		()
{
	xr_delete( dtS );
}

void global_slots_data::process_all_pallete()
{
	R_ASSERT( header( ).version() != u32(-1) );
	R_ASSERT( header( ).x_size() != u32(-1) );
	R_ASSERT( header( ).z_size() != u32(-1) );
	for ( u32 i=0; i < header().slots_count(); ++i )
	{
		
		DetailSlot& DS = get_slot( i );
		process_pallete( DS );
		
		if( is_empty( DS ) )
			recalculation_data.set_slot_calculated( i );
	}
}