#ifndef XRSERVER_UPDATES_COMPRESSOR_INCLUDED
#define XRSERVER_UPDATES_COMPRESSOR_INCLUDED

#include "traffic_optimization.h"

class server_updates_compressor
{
public:
	server_updates_compressor	();
	~server_updates_compressor	();

	typedef xr_vector<NET_Packet*>	send_ready_updates_t;

	void	begin_updates		();
	void	write_update_for	(u16 const enity, NET_Packet & update);
	void	end_updates			(send_ready_updates_t::const_iterator & b,
								 send_ready_updates_t::const_iterator & e);
private:
	//actor update size ~ 150 bytes..
	static u32 const entities_count					= 32;
	static u32 const start_compress_buffer_size		= 1024 * 150 * entities_count;
	typedef std::pair<u16, NET_Packet>				last_update_t;
	typedef last_update_t last_updates_cache_t[entities_count];

	enum_traffic_optimization		m_traffic_optimization;

	NET_Packet						m_acc_buff;
	NET_Packet						m_compress_buf;
	
	last_updates_cache_t			m_last_updates_cache;

	send_ready_updates_t			m_ready_for_send;
	send_ready_updates_t::size_type m_current_update;

	compression::ppmd_trained_stream*			m_trained_stream;
	compression::lzo_dictionary_buffer			m_lzo_dictionary;
	//alligned to 16 bytes m_lzo_working_buffer
	u8*											m_lzo_working_memory;
	u8*											m_lzo_working_buffer;
	
	void			init_compression			();
	void			deinit_compression			();

	void			flush_accumulative_buffer	();
	NET_Packet*		get_current_dest			();
	NET_Packet*		goto_next_dest				();

	IWriter*		dbg_update_bins_writer;
	void			create_update_bin_writer	();	
};//class server_updates_compressor


#endif//#ifndef XRSERVER_UPDATES_COMPRESSOR_INCLUDED