#include "stdafx.h"
#include "level.h"
#include "xrServer_updates_compressor.h"
#include "../xrCore/ppmd_compressor.h"
#include "../xrServerEntities/object_broker.h"
#include "xrMessages.h"


BOOL		g_sv_write_updates_bin	= FALSE;

server_updates_compressor::server_updates_compressor()
{
	u32 const need_to_reserve = (start_compress_buffer_size / sizeof(m_acc_buff.B.data)) + 1;
	for (u32 i = 0; i < need_to_reserve; ++i)
	{
		m_ready_for_send.push_back(xr_new<NET_Packet>());
	}

	m_trained_stream		= NULL;
	m_lzo_working_memory	= NULL;
	m_lzo_working_buffer	= NULL;

	if (!IsGameTypeSingle())
		init_compression();

	dbg_update_bins_writer = NULL;
}

server_updates_compressor::~server_updates_compressor()
{
	delete_data(m_ready_for_send);

	if (g_sv_write_updates_bin && dbg_update_bins_writer)
	{
		FS.w_close(dbg_update_bins_writer);
	}
	deinit_compression();
}

void server_updates_compressor::init_compression()
{
	compression::init_ppmd_trained_stream	(m_trained_stream);
	compression::init_lzo(
		m_lzo_working_memory,
		m_lzo_working_buffer,
		m_lzo_dictionary
	);
}

void server_updates_compressor::deinit_compression()
{
	if (m_trained_stream)
	{
		compression::deinit_ppmd_trained_stream(m_trained_stream);		
	}
	if (m_lzo_working_buffer)
	{
		VERIFY(m_lzo_dictionary.data);
		compression::deinit_lzo(m_lzo_working_buffer, m_lzo_dictionary);
	}
}

void server_updates_compressor::begin_updates()
{
	m_current_update = 0;
	if ((g_sv_traffic_optimization_level & eto_ppmd_compression) ||
		(g_sv_traffic_optimization_level & eto_lzo_compression))
	{
		m_ready_for_send.front()->w_begin(M_COMPRESSED_UPDATE_OBJECTS);
		m_ready_for_send.front()->w_u8(static_cast<u8>(g_sv_traffic_optimization_level));
		m_acc_buff.write_start();
	} else
	{
		m_ready_for_send.front()->write_start();
		m_acc_buff.w_begin(M_UPDATE_OBJECTS);
	}	
}

NET_Packet*	server_updates_compressor::get_current_dest()
{
	return m_ready_for_send[m_current_update];
}

NET_Packet*	server_updates_compressor::goto_next_dest()
{
	++m_current_update;
	NET_Packet*	new_dest = NULL;
	VERIFY(m_ready_for_send.size() >= m_current_update);

	if (m_ready_for_send.size() == m_current_update)
	{
		m_ready_for_send.push_back(xr_new<NET_Packet>());
		new_dest = m_ready_for_send.back();
	} else
	{
		new_dest = m_ready_for_send[m_current_update];
	}

	if (g_sv_traffic_optimization_level & eto_ppmd_compression)
	{
		new_dest->w_begin(M_COMPRESSED_UPDATE_OBJECTS);
		m_ready_for_send.front()->w_u8(static_cast<u8>(g_sv_traffic_optimization_level));
	} else
	{
		new_dest->write_start();
	}
	
	return new_dest;
}

void server_updates_compressor::flush_accumulative_buffer()
{
	NET_Packet*	dst_packet = get_current_dest();
	if ((g_sv_traffic_optimization_level & eto_ppmd_compression) ||
		(g_sv_traffic_optimization_level & eto_lzo_compression))
	{
		Device.Statistic->netServerCompressor.Begin();
		R_ASSERT(m_trained_stream);
		if (g_sv_traffic_optimization_level & eto_ppmd_compression)
		{
			m_compress_buf.B.count = ppmd_trained_compress(
				m_compress_buf.B.data,
				sizeof(m_compress_buf.B.data),
				m_acc_buff.B.data,
				m_acc_buff.B.count,
				m_trained_stream
			);
		} else
		{
			m_compress_buf.B.count = sizeof(m_compress_buf.B.data);
			lzo_compress_dict(
				m_acc_buff.B.data,
				m_acc_buff.B.count,
				m_compress_buf.B.data,
				(lzo_uint*)&m_compress_buf.B.count,
				m_lzo_working_memory,
				m_lzo_dictionary.data, m_lzo_dictionary.size
			);
		}
		Device.Statistic->netServerCompressor.End();
		//(sizeof(u16)*2 + 1) ::= w_begin(2) + compress_type(1) + zero_end(2)
		if (dst_packet->w_tell() + m_compress_buf.B.count + (sizeof(u16)*2 + 1) < sizeof(dst_packet->B.data))
		{
			dst_packet->w_u16(static_cast<u16>(m_compress_buf.B.count));
			dst_packet->w(m_compress_buf.B.data, m_compress_buf.B.count);
			m_acc_buff.write_start();
			return;
		}
		dst_packet->w_u16(0);
		dst_packet = goto_next_dest();
		dst_packet->w_u16(static_cast<u16>(m_compress_buf.B.count));
		dst_packet->w(m_compress_buf.B.data, m_compress_buf.B.count);
		m_acc_buff.write_start();
		return;
	} 
	dst_packet->w(m_acc_buff.B.data, m_acc_buff.B.count);
	goto_next_dest();
	m_acc_buff.w_begin(M_UPDATE_OBJECTS);
}

void server_updates_compressor::write_update_for(u16 const enity, NET_Packet & update)
{
	if (g_sv_traffic_optimization_level & eto_last_change)
	{
		//not implemented 
		NODEFAULT;
	}
	//(sizeof(u16)*2 + 1) ::= w_begin(2) + compress_type(1) + zero_end(2)
	if (m_acc_buff.w_tell() + update.w_tell() + (sizeof(u16)*2 + 1) >= sizeof(m_acc_buff.B.data))
	{
		flush_accumulative_buffer();
	}
	m_acc_buff.w(update.B.data, update.B.count);
}

void server_updates_compressor::end_updates(send_ready_updates_t::const_iterator & b,
											send_ready_updates_t::const_iterator & e)
{
	if (m_acc_buff.w_tell() > 2)
		flush_accumulative_buffer();
	
	if (g_sv_traffic_optimization_level & eto_ppmd_compression)
		get_current_dest()->w_u16(0);

	b = m_ready_for_send.begin();
	e = m_ready_for_send.begin() + m_current_update + 1;


	if (g_sv_write_updates_bin)
	{
		if (!dbg_update_bins_writer)
			create_update_bin_writer();

		VERIFY(dbg_update_bins_writer);
		for (send_ready_updates_t::const_iterator i = b; i != e; ++i)
		{
			dbg_update_bins_writer->w_u16(static_cast<u16>((*i)->B.count));
			dbg_update_bins_writer->w((*i)->B.data, (*i)->B.count);
		}
	}
}

void server_updates_compressor::create_update_bin_writer	()
{
	string_path		bin_name;
	FS.update_path	(bin_name, "$logs$", "updates.bins");
	dbg_update_bins_writer = FS.w_open(bin_name);
	VERIFY(dbg_update_bins_writer);
	
	static u8 const header[] = {'B','I','N','S'};
	dbg_update_bins_writer->w(header, sizeof(header));
}