#pragma once
#include "filetransfer_common.h"

namespace file_transfer
{
	class filereceiver_node
	{
	private:
		shared_str m_file_name;
		u32 m_data_size_to_receive;
		u32 m_user_param;
		IWriter* m_writer;
		bool const m_is_writer_memory;	//if true then IWriter is a CMemoryWriter ...
		receiving_state_callback_t m_process_callback;
		u32 m_last_read_time;

	public:
		filereceiver_node(shared_str const& file_name, receiving_state_callback_t const& callback);
		filereceiver_node(CMemoryWriter* mem_writer, receiving_state_callback_t const& callback);
		filereceiver_node& operator=(filereceiver_node const& copy) = delete;
		~filereceiver_node();

		bool receive_packet(NET_Packet& packet);	//returns true if receiving is complete
		bool is_complete();
		void signal_callback(receiving_status_t status);

		inline	u32	const get_downloaded_size() { return (u32)m_writer->tell(); };
		inline	u32 const get_last_read_time() { return m_last_read_time; };
		inline	void set_last_read_time(u32 const read_time) { m_last_read_time = read_time; };
		inline	IWriter* get_writer() { return m_writer; };
		inline	u32 get_user_param() { return m_user_param; };
	};

	void split_received_to_buffers(u8* data_ptr, u32 data_size, buffer_vector<const_buffer_t>& dst_buffers);
}