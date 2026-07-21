////////////////////////////////////////////////////////////////////////////
//	Module 		: line_edit_control.h
//	Created 	: 21.02.2008
//	Author		: Evgeniy Sokolov
//	Description : line edit control class
////////////////////////////////////////////////////////////////////////////
#pragma once

namespace text_editor
{

void remove_spaces( char* str ); // in & out
void split_cmd( char* first, char* second, const char* str );

class base;

enum key_state // Flags32
{
	ks_free   	= u32(0),
	ks_LShift 	= u32(1) << 0,
	ks_RShift 	= u32(1) << 1,
	ks_LCtrl  	= u32(1) << 2,
	ks_RCtrl  	= u32(1) << 3,
	ks_LAlt   	= u32(1) << 4,
	ks_RAlt   	= u32(1) << 5,
	ks_CapsLock	= u32(1) << 6,

	ks_Shift  	= u32( ks_LShift | ks_RShift ),
	ks_Ctrl   	= u32( ks_LCtrl  | ks_RCtrl  ),
	ks_Alt    	= u32( ks_LAlt   | ks_RAlt   ),

	ks_force  	= u32(-1)

};

enum init_mode
{
	im_standart = 0,
	im_number_only,
	im_read_only,
	im_file_name_mode, // not "/\\:*?\"<>|^()[]%" 

	im_count
};

class ENGINE_API line_edit_control
{
	using Base = base;
	using Callback = xr_delegate<void()>;

public:
	line_edit_control(u32 str_buffer_size);
	void init(u32 str_buffer_size, init_mode mode = im_standart);
	~line_edit_control();

	void clear_states();
	void on_key_press(int sdl_scancode);
	void on_key_hold(int sdl_scancode);
	void on_key_release(int sdl_scancode);
	void on_frame();

	void assign_callback(u32 sdl_scancode, key_state state, Callback const& callback);

	void insert_character(char c);

	ICF bool get_key_state(key_state mask) const { return mask ? !!m_key_state.test(mask) : true; }
	ICF void set_key_state(key_state mask, bool value) { m_key_state.set(mask, value); }

	ICF bool cursor_view() const { return m_cursor_view; }
	ICF bool need_update() const { return m_need_update; }

	ICF const char* str_edit() const { return m_edit_str; }
	ICF const char* str_before_cursor() const { return m_buf0; }
	ICF const char* str_before_mark() const { return m_buf1; }
	ICF const char* str_mark() const { return m_buf2; }
	ICF const char* str_after_mark() const { return m_buf3; }

	void set_edit(const char* str);
	void set_selected_mode(bool status) { m_unselected_mode = !status; }
	bool get_selected_mode() const { return !m_unselected_mode; }

private:
	line_edit_control(line_edit_control const&);
	line_edit_control const& operator=(line_edit_control const&);

	void update_key_states();
	void update_bufs();

	void undo_buf();
	void select_all_buf();
	void flip_insert_mode();

	void copy_to_clipboard();
	void paste_from_clipboard();
	void cut_to_clipboard();

	void move_pos_home();
	void move_pos_end();
	u32 set_pos(u32 index);
	u32 get_pos();
	void move_pos_left();
	void move_pos_right();
	void move_pos_left_word();
	void move_pos_right_word();

	void delete_selected_back();
	void delete_selected_forward();
	void delete_word_back();
	void delete_word_forward();
	void SwitchKL();

	void assign_char_pairs(init_mode mode);
	void create_key_state(u32 dik, key_state state);
	void create_char_pair(u32 dik, char c, char c_shift, bool translate = false);

	void clear_inserted();
	bool empty_inserted();

	void add_inserted_text();

	void delete_selected(bool back);
	void compute_positions();
	void clamp_cur_pos();

	static constexpr u32 CMD_SDL_SCANCODE_COUNT = 256;
	static constexpr u32 MIN_BUF_SIZE = 8u;
	static constexpr u32 MAX_BUF_SIZE = 4096u;

	Base* m_actions[CMD_SDL_SCANCODE_COUNT];

	char* m_edit_str;
	char* m_undo_buf;
	char* m_inserted;
	char* m_buf0;
	char* m_buf1;
	char* m_buf2;
	char* m_buf3;

	u32 m_buffer_size;

	u32 m_cur_pos;
	u32 m_select_start;
	u32 m_p1;
	u32 m_p2;

	float m_accel;
	float m_cur_time;
	float m_rep_time;
	float m_last_key_time;
	u32 m_last_frame_time;
	u32 m_last_changed_frame;
	
	Flags32 m_key_state;

	bool m_hold_mode;
	bool m_insert_mode;
	bool m_repeat_mode;
	bool m_mark;
	bool m_cursor_view;
	bool m_need_update;
	bool m_unselected_mode;
};

}