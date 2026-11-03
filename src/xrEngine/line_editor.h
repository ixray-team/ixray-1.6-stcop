////////////////////////////////////////////////////////////////////////////
//	Module 		: line_editor.h
//	Created 	: 22.02.2008
//	Modified 	: 21.07.2026
//	Author		: Evgeniy Sokolov
//	Description : line editor class, controller of line_edit_control
////////////////////////////////////////////////////////////////////////////
#pragma once

#include "IInputReceiver.h"
#include "line_edit_control.h"

namespace text_editor
{

class line_editor :	public IInputReceiver
{
public:
					line_editor		( u32 str_buffer_size );
	virtual ~line_editor	() override;

	IC line_edit_control&	control	()				{ return m_control; }

			void	on_frame		();

protected:
	virtual void	IR_OnKeyboardPress	( int dik ) override;
	virtual void	IR_OnKeyboardHold	( int dik ) override;
	virtual void	IR_OnKeyboardRelease( int dik ) override;

private:
	line_edit_control		m_control;

}; // class line_editor

} // namespace text_editor
