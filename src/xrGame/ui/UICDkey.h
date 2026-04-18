//
#pragma once
#include "../../xrUI/Widgets/UIEditBox.h"

class CUICDkey final : public CUIEditBox
{
private:
	typedef			CUIEditBox		inherited;

public:
					CUICDkey		();
	virtual	void	SetText			(const char* str) {}
	virtual	const char*	GetText			();

	// CUIOptionsItem
	virtual void	SetCurrentOptValue	();	// opt->current
	virtual void	SaveBackUpOptValue	();	// current->backup
	virtual void	SaveOptValue		();	// current->opt
	virtual void	UndoOptValue		();	// backup->current
	virtual bool	IsChangedOptValue	() const;	// backup!=current
	
			void	CreateCDKeyEntry();			
			void	assign_callbacks();

	virtual void	Show			(bool status);
	virtual void	Draw			();
	virtual void	OnFocusLost		();

	virtual CUIWindow* ui_cast_window() { return this; }

private:
	void 	paste_from_clipboard();

private:
	string512		m_opt_backup_value;
	bool			m_view_access;
}; // class CUICDkey

class CUIMPPlayerName final : public CUIEditBox
{
private:
	typedef			CUIEditBox		inherited;

public:
					CUIMPPlayerName	() {};
	virtual			~CUIMPPlayerName() {};

//	virtual	void	SetText			(const char* str) {}

//	virtual void	SetCurrentValue();
//	virtual void	SaveValue();
//	virtual bool	IsChanged();

	virtual void	OnFocusLost		();

	virtual CUIWindow* ui_cast_window() { return this; }

}; // class CUIMPPlayerName

extern	void	GetCDKey_FromRegistry		(char* cdkey);
extern	void	WriteCDKey_ToRegistry		(LPSTR cdkey);
extern	void	GetPlayerName_FromRegistry	(char* name, u32 const name_size);
extern	void	WritePlayerName_ToRegistry	(LPSTR name);
