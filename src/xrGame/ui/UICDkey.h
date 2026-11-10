//
#pragma once
#include "../../xrUI/Widgets/UIEditBox.h"

class CUICDkey final : public CUIEditBox
{
private:
	typedef			CUIEditBox		inherited;

public:
					CUICDkey		();
	virtual void	SetText			(const char* str) override {}
	virtual const char*	GetText			() override;

	// CUIOptionsItem
	virtual void	SetCurrentOptValue	() override;	// opt->current
	virtual void	SaveBackUpOptValue	() override;	// current->backup
	virtual void	SaveOptValue		() override;	// current->opt
	virtual void	UndoOptValue		() override;	// backup->current
	virtual bool	IsChangedOptValue	() const override;	// backup!=current
	
			void	CreateCDKeyEntry();			
			void	assign_callbacks();

	virtual void	Show			(bool status) override;
	virtual void	Draw			() override;
	virtual void	OnFocusLost		() override;

	virtual CUIWindow* ui_cast_window() override { return this; }

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
	virtual ~CUIMPPlayerName() override {};

//	virtual	void	SetText			(const char* str) {}

//	virtual void	SetCurrentValue();
//	virtual void	SaveValue();
//	virtual bool	IsChanged();

	virtual void	OnFocusLost		() override;

	virtual CUIWindow* ui_cast_window() override { return this; }

}; // class CUIMPPlayerName

extern	void	GetCDKey_FromRegistry		(char* cdkey);
extern	void	WriteCDKey_ToRegistry		(LPSTR cdkey);
extern	void	GetPlayerName_FromRegistry	(char* name, u32 const name_size);
extern	void	WritePlayerName_ToRegistry	(LPSTR name);
