#pragma once

typedef fastdelegate::FastDelegate2<PropValue*, xr_string&> 	TOnDrawTextEvent;
typedef fastdelegate::FastDelegate1<PropItem*> 					TOnClick;
//------------------------------------------------------------------------------

class PropValue
{
	friend class		CPropHelper;
	friend class		PropItem;
protected:
	PropItem* m_Owner;
public:
	u32					tag;
public:
	// base events
	typedef fastdelegate::FastDelegate1<PropValue*> TOnChange;
	TOnChange			OnChangeEvent;
public:
	PropValue() :tag(0), m_Owner(0), OnChangeEvent(0) { ; }
	virtual				~PropValue() {}
	virtual xr_string	GetDrawText(TOnDrawTextEvent OnDrawText) = 0;
	virtual void		ResetValue() = 0;
	virtual bool		Equal(PropValue* prop) = 0;
	IC PropItem* Owner() { return m_Owner; }
};