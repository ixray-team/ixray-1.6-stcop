#pragma once

#include "RenderFactory.h"
#include "../../Include/xrAPI/xrAPI.h"

template<class T> 
class FactoryPtr
{
public:
	FactoryPtr() { CreateObject();}
	~FactoryPtr() { DestroyObject();}

	FactoryPtr(const FactoryPtr<T> &_in)
	{
		CreateObject();
		m_pObject->Copy(*_in.m_pObject);
	}

	FactoryPtr& operator=( const FactoryPtr &_in)
	{
		m_pObject->Copy(*_in.m_pObject);
		return *this;
	}

	T& operator*() const {return *m_pObject;}
	T* operator->() const {return m_pObject;}

	// unspecified bool type
	typedef T const * (FactoryPtr::*unspecified_bool_type) () const;
	operator unspecified_bool_type () const	{return (!m_pObject ? 0 : &FactoryPtr::get);}
	bool operator!	() const { return m_pObject == 0;}

private:
	void CreateObject();
	void DestroyObject();
	T const* get() const { return m_pObject; }
private:
	T*					m_pObject;
};
