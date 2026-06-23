#pragma once

#include "RenderFactory.h"
#include "../../xrCore/API/xrAPI.h"

#define FACTORY_PTR_DECL(Class)                             \
	template <> void FactoryPtr<I##Class>::CreateObject();  \
	template <> void FactoryPtr<I##Class>::DestroyObject(); 

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

FACTORY_PTR_DECL(StatsRender)

FACTORY_PTR_DECL(ThunderboltRender)
FACTORY_PTR_DECL(ThunderboltDescRender)
FACTORY_PTR_DECL(EnvDescriptorRender)
FACTORY_PTR_DECL(EnvDescriptorMixerRender)

FACTORY_PTR_DECL(FlareRender)
FACTORY_PTR_DECL(LensFlareRender)
FACTORY_PTR_DECL(RainRender)
FACTORY_PTR_DECL(EnvironmentRender)
FACTORY_PTR_DECL(WallMarkArray)
FACTORY_PTR_DECL(UIShader)
FACTORY_PTR_DECL(StatGraphRender)
