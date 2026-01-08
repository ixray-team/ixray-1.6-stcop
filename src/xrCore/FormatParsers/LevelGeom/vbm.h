#pragma once

constexpr u32 c_VB_maxSize = 4096*1024;	// bytes
constexpr u32 c_VB_maxVertices = 65535; // count

class XRCORE_API VBContainerBase
{
public:
	VBContainerBase() = default;
	virtual ~VBContainerBase() = default;

	VBContainerBase(const VBContainerBase&) = delete;
	VBContainerBase& operator=(const VBContainerBase&) = delete;
	VBContainerBase(VBContainerBase&&) = default;
	VBContainerBase& operator=(VBContainerBase&&) = default;
	
	virtual size_t size() const = 0;
	virtual void Save(IWriter &fs) const = 0;
};

class XRCORE_API IBContainerBase
{
public:
	IBContainerBase() = default;
	virtual ~IBContainerBase() = default;

	IBContainerBase(const IBContainerBase&) = delete;
	IBContainerBase& operator=(const IBContainerBase&) = delete;
	IBContainerBase(IBContainerBase&&) = default;
	IBContainerBase& operator=(IBContainerBase&&) = default;
	
	virtual size_t size() const = 0;
	virtual void Save(IWriter &fs) const = 0;
};

class XRCORE_API SWIContainerBase
{
public:
	SWIContainerBase() = default;
	virtual ~SWIContainerBase() = default;

	SWIContainerBase(const SWIContainerBase&) = delete;
	SWIContainerBase& operator=(const SWIContainerBase&) = delete;
	SWIContainerBase(SWIContainerBase&&) = default;
	SWIContainerBase& operator=(SWIContainerBase&&) = default;
	
	virtual size_t size() const = 0;
	virtual void Save(IWriter &fs) const = 0;
};
