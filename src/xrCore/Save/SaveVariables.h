#pragma once

#include "xrCore/_types.h"

struct SSaveTask;
class CMemoryBuffer;

enum class XRCORE_API ESaveVariableType : u8 {
	// C++ types
	t_zero_flag,
	t_bool,
	t_float,
	t_double,
	t_u64,
	t_u64_op32,
	t_u64_op16,
	t_u64_op8,
	t_s64,
	t_s64_op32,
	t_s64_op16,
	t_s64_op8,
	t_u32,
	t_u32_op16,
	t_u32_op8,
	t_s32,
	t_s32_op16,
	t_s32_op8,
	t_u16,
	t_u16_op8,
	t_s16,
	t_s16_op8,
	t_u8,
	t_s8,
	t_string,
	t_chunkStart,
	t_chunkEnd,
	t_array,
	t_arrayUnspec,
	t_arrayUnspecEnd,
	t_chunk,
	t_longstring, // in case if some vasyan decided to store enormous string (more than 4k symbols - shared_str limit)
	// Lua types
	//t_luanil,
	//t_luatable,
	//t_luaprimitive,
	//t_luastorage,
	t_invalid = u8(-1),
};

class XRCORE_API ISaveable{
protected:
	virtual void* GetValue() = 0;
public:
	virtual ~ISaveable() = default;
	virtual ESaveVariableType GetVariableType() = 0;
	//virtual bool IsArray() = 0;
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) = 0;

	virtual ISaveable* GetCurrentElement() = 0;
	virtual void Next() = 0;
	virtual void AddVariable(ISaveable* data) = 0;
	virtual u64 GetSize() = 0;
	
	virtual ISaveable* MakeCopy() = 0;
	virtual void Clear() = 0;
	
#ifdef DEBUG
	virtual u64 MemSize() = 0;
#endif
};

class XRCORE_API CSaveVariableBase:
	public ISaveable
{

public:
	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_invalid; }
	//virtual bool IsArray() override { return false; }

	virtual ISaveable* GetCurrentElement() override { return nullptr; };
	virtual void Next() override {};
	virtual void AddVariable(ISaveable* data) override {};
	virtual u64 GetSize() override { return 0; };
	virtual void Clear() override {};
};

class XRCORE_API CSaveVariableArray :
	public ISaveable
{
	using array_type = xr_vector<ISaveable*>;
	u64 _currentReadPos = 0;
	array_type _array;

protected:
	void* GetValue() override { return nullptr; };

public:
	CSaveVariableArray() {}
	~CSaveVariableArray();

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_arrayUnspec; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;

	virtual u64 GetSize() override { return _array.size(); }
	virtual ISaveable* GetCurrentElement() override { VERIFY(_currentReadPos < _array.size()); return _array[_currentReadPos]; }
	virtual void Next() override { ++_currentReadPos; }

	virtual void AddVariable(ISaveable* data) override { _array.emplace_back(data); }

	array_type::iterator begin() { return _array.begin(); }
	array_type::iterator end() { return _array.end(); }
	array_type::const_iterator begin() const { return _array.begin(); }
	array_type::const_iterator end() const { return _array.end(); }
	array_type::const_iterator cbegin() const { return _array.cbegin(); }
	array_type::const_iterator cend() const { return _array.cend(); }
	array_type::reverse_iterator rbegin() { return _array.rbegin(); }
	array_type::reverse_iterator rend() { return _array.rend(); }
	array_type::const_reverse_iterator rbegin() const { return _array.rbegin(); }
	array_type::const_reverse_iterator rend() const { return _array.rend(); }
	array_type::const_reverse_iterator crbegin() const { return _array.crbegin(); }
	array_type::const_reverse_iterator crend() const { return _array.crend(); }
	
	virtual ISaveable* MakeCopy() override;
	virtual void Clear() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		u64 MemSize = 0;
		for (auto& elem : _array)
		{
			MemSize += elem->MemSize();
		}
		return MemSize + sizeof(CSaveVariableArray);
	}
#endif
	
};

class XRCORE_API CSaveVariableBool:
	public CSaveVariableBase 
{
	friend struct SSaveVariableGetter;
	bool _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableBool() = default;
	CSaveVariableBool(bool Value): _value(Value){}

	void SetValue(bool Value) { _value = Value; }
	
	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_bool; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableBool);
	}
#endif
};

class XRCORE_API CSaveVariableFloat :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	float _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableFloat() = default;
	CSaveVariableFloat(float Value) : _value(Value) {}
	
	void SetValue(float Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_float; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableFloat);
	}
#endif
};

class XRCORE_API CSaveVariableDouble :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	double _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableDouble() = default;
	CSaveVariableDouble(double Value) : _value(Value) {}
	
	void SetValue(double Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_double; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableDouble);
	}
#endif
};

class XRCORE_API CSaveVariableU64 :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	u64 _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableU64() = default;
	CSaveVariableU64(u64 Value) : _value(Value) {}
	
	void SetValue(u64 Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_u64; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableU64);
	}
#endif
};

class XRCORE_API CSaveVariableS64 :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	s64 _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableS64() = default;
	CSaveVariableS64(s64 Value) : _value(Value) {}
	
	void SetValue(s64 Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_s64; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableS64);
	}
#endif
};

class XRCORE_API CSaveVariableU32 :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	u32 _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableU32() = default;
	CSaveVariableU32(u32 Value) : _value(Value) {}
	
	void SetValue(u32 Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_u32; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableU32);
	}
#endif
};

class XRCORE_API CSaveVariableS32 :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	s32 _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableS32() = default;
	CSaveVariableS32(s32 Value) : _value(Value) {}
	
	void SetValue(s32 Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_s32; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableS32);
	}
#endif
};

class XRCORE_API CSaveVariableU16 :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	u16 _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableU16() = default;
	CSaveVariableU16(u16 Value) : _value(Value) {}
	
	void SetValue(u16 Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_u16; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableU16);
	}
#endif
};

class XRCORE_API CSaveVariableS16 :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	s16 _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableS16() = default;
	CSaveVariableS16(s16 Value) : _value(Value) {}
	
	void SetValue(s16 Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_s16; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableS16);
	}
#endif
};

class XRCORE_API CSaveVariableU8 :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	u8 _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableU8() = default;
	CSaveVariableU8(u8 Value) : _value(Value) {}
	
	void SetValue(u8 Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_u8; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableU8);
	}
#endif
};

class XRCORE_API CSaveVariableS8 :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	s8 _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableS8() = default;
	CSaveVariableS8(s8 Value) : _value(Value) {}
	
	void SetValue(s8 Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_s8; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableS8);
	}
#endif
};

class XRCORE_API CSaveVariableString :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	shared_str _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableString() = default;
	CSaveVariableString(const xr_string& Value) : _value(Value.c_str()) {}
	CSaveVariableString(const shared_str& Value) : _value(Value) {}
	CSaveVariableString(str_c Value) : _value(Value) {}
	
	void SetValue(const xr_string& Value) { _value = Value.c_str(); }
	void SetValue(const shared_str& Value) { _value = Value; }
	void SetValue(str_c Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_string; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableString) + _value.size()*sizeof(char);
	}
#endif
};

class XRCORE_API CSaveVariableStringLong :
	public CSaveVariableBase
{
	friend struct SSaveVariableGetter;
	xr_string _value;

protected:
	virtual void* GetValue() override { return &_value; }

public:
	CSaveVariableStringLong() = default;
	CSaveVariableStringLong(const xr_string& Value) : _value(Value.c_str()) {}
	CSaveVariableStringLong(const shared_str& Value) : _value(Value.c_str()) {}
	CSaveVariableStringLong(str_c Value) : _value(Value) {}
	
	void SetValue(const xr_string& Value) { _value = Value.c_str(); }
	void SetValue(const shared_str& Value) { _value = Value.c_str(); }
	void SetValue(str_c Value) { _value = Value; }

	virtual ESaveVariableType GetVariableType() override { return ESaveVariableType::t_longstring; }
	virtual void Write(CMemoryBuffer& Buffer, SSaveTask* Task) override;
	
	virtual ISaveable* MakeCopy() override;
	
#ifdef DEBUG
	virtual u64 MemSize() override
	{
		return sizeof(CSaveVariableStringLong) + _value.size()*sizeof(char);
	}
#endif
};

struct SSaveVariableGetter {
	template<typename TType, typename TVarClass>
	static TType GetValue(ISaveable* Var) { return *((TType*)((TVarClass*)Var)->GetValue()); }

	// dangerous, but made to avoid multiple allocations for huge data (like vasyan str)
	template<typename TType, typename TVarClass>
	static TType* GetValuePtr(ISaveable* Var) { return ((TType*)((TVarClass*)Var)->GetValue()); }
};