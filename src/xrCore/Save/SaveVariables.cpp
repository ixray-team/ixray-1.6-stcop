#include "stdafx.h"
#include "SaveVariables.h"
#include "MemoryBuffer.h"
#include "SaveManager.h"

ISaveable* CSaveVariableArray::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableArray>(ESaveVariableType::t_arrayUnspec);
	for (auto elem : _array)
	{
		copy->AddVariable(elem->MakeCopy());
	}
	return copy;
}

void CSaveVariableArray::Clear()
{
	for (auto elem : _array)
	{
		elem->Clear();
		CSaveManager::GetInstance().ReleaseSaveable(elem);
	}
	_array.clear();
	_currentReadPos = 0;
}

void CSaveVariableBool::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	Buffer.Write(ESaveVariableType::t_bool);
	Task->ConditionalWriteBool(_value, Buffer);
}

ISaveable* CSaveVariableBool::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableBool>(ESaveVariableType::t_bool);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableFloat::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	Buffer.Write(ESaveVariableType::t_float);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableFloat::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableFloat>(ESaveVariableType::t_float);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableDouble::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	Buffer.Write(ESaveVariableType::t_double);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableDouble::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableDouble>(ESaveVariableType::t_double);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableU64::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	if (CSaveManager::GetInstance().TestFlag(CSaveManager::ESaveManagerFlagsGeneral::EUseIntOptimization)) {
		if (_value <= std::numeric_limits<u8>::max()) {
			Buffer.Write(ESaveVariableType::t_u64_op8);
			Buffer.Write<u8>(_value);
			return;
		}
		if (_value <= std::numeric_limits<u16>::max()) {
			Buffer.Write(ESaveVariableType::t_u64_op16);
			Buffer.Write<u16>(_value);
			return;
		}
		if (_value <= std::numeric_limits<u32>::max()) {
			Buffer.Write(ESaveVariableType::t_u64_op32);
			Buffer.Write<u32>(_value);
			return;
		}
	}
	Buffer.Write(ESaveVariableType::t_u64);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableU64::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableU64>(ESaveVariableType::t_u64);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableS64::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	if (CSaveManager::GetInstance().TestFlag(CSaveManager::ESaveManagerFlagsGeneral::EUseIntOptimization)) {
		if (_value <= std::numeric_limits<s8>::max() && _value >= std::numeric_limits<s8>::min()) {
			Buffer.Write(ESaveVariableType::t_s64_op8);
			Buffer.Write<s8>(_value);
			return;
		}
		if (_value <= std::numeric_limits<s16>::max() && _value >= std::numeric_limits<s16>::min()) {
			Buffer.Write(ESaveVariableType::t_s64_op16);
			Buffer.Write<s16>(_value);
			return;
		}
		if (_value <= std::numeric_limits<s32>::max() && _value >= std::numeric_limits<s32>::min()) {
			Buffer.Write(ESaveVariableType::t_s64_op32);
			Buffer.Write<s32>(_value);
			return;
		}
	}
	Buffer.Write(ESaveVariableType::t_s64);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableS64::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableS64>(ESaveVariableType::t_s64);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableU32::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	if (CSaveManager::GetInstance().TestFlag(CSaveManager::ESaveManagerFlagsGeneral::EUseIntOptimization)) {
		if (_value <= std::numeric_limits<u8>::max()) {
			Buffer.Write(ESaveVariableType::t_u32_op8);
			Buffer.Write<u8>(_value);
			return;
		}
		if (_value <= std::numeric_limits<u16>::max()) {
			Buffer.Write(ESaveVariableType::t_u32_op16);
			Buffer.Write<u16>(_value);
			return;
		}
	}
	Buffer.Write(ESaveVariableType::t_u32);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableU32::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableU32>(ESaveVariableType::t_u32);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableS32::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	if (CSaveManager::GetInstance().TestFlag(CSaveManager::ESaveManagerFlagsGeneral::EUseIntOptimization)) {
		if (_value <= std::numeric_limits<s8>::max() && _value >= std::numeric_limits<s8>::min()) {
			Buffer.Write(ESaveVariableType::t_s32_op8);
			Buffer.Write<s8>(_value);
			return;
		}
		if (_value <= std::numeric_limits<s16>::max() && _value >= std::numeric_limits<s16>::min()) {
			Buffer.Write(ESaveVariableType::t_s32_op16);
			Buffer.Write<s16>(_value);
			return;
		}
	}
	Buffer.Write(ESaveVariableType::t_s32);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableS32::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableS32>(ESaveVariableType::t_s32);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableU16::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	if (CSaveManager::GetInstance().TestFlag(CSaveManager::ESaveManagerFlagsGeneral::EUseIntOptimization)) {
		if (_value <= std::numeric_limits<u8>::max()) {
			Buffer.Write(ESaveVariableType::t_u16_op8);
			Buffer.Write<u8>(_value);
			return;
		}
	}
	Buffer.Write(ESaveVariableType::t_u16);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableU16::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableU16>(ESaveVariableType::t_u16);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableS16::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	if (CSaveManager::GetInstance().TestFlag(CSaveManager::ESaveManagerFlagsGeneral::EUseIntOptimization)) {
		if (_value <= std::numeric_limits<s8>::max() && _value >= std::numeric_limits<s8>::min()) {
			Buffer.Write(ESaveVariableType::t_s16_op8);
			Buffer.Write<s8>(_value);
			return;
		}
	}
	Buffer.Write(ESaveVariableType::t_s16);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableS16::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableS16>(ESaveVariableType::t_s16);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableU8::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	Buffer.Write(ESaveVariableType::t_u8);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableU8::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableU8>(ESaveVariableType::t_u8);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableS8::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	Buffer.Write(ESaveVariableType::t_s8);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableS8::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableS8>(ESaveVariableType::t_s8);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableString::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	Buffer.Write(ESaveVariableType::t_string);
	Task->ConditionalWriteString(_value, Buffer);
}

ISaveable* CSaveVariableString::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableString>(ESaveVariableType::t_string);
	copy->SetValue(_value);
	return copy;
}

void CSaveVariableStringLong::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	Buffer.Write(ESaveVariableType::t_longstring);
	Buffer.Write(_value);
}

ISaveable* CSaveVariableStringLong::MakeCopy()
{
	auto copy = CSaveManager::GetInstance().GetSaveable<CSaveVariableStringLong>(ESaveVariableType::t_longstring);
	copy->SetValue(_value);
	return copy;
}

CSaveVariableArray::~CSaveVariableArray()
{
	for (size_t i = 0; i < _array.size(); ++i) {
		xr_delete(_array[i]);
	}
}

void CSaveVariableArray::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	Buffer.Write(ESaveVariableType::t_arrayUnspec);
	for (const auto& elem : _array) {
		elem->Write(Buffer, Task);
	}
	Buffer.Write(ESaveVariableType::t_arrayUnspecEnd);
}
