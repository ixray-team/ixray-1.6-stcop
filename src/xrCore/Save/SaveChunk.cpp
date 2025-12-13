#include "stdafx.h"
#include "SaveChunk.h"
#include "xrDebug_macros.h"
#include <magic_enum/magic_enum.hpp>

#include "MemoryBuffer.h"
#include "SaveManager.h"

CSaveChunk::~CSaveChunk()
{
	IVERIFY(_subchunks.empty());
	IVERIFY(_variables.empty());
	for (auto& elem : _subchunks) {
		xr_delete(elem.second);
	}
	for (size_t i = 0; i < _variables.size(); ++i) {
		xr_delete(_variables[i]);
	}
}

void CSaveChunk::Clear()
{
	for (auto& subchunk : _subchunks)
	{
		subchunk.second->Clear();
		CSaveManager::GetInstance().ReleaseSaveable(subchunk.second);
	}
	_subchunks.clear();
	for (auto& var : _variables)
	{
		var->Clear();
		CSaveManager::GetInstance().ReleaseSaveable(var);
	}
	_variables.clear();
	_currentReadIndex = 0;
	while (!_currentArrayStack.empty())
	{
		_currentArrayStack.pop();
	}
	_chunkName = "";
}

bool CSaveChunk::ContainsSubchunk(shared_str subchunkName)
{
	if (_currentArrayStack.empty()) {
		auto Chunk = _subchunks.find(subchunkName);
		return Chunk != _subchunks.end();
	}
	for (auto& TopArray = *_currentArrayStack.top();
		const auto& elem : TopArray)
	{
		if (elem->GetVariableType() != ESaveVariableType::t_chunk)
		{
			xr_string Message = "Chunk: ";
			Message+=_chunkName.c_str();
			Message+=", Subchunk: ";
			Message+=subchunkName.c_str();
			R_ASSERT4(elem->GetVariableType() != ESaveVariableType::t_chunk,
				"Attempt to find chunk in array, but it contains something else!", Message.c_str(), std::string(magic_enum::enum_name(elem->GetVariableType())).c_str());
		}
		if (((CSaveChunk*)elem)->GetChunkName() == subchunkName)
		{
			return true;
		}
		R_ASSERT4(_currentArrayStack.empty(),
			"Different save chunks are not designed to be in same array!", _chunkName.c_str(), subchunkName.c_str());
	}
	return false;
}

bool CSaveChunk::DetachSubchunk(CSaveChunk& subchunk)
{
	VERIFY(_currentArrayStack.empty());
	return _subchunks.erase(subchunk._chunkName);
}

void CSaveChunk::Write(CMemoryBuffer& Buffer, SSaveTask* Task)
{
	Buffer.Write((u8)ESaveVariableType::t_chunkStart);
	Task->ConditionalWriteString(_chunkName, Buffer); // TODO: Optimize and store only hash?
	for (const auto& elem : _subchunks) {
		elem.second->Write(Buffer, Task);
	}
	for (const auto& elem : _variables) {
		elem->Write(Buffer, Task);
	}
	Buffer.Write((u8)ESaveVariableType::t_chunkEnd);
}

void CSaveChunk::ReadArray(u64& Size)
{
	if (_currentArrayStack.empty()) {
		auto Array = smart_cast<CSaveVariableArray*>(_variables[_currentReadIndex++]);
		R_ASSERT3(Array, "Unable to read array", _chunkName.c_str());
		_currentArrayStack.push(Array);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		auto CurrentElement = smart_cast<CSaveVariableArray*>(CurrentArray->GetCurrentElement());
		R_ASSERT3(CurrentElement, "Unable to read array", _chunkName.c_str());
		_currentArrayStack.push(CurrentElement);
	}
	Size = _currentArrayStack.top()->GetSize();
}

void CSaveChunk::WriteArray()
{
	_variables.emplace_back(CSaveManager::GetInstance().GetSaveable<CSaveVariableArray>(ESaveVariableType::t_arrayUnspec));
	auto ArrayPtr = (CSaveVariableArray*)_variables.back();
	_currentArrayStack.push(ArrayPtr);

}

void CSaveChunk::EndArray()
{
	_currentArrayStack.pop();
}

CSaveChunk* CSaveChunk::BeginChunk(shared_str ChunkName)
{
	if (_currentArrayStack.empty()) {
		R_ASSERT4(_subchunks.find(ChunkName) == _subchunks.end(), "There is already a subchunk with same name", _chunkName.c_str(), ChunkName.c_str());
		auto NewChunk = CSaveManager::GetInstance().GetSaveable<CSaveChunk>(ESaveVariableType::t_chunk);
		NewChunk->SetChunkName(ChunkName);
		return _subchunks.emplace(ChunkName, NewChunk).first->second;
	}
	auto Array = _currentArrayStack.top();
	auto Value = CSaveManager::GetInstance().GetSaveable<CSaveChunk>(ESaveVariableType::t_chunk);
	Value->SetChunkName(ChunkName);
	Array->AddVariable(Value);
	return Value;
}

CSaveChunk* CSaveChunk::FindChunk(shared_str ChunkName)
{
	if (_currentArrayStack.empty()) {
		auto Chunk = _subchunks.find(ChunkName);
		R_ASSERT4(Chunk != _subchunks.end(), "Unable to find subchunk in chunk", _chunkName.c_str(), ChunkName.c_str());
		return Chunk->second;
	}
	auto Array = _currentArrayStack.top();
	auto CurrentElement = Array->GetCurrentElement(); 
	Array->Next();
	R_ASSERT3(CurrentElement->GetVariableType() == ESaveVariableType::t_chunk, "Invalid variable type access in chunk", _chunkName.c_str());
	auto CastedElement = (CSaveChunk*)CurrentElement;
	R_ASSERT4(CastedElement->_chunkName == ChunkName, "Search for invalid chunk in array", _chunkName.c_str(), ChunkName.c_str());
	return CastedElement;
}

void CSaveChunk::w_bool(bool a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableBool>(ESaveVariableType::t_bool);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_float(float a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableFloat>(ESaveVariableType::t_float);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_double(double a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableDouble>(ESaveVariableType::t_double);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_u64(u64 a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableU64>(ESaveVariableType::t_u64);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_s64(s64 a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableS64>(ESaveVariableType::t_s64);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_u32(u32 a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableU32>(ESaveVariableType::t_u32);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_s32(s32 a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableS32>(ESaveVariableType::t_s32);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_u16(u16 a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableU16>(ESaveVariableType::t_u16);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_s16(s16 a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableS16>(ESaveVariableType::t_s16);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_u8(u8 a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableU8>(ESaveVariableType::t_u8);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_s8(s8 a)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableS8>(ESaveVariableType::t_s8);
	NewElem->SetValue(a);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		_currentArrayStack.top()->AddVariable(NewElem);
	}
}

void CSaveChunk::w_string(shared_str S)
{
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableString>(ESaveVariableType::t_string);
	NewElem->SetValue(S);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		((CSaveVariableArray*)_currentArrayStack.top())->AddVariable(NewElem);
	}
}

void CSaveChunk::w_string_long(LPCSTR S)
{
#ifndef MASTER_GOLD
	static bool ignore_always = false;
	if (!ignore_always) ::Debug.fail(
		"false",
		"Fallback for very long string serialization activated! "
		"Are you OK? Why do you need to save such long string? Usually, this is not OK."
		"This can lead to save size blow -> players won't be happy. "
		"If you required to serialize this - please, come to IX-Ray Discord server and tell us the reason - we're very curious about that. "
		"The save remains valid and you will get this string, but consider to use localization tables for such madness.",
		DEBUG_INFO, ignore_always);
#endif
	auto NewElem = CSaveManager::GetInstance().GetSaveable<CSaveVariableStringLong>(ESaveVariableType::t_longstring);
	NewElem->SetValue(S);
	if (_currentArrayStack.empty()) {
		_variables.emplace_back(NewElem);
	}
	else {
		(_currentArrayStack.top())->AddVariable(NewElem);
	}
}

void CSaveChunk::CopySubchunks(CSaveChunk* Chunk)
{
	const auto& OtherSubchunks = Chunk->_subchunks;
	for (auto& element : OtherSubchunks)
	{
		auto VerificationFind = _subchunks.find(element.first);
		if (VerificationFind != _subchunks.end())
		{
			VERIFY(VerificationFind == _subchunks.end(), "There is already a subchunk with name", element.first.c_str());
			continue;
		}
		_subchunks[element.first] = (CSaveChunk*)element.second->MakeCopy();
	}
}

void CSaveChunk::AttachSubchunk(CSaveChunk* Chunk)
{
	VERIFY(Chunk);
	VERIFY(Chunk->_chunkName == _chunkName);
	VERIFY(_subchunks.empty());
	for (auto& element : Chunk->_subchunks)
	{
		_subchunks[element.first] = (CSaveChunk*)element.second->MakeCopy();
	}
}

void CSaveChunk::r_bool(bool& A)
{
	if (_currentArrayStack.empty()) {
		// Fallback for legacy BOOL serialization type
		if (!IVERIFY_M(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_bool, "Try to read bool in chunk [%s], got something other. Try fallback for BOOL", _chunkName.c_str()))
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s32, "Invalid variable type access in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s32, CSaveVariableS32>(_variables[_currentReadIndex++]);	
		} else
		{
			A = SSaveVariableGetter::GetValue<bool, CSaveVariableBool>(_variables[_currentReadIndex++]);
		}
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		if (!IVERIFY_M(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_bool, "Try to read bool in chunk [%s], got something other. Try fallback for BOOL", _chunkName.c_str()))
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_s32, "Invalid variable type access in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s32, CSaveVariableS32>(CurrentArray->GetCurrentElement());
		} else
		{
			A = SSaveVariableGetter::GetValue<bool, CSaveVariableBool>(CurrentArray->GetCurrentElement());
		}
		CurrentArray->Next();
	}
}

void CSaveChunk::r_float(float& A)
{
	if (_currentArrayStack.empty()) {
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_float, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<float, CSaveVariableFloat>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_float, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<float, CSaveVariableFloat>((CSaveVariableFloat*)CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
}

void CSaveChunk::r_double(double& A)
{
	if (_currentArrayStack.empty()) {
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_float)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_float, "Attempt to read float as double in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<float, CSaveVariableFloat>(_variables[_currentReadIndex++]);
			return;
		}
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_double, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<double, CSaveVariableDouble>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		if (CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_float)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_float, "Attempt to read float as double in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<float, CSaveVariableFloat>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_double, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<double, CSaveVariableDouble>((CSaveVariableDouble*)CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
}

void CSaveChunk::r_u64(u64& A)
{
	if (_currentArrayStack.empty()) {
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_u8)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_u8, "Attempt to read u8 as u64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u8, CSaveVariableU8>(_variables[_currentReadIndex++]);
			return;
		}
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_u16)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_u16, "Attempt to read u16 as u64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u16, CSaveVariableU16>(_variables[_currentReadIndex++]);
			return;
		}
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_u32)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_u32, "Attempt to read u32 as u64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u32, CSaveVariableU32>(_variables[_currentReadIndex++]);
			return;
		}
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_u64, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<u64, CSaveVariableU64>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		if (CurrentArray->GetVariableType() == ESaveVariableType::t_u8)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_u8, "Attempt to read u8 as u64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u8, CSaveVariableU8>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		if (CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_u16)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_u16, "Attempt to read u16 as u64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u16, CSaveVariableU16>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		if (CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_u32)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_u32, "Attempt to read u32 as u64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u32, CSaveVariableU32>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_u64, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<u64, CSaveVariableU64>((CSaveVariableU64*)CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
}

void CSaveChunk::r_s64(s64& A)
{
	if (_currentArrayStack.empty()) {
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s8)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_s8, "Attempt to read s8 as s64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s8, CSaveVariableS8>(_variables[_currentReadIndex++]);
			return;
		}
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s16)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_s16, "Attempt to read s16 as s64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s16, CSaveVariableS16>(_variables[_currentReadIndex++]);
			return;
		}
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s32)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_s32, "Attempt to read s32 as s64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s32, CSaveVariableS32>(_variables[_currentReadIndex++]);
			return;
		}
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s64, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<s64, CSaveVariableS64>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		if (CurrentArray->GetVariableType() == ESaveVariableType::t_s8)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_s8, "Attempt to read s8 as s64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s8, CSaveVariableS8>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		if (CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_s16)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_s16, "Attempt to read s16 as s64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s16, CSaveVariableS16>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		if (CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_s32)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_s32, "Attempt to read s32 as s64 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s32, CSaveVariableS32>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_s64, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<s64, CSaveVariableS64>((CSaveVariableS64*)CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
}

void CSaveChunk::r_u32(u32& A)
{
	if (_currentArrayStack.empty()) {
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_u8)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_u8, "Attempt to read u8 as u32 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u8, CSaveVariableU8>(_variables[_currentReadIndex++]);
			return;
		}
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_u16)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_u16, "Attempt to read u16 as u32 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u16, CSaveVariableU16>(_variables[_currentReadIndex++]);
			return;
		}
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_u32, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<u32, CSaveVariableU32>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		if (CurrentArray->GetVariableType() == ESaveVariableType::t_u8)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_u8, "Attempt to read u8 as u32 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u8, CSaveVariableU8>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		if (CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_u16)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_u16, "Attempt to read u16 as u32 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u16, CSaveVariableU16>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_u32, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<u32, CSaveVariableU32>((CSaveVariableU32*)CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
}

void CSaveChunk::r_s32(s32& A)
{
	if (_currentArrayStack.empty()) {
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s8)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_s8, "Attempt to read s8 as s32 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s8, CSaveVariableS8>(_variables[_currentReadIndex++]);
			return;
		}
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s16)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_s16, "Attempt to read s16 as s32 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s16, CSaveVariableS16>(_variables[_currentReadIndex++]);
			return;
		}
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s32, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<s32, CSaveVariableS32>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		if (CurrentArray->GetVariableType() == ESaveVariableType::t_s8)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_s8, "Attempt to read s8 as s32 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s8, CSaveVariableS8>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		if (CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_s16)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_s16, "Attempt to read s16 as s32 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s16, CSaveVariableS16>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_s32, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<s32, CSaveVariableS32>((CSaveVariableS32*)CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
}

void CSaveChunk::r_u16(u16& A)
{
	if (_currentArrayStack.empty()) {
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_u8)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_u8, "Attempt to read u8 as u16 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u8, CSaveVariableU8>(_variables[_currentReadIndex++]);
			return;
		}
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_u16, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<u16, CSaveVariableU16>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		if (CurrentArray->GetVariableType() == ESaveVariableType::t_u8)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_u8, "Attempt to read u8 as u16 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<u8, CSaveVariableU8>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_u16, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<u16, CSaveVariableU16>((CSaveVariableU16*)CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
}

void CSaveChunk::r_s16(s16& A)
{
	if (_currentArrayStack.empty()) {
		if (_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s8)
		{
			R_ASSERT(_variables[_currentReadIndex]->GetVariableType() != ESaveVariableType::t_s8, "Attempt to read s8 as s16 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s8, CSaveVariableS8>(_variables[_currentReadIndex++]);
			return;
		}
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s16, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<s16, CSaveVariableS16>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		if (CurrentArray->GetVariableType() == ESaveVariableType::t_s8)
		{
			R_ASSERT(CurrentArray->GetCurrentElement()->GetVariableType() != ESaveVariableType::t_s8, "Attempt to read s8 as s16 in chunk", _chunkName.c_str());
			A = SSaveVariableGetter::GetValue<s8, CSaveVariableS8>(CurrentArray->GetCurrentElement());
			CurrentArray->Next();
			return;
		}
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_s16, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<s16, CSaveVariableS16>((CSaveVariableS16*)CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
}

void CSaveChunk::r_u8(u8& A)
{
	if (_currentArrayStack.empty()) {
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_u8, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<u8, CSaveVariableU8>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_u8, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<u8, CSaveVariableU8>((CSaveVariableU8*)CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
}

void CSaveChunk::r_s8(s8& A)
{
	if (_currentArrayStack.empty()) {
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_s8, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<s8, CSaveVariableS8>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_s8, "Invalid variable type access in chunk", _chunkName.c_str());
		A = SSaveVariableGetter::GetValue<s8, CSaveVariableS8>((CSaveVariableS8*)CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
}

void CSaveChunk::r_string(shared_str& S)
{
	if (_currentArrayStack.empty()) {
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_string, "Invalid variable type access in chunk", _chunkName.c_str());
		S = SSaveVariableGetter::GetValue<shared_str, CSaveVariableString>(_variables[_currentReadIndex++]).c_str();
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_string, "Invalid variable type access in chunk", _chunkName.c_str());
		S = SSaveVariableGetter::GetValue<shared_str, CSaveVariableString>(CurrentArray->GetCurrentElement()).c_str();
		CurrentArray->Next();
	}
}

xr_string* CSaveChunk::r_string_long()
{
	xr_string* S;
	if (_currentArrayStack.empty()) {
		R_ASSERT3(_variables[_currentReadIndex]->GetVariableType() == ESaveVariableType::t_longstring, "Invalid variable type access in chunk", _chunkName.c_str());
		S = SSaveVariableGetter::GetValuePtr<xr_string, CSaveVariableStringLong>(_variables[_currentReadIndex++]);
	}
	else {
		auto CurrentArray = _currentArrayStack.top();
		R_ASSERT3(CurrentArray->GetCurrentElement()->GetVariableType() == ESaveVariableType::t_longstring, "Invalid variable type access in chunk", _chunkName.c_str());
		S = SSaveVariableGetter::GetValuePtr<xr_string, CSaveVariableStringLong>(CurrentArray->GetCurrentElement());
		CurrentArray->Next();
	}
#ifndef MASTER_GOLD
	static bool ignore_always = false;
	if (!ignore_always) ::Debug.fail(
		"false",
		"Fallback for very long string serialization activated! "
		"Are you OK? Why do you need to save such long string? Usually, this is not OK."
		"This can lead to save size blow -> players won't be happy. "
		"If you required to serialize this - please, come to IX-Ray Discord server and tell us the reason - we're very curious about that. "
		"The save remains valid and you will get this string, but consider to use localization tables for such madness.",
		DEBUG_INFO, ignore_always);
#endif
	return S;
}

void CSaveChunk::Parse(IReader* stream)
{
	{
		ESaveVariableType type;
		stream->r(&type, sizeof(ESaveVariableType));
		while(type == ESaveVariableType::t_chunkStart) {
			shared_str subchunk_name;
			CSaveManager::GetInstance().ConditionalReadString(stream, subchunk_name);
			auto NewChunk = CSaveManager::GetInstance().GetSaveable<CSaveChunk>(ESaveVariableType::t_chunk);
			NewChunk->SetChunkName(subchunk_name);
			NewChunk->Parse(stream);
			_subchunks[subchunk_name] = NewChunk;
			stream->r(&type, sizeof(ESaveVariableType));
		}
		ParseRec(stream, type);
	}
}

void CSaveChunk::ParseRec(IReader* stream, ESaveVariableType type_key)
{
	auto& inst = CSaveManager::GetInstance();
	ESaveVariableType type = type_key;
	while (type != ESaveVariableType::t_chunkEnd) {
		switch (type)
		{
		case ESaveVariableType::t_chunkStart: {
			if (_currentArrayStack.empty()) {
				Msg("_currentArrayStack.empty()");
			}
			//VERIFY(!_currentArrayStack.empty());
			shared_str subchunk_name;
			CSaveManager::GetInstance().ConditionalReadString(stream, subchunk_name);
			CSaveChunk* NewChunk = inst.GetSaveable<CSaveChunk>(ESaveVariableType::t_chunk);
			NewChunk->SetChunkName(subchunk_name);
			NewChunk->Parse(stream);
			VERIFY(!_currentArrayStack.empty());
			_currentArrayStack.top()->AddVariable(NewChunk);
			break;
		}
		case ESaveVariableType::t_bool: {
			bool Value;
			CSaveManager::GetInstance().ConditionalReadBool(stream, Value);
			auto Var = inst.GetSaveable<CSaveVariableBool>(ESaveVariableType::t_bool);
			Var->SetValue(Value);
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_float: {
			auto Var = inst.GetSaveable<CSaveVariableFloat>(ESaveVariableType::t_float);
			Var->SetValue(stream->r_float());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_double: {
			double Value;
			stream->r(&Value, sizeof(double));
			auto Var = inst.GetSaveable<CSaveVariableDouble>(ESaveVariableType::t_double);
			Var->SetValue(Value);
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_u64: {
			auto Var = inst.GetSaveable<CSaveVariableU64>(ESaveVariableType::t_u64);
			Var->SetValue(stream->r_u64());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_u64_op32: {
			auto Var = inst.GetSaveable<CSaveVariableU64>(ESaveVariableType::t_u64);
			Var->SetValue(stream->r_u32());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_u64_op16: {
			auto Var = inst.GetSaveable<CSaveVariableU64>(ESaveVariableType::t_u64);
			Var->SetValue(stream->r_u16());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_u64_op8: {
			auto Var = inst.GetSaveable<CSaveVariableU64>(ESaveVariableType::t_u64);
			Var->SetValue(stream->r_u8());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_s64: {
			auto Var = inst.GetSaveable<CSaveVariableS64>(ESaveVariableType::t_s64);
			Var->SetValue(stream->r_s64());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_s64_op32: {
			auto Var = inst.GetSaveable<CSaveVariableS64>(ESaveVariableType::t_s64);
			Var->SetValue(stream->r_s32());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_s64_op16: {
			auto Var = inst.GetSaveable<CSaveVariableS64>(ESaveVariableType::t_s64);
			Var->SetValue(stream->r_s16());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_s64_op8: {
			auto Var = inst.GetSaveable<CSaveVariableS64>(ESaveVariableType::t_s64);
			Var->SetValue(stream->r_s8());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_u32: {
			auto Var = inst.GetSaveable<CSaveVariableU32>(ESaveVariableType::t_u32);
			Var->SetValue(stream->r_u32());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_u32_op16: {
			auto Var = inst.GetSaveable<CSaveVariableU32>(ESaveVariableType::t_u32);
			Var->SetValue(stream->r_u16());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_u32_op8: {
			auto Var = inst.GetSaveable<CSaveVariableU32>(ESaveVariableType::t_u32);
			Var->SetValue(stream->r_u8());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_s32: {
			auto Var = inst.GetSaveable<CSaveVariableS32>(ESaveVariableType::t_s32);
			Var->SetValue(stream->r_s32());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_s32_op16: {
			auto Var = inst.GetSaveable<CSaveVariableS32>(ESaveVariableType::t_s32);
			Var->SetValue(stream->r_s16());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_s32_op8: {
			auto Var = inst.GetSaveable<CSaveVariableS32>(ESaveVariableType::t_s32);
			Var->SetValue(stream->r_s8());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_u16: {
			auto Var = inst.GetSaveable<CSaveVariableU16>(ESaveVariableType::t_u16);
			Var->SetValue(stream->r_u16());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_u16_op8: {
			auto Var = inst.GetSaveable<CSaveVariableU16>(ESaveVariableType::t_u16);
			Var->SetValue(stream->r_u8());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_s16: {
			auto Var = inst.GetSaveable<CSaveVariableS16>(ESaveVariableType::t_s16);
			Var->SetValue(stream->r_s16());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_s16_op8: {
			auto Var = inst.GetSaveable<CSaveVariableS16>(ESaveVariableType::t_s16);
			Var->SetValue(stream->r_s8());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_u8: {
			auto Var = inst.GetSaveable<CSaveVariableU8>(ESaveVariableType::t_u8);
			Var->SetValue(stream->r_u8());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_s8: {
			auto Var = inst.GetSaveable<CSaveVariableS8>(ESaveVariableType::t_s8);
			Var->SetValue(stream->r_s8());
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_string: {
			shared_str Value;
			CSaveManager::GetInstance().ConditionalReadString(stream, Value);
			auto Var = inst.GetSaveable<CSaveVariableString>(ESaveVariableType::t_string);
			Var->SetValue(Value);
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_longstring: {
			xr_string Str;
			stream->r_stringZ(Str);
			auto Var = inst.GetSaveable<CSaveVariableStringLong>(ESaveVariableType::t_longstring);
			Var->SetValue(Str);
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			break;
		}
		case ESaveVariableType::t_arrayUnspec: {
			auto Var = inst.GetSaveable<CSaveVariableArray>(ESaveVariableType::t_arrayUnspec);
			if (_currentArrayStack.empty()) {
				_variables.emplace_back(Var);
			}
			else {
				_currentArrayStack.top()->AddVariable(Var);
			}
			_currentArrayStack.push(Var);
			stream->r(&type, sizeof(ESaveVariableType));
			while (type != ESaveVariableType::t_arrayUnspecEnd) {
				ParseRec(stream, type);
				stream->r(&type, sizeof(ESaveVariableType));
			}
			_currentArrayStack.pop();
			break;
		}
		default: {
			FATAL("Invalid save chunk type!");
		}
		}
		{
			auto Pos = stream->tell();
			stream->r(&type, sizeof(ESaveVariableType));
			if (type == ESaveVariableType::t_arrayUnspecEnd) {
				stream->seek(Pos);
				return;
			}
		}
	}
}

ISaveable* CSaveChunk::MakeCopy()
{
	auto& inst = CSaveManager::GetInstance();
	CSaveChunk* Copy = inst.GetSaveable<CSaveChunk>(ESaveVariableType::t_chunk);
	Copy->SetChunkName(_chunkName);
	for (auto& Var : _variables)
	{
		Copy->_variables.emplace_back(Var->MakeCopy());
	}
	for (auto& SubChunk : _subchunks)
	{
		Copy->_subchunks[SubChunk.first] = (CSaveChunk*)(SubChunk.second->MakeCopy());
	}
	return Copy;
}
