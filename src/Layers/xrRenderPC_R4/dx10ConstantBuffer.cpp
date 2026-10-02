#include "stdafx.h"
#include "dx10ConstantBuffer.h"

#include "dx10BufferUtils.h"
#include "dx10FixedConstants.h"
#include "dxRenderDeviceRender.h"
#include "Utils/dxHashHelper.h"

dx10ConstantBuffer::~dx10ConstantBuffer()
{
	DEV->_DeleteConstantBuffer(this);
	_RELEASE(m_pBuffer);
	xr_free(m_pBufferData);
}

dx10ConstantBuffer::dx10ConstantBuffer(const RHIShaderBufferDesc* pTable)
	: m_bChanged(true)
{
	const auto& Desc = *pTable;
	m_strBufferName = Desc.Name;
	m_eBufferType = Desc.Type;
	m_uiBufferSize = Desc.Size;
	m_bFixed = FixedConstants::IsFixedName(Desc.Name.c_str());
	m_MembersList.resize(Desc.Variables.size());
	m_MembersNames.resize(Desc.Variables.size());
	dxHashHelper hash;
	for (u32 member_idx = 0; member_idx < Desc.Variables.size(); ++member_idx) {
		const auto& type = Desc.Variables[member_idx].Type;
		m_MembersList[member_idx] = type;
		m_MembersNames[member_idx] = Desc.Variables[member_idx].Name;
		u32 type_desc[] = { u32(type.Class), u32(type.Type), type.Rows, type.Columns, type.Elements, type.Members, type.Offset };
		hash.AddData(type_desc, sizeof(type_desc));
		const char* type_name = type.Name.c_str();
		hash.AddData(&type_name, sizeof(type_name));
	}
	m_uiMembersCRC = hash.GetHash();

	R_CHK(RHIUtils::CreateConstantBuffer(&m_pBuffer, Desc.Size));
	VERIFY(m_pBuffer);
	m_pBufferData = xr_malloc(Desc.Size);
	VERIFY(m_pBufferData);
}

bool dx10ConstantBuffer::Similar(dx10ConstantBuffer &_in)
{
	if ( m_strBufferName._get() != _in.m_strBufferName._get() )
		return false;

	if ( m_eBufferType != _in.m_eBufferType )
		return false;

	if ( m_uiMembersCRC != _in.m_uiMembersCRC )
		return false;

	if ( m_MembersList.size() != _in.m_MembersList.size() )
		return false;

	if (!std::equal(m_MembersList.begin(), m_MembersList.end(), _in.m_MembersList.begin()))
		return false;

	VERIFY(m_MembersNames.size() == _in.m_MembersNames.size());

	int iMemberNum = (int)m_MembersNames.size();
	for ( int i=0; i<iMemberNum; ++i)
	{
		if (m_MembersNames[i].c_str()!=_in.m_MembersNames[i].c_str())
			return false;
	}

	return true;
}

void dx10ConstantBuffer::Flush()
{
	if (m_bChanged)
	{
		if (!m_pBuffer)
			return;

		RHIMappedSubresource pSubRes = {};
		R_ASSERT(m_pBuffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &pSubRes));

		void* dst = pSubRes.pData;
		void* src = m_pBufferData;

		VERIFY(dst);
		VERIFY(src);

		u32 buff_size = m_uiBufferSize >> 4u; // m_uiBufferSize / sizeof(Fvector4)
#ifndef IXR_CLANG_BUILD
		if (CPU::ID().hasFeature(CPUFeature::AVX))
		{
			for (u32 i = 0u; i < (buff_size>>1u); ++i) // buff_size / 2
				((__m256*)dst)[i] = ((__m256*)src)[i];
			if (buff_size&1u)
				((__m128*)dst)[buff_size-1u] = ((__m128*)src)[buff_size-1u];
		}
		else if (CPU::ID().hasFeature(CPUFeature::SSE))
		{
			for (u32 i = 0u; i < buff_size; ++i)
				((__m128*)dst)[i] = ((__m128*)src)[i];
		}
		else
#endif
		{
			for (u32 i = 0u; i < buff_size; ++i)
				((Fvector4*)dst)[i] = ((Fvector4*)src)[i];
		}

		m_pBuffer->Unmap();
		m_bChanged = false;
	}
}