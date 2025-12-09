#pragma once

struct	RHIShaderConstant;
struct	RHIShaderConstant::Loader;

class dx10ConstantBuffer :
	public xr_resource_named
{
public:
	dx10ConstantBuffer(ID3DShaderReflectionConstantBuffer* pTable);
	~dx10ConstantBuffer();

	bool			Similar(dx10ConstantBuffer &_in);
	IRHIBuffer*		GetBuffer() { return m_pBuffer; }

	void			Flush();

	//	Set copy data into constant buffer
	//	Plain buffer member
	void			set(RHIShaderConstant* C, RHIShaderConstant::Loader& L, const Fmatrix& A);
	void			set(RHIShaderConstant* C, RHIShaderConstant::Loader& L, const Fvector4& A);
	void			set(RHIShaderConstant* C, RHIShaderConstant::Loader& L, float A);
	void			set(RHIShaderConstant* C, RHIShaderConstant::Loader& L, int A);
	//	Array buffer member
	void			seta(RHIShaderConstant* C, RHIShaderConstant::Loader& L, u32 e, const Fmatrix& A);
	void			seta(RHIShaderConstant* C, RHIShaderConstant::Loader& L, u32 e, const Fvector4& A);

	void*			AccessDirect(RHIShaderConstant::Loader& L, u32 DataSize);

private:
	Fvector4*		Access(u16	offset);

private:
	shared_str							m_strBufferName;
	D3D_CBUFFER_TYPE					m_eBufferType;

	//	Buffer data description
	u32									m_uiMembersCRC;
	xr_vector<D3D_SHADER_TYPE_DESC>	m_MembersList;
	xr_vector<shared_str>				m_MembersNames;

	IRHIBuffer*							m_pBuffer;
	u32									m_uiBufferSize;	//	Cache buffer size for debug validation
	void*								m_pBufferData;
	bool								m_bChanged;

	static const u32					lineSize = sizeof(Fvector4);

	//	Never try to copy objects of this class due to the pointer and autoptr members
	dx10ConstantBuffer( const dx10ConstantBuffer&);
	dx10ConstantBuffer& operator=(dx10ConstantBuffer&);
};

typedef	resptr_core<dx10ConstantBuffer,resptr_base<dx10ConstantBuffer> > ref_cbuffer;