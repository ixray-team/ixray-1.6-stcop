#pragma once

class TShaderDefinesContainer
{
	friend class ShaderCompiler;
	friend class ShaderContainer;
	friend class XMaterialShaderManager;

	static constexpr u32 CRC32_INITIAL = 0xFFFFFFFF;

public:
									TShaderDefinesContainer			();
									~TShaderDefinesContainer		() = default;
									
									TShaderDefinesContainer			(const TShaderDefinesContainer& Right);
									TShaderDefinesContainer			(TShaderDefinesContainer&& Right);

	void							Copy							(const TShaderDefinesContainer&Right);
	void							Swap							(TShaderDefinesContainer& Right);
	
	TShaderDefinesContainer&		operator=						(TShaderDefinesContainer&& Right);
	TShaderDefinesContainer&		operator=						(const TShaderDefinesContainer& Right);
	
	void							Add								(shared_str define);
	void							UpdateCRC32						();
	
	bool							operator<						(const TShaderDefinesContainer& Right) const;
	bool							operator==						(const TShaderDefinesContainer& Right) const;
	bool							operator!=						(const TShaderDefinesContainer& Right) const;
	IC const xr_vector<shared_str>&	GetDefines						() const { return Defines; };
	IC u32							GetCRC32						() const { return CRC32; };
	
public:
	xr_vector<shared_str>			Defines;
	u32								CRC32;
};


IC IWriter& operator<<(IWriter& Writer, const TShaderDefinesContainer& Container)
{
	Writer.w_u32(static_cast<u32>(Container.Defines.size()));

	for (const shared_str& Define : Container.Defines)
	{
		Writer.w_stringZ(Define);
	}

	return Writer;
}

IC IReader& operator>>(IReader& Reader, TShaderDefinesContainer& Container)
{
	u32 Size = Reader.r_u32();
	Container.Defines.reserve(Size);

	for (u32 i = 0; i < Size; ++i)
	{
		shared_str Name;
		Reader.r_stringZ(Name);
		Container.Defines.push_back(std::move(Name));
	}

	Container.UpdateCRC32();
	return Reader;
}
