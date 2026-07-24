#pragma once

#include "TiramisuRenderTypes.h"
// DXC include handler, разрешающий shader includes через engine filesystem.
struct DXCInluder : public IDxcIncludeHandler
{
	DXCInluder(IDxcLibrary* pLibrary)
	{
		DxcLibrary = pLibrary;
	}
	HRESULT STDMETHODCALLTYPE QueryInterface(REFIID iid, void** ppvObject) override
	{
		return E_FAIL;
	}
	virtual ULONG STDMETHODCALLTYPE AddRef(void)
	{
		return 1;
	}

	virtual ULONG STDMETHODCALLTYPE Release(void)
	{
		return 1;
	}
	IC bool ReadFile(const char* Name, xr_vector<char>& Data)
	{
		FILE* File = fopen(Name, "rb");
		if (!File)
		{
			return false;
		}
		fseek(File, 0, SEEK_END);
		size_t Size = _ftelli64(File);
		fseek(File, 0, SEEK_SET);
		Data.resize(Size);
		fread(Data.data(), 1, Size, File);
		fclose(File);
		return true;
	}

	xr_vector<IReader*> Readers;
	xr_vector<IDxcBlobEncoding*> BlobEncodings;

	DXCInluder() {}
	~DXCInluder()
	{
		for (IReader* i : Readers)
		{
			FS.r_close(i);
		}
	}

	virtual HRESULT STDMETHODCALLTYPE LoadSource(LPCWSTR pFilename, IDxcBlob** ppIncludeSource)
	{
		string_path Name;
		if (wcsncmp(pFilename, L".---", 4) == 0)
		{
			pFilename += 4;
		}
		if (wcsncmp(pFilename, L".-", 2) == 0)
		{
			pFilename += 2;
		}
		if (wcsncmp(pFilename, L"./", 2) == 0)
		{
			pFilename += 2;
		}
		{
			u32 i = 0;
			for (; pFilename[i]; i++)
			{
				Name[i] = static_cast<char>(pFilename[i]);
			}
			Name[i] = 0;
		}
		for (size_t i = 0; Name[i]; i++)
		{
			if (Name[i] == '/')
			{
				Name[i] = '\\';
			}
		}
		xr_strlwr(Name);
		string_path FileName;
		xr_strcpy(FileName, Name);
		bool IsResolved = FS.exist(FileName) != nullptr;

		auto TryGameShader = [&FileName](const char* RelativeName)
		{
			return FS.exist(FileName, "$game_shaders$", RelativeName) !=
				   nullptr;
		};

		// DXC может передать include как исходное имя либо как путь,
		// уже объединённый с каталогом source-файла. Проверяем обе формы.
		const char* R5Path = strstr(Name, "r5\\");
		if (!IsResolved && R5Path)
		{
			IsResolved = TryGameShader(R5Path);
		}

		const char* RequestedName = Name;
		const char* GlobalPath = strstr(Name, "\\global\\");
		if (GlobalPath)
		{
			RequestedName = GlobalPath + xr_strlen("\\global\\");
		}

		string_path RelativeName;
		if (!IsResolved)
		{
			xr_strconcat(RelativeName, "r5\\", RequestedName);
			IsResolved = TryGameShader(RelativeName);
		}
		if (!IsResolved)
		{
			xr_strconcat(RelativeName, "r5\\common\\", RequestedName);
			IsResolved = TryGameShader(RelativeName);
		}
		if (!IsResolved)
		{
			const char* BaseName = strrchr(Name, '\\');
			BaseName = BaseName ? BaseName + 1 : Name;
			xr_strconcat(RelativeName, "r5\\common\\", BaseName);
			IsResolved = TryGameShader(RelativeName);
		}
		if (!IsResolved)
		{
			return E_FAIL;
		}
		xr_vector<char> Data;
		if (!ReadFile(FileName, Data))
		{
			return E_FAIL;
		}
		IDxcBlobEncoding* PointerTextBlob = nullptr;
		bool bIsUTF8 = false;

		if (Data.size())
		{
			bIsUTF8 = Data[0] == 0xEF;
			bIsUTF8 = bIsUTF8 && Data[1] == 0xBB;
			bIsUTF8 = bIsUTF8 && Data[2] == 0xBF;
		}

		for (char& ch : Data)
		{
			if (ch > 0x80)
			{
				ch = '?';
			}
		}

		DX_CHK(DxcLibrary->CreateBlobWithEncodingOnHeapCopy(Data.data(), static_cast<UINT32>(Data.size()), bIsUTF8 ? DXC_CP_UTF8 : DXC_CP_ACP, &PointerTextBlob));
		*ppIncludeSource = static_cast<IDxcBlob*>(PointerTextBlob);

		BlobEncodings.push_back(PointerTextBlob);
		return S_OK;
	}
	IDxcLibrary* DxcLibrary;
};
