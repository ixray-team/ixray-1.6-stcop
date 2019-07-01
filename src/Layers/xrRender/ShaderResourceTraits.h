#pragma once

#ifdef USE_DX11

#	include "ResourceManager.h"

	template<typename T>
	struct ShaderTypeTraits;

	template<>
	struct ShaderTypeTraits<SHS>
	{
		typedef CResourceManager::map_HS	MapType;
		typedef ID3D11HullShader DXIface;

		static const char* GetShaderExt() {return ".hs";}
		static const char* GetCompilationTarget() {return "hs_5_0";}
		static DXIface* CreateHWShader(void* buffer, size_t size)
		{
			DXIface* hs = 0;
			R_CHK(HW.pDevice->CreateHullShader(buffer, size, NULL, &hs));
			return hs;
		}

		static u32 GetShaderDest() {return RC_dest_hull;}
	};

	template<>
	struct ShaderTypeTraits<SDS>
	{
		typedef CResourceManager::map_DS	MapType;
		typedef ID3D11DomainShader			DXIface;

		static const char* GetShaderExt() {return ".ds";}
		static const char* GetCompilationTarget() {return "ds_5_0";}
		static DXIface* CreateHWShader(void* buffer, size_t size)
		{
			DXIface* hs = 0;
			R_CHK(HW.pDevice->CreateDomainShader(buffer, size, NULL, &hs));
			return hs;
		}

		static u32 GetShaderDest() {return RC_dest_domain;}
	};

	template<>
	struct ShaderTypeTraits<SCS>
	{
		typedef CResourceManager::map_CS	MapType;
		typedef ID3D11ComputeShader			DXIface;

		static const char* GetShaderExt() {return ".cs";}
		static const char* GetCompilationTarget() {return "cs_5_0";}
		static DXIface* CreateHWShader(void* buffer, size_t size)
		{
			DXIface* cs = 0;
			R_CHK(HW.pDevice->CreateComputeShader(buffer, size, NULL, &cs));
			return cs;
		}

		static u32 GetShaderDest() {return RC_dest_compute;}
	};

	template<>
	CResourceManager::map_DS& CResourceManager::GetShaderMap(){return m_ds;}

	template<>
	CResourceManager::map_HS& CResourceManager::GetShaderMap(){return m_hs;}

	template<>
	CResourceManager::map_CS& CResourceManager::GetShaderMap(){return m_cs;}

    template<typename T>
	T* CResourceManager::CreateShader(const char* name)
	{
		ShaderTypeTraits<T>::MapType& sh_map = GetShaderMap<ShaderTypeTraits<T>::MapType>();
		LPSTR	N = LPSTR(name);
		ShaderTypeTraits<T>::MapType::iterator	I = sh_map.find(N);

		if (I!=sh_map.end())
			return		I->second;
		else
		{
			T*		sh = xr_new<T>();

			sh->dwFlags |= xr_resource_flagged::RF_REGISTERED;
			sh_map.insert(mk_pair(sh->set_name(name),sh));
			if (0==stricmp(name,"null"))
			{
				sh->sh				= NULL;
				return sh;
			}

			string_path					shName;
			const char*	pchr = strchr(name, '(');
			ptrdiff_t	strSize = pchr?pchr-name:xr_strlen(name);
			strncpy(shName, name, strSize);
			shName[strSize] = 0;

			// Open file
			includer					Includer;
			string_path					cname;
			strconcat					(sizeof(cname), cname,::Render->getShaderPath(),/*name*/shName, ShaderTypeTraits<T>::GetShaderExt());
			FS.update_path				(cname,	"$game_shaders$", cname);

			// duplicate and zero-terminate
			IReader*		R		= FS.r_open(cname);
			if (!R)
			{
				FATAL("Can't find shader source file");
			}

			R_ASSERT2				(R,cname);
			u32				size	= R->length();
			char*			data	= xr_alloc<char>(size + 1);
			CopyMemory			(data,R->pointer(),size);
			data[size]				= 0;
			FS.r_close				(R);

			// Select target
			LPCSTR						c_target	= ShaderTypeTraits<T>::GetCompilationTarget();
			LPCSTR						c_entry		= "main";

			// Compile
			ID3DBlob*					pShaderBuf	= NULL;
			ID3DBlob*					pErrorBuf	= NULL;
			HRESULT						_hr			= S_OK;
			_hr = ::Render->shader_compile(name,data,size, NULL, &Includer, c_entry, c_target, D3D10_SHADER_PACK_MATRIX_ROW_MAJOR, &pShaderBuf, &pErrorBuf, NULL);
			xr_free						(data);

			if (SUCCEEDED(_hr))
			{
				if (pShaderBuf)
				{
					sh->sh = ShaderTypeTraits<T>::CreateHWShader(pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize());

					ID3DShaderReflection *pReflection = 0;

					_hr = D3DReflect( pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize(), IID_ID3DShaderReflection, (void**)&pReflection);
					if (SUCCEEDED(_hr) && pReflection)
					{
						// Parse constant table data
						sh->constants.parse(pReflection, ShaderTypeTraits<T>::GetShaderDest());

						_RELEASE(pReflection);
					}
					else
					{
						Msg("! D3DReflectShader %s hr == 0x%08x", cname, _hr);
					}

					_RELEASE(pShaderBuf);
				}
				else
				{
					Msg	("! %s pShaderBuf == NULL", cname);
					_hr = E_FAIL;
				}
			}
			else
			{
				if(pErrorBuf)
					Msg("Can't compile %s%s", cname, (LPCSTR)pErrorBuf->GetBufferPointer());
				else
					Msg("Can't compile %s hr=0x%08x", cname, _hr);
			}

			_RELEASE		(pShaderBuf);
			_RELEASE		(pErrorBuf);

			CHECK_OR_EXIT(
				!FAILED(_hr),
				make_string("Your video card doesn't meet game requirements.\n\nTry to lower game settings.")
			);

			return			sh;
		}
	}

	template<typename T>
	void CResourceManager::DestroyShader(const T* sh)
	{
		ShaderTypeTraits<T>::MapType& sh_map = GetShaderMap<ShaderTypeTraits<T>::MapType>();

		if (0==(sh->dwFlags&xr_resource_flagged::RF_REGISTERED))
			return;

		LPSTR N = LPSTR(*sh->cName);
		typename ShaderTypeTraits<T>::MapType::iterator I = sh_map.find(N);
		
		if (I!=sh_map.end())
		{
			sh_map.erase(I);
			return;
		}
		Msg	("! ERROR: Failed to find compiled geometry shader '%s'", *sh->cName);
	}

#endif