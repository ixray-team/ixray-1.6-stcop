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

		static inline const char* GetShaderExt() {return ".hs.hlsl";}
		static inline const char* GetCompilationTarget() {return "hs_5_0";}
		static inline DXIface* CreateHWShader(DWORD const* buffer, size_t size)
		{
			DXIface* hs = 0;
			R_CHK(RDevice->CreateHullShader(buffer, size, NULL, &hs));
			return hs;
		}

		static inline u32 GetShaderDest() {return RC_dest_hull;}
	};

	template<>
	struct ShaderTypeTraits<SDS>
	{
		typedef CResourceManager::map_DS	MapType;
		typedef ID3D11DomainShader			DXIface;

		static inline const char* GetShaderExt() {return ".ds.hlsl";}
		static inline const char* GetCompilationTarget() {return "ds_5_0";}
		static inline DXIface* CreateHWShader(DWORD const* buffer, size_t size)
		{
			DXIface* hs = 0;
			R_CHK(RDevice->CreateDomainShader(buffer, size, NULL, &hs));
			return hs;
		}

		static inline u32 GetShaderDest() {return RC_dest_domain;}
	};

	template<>
	struct ShaderTypeTraits<SCS>
	{
		typedef CResourceManager::map_CS	MapType;
		typedef ID3D11ComputeShader			DXIface;

		static inline const char* GetShaderExt() {return ".cs.hlsl";}
		static inline const char* GetCompilationTarget() {return "cs_5_0";}
		static inline DXIface* CreateHWShader(DWORD const* buffer, size_t size)
		{
			DXIface* cs = 0;
			R_CHK(RDevice->CreateComputeShader(buffer, size, NULL, &cs));
			return cs;
		}

		static inline u32 GetShaderDest() {return RC_dest_compute;}
	};

	template<>
	inline CResourceManager::map_DS& CResourceManager::GetShaderMap(){return m_ds;}

	template<>
	inline CResourceManager::map_HS& CResourceManager::GetShaderMap(){return m_hs;}

	template<>
	inline CResourceManager::map_CS& CResourceManager::GetShaderMap(){return m_cs;}

    template<typename T>
	inline T* CResourceManager::CreateShader(const char* _name)
	{
		xrCriticalSectionGuard guard(creationGuard);
		xr_string res_name = _name + RImplementation.getShaderParams();
		LPCSTR name = res_name.c_str();
		LPSTR N = LPSTR(name);

		typename ShaderTypeTraits<T>::MapType& sh_map = GetShaderMap<typename ShaderTypeTraits<T>::MapType>();
		typename ShaderTypeTraits<T>::MapType::iterator	I = sh_map.find(N);

		if (I!=sh_map.end())
			return		I->second;
		else
		{
			T*		sh = new T();

			sh->dwFlags |= xr_resource_flagged::RF_REGISTERED;
			sh_map.insert(std::make_pair(sh->set_name(name),sh));
			if (0==_stricmp(_name,"null"))
			{
				sh->sh				= NULL;
				return sh;
			}

			// Open file
			string_path					cname;
			xr_strconcat(cname,::Render->getShaderPath(), _name, ShaderTypeTraits<T>::GetShaderExt());
			FS.update_path				(cname,	"$game_shaders$", cname);

			// duplicate and zero-terminate
			IReader* file				= FS.r_open(cname);
			R_ASSERT2					( file, cname );

			// Select target
			LPCSTR						c_target	= ShaderTypeTraits<T>::GetCompilationTarget();
			LPCSTR						c_entry		= "main";

			// Compile
			HRESULT	const _hr = ::Render->shader_compile(name, (DWORD const*)file->pointer(), file->length(), c_entry, c_target, D3DCOMPILE_PACK_MATRIX_ROW_MAJOR, (void*&)sh);

			R_ASSERT3(SUCCEEDED(_hr), "Can't compile shader", name);

			FS.r_close					( file );

			return			sh;
		}
	}

	template<typename T>
	inline void CResourceManager::DestroyShader(const T* sh)
	{
		xrCriticalSectionGuard guard(creationGuard);
		typename ShaderTypeTraits<T>::MapType& sh_map = GetShaderMap<typename ShaderTypeTraits<T>::MapType>();

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