#pragma once

struct Shader_xrLC
{
public:
	enum {
		flCollision			= 1<<0,
		flRendering			= 1<<1,
		flOptimizeUV		= 1<<2,
		flLIGHT_Vertex		= 1<<3,
		flLIGHT_CastShadow	= 1<<4,
		flLIGHT_Sharp		= 1<<5,
	};
	struct Flags {
		u32 bCollision				: 1;
		u32 bRendering				: 1;
		u32 bOptimizeUV				: 1;
		u32 bLIGHT_Vertex			: 1;
		u32 bLIGHT_CastShadow		: 1;
		u32 bLIGHT_Sharp			: 1;
	};
public:
	char		Name		[128];
	union{
		Flags32	m_Flags;
        Flags	flags;
    };
	float		vert_translucency;
	float		vert_ambient;
	float		lm_density;

	Shader_xrLC()	{
		xr_strcpy					(Name,"unknown");
		m_Flags.assign			(0);
		flags.bCollision		= true;
		flags.bRendering		= true;
		flags.bOptimizeUV		= true;
		flags.bLIGHT_Vertex		= false;
		flags.bLIGHT_CastShadow = true;
		flags.bLIGHT_Sharp		= true;
		vert_translucency		= .5f;
		vert_ambient			= .0f;
		lm_density				= 1.f;
	}
};

using Shader_xrLCVec = xr_vector<Shader_xrLC>;
using Shader_xrLCIt = Shader_xrLCVec::iterator;

class Shader_xrLC_LIB
{
	xr_vector<Shader_xrLC> library;
	xr_hash_map<shared_str, u32> NameToIndex;
	
	void Rehash()
	{
		NameToIndex.clear();
		for (u32 i = 0; i < library.size(); ++i)
		{
			NameToIndex[library[i].Name] = i;
		}
	}
	
public:
	void Load(str_c name)
	{
		auto fs = FS.rg_open(name);
		if(!fs){
			string256 inf;
			xr_sprintf (inf,sizeof(inf),"Build failed!\nCan't load shaders library: '%s'",name);
			FATAL(inf);
			return;
		};

		int count = fs->length()/sizeof(Shader_xrLC);
		R_ASSERT(int(fs->length()) == int(count*sizeof(Shader_xrLC)));
		library.resize(count);
		fs->r(library.data(),fs->length());
		Rehash();
	}
	bool Save(const char* name)
	{
		auto F = FS.wg_open(name);
		if (!F) {
			return false;
		}
		F->w(library.data(),(u32)library.size()*sizeof(Shader_xrLC));
		return true;
	}
	
	void Unload()
	{
		library.clear();
		NameToIndex.clear();
	}
	
	u32 GetID(shared_str name) const
	{
		auto it = NameToIndex.find(name);
		if (it == NameToIndex.end())
		{
			return u32(-1);
		}
		return it->second;
	}
	Shader_xrLC* Get(const char* name)
	{
		auto ID = GetID(name);
		if (ID == u32(-1))
		{
			return nullptr;
		}
		return &library[ID];
	}
	Shader_xrLC& Get(int id)
	{
		return library[id];
	}
	Shader_xrLC* Append(Shader_xrLC* parent=nullptr)
	{
		auto& elem = library.emplace_back(parent?Shader_xrLC(*parent):Shader_xrLC());
		Rehash();
		return &elem;
	}
	void Remove(const char* name)
	{
		auto ID = GetID(name);
		if (ID == u32(-1))
		{
			return;
		}
		auto it = library.begin() + GetID(name);
		VERIFY(it < library.end());
		library.erase(it);
		Rehash();
	}
	void Remove(int id)
	{
		library.erase(library.begin()+id);
		Rehash();
	}
	auto& Library(){return library;}
	const auto& Library	() const {return library;}
};