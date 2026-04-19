#pragma once

#include <charconv>
#include "XR_IOConsole.h"

#define CMD0(cls)					{ static cls x##cls();				Console->AddCommand(&x##cls);}
#define CMD1(cls,p1)				{ static cls x##cls(p1);			Console->AddCommand(&x##cls);}
#define CMD2(cls,p1,p2)				{ static cls x##cls(p1,p2);			Console->AddCommand(&x##cls);}
#define CMD3(cls,p1,p2,p3)			{ static cls x##cls(p1,p2,p3);		Console->AddCommand(&x##cls);}
#define CMD4(cls,p1,p2,p3,p4)		{ static cls x##cls(p1,p2,p3,p4);	Console->AddCommand(&x##cls);}

class ENGINE_API CCC_Mask64;
class ENGINE_API CCC_Mask32;
class ENGINE_API CCC_Mask16;
class ENGINE_API CCC_ToggleMask;
class ENGINE_API CCC_Token;
class ENGINE_API CCC_Float;
class ENGINE_API CCC_Vector3;
class ENGINE_API CCC_Integer;
class ENGINE_API CCC_Boolean;
class ENGINE_API CCC_String;

class ENGINE_API IConsole_Command
{
public:
	friend class	CConsole;
	using TInfo		= char[512];
	using TStatus	= char[256];
	using vecTips	= xr_vector<shared_str>;
	using vecLRU	= xr_vector<shared_str>;

protected	:
	const char*			cName;
	bool			bEnabled;
	bool			bLowerCaseArgs;
	bool			bEmptyArgsHandled;
	
	vecLRU			m_LRU;

	enum {
		LRU_MAX_COUNT = 10
	};

	IC	bool		EQ(const char* S1, const char* S2) { return xr_strcmp(S1,S2)==0; }
public		:
	IConsole_Command		(const char* N) : 
	  cName				(N),
	  bEnabled			(true),
	  bLowerCaseArgs	(true),
	  bEmptyArgsHandled	(false) {
		  m_LRU.reserve(LRU_MAX_COUNT + 1);
		  m_LRU.clear();
	  }
	virtual ~IConsole_Command()
	{
		if(Console)
			Console->RemoveCommand(this);
	};

	const char*			Name()			{ return cName;	}
	void			InvalidSyntax() {
		TInfo I; Info(I);
		Msg("~ Invalid syntax in call to '%s'",cName);
		Msg("~ Valid arguments: %s", I);
	}
	virtual void	Execute	(const char* args)	= 0;
	virtual void	Status	(TStatus& S)	{ S[0]=0; }
	virtual void	Info	(TInfo& I)		{ xr_strcpy(I,"(no arguments)"); }
	virtual void	Save	(IWriter *F)	{
		TStatus		S = {};	Status(S);
		if (S[0])	F->w_printf("%s %s\r\n",cName,S); 
	}

	virtual void fill_tips(vecTips& tips, u32 mode) {
		add_LRU_to_tips( tips );
	}

	virtual void	add_to_LRU		(shared_str const& arg);
			void	add_LRU_to_tips	(vecTips& tips);

	virtual IConsole_Command* dcast_icommand(){return this;}
	virtual CCC_Mask64* dcast_mask64() { return nullptr; }
	virtual CCC_Mask32* dcast_mask32() { return nullptr; }
	virtual CCC_Mask16* dcast_mask16() { return nullptr; }
	virtual CCC_ToggleMask* dcast_tmask() { return nullptr; }
	virtual CCC_Token* dcast_token() { return nullptr; }
	virtual CCC_Float* dcast_float() { return nullptr; }
	virtual CCC_Vector3* dcast_vector() { return nullptr; }
	virtual CCC_Integer* dcast_int() { return nullptr; }
	virtual CCC_Boolean* dcast_bool() { return nullptr; }
	virtual CCC_String* dcast_string() { return nullptr; }
}; // class IConsole_Command

class ENGINE_API CCC_Mask64 : public IConsole_Command
{
protected :
	Flags64* value;
	u64 mask;

public :
	CCC_Mask64(const char* N, Flags64* V, u64 M) :
		IConsole_Command(N),
		value(V),
		mask(M)
	{
	}

	bool GetValue() const
	{
		return value->test(mask);
	}

	void SetValue(u64 mask, bool state)
	{
		value->set(mask, state);
	}

	void Toggle()
	{
		value->invert(mask);
	}

	void Execute(const char* args) override
	{
		if (EQ(args, "on") || EQ(args, "1"))
			value->set(mask,true);
		else if (EQ(args, "off") || EQ(args, "0"))
			value->set(mask,false);
		else InvalidSyntax();
	}

	void Status(TStatus& S) override
	{
		xr_strcpy(S, value->test(mask) ? "on" : "off");
	}

	void Info(TInfo& I) override
	{
		xr_strcpy(I, "'on/off' or '1/0'");
	}

	void fill_tips(vecTips& tips, u32 mode) override
	{
		TStatus str;

		xr_sprintf(str, sizeof(str), "%s (current) [on/off]", value->test(mask) ? "on" : "off");
		tips.emplace_back(str);

		IConsole_Command::fill_tips(tips, mode);
	}

	static Flags64& FastCommand(const char* command_name, Flags64 default_value = {}, u64 mask = 0)
	{
		auto it = Console->Commands.find(command_name);

		if (it == Console->Commands.end())
		{
			auto new_cmd = new CCC_Mask64(command_name, new Flags64, mask);
			Console->AddCommand(new_cmd);
			*new_cmd->value = default_value;
			return *new_cmd->value;
		}
		return *static_cast<CCC_Mask64*>(it->second)->value;
	}

	CCC_Mask64* dcast_mask64() override { return this; }
};

class ENGINE_API CCC_Mask32 : public IConsole_Command
{
protected :
	Flags32* value;
	u32 mask;

public :
	CCC_Mask32(const char* N, Flags32* V, u32 M) :
		IConsole_Command(N),
		value(V),
		mask(M)
	{
	}

	bool GetValue() const
	{
		return value->test(mask);
	}

	void SetValue(u32 mask, bool state)
	{
		value->set(mask, state);
	}

	void Toggle()
	{
		value->invert(mask);
	}

	virtual void Execute(const char* args)
	{
		if (EQ(args, "on") || EQ(args, "1"))
			value->set(mask,true);
		else if (EQ(args, "off") || EQ(args, "0"))
			value->set(mask,false);
		else InvalidSyntax();
	}

	virtual void Status(TStatus& S)
	{
		xr_strcpy(S, value->test(mask) ? "on" : "off");
	}

	virtual void Info(TInfo& I)
	{
		xr_strcpy(I, "'on/off' or '1/0'");
	}

	virtual void fill_tips(vecTips& tips, u32 mode)
	{
		TStatus str;

		xr_sprintf(str, sizeof(str), "%s (current) [on/off]", value->test(mask) ? "on" : "off");
		tips.emplace_back(str);

		IConsole_Command::fill_tips(tips, mode);
	}

	static Flags32& FastCommand(const char* command_name, Flags32 default_value = {0}, u32 mask = 0)
	{
		auto it = Console->Commands.find(command_name);

		if (it == Console->Commands.end())
		{
			CCC_Mask32* new_cmd = new CCC_Mask32(command_name, new Flags32, mask);
			Console->AddCommand(new_cmd);
			*new_cmd->value = default_value;
			return *(new_cmd)->value;
		}
		return *static_cast<CCC_Mask32*>((*it).second)->value;
	}

	virtual CCC_Mask32* dcast_mask32() { return this; }
};

class ENGINE_API CCC_Mask16 : 
	public IConsole_Command
{
protected	:
	Flags16*	value;
	u16			mask;
public		:
	CCC_Mask16(const char* N, Flags16* V, u32 M) :
	  IConsole_Command(N),
	  value(V),
	  mask(M)
	{};

	bool GetValue() const { return value->test(mask); }

	void SetValue(u16 mask, bool state)
	{
		value->set(mask, state);
	}

	void Toggle()
	{
		value->invert(mask);
	}

	virtual void Execute	(const char* args)
	{
		if (EQ(args, "on") || EQ(args, "1"))
			value->set(mask,true);
		else if (EQ(args, "off") || EQ(args, "0"))
			value->set(mask,false);
		else InvalidSyntax();
	}
	virtual void	Status	(TStatus& S)
	{	xr_strcpy(S,value->test(mask)?"on":"off"); }
	virtual void	Info	(TInfo& I)
	{	xr_strcpy(I,"'on/off' or '1/0'"); }

	virtual void fill_tips(vecTips& tips, u32 mode)
	{
		TStatus str;
		
		xr_sprintf(str, sizeof(str), "%s (current) [on, off]", value->test(mask) ? "on" : "off");
		tips.emplace_back(str);

		IConsole_Command::fill_tips(tips, mode);
	}

	static Flags16& FastCommand(const char* command_name, Flags16 default_value = { 0 }, u32 mask = 0)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Mask16* new_cmd = new CCC_Mask16(command_name, new Flags16, mask);
			Console->AddCommand(new_cmd);
			*new_cmd->value = default_value;
			return *static_cast<CCC_Mask16*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_Mask16*>((*it).second)->value;
	}

	virtual CCC_Mask16* dcast_mask16() { return this; }
};

class ENGINE_API CCC_ToggleMask : public IConsole_Command
{
protected	:
	Flags32*	value;
	u32			mask;
public		:
	CCC_ToggleMask(const char* N, Flags32* V, u32 M) :
	  IConsole_Command(N),
	  value(V),
	  mask(M)
	{bEmptyArgsHandled=true;};
	  const bool GetValue()const{ return value->test(mask); }
	virtual void	Execute	(const char* args)
	{
		value->set(mask,!GetValue());
		TStatus S;
		xr_strconcat(S,cName," is ", value->test(mask)?"on":"off");
		Log(S);
	}
	virtual void	Status	(TStatus& S)
	{	xr_strcpy(S,value->test(mask)?"on":"off"); }
	virtual void	Info	(TInfo& I)
	{	xr_strcpy(I,"'on/off' or '1/0'"); }

	void fill_tips(vecTips& tips, u32 mode)
	{
		TStatus str;
	  	
		xr_sprintf(str, sizeof(str), "%s (current) [on, off]", value->test(mask) ? "on" : "off");
		tips.emplace_back(str);

		IConsole_Command::fill_tips(tips, mode);
	}

	static Flags32& FastCommand(const char* command_name, Flags32 default_value = { 0 }, u32 mask = 0)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_ToggleMask* new_cmd = new CCC_ToggleMask(command_name, new Flags32, mask);
			Console->AddCommand(new_cmd);
			*new_cmd->value = default_value;
			return *static_cast<CCC_ToggleMask*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_ToggleMask*>((*it).second)->value;
	}

	virtual CCC_ToggleMask* dcast_tmask() { return this; }
};

class ENGINE_API CCC_Token : public IConsole_Command
{
protected	:
	xr_token*		tokens;
public		:
	u32* value;

	CCC_Token(const char* N, u32* V, xr_token* T) :
	  IConsole_Command(N),
	  value(V),
	  tokens(T)
	{};

	virtual void	Execute	(const char* args)
	{
		xr_token* tok = tokens;
		while (tok->name) {
			if (_stricmp(tok->name,args)==0) {
				*value=tok->id;
				break;
			}
			tok++;
		}
		if (!tok->name) InvalidSyntax();
	}
	virtual void	Status	(TStatus& S)
	{
		xr_token *tok = tokens;
		while (tok->name) {
			if (tok->id==(int)(*value)) {
				xr_strcpy(S,tok->name);
				return;
			}
			tok++;
		}
		xr_strcpy(S,"?");
		return;
	}
	virtual void	Info	(TInfo& I)
	{	
		I[0]=0;
		xr_token *tok = GetToken();
		for (int Iter = 0;;Iter++) {
			if (tok[Iter].name == nullptr) {
				break;
			}

			if (I[0]) xr_strcat(I,"/");
			xr_strcat(I, tok[Iter].name);
		}
	}
	virtual xr_token* GetToken(){return tokens;}
	
	virtual void fill_tips(vecTips& tips, u32 mode)
	{
		TStatus str;
		bool res = false;
		xr_token* tok = GetToken();

		while (tok->name && !res)
		{
			if (tok->id == static_cast<int>(*value))
			{
				xr_sprintf(str, sizeof(str), "%s (current)", tok->name);
				tips.emplace_back(str);
				res = true;
			}
			tok++;
		}

		if (!res)
			tips.emplace_back("--- (current)");
		
		tok = GetToken();
		
		while (tok->name)
		{
			tips.emplace_back(tok->name);
			tok++;
		}

		IConsole_Command::fill_tips(tips, mode);
	}

	static u32& FastCommand(const char* command_name, xr_token&& token, u32 default_value = 0)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Token* new_cmd = new CCC_Token(command_name, new u32, new xr_token(std::move(token)));
			Console->AddCommand(new_cmd);
			*new_cmd->value = default_value;
			return *static_cast<CCC_Token*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_Token*>((*it).second)->value;
	}

	virtual CCC_Token* dcast_token() { return this; }
};

#undef min
#undef max

class ENGINE_API CCC_Float : public IConsole_Command
{
public:
	float*			value;
	float			min, max;

	CCC_Float(const char* N, float* V, float _min=0, float _max=1) :
	  IConsole_Command(N),
	  value(V),
	  min(_min),
	  max(_max)
	{};
	  const float	GetValue	() const {return *value;};
	void GetBounds(float& fmin, float& fmax) const {
		fmin = min;
		fmax = max;
	}

	virtual void	Execute	(const char* args)
	{
		float v = float(atof(args));
		if (v<(min-EPS) || v>(max+EPS) ) InvalidSyntax();
		else	*value = v;
	}
	virtual void	Status	(TStatus& S)
	{	
		xr_sprintf	(S,sizeof(S),"%3.3f",*value);
		while	(xr_strlen(S) && ('0'==S[xr_strlen(S)-1]))	S[xr_strlen(S)-1] = 0;
	}
	virtual void	Info	(TInfo& I)
	{	
		xr_sprintf(I,sizeof(I),"Float value in range [%3.3f, %3.3f]",min,max);
	}
	
	virtual void fill_tips(vecTips& tips, u32 mode)
	{
		TStatus str;
		
		xr_sprintf(str, sizeof(str), "%3.3f (current) [%3.3f, %3.3f]", *value, min, max);
		tips.emplace_back(str);
		
		IConsole_Command::fill_tips(tips, mode);
	}

	static float& FastCommand(const char* command_name, float default_value = 0.f, float min = -1000.f, float max = 1000.f)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Float* new_cmd = new CCC_Float(command_name, new float, min, max);
			Console->AddCommand(new_cmd);
			*new_cmd->value = default_value;
			return *static_cast<CCC_Float*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_Float*>((*it).second)->value;
	}

	virtual CCC_Float* dcast_float() { return this; }
};

class ENGINE_API CCC_Vector3 : public IConsole_Command
{
public:
	Fvector*		value;
	Fvector			min, max;

	CCC_Vector3(const char* N, Fvector* V, Fvector _min = {-1000.f, -1000.f, -1000.f }, Fvector _max = { 1000.f, 1000.f, 1000.f }) :
	  IConsole_Command(N),
	  value(V)
	{
		min.set(_min);
		max.set(_max);
	};
	const Fvector	GetValue	() const {return *value;};
	Fvector*		GetValuePtr	() const {return value;};

	virtual void Execute(const char* args)
	{
		Fvector v;
		if (3 != sscanf(args, "%f,%f,%f", &v.x, &v.y, &v.z))
		{
			if (3 != sscanf(args, "(%f,%f,%f)", &v.x, &v.y, &v.z))
			{
				InvalidSyntax();
				return;
			}
		}

		if (v.x < min.x || v.y < min.y || v.z < min.z)
		{
			InvalidSyntax();
			return;
		}

		if (v.x > max.x || v.y > max.y || v.z > max.z)
		{
			InvalidSyntax();
			return;
		}

		value->set(v);
	}

	virtual void Status(TStatus& S)
	{
		xr_sprintf(S, sizeof(S), "%3.3f, %3.3f, %3.3f", value->x, value->y, value->z);
	}

	virtual void Info(TInfo& I)
	{
		xr_sprintf(I, sizeof(I), "vector3 in range [(%3.3f, %3.3f, %3.3f) - (%3.3f, %3.3f, %3.3f)]", min.x, min.y, min.z, max.x, max.y, max.z);
	}
	
	virtual void fill_tips(vecTips& tips, u32 mode)
	{
		TStatus str;
		
		xr_sprintf(str, sizeof(str), "%3.3f, %3.3f, %3.3f [(%3.3f, %3.3f, %3.3f) - (%3.3f, %3.3f, %3.3f)]", value->x, value->y, value->z, min.x, min.y, min.z, max.x, max.y, max.z);
		tips.emplace_back(str);
		
		IConsole_Command::fill_tips(tips, mode);
	}

	static Fvector& FastCommand(const char* command_name, Fvector default_value = zero_vel, Fvector min = { -1000.f, -1000.f, -1000.f }, Fvector max = { 1000.f, 1000.f, 1000.f })
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Vector3* new_cmd = new CCC_Vector3(command_name, new Fvector, min, max);
			*new_cmd->value = default_value;
			Console->AddCommand(new_cmd);
			return *static_cast<CCC_Vector3*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_Vector3*>((*it).second)->value;
	}

	virtual CCC_Vector3* dcast_vector() { return this; }
};

class ENGINE_API CCC_Integer : public IConsole_Command
{
public:
	int* value;
	int min, max;

	int GetValue() const
	{
		return *value;
	}

	void SetValue(int new_value)
	{
		*value = new_value;
	}

	void Toggle()
	{
		min == 0 && max == 1 ? *value = !GetValue() : false;
	}

	void GetBounds(int& imin, int& imax) const
	{
		imin = min;
		imax = max;
	}

	CCC_Integer(const char* N, int* V, int _min = 0, int _max = 999) :
		IConsole_Command(N),
		value(V),
		min(_min),
		max(_max)
	{
	};

	void Execute(const char* args) override
	{
		int v = atoi(args);
		if (v < min || v > max) InvalidSyntax();
		else *value = v;
	}

	void Status(TStatus& S) override
	{
		_itoa(*value, S, 10);
	}

	void Info(TInfo& I) override
	{
		xr_sprintf(I, sizeof(I), "Integer value in range [%d, %d]", min, max);
	}

	void fill_tips(vecTips& tips, u32 mode) override
	{
		TStatus str;
		
		xr_sprintf(str, sizeof(str), "%d (current) [%d, %d]", *value, min, max);
		tips.emplace_back(str);
		
		IConsole_Command::fill_tips(tips, mode);
	}

	static int& FastCommand(const char* command_name, int default_value = 0, int min = -1000, int max = 1000)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			auto new_cmd = new CCC_Integer(command_name, new int, min, max);
			*new_cmd->value = default_value;
			Console->AddCommand(new_cmd);
			return *new_cmd->value;
		}
		return *static_cast<CCC_Integer*>(it->second)->value;
	}

	CCC_Integer* dcast_int() override { return this; }
};

class ENGINE_API CCC_Boolean : public IConsole_Command
{
public:
	bool* value;

	bool GetValue() const
	{
		return *value;
	}
	
	void SetValue(bool new_value)
	{
		*value = new_value;
	}
	
	void Toggle()
	{
		*value = !GetValue();
	}

	CCC_Boolean(const char* N, bool* V, bool min = false, bool max = true) :
	  IConsole_Command(N),
	  value(V)
	{}

	virtual void	Execute	(const char* args)
	{
		int Value = 0;
		if (std::from_chars(args, args + std::strlen(args), Value).ec != std::errc{}) 
		{
			*value = !strcmp(args, "true");
		} 
		else if (EQ(args, "1"))
		{
			*value = true;
		}
		else
		{
			*value = false;
		}
	}

	virtual void Status(TStatus& S)
	{
		S[0] = 0;
		*value ? xr_strcat(S, "true") : xr_strcat(S, "false");
	}

	virtual void fill_tips(vecTips& tips, u32 mode)
	{
		TStatus str;

		xr_sprintf(str, sizeof(str), "%s [off/false, on/true]", *value ? "true" : "false");
		tips.emplace_back(str);

		IConsole_Command::fill_tips(tips, mode);
	}

	static bool& FastCommand(const char* command_name, bool default_value = false, bool min = false, bool max = true)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Boolean* new_cmd = new CCC_Boolean(command_name, new bool, min, max);
			*new_cmd->value = default_value;
			Console->AddCommand(new_cmd);
			return *static_cast<CCC_Boolean*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_Boolean*>((*it).second)->value;
	}

	virtual CCC_Boolean* dcast_bool() { return this; }
};

class ENGINE_API CCC_String : public IConsole_Command
{
public:
	LPSTR			value;
	int				size;

	CCC_String(const char* N, LPSTR V, int _size=2) :
		IConsole_Command(N),
		value	(V),
		size	(_size)
	{
		bLowerCaseArgs	=	false;
		R_ASSERT(V);
		R_ASSERT(size>1);
	}

	virtual void Execute(const char* args)
	{
		strncpy_s(value, size, args, size - 1);
	}

	virtual void Status(TStatus& S)
	{
		xr_strcpy(S, value);
	}

	virtual void Info(TInfo& I)
	{
		xr_sprintf(I, sizeof(I), "String with up to %d characters", size);
	}

	virtual void fill_tips(vecTips& tips, u32 mode)
	{
		TStatus str;

		xr_sprintf(str, sizeof(str), "%s (current)", value);
		tips.emplace_back(str);

		IConsole_Command::fill_tips(tips, mode);
	}

	static LPSTR FastCommand(const char* command_name, const char* default_value = "\0", int _size = 512)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_String* new_cmd = new CCC_String(command_name, strdup(default_value), _size);
			Console->AddCommand(new_cmd);
			return static_cast<CCC_String*>(new_cmd)->value;
		}
		else
			return static_cast<CCC_String*>((*it).second)->value;
	}

	virtual CCC_String* dcast_string() { return this; }
};

class ENGINE_API CCC_SaveCFG : public IConsole_Command
{
public:
	CCC_SaveCFG(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; };
	virtual void Execute(const char* args);
};

class ENGINE_API CCC_LoadCFG : public IConsole_Command
{
public:
	CCC_LoadCFG(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; };
	virtual bool	allow(const char* cmd) { return true; };
	virtual void	Execute			(const char* args);
};

class ENGINE_API CCC_LoadCFG_custom : public CCC_LoadCFG
{
	string64		m_cmd;
public:
					CCC_LoadCFG_custom(const char* cmd);
	virtual bool	allow			(const char* cmd);
};