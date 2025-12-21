#pragma once
#include "XR_IOConsole.h"
#define CMD0(cls)					{ static cls x##cls();				Console->AddCommand(&x##cls);}
#define CMD1(cls,p1)				{ static cls x##cls(p1);			Console->AddCommand(&x##cls);}
#define CMD2(cls,p1,p2)				{ static cls x##cls(p1,p2);			Console->AddCommand(&x##cls);}
#define CMD3(cls,p1,p2,p3)			{ static cls x##cls(p1,p2,p3);		Console->AddCommand(&x##cls);}
#define CMD4(cls,p1,p2,p3,p4)		{ static cls x##cls(p1,p2,p3,p4);	Console->AddCommand(&x##cls);}

#include <charconv>

class ENGINE_API CCC_Mask;
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
public		:
	friend class	CConsole;
	typedef char	TInfo	[512];
	typedef char	TStatus	[256];
	using vecTips = xr_vector<shared_str>;
	using vecLRU = xr_vector<shared_str>;

protected	:
	LPCSTR			cName;
	bool			bEnabled;
	bool			bLowerCaseArgs;
	bool			bEmptyArgsHandled;
	
	vecLRU			m_LRU;

	enum {
		LRU_MAX_COUNT = 10
	};

	IC	bool		EQ(LPCSTR S1, LPCSTR S2) { return xr_strcmp(S1,S2)==0; }
public		:
	IConsole_Command		(LPCSTR N) : 
	  cName				(N),
	  bEnabled			(TRUE),
	  bLowerCaseArgs	(TRUE),
	  bEmptyArgsHandled	(FALSE) {
		  m_LRU.reserve(LRU_MAX_COUNT + 1);
		  m_LRU.clear();
	  }
	virtual ~IConsole_Command()
	{
		if(Console)
			Console->RemoveCommand(this);
	};

	LPCSTR			Name()			{ return cName;	}
	void			InvalidSyntax() {
		TInfo I; Info(I);
		Msg("~ Invalid syntax in call to '%s'",cName);
		Msg("~ Valid arguments: %s", I);
	}
	virtual void	Execute	(LPCSTR args)	= 0;
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
	virtual CCC_Mask* dcast_mask() { return nullptr; }
	virtual CCC_Mask16* dcast_mask16() { return nullptr; }
	virtual CCC_ToggleMask* dcast_tmask() { return nullptr; }
	virtual CCC_Token* dcast_token() { return nullptr; }
	virtual CCC_Float* dcast_float() { return nullptr; }
	virtual CCC_Vector3* dcast_vector() { return nullptr; }
	virtual CCC_Integer* dcast_int() { return nullptr; }
	virtual CCC_Boolean* dcast_bool() { return nullptr; }
	virtual CCC_String* dcast_string() { return nullptr; }
}; // class IConsole_Command

class ENGINE_API	CCC_Mask : public IConsole_Command
{
protected	:
	Flags32*	value;
	u32			mask;
public		:
	CCC_Mask(LPCSTR N, Flags32* V, u32 M) :
	  IConsole_Command(N),
	  value(V),
	  mask(M) {}

	BOOL GetValue() const {return value->test(mask);}
	void SetInverseValue() {value->invert(mask);}
	
	virtual void	Execute	(LPCSTR args)
	{
		if (EQ(args,"on"))			value->set(mask,TRUE);
		else if (EQ(args,"off"))	value->set(mask,FALSE);
		else if (EQ(args,"1"))		value->set(mask,TRUE);
		else if (EQ(args,"0"))		value->set(mask,FALSE);
		else InvalidSyntax();
	}
	virtual void	Status	(TStatus& S)
	{	xr_strcpy(S,value->test(mask)?"on":"off"); }
	virtual void	Info	(TInfo& I)
	{	xr_strcpy(I,"'on/off' or '1/0'"); }

	virtual void fill_tips(vecTips& tips, u32 mode) {
		TStatus  str;
		xr_sprintf( str, sizeof(str), "%s  (current)  [on/off]", value->test(mask)?"on":"off" );
		tips.push_back( str );
	}

	static Flags32& FastCommand(LPCSTR command_name, u32 mask = 0)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Mask* new_cmd = new CCC_Mask(command_name, new Flags32, mask);
			Console->Commands[command_name] = new_cmd;
			*new_cmd->value = { 0 };
			return *static_cast<CCC_Mask*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_Mask*>((*it).second)->value;
	}

	virtual CCC_Mask* dcast_mask() { return this; }
};

class ENGINE_API CCC_Mask16 : 
	public IConsole_Command
{
protected	:
	Flags16*	value;
	u32			mask;
public		:
	CCC_Mask16(LPCSTR N, Flags16* V, u32 M) :
	  IConsole_Command(N),
	  value(V),
	  mask(M)
	{};

	const BOOL GetValue() const { return value->test(mask); }

	virtual void Execute	(LPCSTR args)
	{
		if (EQ(args,"on"))			value->set(mask,TRUE);
		else if (EQ(args,"off"))	value->set(mask,FALSE);
		else if (EQ(args,"1"))		value->set(mask,TRUE);
		else if (EQ(args,"0"))		value->set(mask,FALSE);
		else InvalidSyntax();
	}
	virtual void	Status	(TStatus& S)
	{	xr_strcpy(S,value->test(mask)?"on":"off"); }
	virtual void	Info	(TInfo& I)
	{	xr_strcpy(I,"'on/off' or '1/0'"); }

	virtual void fill_tips(vecTips& tips, u32 mode) {
		TStatus  str;
		xr_sprintf( str, sizeof(str), "%s  (current)  [on/off]", value->test(mask)?"on":"off" );
		tips.push_back( str );
	}

	static Flags16& FastCommand(LPCSTR command_name, u32 mask = 0)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Mask16* new_cmd = new CCC_Mask16(command_name, new Flags16, mask);
			Console->Commands[command_name] = new_cmd;
			*new_cmd->value = { 0 };
			return *static_cast<CCC_Mask16*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_Mask16*>((*it).second)->value;
	}

	virtual CCC_Mask16* dcast_mask16() { return this; }
};

class ENGINE_API	CCC_ToggleMask : public IConsole_Command
{
protected	:
	Flags32*	value;
	u32			mask;
public		:
	CCC_ToggleMask(LPCSTR N, Flags32* V, u32 M) :
	  IConsole_Command(N),
	  value(V),
	  mask(M)
	{bEmptyArgsHandled=TRUE;};
	  const BOOL GetValue()const{ return value->test(mask); }
	virtual void	Execute	(LPCSTR args)
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

	virtual void fill_tips(vecTips& tips, u32 mode) {
		TStatus  str;
		xr_sprintf( str, sizeof(str), "%s  (current)  [on/off]", value->test(mask)?"on":"off" );
		tips.push_back( str );
	}

	static Flags32& FastCommand(LPCSTR command_name, u32 mask = 0)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_ToggleMask* new_cmd = new CCC_ToggleMask(command_name, new Flags32, mask);
			Console->Commands[command_name] = new_cmd;
			*new_cmd->value = { 0 };
			return *static_cast<CCC_ToggleMask*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_ToggleMask*>((*it).second)->value;
	}

	virtual CCC_ToggleMask* dcast_tmask() { return this; }
};

class ENGINE_API	CCC_Token : public IConsole_Command
{
protected	:
	xr_token*		tokens;
public		:
	u32* value;

	CCC_Token(LPCSTR N, u32* V, xr_token* T) :
	  IConsole_Command(N),
	  value(V),
	  tokens(T)
	{};

	virtual void	Execute	(LPCSTR args)
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
	
	virtual void fill_tips(vecTips& tips, u32 mode) {
		TStatus  str;
		bool res = false;
		xr_token* tok = GetToken();
		while (tok->name && !res) {
			if (tok->id == (int)(*value)) {
				xr_sprintf(str, sizeof(str), "%s  (current)", tok->name);
				tips.push_back( str );
				res = true;
			}
			tok++;
		}

		if (!res) {
			tips.push_back( "---  (current)" );
		}
		tok = GetToken();
		while (tok->name) {
			tips.push_back( tok->name );
			tok++;
		}
	}

	static u32& FastCommand(LPCSTR command_name, xr_token& token)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Token* new_cmd = new CCC_Token(command_name, new u32, new xr_token(std::move(token)));
			Console->Commands[command_name] = new_cmd;
			*new_cmd->value = 0;
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

	CCC_Float(LPCSTR N, float* V, float _min=0, float _max=1) :
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

	virtual void	Execute	(LPCSTR args)
	{
		float v = float(atof(args));
		if (v<(min-EPS) || v>(max+EPS) ) InvalidSyntax();
		else	*value = v;
	}
	virtual void	Status	(TStatus& S)
	{	
		xr_sprintf	(S,sizeof(S),"%3.5f",*value);
		while	(xr_strlen(S) && ('0'==S[xr_strlen(S)-1]))	S[xr_strlen(S)-1] = 0;
	}
	virtual void	Info	(TInfo& I)
	{	
		xr_sprintf(I,sizeof(I),"float value in range [%3.3f,%3.3f]",min,max);
	}
	virtual void fill_tips(vecTips& tips, u32 mode) {
		TStatus  str;
		xr_sprintf( str, sizeof(str), "%3.5f  (current)  [%3.3f,%3.3f]", *value, min, max );
		tips.push_back( str );
		IConsole_Command::fill_tips( tips, mode );
	}

	static float& FastCommand(LPCSTR command_name, float min = -1000.f, float max = 1000.f)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Float* new_cmd = new CCC_Float(command_name, new float, min, max);
			Console->Commands[command_name] = new_cmd;
			*new_cmd->value = 0.f;
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

	CCC_Vector3(LPCSTR N, Fvector* V, Fvector _min = {-1000.f, -1000.f, -1000.f }, Fvector _max = { 1000.f, 1000.f, 1000.f }) :
	  IConsole_Command(N),
	  value(V)
	{
		min.set(_min);
		max.set(_max);
	};
	const Fvector	GetValue	() const {return *value;};
	Fvector*		GetValuePtr	() const {return value;};

	virtual void Execute(pcstr args)
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

	virtual void	Status	(TStatus& S)
	{	
		xr_sprintf	(S,sizeof(S),"%f,%f,%f",value->x,value->y,value->z);
	}
	virtual void	Info	(TInfo& I)
	{	
		xr_sprintf(I,sizeof(I),"vector3 in range [%f,%f,%f]-[%f,%f,%f]",min.x,min.y,min.z,max.x,max.y,max.z);
	}
	virtual void fill_tips(vecTips& tips, u32 mode)
	{
		TStatus  str;
		xr_sprintf(str, sizeof(str), "%f, %f, %f", value->x, value->y, value->z, min.x, min.y, min.z, max.x, max.y, max.z);
		tips.push_back(str);
		xr_sprintf(str, sizeof(str), "[(%f,%f,%f)-(%f,%f,%f)]", value->x, value->y, value->z, min.x, min.y, min.z, max.x, max.y, max.z);
		tips.push_back(str);
		IConsole_Command::fill_tips( tips, mode );
	}

	static Fvector& FastCommand(LPCSTR command_name, Fvector min = { -1000.f, -1000.f, -1000.f }, Fvector max = { 1000.f, 1000.f, 1000.f })
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Vector3* new_cmd = new CCC_Vector3(command_name, new Fvector, min, max);
			*new_cmd->value = zero_vel;
			Console->Commands[command_name] = new_cmd;
			return *static_cast<CCC_Vector3*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_Vector3*>((*it).second)->value;
	}

	virtual CCC_Vector3* dcast_vector() { return this; }
};

class ENGINE_API	CCC_Integer : public IConsole_Command
{
public:
	int*			value;
	int				min, max;
	  const int GetValue	() const {return *value;};
	void GetBounds(int& imin, int& imax) const {
		imin = min;
		imax = max;
	}

	CCC_Integer(LPCSTR N, int* V, int _min=0, int _max=999) :
	  IConsole_Command(N),
	  value(V),
	  min(_min),
	  max(_max)
	{};

	virtual void	Execute	(LPCSTR args)
	{
		int v = atoi(args);
		if (v<min || v>max) InvalidSyntax();
		else	*value = v;
	}
	virtual void	Status	(TStatus& S)
	{	
		_itoa(*value,S,10);
	}
	virtual void	Info	(TInfo& I)
	{	
		xr_sprintf(I,sizeof(I),"integer value in range [%d,%d]",min,max);
	}
	virtual void fill_tips(vecTips& tips, u32 mode) {
		TStatus  str;
		xr_sprintf( str, sizeof(str), "%d  (current)  [%d,%d]", *value, min, max );
		tips.push_back( str );
 		IConsole_Command::fill_tips( tips, mode );
	}

	static int& FastCommand(LPCSTR command_name, int min = -1000, int max = 1000)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Integer* new_cmd = new CCC_Integer(command_name, new int, min, max);
			*new_cmd->value = 0;
			Console->Commands[command_name] = new_cmd;
			return *static_cast<CCC_Integer*>(new_cmd)->value;
		}
		else
			return *static_cast<CCC_Integer*>((*it).second)->value;
	}

	virtual CCC_Integer* dcast_int() { return this; }
};

class ENGINE_API CCC_Boolean : public IConsole_Command
{
public:
	bool* value;

	bool GetValue() const {return *value;}
	
	void SetInverseValue() const
	{
		bool old_val = *value;
		*value = !old_val;
	}

	CCC_Boolean(LPCSTR N, bool* V, bool min = false, bool max = true) :
	  IConsole_Command(N),
	  value(V)
	{}

	virtual void	Execute	(LPCSTR args)
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
	virtual void	Status	(TStatus& S)
	{	
		S[0] = 0;
		bool bStatus = *value;

		if (bStatus)
			xr_strcat(S, "true");
		else
			xr_strcat(S, "false");
	}

	virtual void fill_tips(vecTips& tips, u32 mode) {
		TStatus str{};
		xr_sprintf(str, sizeof(str), "%s", *value ? "true" : "false");
		tips.push_back(str);
		IConsole_Command::fill_tips(tips, mode);
	}

	static bool& FastCommand(LPCSTR command_name, bool min = false, bool max = true)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_Boolean* new_cmd = new CCC_Boolean(command_name, new bool, min, max);
			*new_cmd->value = false;
			Console->Commands[command_name] = new_cmd;
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

	CCC_String(LPCSTR N, LPSTR V, int _size=2) :
		IConsole_Command(N),
		value	(V),
		size	(_size)
	{
		bLowerCaseArgs	=	FALSE;
		R_ASSERT(V);
		R_ASSERT(size>1);
	};

	virtual void Execute(LPCSTR args)
	{
		strncpy_s(value, size, args, size-1);
	}
	virtual void	Status	(TStatus& S)
	{	
		xr_strcpy	(S,value);
	}
	virtual void	Info	(TInfo& I)
	{	
		xr_sprintf(I,sizeof(I),"string with up to %d characters",size);
	}
	virtual void fill_tips(vecTips& tips, u32 mode) {
		tips.push_back( (LPCSTR)value );
		IConsole_Command::fill_tips( tips, mode );
	}

	static LPSTR FastCommand(LPCSTR command_name, int _size = 512)
	{
		auto it = Console->Commands.find(command_name);
		if (it == Console->Commands.end())
		{
			CCC_String* new_cmd = new CCC_String(command_name, new char[_size], _size);
			Console->Commands[command_name] = new_cmd;
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
	CCC_SaveCFG(LPCSTR N) : IConsole_Command(N) { bEmptyArgsHandled = TRUE; };
	virtual void Execute(LPCSTR args);
};

class ENGINE_API CCC_LoadCFG : public IConsole_Command
{
public:
	CCC_LoadCFG(LPCSTR N) : IConsole_Command(N) { bEmptyArgsHandled = TRUE; };
	virtual bool	allow(LPCSTR cmd) { return true; };
	virtual void	Execute			(LPCSTR args);
};

class ENGINE_API CCC_LoadCFG_custom : public CCC_LoadCFG
{
	string64		m_cmd;
public:
					CCC_LoadCFG_custom(LPCSTR cmd);
	virtual bool	allow			(LPCSTR cmd);
};