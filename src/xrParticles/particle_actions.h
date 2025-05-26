//---------------------------------------------------------------------------
#pragma once
#include <magic_enum/magic_enum.hpp>

namespace PAPI
{
	// refs
	struct ParticleEffect;
	struct PARTICLES_API ParticleAction
	{
		enum
		{
			ALLOW_ROTATE	= (1<<1)
		};
		Flags32			m_Flags;
		PActionEnum		type;	// Type field
		ParticleAction	() : type(action_enum_force_dword) {m_Flags.zero();}

		virtual ~ParticleAction() = default;
		virtual void 	Execute		(ParticleEffect *pe, const float dt, float& m_max)	= 0;
		virtual void 	Transform	(const Fmatrix& m)				= 0;

		virtual void 	Load		(IReader& F)=0;
		virtual void 	Save		(IWriter& F)=0;

		template<typename T, typename TEnum>
		T* GetVariable(TEnum VarID)
		{
			return (T*)GetVariableImpl((u8)VarID);
		}

	protected:
		virtual void* GetVariableImpl(u8 VarID) = 0;
	};

	using PAVec = xr_vector<ParticleAction*>;
	using PAVecIt = PAVec::iterator;

	class ParticleActions
	{
		PAVec actions;
	public:
		IC ParticleActions();
		IC ~ParticleActions();

		IC void			clear	();

		IC void reserve(u32 capacity);

		IC void			append	(ParticleAction* pa)	{actions.push_back(pa);	}
		IC bool			empty	()						{return	actions.empty();}
		IC PAVecIt		begin	()						{return	actions.begin();}
		IC PAVecIt		end		()						{return actions.end();	}
        IC int			size	()						{return (int)actions.size();	}		
		IC void			resize	(int cnt)				{ actions.resize(cnt); }
		IC ParticleAction* find (PActionEnum type)
		{
			auto it = std::find_if(actions.begin(), actions.end(), [&](ParticleAction* pa){return pa->type == type;});
			R_ASSERT3(it != actions.end(), "Failed to find action", magic_enum::enum_name<PActionEnum>(type).data());
			return it != actions.end() ? (*it) : nullptr;
		}
	};
};

IC PAPI::ParticleActions::ParticleActions()
{
	actions.reserve(4); 
}

IC PAPI::ParticleActions::~ParticleActions()
{
	clear();
}

IC void PAPI::ParticleActions::clear()
{
	for (ParticleAction* pPAction: actions)
		xr_delete(pPAction);

	actions.clear();
}


IC void PAPI::ParticleActions::reserve(u32 capacity)
{
	actions.reserve(capacity);
}