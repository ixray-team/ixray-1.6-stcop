//---------------------------------------------------------------------------
#pragma once
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
		ParticleAction	(){m_Flags.zero();}
        
		virtual void 	Execute		(ParticleEffect *pe, const float dt, float& m_max)	= 0;
		virtual void 	Transform	(const Fmatrix& m)				= 0;

		virtual void 	Load		(IReader& F)=0;
		virtual void 	Save		(IWriter& F)=0;
	};

	using PAVec = xr_vector<ParticleAction*>;
	using PAVecIt = PAVec::iterator;

	class ParticleActions
	{
		PAVec actions;
		bool m_bLocked;
	public:
		IC ParticleActions();
		IC ~ParticleActions();

		IC void			clear	();

		IC void			append	(ParticleAction* pa)	{R_ASSERT(!m_bLocked);actions.push_back(pa);	}
		IC bool			empty	()						{return	actions.empty();}
		IC PAVecIt		begin	()						{return	actions.begin();}
		IC PAVecIt		end		()						{return actions.end();	}
        IC int			size	()						{return (int)actions.size();	}
        IC void			resize	(int cnt)        		{R_ASSERT(!m_bLocked);actions.resize(cnt);	}
        void			copy	(ParticleActions* src);
		void			lock	()						{R_ASSERT(!m_bLocked);m_bLocked=true;}
		void			unlock	()						{R_ASSERT(m_bLocked);m_bLocked=false;}
	};
};

IC PAPI::ParticleActions::ParticleActions()
{
	actions.reserve(4); 
	m_bLocked = false;
}

IC PAPI::ParticleActions::~ParticleActions()
{
	clear();
}

IC void PAPI::ParticleActions::clear()
{
	R_ASSERT(!m_bLocked);

	for (ParticleAction* pPAction: actions)
		xr_delete(pPAction);

	actions.clear();
}