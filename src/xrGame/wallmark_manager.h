#pragma once



// Igor 
//using SHADER_VECTOR = xr_vector<ref_shader>;
//using SHADER_VECTOR_IT = SHADER_VECTOR::iterator;

class CWalmarkManager
{
private:
	FactoryPtr<IWallMarkArray>		m_wallmarks;
	Fvector							m_pos;
public:
	CObject*						m_owner;
				CWalmarkManager		()																																					;
				~CWalmarkManager	()																																					;
		void	Load				(LPCSTR section)																																	;
		void	Clear				()																																					;
		void	AddWallmark			(const Fvector& dir, const Fvector& start_pos, float range, float wallmark_size,IWallMarkArray &wallmarks_vector,int t);
		void	PlaceWallmarks		( const Fvector& start_pos);
		void	PlaceWallmarks		( const Fvector& start_pos, shared_str Sect);
		
		void	 StartWorkflow		(const shared_str& Sect, bool UseCamDir = false);
};