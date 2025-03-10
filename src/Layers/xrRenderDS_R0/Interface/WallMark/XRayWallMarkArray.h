#pragma once
class CDS0_WallMarkArray:public IWallMarkArray 
{
public:
	CDS0_WallMarkArray();
	virtual void Copy(IWallMarkArray &_in) ;

	virtual void	AppendMark(LPCSTR s_textures);
	virtual void	clear();
	virtual bool	empty() ;
	virtual wm_shader GenerateWallmark();
};
