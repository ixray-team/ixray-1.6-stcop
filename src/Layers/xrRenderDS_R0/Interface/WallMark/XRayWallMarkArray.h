#pragma once
class CDS0_WallMarkArray:public IWallMarkArray 
{
public:
	CDS0_WallMarkArray();
	virtual void Copy(IWallMarkArray &_in) override;

	virtual void AppendMark(str_c s_textures) override;
	virtual void AppendMark(str_c s_shader, str_c s_textures) override;
	virtual void clear() override;
	virtual bool empty() override;
	virtual wm_shader GenerateWallmark() override;
};
