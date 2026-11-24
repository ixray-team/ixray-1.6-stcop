#pragma once

// refs
struct	FSlideWindowItem;

#include "FBasicVisual.h"

class	FTreeVisual				:	public dxRender_Visual, public IRender_Mesh
{
private:
	struct	_5color
	{
		Fvector					rgb;		// - all static lighting
		float					hemi;		// - hemisphere
		float					sun;		// - sun
	};
protected:
	_5color						c_scale;
	_5color						c_bias;
	Fmatrix						xform;
public:
	float GetHemiScale() const { return c_scale.hemi; }
	float GetHemiBias() const { return c_bias.hemi; }
	// What the baked light of its vertices is scaled and offset by: rgb and hemi, then the sun's scale and bias
	void GetLight(Fvector4& Scale, Fvector4& Bias, Fvector2& Sun) const
	{
		Scale.set(c_scale.rgb.x, c_scale.rgb.y, c_scale.rgb.z, c_scale.hemi);
		Bias.set(c_bias.rgb.x, c_bias.rgb.y, c_bias.rgb.z, c_bias.hemi);
		Sun.set(c_scale.sun, c_bias.sun);
	}
	virtual void Render			(GfxCmdList& Cmd, float LOD		) override;									// LOD - Level Of Detail  [0.0f - min, 1.0f - max], Ignored
	virtual void Load			(const char* N, IReader *data, u32 dwFlags) override;
	virtual void Copy			(dxRender_Visual *pFrom	) override;
	virtual void Release		() override;

	const Fmatrix& GetTransform() const { return xform; }

	FTreeVisual(void);
	virtual ~FTreeVisual(void) override;
};

class FTreeVisual_ST :	public FTreeVisual
{
	typedef FTreeVisual inherited;
public:
					FTreeVisual_ST	(void);
	virtual ~FTreeVisual_ST	(void) override;

	virtual void	Render			(GfxCmdList& Cmd, float LOD		) override;									// LOD - Level Of Detail  [0.0f - min, 1.0f - max], Ignored
	virtual void	Load			(const char* N, IReader *data, u32 dwFlags) override;
	virtual void	Copy			(dxRender_Visual *pFrom	) override;
	virtual void	Release			() override;
private:
	FTreeVisual_ST				(const FTreeVisual_ST& other);
	void	operator=			( const FTreeVisual_ST& other);
};

class FTreeVisual_PM :	public FTreeVisual
{
	typedef FTreeVisual inherited;
private:
	FSlideWindowItem*	pSWI;
	u32					last_lod;
public:
					FTreeVisual_PM	(void);
	const FSlideWindow*	GetWindow	() const { return pSWI && pSWI->count ? pSWI->sw : nullptr; }
	virtual ~FTreeVisual_PM	(void) override;

	virtual void	Render			(GfxCmdList& Cmd, float LOD		) override;									// LOD - Level Of Detail  [0.0f - min, 1.0f - max], Ignored
	virtual void	Load			(const char* N, IReader *data, u32 dwFlags) override;
	virtual void	Copy			(dxRender_Visual *pFrom	) override;
	virtual void	Release			() override;
private:
	FTreeVisual_PM				(const FTreeVisual_PM& other);
	void	operator=			( const FTreeVisual_PM& other);
};

const int		FTreeVisual_tile	= 16;
const int		FTreeVisual_quant	= 32768/FTreeVisual_tile;
