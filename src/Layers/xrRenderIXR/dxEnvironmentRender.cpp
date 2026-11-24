#include "stdafx.h"
#include "dxEnvironmentRender.h"
#include "AmbientCubeLevel.h"

#include "dxRenderDeviceRender.h"

#include "../../xrEngine/Environment.h"
#include "ResourceManager.h"

#include "../../xrEngine/xr_efflensflare.h"
#include "dxLensFlareRender.h"
#include "r__types.h"
#include "ShaderBindings.h"

namespace
{
	class CameraCenteredView
	{
	public:
		CameraCenteredView(GfxCmdList& Cmd) : Cmd(Cmd), Saved(Cmd.GetView())
		{
			GfxView View = Saved;
			View.View.c.set(0.f, 0.f, 0.f);
			View.ViewOld.c.set(0.f, 0.f, 0.f);

			Cmd.SetView(View);
		}

		~CameraCenteredView()
		{
			Cmd.SetView(Saved);
		}

		CameraCenteredView(const CameraCenteredView&) = delete;
		CameraCenteredView& operator=(const CameraCenteredView&) = delete;

	private:
		GfxCmdList& Cmd;
		GfxView Saved;
	};
}


//////////////////////////////////////////////////////////////////////////
// half box def
static	Fvector3	hbox_verts[12] =
{
	{-1.f,	-1.f,	-1.f},	// down
	{ 1.f,	-1.f,	-1.f},	// down
	{-1.f,	-1.f,	 1.f},	// down
	{ 1.f,	-1.f,	 1.f},	// down
	{-1.f,	 1.f,	-1.f},
	{ 1.f,	 1.f,	-1.f},
	{-1.f,	 1.f,	 1.f},
	{ 1.f,	 1.f,	 1.f},
	{-1.f,	 0.f,	-1.f},	// half
	{ 1.f,	 0.f,	-1.f},	// half
	{ 1.f,	 0.f,	 1.f},	// half
	{-1.f,	 0.f,	 1.f}	// half
};
static	u16			hbox_faces[20 * 3] =
{
	0,	 2,	 3,
	3,	 1,	 0,
	4,	 5,	 7,
	7,	 6,	 4,
	0,	 1,	 9,
	9,	 8,	 0,
	8,	 9,	 5,
	5,	 4,	 8,
	1,	 3,	10,
	10,	 9,	 1,
	9,	10,	 7,
	7,	 5,	 9,
	3,	 2,	11,
	11,	10,	 3,
	10,	11,	 6,
	6,	 7,	10,
	2,	 0,	 8,
	8,	11,	 2,
	11,	 8,	 4,
	4,	 6,	11
};

void dxEnvDescriptorRender::Copy(IEnvDescriptorRender& _in) {
	*this = *(dxEnvDescriptorRender*)&_in;
}

void dxEnvDescriptorMixerRender::Copy(IEnvDescriptorMixerRender& _in) {
	dxEnvDescriptorMixerRender* other = (dxEnvDescriptorMixerRender*)&_in;
	clouds_r_textures.assign(other->clouds_r_textures.begin(), other->clouds_r_textures.end());
	clouds_r_textures.dwReference++;

	sky_r_textures.assign(other->sky_r_textures.begin(), other->sky_r_textures.end());
	sky_r_textures.dwReference++;

	sky_r_textures_env.assign(other->sky_r_textures_env.begin(), other->sky_r_textures_env.end());
	sky_r_textures_env.dwReference++;
}

void dxEnvironmentRender::Copy(IEnvironmentRender& _in) {
	*this = *(dxEnvironmentRender*)&_in;
}

particles_systems::library_interface const& dxEnvironmentRender::particles_systems_library() {
	return	(RImplementation.PSLibrary);
}

void dxEnvDescriptorMixerRender::Destroy() {
	sky_r_textures.clear();
	sky_r_textures_env.clear();
	clouds_r_textures.clear();
}

void dxEnvDescriptorMixerRender::Clear() {
	std::pair<u32, ref_texture>	zero = std::make_pair(u32(0), ref_texture(nullptr));
	sky_r_textures.clear();
	sky_r_textures.push_back(zero);
	sky_r_textures.push_back(zero);
	sky_r_textures.push_back(zero);

	sky_r_textures_env.clear();
	sky_r_textures_env.push_back(zero);
	sky_r_textures_env.push_back(zero);
	sky_r_textures_env.push_back(zero);

	clouds_r_textures.clear();
	clouds_r_textures.push_back(zero);
	clouds_r_textures.push_back(zero);
	clouds_r_textures.push_back(zero);
}

void dxEnvDescriptorMixerRender::lerp(IEnvDescriptorRender* inA, IEnvDescriptorRender* inB) {
	dxEnvDescriptorRender* pA = (dxEnvDescriptorRender*)inA;
	dxEnvDescriptorRender* pB = (dxEnvDescriptorRender*)inB;

	sky_r_textures.clear();
	sky_r_textures.push_back(std::make_pair(ShaderBindings::Texture("s_sky0"), pA->sky_texture));
	sky_r_textures.push_back(std::make_pair(ShaderBindings::Texture("s_sky1"), pB->sky_texture));

	sky_r_textures_env.clear();

	sky_r_textures_env.push_back(std::make_pair(0, pA->sky_texture_env));
	sky_r_textures_env.push_back(std::make_pair(1, pB->sky_texture_env));

	clouds_r_textures.clear();
	clouds_r_textures.push_back(std::make_pair(ShaderBindings::Texture("s_clouds0"), pA->clouds_texture));
	clouds_r_textures.push_back(std::make_pair(ShaderBindings::Texture("s_clouds1"), pB->clouds_texture));
}

void dxEnvDescriptorRender::OnDeviceCreate(CEnvDescriptor& owner) {
	if(owner.sky_texture_name.size())
		sky_texture.create(owner.sky_texture_name.c_str());

	if(owner.sky_texture_env_name.size())
	{
		sky_texture_env.create(owner.sky_texture_env_name.c_str());
		AmbientLevel = AmbientCubeLevel(owner.sky_texture_env_name.c_str());
	}

	if(owner.clouds_texture_name.size())
		clouds_texture.create(owner.clouds_texture_name.c_str());
}

void dxEnvDescriptorRender::OnDeviceDestroy() 
{
	sky_texture.destroy();
	sky_texture_env.destroy();
	clouds_texture.destroy();
}

dxEnvironmentRender::dxEnvironmentRender() 
{
	tsky0 = DEV->_CreateTexture(r2_T_sky0);
	tsky1 = DEV->_CreateTexture(r2_T_sky1);
}

static bool IsSkySync = false;

void dxEnvironmentRender::Sync()
{
	if (g_pGamePersistent && g_pGamePersistent->pEnvironment && RFrame.HasEnv())
	{
		IsSkySync = true;
		g_pGamePersistent->pEnvironment->m_pRender->OnFrame(*g_pGamePersistent->pEnvironment);
		IsSkySync = false;
	}
}

void dxEnvironmentRender::OnFrame(CEnvironment& env) {
	// The game's frame calls this while the frame before is drawn with the textures it would swap
	if (RenderThread::IsPipelined() && g_AppInfo.IsPrimaryThread() && !IsSkySync)
	{
		return;
	}

	PROF_EVENT("EnvironmentRender::OnFrame");
	dxEnvDescriptorMixerRender& mixRen = *(dxEnvDescriptorMixerRender*)&*RFrame.Env().m_pDescriptorMixer;

	// The two skies of the current blend and their ambient cubes, for every pass of either frame that names them
	CTexture* const Aliases[2][2] = { { tsky0._get(), tsky1._get() }, { RImplementation.Target->t_envmap_0._get(), RImplementation.Target->t_envmap_1._get() } };
	STextureList* const Lists[2] = { &mixRen.sky_r_textures, &mixRen.sky_r_textures_env };

	for (u32 List = 0; List < 2; ++List)
	{
		for (u32 Index = 0; Index < 2 && Index < Lists[List]->size(); ++Index)
		{
			const ref_texture& Source = (*Lists[List])[Index].second;
			IRHISurface* Surface = Source ? Source->surface_get() : nullptr;
			Aliases[List][Index]->surface_set(Surface);
			_RELEASE(Surface);
		}
	}
}

void dxEnvironmentRender::OnLoad()
{
}

void dxEnvironmentRender::OnUnload() 
{
}

void dxEnvironmentRender::RenderSky(CEnvironment& env) 
{
	PROF_EVENT("EnvironmentRender::RenderSky");
	GfxCmdList& Cmd = RImplementation.Imm();

	if(env.bNeed_re_create_env)
	{
		OnDeviceDestroy();
		OnDeviceCreate();

		OnFrame(env);

		env.bNeed_re_create_env = false;
	}

	::Render->rmFar();

	dxEnvDescriptorMixerRender& mixRen = *(dxEnvDescriptorMixerRender*)&*RFrame.Env().m_pDescriptorMixer;

	// draw sky box
	static Fmatrix mSky = Fidentity;
	Fmatrix mSkyOld = mSky;

	mSky.rotateY(RFrame.Env().sky_rotation);

	u32 v_offset;
	u32 C = color_rgba(iFloor(RFrame.Env().sky_color.x * 255.f), iFloor(RFrame.Env().sky_color.y * 255.f), iFloor(RFrame.Env().sky_color.z * 255.f), iFloor(RFrame.Env().weight * 255.f));

	// The faces' vertices one after another: the stream draws no index buffer
	DynamicGeometry& Stream = RImplementation.Dynamic;
	PackedVertex* pv = (PackedVertex*)Stream.Lock(20 * 3, EVertexFormat::Packed, v_offset);

	for (u32 Index = 0; Index < 20 * 3; ++Index)
	{
		pv[Index] = { hbox_verts[hbox_faces[Index]], 0, 0, 0, C, 0 };
	}

	Stream.Unlock(20 * 3);

	// Render
	{
		CameraCenteredView View(Cmd);

		Cmd.SetObjectTransform(mSky, mSkyOld);

		Cmd.SetGeometry(sh_2geom);
		Cmd.SetPass(sh_2sky);

		Cmd.SetTextures(&mixRen.sky_r_textures);

		Stream.Draw(Cmd, ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 20 * 3, v_offset);
	}

	dxLensFlareRender::Draw(true, false, false);

	// Sun
	::Render->rmNormal();
}

void dxEnvironmentRender::RenderClouds(CEnvironment& env)
{
	PROF_EVENT("EnvironmentRender::RenderClouds");
	GfxCmdList& Cmd = RImplementation.Imm();
	::Render->rmFar();

	Fmatrix mXFORM, mScale;
	mScale.scale(10, 0.4f, 10);
	mXFORM.rotateY(RFrame.Env().sky_rotation);
	mXFORM.mulB_43(mScale);

	Fvector wd0, wd1;
	Fvector4 wind_dir;
	wd0.setHP(PI_DIV_4, 0);
	wd1.setHP(PI_DIV_4 + PI_DIV_8, 0);
	wind_dir.set(wd0.x, wd0.z, wd1.x, wd1.z).mul(0.5f).add(0.5f).mul(255.f);
	u32 v_offset;
	u32 C0 = color_rgba(iFloor(wind_dir.x), iFloor(wind_dir.y), iFloor(wind_dir.w), iFloor(wind_dir.z));
	u32 C1 = color_rgba(iFloor(RFrame.Env().clouds_color.x * 255.f), iFloor(RFrame.Env().clouds_color.y * 255.f), iFloor(RFrame.Env().clouds_color.z * 255.f), iFloor(RFrame.Env().clouds_color.w * 255.f));

	// The triangles' vertices one after another: the stream draws no index buffer
	DynamicGeometry& Stream = RImplementation.Dynamic;
	const u32 Count = u32(env.CloudsIndices.size());
	PackedVertex* pv = (PackedVertex*)Stream.Lock(Count, EVertexFormat::Packed, v_offset);

	for (u32 Index = 0; Index < Count; ++Index)
	{
		pv[Index] = { env.CloudsVerts[env.CloudsIndices[Index]], 0, 0, 0, C0, C1 };
	}

	Stream.Unlock(Count);

	// Render
	CameraCenteredView View(Cmd);

	Cmd.SetObjectTransform(mXFORM);

	Cmd.SetGeometry(clouds_geom);
	Cmd.SetPass(clouds_sh);
	dxEnvDescriptorMixerRender& mixRen = *(dxEnvDescriptorMixerRender*)&*RFrame.Env().m_pDescriptorMixer;
	Cmd.SetTextures(&mixRen.clouds_r_textures);
	Stream.Draw(Cmd, ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Count, v_offset);

	::Render->rmNormal();
}

void dxEnvironmentRender::OnDeviceCreate() {
	sh_2sky.create(&m_b_skybox, "skybox_2t");
	sh_2geom.create(EGeomSource::Ring);

	clouds_sh.create("clouds", "null");
	clouds_geom.create(EGeomSource::Ring);
}

void dxEnvironmentRender::OnDeviceDestroy() {
	tsky0->surface_set(nullptr);
	tsky1->surface_set(nullptr);

	sh_2sky.destroy();
	sh_2geom.destroy();
	clouds_sh.destroy();
	clouds_geom.destroy();
}