#include "StdAfx.h"
#include "BuildTextureLoader.h"

namespace BuildTextureDds
{
	constexpr u32 MakeFourCC(char A, char B, char C, char D)
	{
		return u32(u8(A)) | (u32(u8(B)) << 8) | (u32(u8(C)) << 16) | (u32(u8(D)) << 24);
	}

	constexpr u32 DdsMagic = MakeFourCC('D', 'D', 'S', ' ');
	constexpr u32 DdsHeaderSize = 124;
	constexpr u32 DdsHeaderDx10Size = 20;

	// Offsets inside the file (magic included)
	constexpr u32 DdsOffsetHeight = 12;
	constexpr u32 DdsOffsetWidth = 16;
	constexpr u32 DdsOffsetPfFlags = 80;
	constexpr u32 DdsOffsetPfFourCC = 84;
	constexpr u32 DdsOffsetPfAlphaMask = 104;
	constexpr u32 DdsOffsetDx10Format = 128;

	constexpr u32 DdpfAlphaPixels = 0x1;
	constexpr u32 DdpfAlpha = 0x2;
	constexpr u32 DdpfFourCC = 0x4;

	u32 ReadU32At(IReader& F, u32 Offset)
	{
		F.seek(Offset);
		return F.r_u32();
	}

	bool DxgiFormatHasAlpha(u32 Format, bool& OneBit)
	{
		OneBit = false;
		switch (Format)
		{
		case 70: // BC1_TYPELESS
		case 71: // BC1_UNORM
		case 72: // BC1_UNORM_SRGB
			OneBit = true;
			return true;
		case 2:  // R32G32B32A32_FLOAT
		case 10: // R16G16B16A16_FLOAT
		case 11: // R16G16B16A16_UNORM
		case 24: // R10G10B10A2_TYPELESS
		case 25: // R10G10B10A2_UNORM
		case 27: // R8G8B8A8_TYPELESS
		case 28: // R8G8B8A8_UNORM
		case 29: // R8G8B8A8_UNORM_SRGB
		case 65: // A8_UNORM
		case 73: // BC2_TYPELESS
		case 74: // BC2_UNORM
		case 75: // BC2_UNORM_SRGB
		case 76: // BC3_TYPELESS
		case 77: // BC3_UNORM
		case 78: // BC3_UNORM_SRGB
		case 86: // B5G5R5A1_UNORM
		case 87: // B8G8R8A8_UNORM
		case 90: // B8G8R8A8_TYPELESS
		case 91: // B8G8R8A8_UNORM_SRGB
		case 97: // BC7_TYPELESS
		case 98: // BC7_UNORM
		case 99: // BC7_UNORM_SRGB
		case 115: // B4G4R4A4_UNORM
			return true;
		default:
			return false;
		}
	}
}

CBuildTextureLoader::CBuildTextureLoader(const SOptions& InOptions) :
	Options(InOptions)
{
}

void CBuildTextureLoader::Load(b_BuildTexture& BT) const
{
	BT.pSurface.Clear();

	char* Name = BT.name;
	if (char* Ext = strchr(Name, '.'))
	{
		*Ext = 0;
	}
	xr_strlwr(Name);

	BT.SetHasSurface(false);

	if (0 == xr_strcmp(Name, "level_lods"))
	{
		// HACK: merged lod textures
		BT.dwWidth = 1024;
		BT.dwHeight = 1024;
		BT.bHasAlpha = Options.LevelLodsHasAlpha;
		return;
	}

	if (ReadThm(BT, Name))
	{
		LoadWithThm(BT, Name);
	}
	else
	{
		LoadWithoutThm(BT, Name);
	}
}

bool CBuildTextureLoader::ReadThm(b_BuildTexture& BT, const char* Name) const
{
	string_path ThmName;
	xr_strconcat(ThmName, Name, ".thm");

	IReader* Thm = FS.r_open("$game_textures$", ThmName);
	if (!Thm)
	{
		return false;
	}

	u32 Version = 0;
	R_ASSERT2(Thm->r_chunk(THM_CHUNK_VERSION, &Version), ThmName);
	R_ASSERT2(Thm->find_chunk(THM_CHUNK_TEXTUREPARAM), ThmName);

	Thm->r(&BT.THM.fmt, sizeof(STextureParams::ETFormat));
	BT.THM.flags.assign(Thm->r_u32());
	BT.THM.border_color = Thm->r_u32();
	BT.THM.fade_color = Thm->r_u32();
	BT.THM.fade_amount = Thm->r_u32();
	BT.THM.mip_filter = Thm->r_u32();
	BT.THM.width = Thm->r_u32();
	BT.THM.height = Thm->r_u32();

	FS.r_close(Thm);
	return true;
}

void CBuildTextureLoader::LoadWithThm(b_BuildTexture& BT, const char* Name) const
{
	BT.dwWidth = BT.THM.width;
	BT.dwHeight = BT.THM.height;
	BT.bHasAlpha = BT.THM.HasAlphaChannel();
	BT.SetHasSurface(false);

	if (IsLodTexture(Name))
	{
		return;
	}

	// Load the surface only if it has an alpha channel or the "implicit lighting" flag
	if (!BT.bHasAlpha && !BT.THM.flags.test(STextureParams::flImplicitLighted))
	{
		return;
	}

	clMsg("- loading: %s W[%u] H[%u]", Name, BT.dwWidth, BT.dwHeight);

	IReader* Dds = OpenDds(Name);
	const bool IsLoaded = Dds && LoadSurface(BT, *Dds);
	if (Dds)
	{
		FS.r_close(Dds);
	}

	if (!IsLoaded)
	{
		Msg("! cannot find dds texture: %s", Name);
		if (Options.DropAlphaWithoutSurface)
		{
			BT.bHasAlpha = false;
		}
		return;
	}

	if (BT.pSurface.GetWidth() != BT.dwWidth || BT.pSurface.GetHeight() != BT.dwHeight)
	{
		Msg("! THM doesn't correspond to the texture: %dx%d -> %dx%d", BT.dwWidth, BT.dwHeight, BT.pSurface.GetWidth(), BT.pSurface.GetHeight());
		BT.dwWidth = BT.THM.width = u32(BT.pSurface.GetWidth());
		BT.dwHeight = BT.THM.height = u32(BT.pSurface.GetHeight());
	}
}

void CBuildTextureLoader::LoadWithoutThm(b_BuildTexture& BT, const char* Name) const
{

	// Defaults when nothing can be read
	BT.dwWidth = BT.THM.width = 1024;
	BT.dwHeight = BT.THM.height = 1024;
	BT.bHasAlpha = false;
	BT.THM.fmt = STextureParams::tfDXT1;
	BT.THM.flags.zero();

	IReader* Dds = OpenDds(Name);
	if (!Dds)
	{
		Msg("! cannot find thm and dds texture: %s", Name);
		return;
	}

	SDdsInfo Info;
	if (!ParseDdsHeader(*Dds, Info))
	{
		Msg("! cannot find thm, dds texture is invalid: %s", Name);
		FS.r_close(Dds);
		return;
	}

	BT.dwWidth = BT.THM.width = Info.Width;
	BT.dwHeight = BT.THM.height = Info.Height;

	// Alpha usage is decided by the real pixels, so the surface is loaded for any alpha-capable format
	if (Info.Alpha != EAlphaKind::None && !IsLodTexture(Name) && LoadSurface(BT, *Dds))
	{
		BT.dwWidth = BT.THM.width = u32(BT.pSurface.GetWidth());
		BT.dwHeight = BT.THM.height = u32(BT.pSurface.GetHeight());

		if (SurfaceUsesAlpha(BT))
		{
			BT.bHasAlpha = true;
			BT.THM.fmt = Info.Alpha == EAlphaKind::OneBit ? STextureParams::tfADXT1 : STextureParams::tfDXT5;
		}
		else
		{
			BT.pSurface.Clear();
			BT.SetHasSurface(false);
		}
	}
	else if (Info.Alpha != EAlphaKind::None && IsLodTexture(Name))
	{
		// LOD surfaces are never sampled, trust the format
		BT.bHasAlpha = true;
		BT.THM.fmt = Info.Alpha == EAlphaKind::OneBit ? STextureParams::tfADXT1 : STextureParams::tfDXT5;
	}

	FS.r_close(Dds);

	Msg("! cannot find thm: %s, taken from dds: %ux%u, alpha: %s, implicit lighting is unknown", Name, BT.dwWidth, BT.dwHeight, BT.bHasAlpha ? "yes" : "no");
}

bool CBuildTextureLoader::LoadSurface(b_BuildTexture& BT, IReader& Dds) const
{
	if (!BT.pSurface.LoadFromMemory(Dds.pointer(), size_t(Dds.length())))
	{
		BT.pSurface.Clear();
		BT.SetHasSurface(false);
		return false;
	}

	BT.pSurface.ClearMipLevels();
	BT.pSurface.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);
	BT.pSurface.SwapRB();
	BT.SetHasSurface(true);
	return true;
}

IReader* CBuildTextureLoader::OpenDds(const char* Name)
{
	string_path DdsName;
	xr_strconcat(DdsName, Name, ".dds");
	return FS.r_open("$game_textures$", DdsName);
}

bool CBuildTextureLoader::ParseDdsHeader(IReader& Dds, SDdsInfo& Info)
{
	if (u32(Dds.length()) < 4 + BuildTextureDds::DdsHeaderSize)
	{
		return false;
	}

	Dds.seek(0);
	if (Dds.r_u32() != BuildTextureDds::DdsMagic || Dds.r_u32() != BuildTextureDds::DdsHeaderSize)
	{
		return false;
	}

	Info.Height = BuildTextureDds::ReadU32At(Dds, BuildTextureDds::DdsOffsetHeight);
	Info.Width = BuildTextureDds::ReadU32At(Dds, BuildTextureDds::DdsOffsetWidth);

	const u32 Flags = BuildTextureDds::ReadU32At(Dds, BuildTextureDds::DdsOffsetPfFlags);
	const u32 FourCC = BuildTextureDds::ReadU32At(Dds, BuildTextureDds::DdsOffsetPfFourCC);
	const u32 AlphaMask = BuildTextureDds::ReadU32At(Dds, BuildTextureDds::DdsOffsetPfAlphaMask);
	Dds.seek(0);

	Info.Alpha = EAlphaKind::None;
	if (Flags & BuildTextureDds::DdpfFourCC)
	{
		if (FourCC == BuildTextureDds::MakeFourCC('D', 'X', 'T', '1'))
		{
			Info.Alpha = EAlphaKind::OneBit;
		}
		else if (FourCC == BuildTextureDds::MakeFourCC('D', 'X', 'T', '2') || FourCC == BuildTextureDds::MakeFourCC('D', 'X', 'T', '3') || FourCC == BuildTextureDds::MakeFourCC('D', 'X', 'T', '4') || FourCC == BuildTextureDds::MakeFourCC('D', 'X', 'T', '5'))
		{
			Info.Alpha = EAlphaKind::Full;
		}
		else if (FourCC == BuildTextureDds::MakeFourCC('D', 'X', '1', '0'))
		{
			if (u32(Dds.length()) < 4 + BuildTextureDds::DdsHeaderSize + BuildTextureDds::DdsHeaderDx10Size)
			{
				return false;
			}

			bool OneBit = false;
			if (BuildTextureDds::DxgiFormatHasAlpha(BuildTextureDds::ReadU32At(Dds, BuildTextureDds::DdsOffsetDx10Format), OneBit))
			{
				Info.Alpha = OneBit ? EAlphaKind::OneBit : EAlphaKind::Full;
			}
			Dds.seek(0);
		}
	}
	else if ((Flags & (BuildTextureDds::DdpfAlphaPixels | BuildTextureDds::DdpfAlpha)) && AlphaMask != 0)
	{
		Info.Alpha = EAlphaKind::Full;
	}

	return Info.Width != 0 && Info.Height != 0;
}

bool CBuildTextureLoader::SurfaceUsesAlpha(const b_BuildTexture& BT)
{
	const u32* Texels = static_cast<const u32*>(*BT.pSurface);
	const size_t Count = BT.pSurface.GetWidth() * BT.pSurface.GetHeight();
	for (size_t i = 0; i < Count; i++)
	{
		if ((Texels[i] >> 24) != 0xFF)
		{
			return true;
		}
	}
	return false;
}

bool CBuildTextureLoader::IsLodTexture(const char* Name)
{
	return 0 == strncmp(Name, "lod\\", 4);
}
