#include "stdafx.h"
#include "UITextureViewer.h"

CUITextureViewer::CUITextureViewer()
{
	Texture = new CTexture;
}

CUITextureViewer::~CUITextureViewer()
{
}

void CUITextureViewer::Draw()
{
	if (!bOpen)
	{
		return;
	}

	ImGui::Begin("Texture Viewer", &bOpen);

	ImVec2 buttonSize(30, 30);

	auto DrawButtonLambda = [&](const char* label, u8 bit, ImVec4 color)
	{
		bool active = (ChannelMask & bit) != 0;
		ImVec4 col = active ? color : ImVec4(0.3f, 0.3f, 0.3f, 1.0f);
		ImVec4 textCol = active ? ImVec4(0, 0, 0, 1) : ImVec4(1, 1, 1, 1);

		ImGui::PushStyleColor(ImGuiCol_Button, col);
		ImGui::PushStyleColor(ImGuiCol_Text, textCol);
		if (ImGui::Button(label, buttonSize))
		{
			if (active) ChannelMask &= ~bit;
			else        ChannelMask |= bit;
			UpdateTexture();
		}
		ImGui::PopStyleColor(2);

		ImGui::SameLine(0.0f, 0.0f);
	};

	DrawButtonLambda("R", Channel_R, ImVec4(1, 0, 0, 1));
	DrawButtonLambda("G", Channel_G, ImVec4(0, 1, 0, 1));
	DrawButtonLambda("B", Channel_B, ImVec4(0, 0, 1, 1));
	DrawButtonLambda("A", Channel_A, ImVec4(1, 1, 1, 1));

	ImVec4 col = GrayMode ? ImVec4(0.7f, 0.7f, 0.7f, 1.0f) : ImVec4(0.3f, 0.3f, 0.3f, 1.0f);
	ImGui::SameLine(0.0f, 10.0f);
	ImGui::PushStyleColor(ImGuiCol_Button, col);
	if (ImGui::Button("Gray", buttonSize))
	{
		GrayMode = !GrayMode;
		UpdateTexture();
	}
	ImGui::PopStyleColor();
	ImGui::Separator();

	if (Texture)
	{
		ImVec2 avail = ImGui::GetContentRegionAvail();
		ImGui::Image((void*)Texture->pSurface, avail);
	}

	ImGui::End();
}


void CUITextureViewer::LoadFromFile(const xr_path& File)
{
	SrcData = DXTUtils::GitPixels(File.xstring().c_str());

	IDirect3DTexture9* pTex = nullptr;
	HRESULT hr = RDevice->CreateTexture
	(
		(UINT)SrcData.W,
		(UINT)SrcData.H,
		1,
		0,
		D3DFMT_A8R8G8B8,
		D3DPOOL_MANAGED,
		&pTex,
		nullptr
	);

	if (FAILED(hr) || !pTex)
	{
		Msg("! Failed to create texture for viewer");
		return;
	}

	Texture->pSurface = pTex;
	UpdateTexture();
}

void CUITextureViewer::UpdateTexture()
{
	if (!Texture)
	{
		return;
	}

	IDirect3DTexture9* tex = (IDirect3DTexture9*)Texture->pSurface;
	if (!tex)
	{
		return;
	}

	D3DLOCKED_RECT rect;
	if (FAILED(tex->LockRect(0, &rect, nullptr, 0)))
	{
		return;
	}

	uint8_t* dst = (uint8_t*)rect.pBits;
	bool fullMask = (ChannelMask == (Channel_R | Channel_G | Channel_B | Channel_A));

	for (size_t y = 0; y < SrcData.H; y++)
	{
		uint8_t* row = dst + y * rect.Pitch;
		for (size_t x = 0; x < SrcData.W; x++)
		{
			size_t idx = (y * SrcData.W + x) * 4;
			uint8_t r = SrcData.P[idx + 0];
			uint8_t g = SrcData.P[idx + 1];
			uint8_t b = SrcData.P[idx + 2];
			uint8_t a = SrcData.P[idx + 3];

			uint8_t R, G, B, A;

			if (fullMask)
			{
				R = b;
				G = g;
				B = r;
				A = a;
			}

			else if (GrayMode)
			{
				uint8_t v = 0;
				if (ChannelMask & Channel_R) v = b;
				if (ChannelMask & Channel_G) v = g;
				if (ChannelMask & Channel_B) v = r;
				if (ChannelMask & Channel_A) v = a;

				R = G = B = v;
				A = 255;
			}
			else
			{
				R = (ChannelMask & Channel_R) ? b : 0;
				G = (ChannelMask & Channel_G) ? g : 0;
				B = (ChannelMask & Channel_B) ? r : 0;
				A = (ChannelMask & Channel_A) ? a : 255;
			}

			row[x * 4 + 0] = B;
			row[x * 4 + 1] = G;
			row[x * 4 + 2] = R;
			row[x * 4 + 3] = A;
		}
	}

	tex->UnlockRect(0);
}
