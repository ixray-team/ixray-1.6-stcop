//---------------------------------------------------------------------------
#include "stdafx.h"
#include "SHPreviewObject.h"
#include "../xrECore/Editor/EditMesh.h"
#include <algorithm>

static DXGI_FORMAT CompTypeToFormat(D3D_REGISTER_COMPONENT_TYPE e, UINT mask, u32& outSize)
{
	UINT comps = 0;
	for (UINT b = 0; b < 4; b++)
		if (mask & (1u << b)) comps++;
	if (comps == 0) comps = 1;
	outSize = comps * 4; // xr shaders use 32-bit vertex components

	DXGI_FORMAT fmt = DXGI_FORMAT_UNKNOWN;
	switch (e)
	{
	case D3D_REGISTER_COMPONENT_FLOAT32:
		switch (comps) { case 1: fmt = DXGI_FORMAT_R32_FLOAT; break; case 2: fmt = DXGI_FORMAT_R32G32_FLOAT; break; case 3: fmt = DXGI_FORMAT_R32G32B32_FLOAT; break; default: fmt = DXGI_FORMAT_R32G32B32A32_FLOAT; break; }
		break;
	case D3D_REGISTER_COMPONENT_UINT32:
		switch (comps) { case 1: fmt = DXGI_FORMAT_R32_UINT; break; case 2: fmt = DXGI_FORMAT_R32G32_UINT; break; case 3: fmt = DXGI_FORMAT_R32G32B32_UINT; break; default: fmt = DXGI_FORMAT_R32G32B32A32_UINT; break; }
		break;
	case D3D_REGISTER_COMPONENT_SINT32:
		switch (comps) { case 1: fmt = DXGI_FORMAT_R32_SINT; break; case 2: fmt = DXGI_FORMAT_R32G32_SINT; break; case 3: fmt = DXGI_FORMAT_R32G32B32_SINT; break; default: fmt = DXGI_FORMAT_R32G32B32A32_SINT; break; }
		break;
	default:
		switch (comps) { case 1: fmt = DXGI_FORMAT_R32_FLOAT; break; case 2: fmt = DXGI_FORMAT_R32G32_FLOAT; break; case 3: fmt = DXGI_FORMAT_R32G32B32_FLOAT; break; default: fmt = DXGI_FORMAT_R32G32B32A32_FLOAT; break; }
		break;
	}
	return fmt;
}

static xr_string DeclFmtName(ERHI_FORMAT f)
{
	auto name = magic_enum::enum_name(f);
	return name.empty() ? xr_string("?") : xr_string(name.data(), name.size());
}

static void LogDeclaration(const char* title, const xr_vector<RHIInputElementDesc>& d)
{
	Log(title);
	for (const auto& e : d)
	{
		xr_string l = "    ";
		l += xr_string(e.SemanticName ? e.SemanticName : "?");
		l += xr_string::ToString(e.SemanticIndex) +
			" fmt=" + DeclFmtName(e.Format) +
			 " off=" + xr_string::ToString(e.AlignedByteOffset) +
			 " slot=" + xr_string::ToString(e.InputSlot);
		Log(l.c_str());
	}
}

bool CPreviewObject::EnumerateVSInputs(ID3DBlob* signature, xr_vector<SVSInput>& out)
{
	if (!signature) return false;
	ID3D11ShaderReflection* reflect = nullptr;
	HRESULT hr = D3DReflect(signature->GetBufferPointer(), signature->GetBufferSize(), IID_ID3D11ShaderReflection, (void**)&reflect);
	if (FAILED(hr) || !reflect) return false;

	D3D11_SHADER_DESC sd;
	if (FAILED(reflect->GetDesc(&sd)))
	{
		reflect->Release();
		return false;
	}

	for (UINT i = 0; i < sd.InputParameters; i++)
	{
		D3D11_SIGNATURE_PARAMETER_DESC spd;
		if (FAILED(reflect->GetInputParameterDesc(i, &spd)))
			continue;

		SVSInput in;
		in.semantic = spd.SemanticName;
		in.index = spd.SemanticIndex;
		in.format = (ERHI_FORMAT)CompTypeToFormat(spd.ComponentType, spd.Mask, in.byteSize);
		out.push_back(in);
	}
	reflect->Release();
	return true;
}

CPreviewObject::CPreviewObject()
	: Object(nullptr)
	, VSSignature(nullptr)
{
}

CPreviewObject::~CPreviewObject()
{
	VSSignature = nullptr;
}

void CPreviewObject::SetVSSignature(ID3DBlob* sig)
{
	VSSignature = sig;
	Required.clear();
	EnumerateVSInputs(VSSignature, Required);
}

void CPreviewObject::SetSourceDeclaration(const xr_vector<RHIInputElementDesc>& src)
{
	SourceDecl = src;
}

const xr_vector<RHIInputElementDesc>& CPreviewObject::BuildCompatibleDeclaration(bool bLog)
{
	CompatibleDecl.clear();
	SemanticStorage.clear();
	// Reserve so push_back() never reallocates: SemanticName pointers stored in
	// CompatibleDecl must stay valid for the whole declaration's lifetime.
	SemanticStorage.reserve(Required.size());

	if (Required.empty())
	{
		CompatibleDecl = SourceDecl;
		if (bLog)
		{
			Log("=== CPreviewObject: VS requires no extra inputs; using source declaration as-is ===");
			LogDeclaration("  source/original declaration:", OriginalDecl);
		}
		return CompatibleDecl;
	}

	// Working copy; SemanticName from source points at stable literals.
	xr_vector<RHIInputElementDesc> src = SourceDecl;
	xr_vector<bool> used(src.size(), false);

	// The editor mesh layout packs the 2nd texcoord as B8G8R8A8_UNORM (a legacy
	// 'ind' color), not a float channel. Never bind a shader input there or
	// CreateInputLayout rejects the format (E_INVALIDARG); its data is also
	// meaningless for previewing.
	auto IsUsable = [&](size_t i) { return src[i].Format != ERHI_FORMAT::B8G8R8A8_UNORM; };

	for (auto& req : Required)
	{
		RHIInputElementDesc d;
		d.SemanticName = nullptr;
		d.SemanticIndex = req.index;
		d.Format = req.format;
		d.InputSlot = 0;
		d.AlignedByteOffset = RHI_APPEND_ALIGNED_ELEMENT;
		d.InputSlotClass = ERHI_INPUT_CLASSIFICATION::VERTEX_DATA;
		d.InstanceDataStepRate = 0;

		// No source channels at all: cannot map this input. Skip it so the VS
		// receives the default value instead of reading out of bounds.
		if (src.empty())
			continue;

		bool assigned = false;
		const RHIInputElementDesc* srcEl = nullptr;

		// 1) exact semantic + index
		for (size_t i = 0; i < src.size() && !assigned; i++)
		{
			if (!used[i] && IsUsable(i) && _stricmp(src[i].SemanticName, req.semantic.c_str()) == 0 && src[i].SemanticIndex == req.index)
			{
				srcEl = &src[i];
				d.SemanticName = src[i].SemanticName;
				d.Format = src[i].Format;
				d.AlignedByteOffset = src[i].AlignedByteOffset;
				d.InputSlot = src[i].InputSlot;
				used[i] = true;
				assigned = true;
			}
		}
		// 2) same semantic, different index (reuse channel, rebind index)
		for (size_t i = 0; i < src.size() && !assigned; i++)
		{
			if (!used[i] && IsUsable(i) && _stricmp(src[i].SemanticName, req.semantic.c_str()) == 0)
			{
				srcEl = &src[i];
				d.SemanticName = src[i].SemanticName;
				d.Format = src[i].Format;
				d.AlignedByteOffset = src[i].AlignedByteOffset;
				d.InputSlot = src[i].InputSlot;
				used[i] = true;
				assigned = true;
			}
		}
		// 3) reuse a free source channel, overriding its semantic to the
		//    required one. Prefer a channel from the same semantic family so
		//    lighting/uv data is not hijacked by an unrelated attribute
		//    (e.g. COLOR must not steal NORMAL/TANGENT); fall back to the
		//    first free channel.
		bool reqColor = (_stricmp(req.semantic.c_str(), "COLOR") == 0);
		bool reqTex = (_stricmp(req.semantic.c_str(), "TEXCOORD") == 0);
		bool reqNormal = (_stricmp(req.semantic.c_str(), "NORMAL") == 0) ||
						 (_stricmp(req.semantic.c_str(), "TANGENT") == 0) ||
						 (_stricmp(req.semantic.c_str(), "BINORMAL") == 0);
		auto sameFamily = [&](const char* name) {
			bool sf_color = (_stricmp(name, "COLOR") == 0);
			bool sf_tex = (_stricmp(name, "TEXCOORD") == 0);
			bool sn = (_stricmp(name, "NORMAL") == 0) || (_stricmp(name, "TANGENT") == 0) ||
					  (_stricmp(name, "BINORMAL") == 0);
			return (reqColor && sf_color) || (reqTex && sf_tex) || (reqNormal && sn);
		};
		for (size_t i = 0; i < src.size() && !assigned; i++)
		{
			if (!used[i] && IsUsable(i) && sameFamily(src[i].SemanticName))
			{
				srcEl = &src[i];
				SemanticStorage.push_back(req.semantic);
				d.SemanticName = SemanticStorage.back().c_str();
				d.Format = src[i].Format;
				d.AlignedByteOffset = src[i].AlignedByteOffset;
				d.InputSlot = src[i].InputSlot;
				used[i] = true;
				assigned = true;
			}
		}
		for (size_t i = 0; i < src.size() && !assigned; i++)
		{
			if (!used[i] && IsUsable(i))
			{
				srcEl = &src[i];
				SemanticStorage.push_back(req.semantic);
				d.SemanticName = SemanticStorage.back().c_str();
				d.Format = src[i].Format;
				d.AlignedByteOffset = src[i].AlignedByteOffset;
				d.InputSlot = src[i].InputSlot;
				used[i] = true;
				assigned = true;
			}
		}
		// 4) no usable source channel left in the right family: bind to an
		//    existing float channel so the VS reads valid data instead of
		//    garbage/NaN from a non-existent slot. Prefer a bounded channel
		//    (NORMAL/TANGENT/BINORMAL) over POSITION: binding e.g. COLOR to
		//    POSITION would feed the shader huge vertex positions and produce
		//    spikes/displacement artifacts.
		if (!assigned)
		{
			size_t bind = (size_t)-1;
			auto pick = [&](const char* name) -> size_t {
				for (size_t i = 0; i < src.size(); i++)
					if (IsUsable(i) && _stricmp(src[i].SemanticName, name) == 0 && src[i].SemanticIndex == 0)
						return i;
				return (size_t)-1;
			};
			if (bind == (size_t)-1) bind = pick("NORMAL");
			if (bind == (size_t)-1) bind = pick("TANGENT");
			if (bind == (size_t)-1) bind = pick("BINORMAL");
			if (bind == (size_t)-1) bind = pick("TEXCOORD");
			if (bind == (size_t)-1)
			{
				for (size_t i = 0; i < src.size(); i++)
					if (IsUsable(i)) { bind = i; break; }
			}
			if (bind == (size_t)-1) bind = pick("POSITION");
			if (bind == (size_t)-1)
			{
				for (size_t i = 0; i < src.size(); i++)
					if (IsUsable(i)) { bind = i; break; }
			}
			if (bind == (size_t)-1 && !src.empty()) bind = 0;
			srcEl = &src[bind];
			SemanticStorage.push_back(req.semantic);
			d.SemanticName = SemanticStorage.back().c_str();
			d.Format = src[bind].Format;
			d.AlignedByteOffset = src[bind].AlignedByteOffset;
			d.InputSlot = src[bind].InputSlot;
			assigned = true;
		}

		CompatibleDecl.push_back(d);

		if (bLog)
		{
			xr_string tag;
			if (srcEl && _stricmp(srcEl->SemanticName, req.semantic.c_str()) == 0 && srcEl->SemanticIndex == req.index)
				tag = "EXACT";
			else if (srcEl && srcEl->AlignedByteOffset == 0 && !(_stricmp(req.semantic.c_str(), "POSITION") == 0 && req.index == 0))
				tag = "FALLBACK->POSITION";
			else
				tag = "REMAP";
			xr_string l = xr_string("    [") + tag + "] REQ " + req.semantic + xr_string::ToString(req.index) +
				" (fmt=" + DeclFmtName(req.format) + ")  ->  " +
						  xr_string(d.SemanticName ? d.SemanticName : "?") + xr_string::ToString(d.SemanticIndex) +
						  " off=" + xr_string::ToString(d.AlignedByteOffset) +
				" fmt=" + DeclFmtName(d.Format) +
				(srcEl ? xr_string("  (src: ") + srcEl->SemanticName + xr_string::ToString(srcEl->SemanticIndex) + ")" : xr_string(""));
			Log(l.c_str());
		}
	}

	// D3D11 requires input elements that share an input slot to be listed in
	// ascending AlignedByteOffset order. REMAP/EXACT picks reuse a source
	// channel's offset, which can place a later semantic before an earlier one
	// (e.g. COLOR0 @16 after TEXCOORD0 @64), making CreateInputLayout fail with
	// E_INVALIDARG. Order by offset; CreateInputLayout matches by semantic, so
	// reordering is harmless.
	std::sort(CompatibleDecl.begin(), CompatibleDecl.end(),
		[](const RHIInputElementDesc& a, const RHIInputElementDesc& b)
		{ return a.AlignedByteOffset < b.AlignedByteOffset; });

	if (bLog)
		LogDeclaration("  patched/compatible declaration:", CompatibleDecl);
	return CompatibleDecl;
}

void CPreviewObject::GetMissingSemantics(xr_vector<SVSInput>& missing) const
{
	for (auto& req : Required)
	{
		bool found = false;
		for (auto& s : SourceDecl)
		{
			if (_stricmp(s.SemanticName, req.semantic.c_str()) == 0 && s.SemanticIndex == req.index)
			{
				found = true;
				break;
			}
		}
		if (!found) missing.push_back(req);
	}
}

void CPreviewObject::ApplyToDeclaration(SDeclaration* dcl, bool bLog)
{
	if (!dcl) return;
	dcl->dx10_dcl_code = BuildCompatibleDeclaration(bLog);
	// force a fresh input layout for the (possibly) new declaration
	dcl->vs_to_layout.clear();
}

bool CPreviewObject::Apply(CEditableObject* o)
{
	if (o) Object = o;
	if (!Object) return false;

	Log("=== CPreviewObject::Apply: declaration diagnosis ===");
	{
		xr_string s = xr_string("  VS required inputs (") + xr_string::ToString((u32)Required.size()) + "):";
		Log(s.c_str());
		for (auto& r : Required)
		{
			xr_string l = "    REQ " + r.semantic + xr_string::ToString(r.index) +
				" fmt=" + DeclFmtName(r.format);
			Log(l.c_str());
		}
	}

	bool bFirst = true;
	// Map every VS input from the pristine declaration (dcl->dx10_dcl_code_pristine),
	// which is captured once at creation and never patched. ApplyToDeclaration
	// permanently overwrites dcl->dx10_dcl_code to satisfy the active VS, so using
	// that as the source on the next shader switch would drift the channel set and
	// produce bogus mappings (e.g. COLOR0 "found" at a normal's offset).
	for (auto it = Object->FirstMesh(); it != Object->LastMesh(); ++it)
	{
		CEditableMesh* M = *it;
		if (!M) continue;
		SDeclaration* dcl = GetMeshDeclaration(M);
		if (dcl)
		{
			OriginalDecl = dcl->dx10_dcl_code_pristine;
			if (bFirst)
				LogDeclaration("  source/original declaration:", OriginalDecl);
			SetSourceDeclaration(OriginalDecl);
			ApplyToDeclaration(dcl, bFirst);
			bFirst = false;
		}
	}

	bNotransform = false;
	for (auto& r : Required)
		if (r.semantic == "POSITIONT")
			{ bNotransform = true; break; }

	return true;
}

void CPreviewObject::UpdateClipSpace(const Fmatrix& WVP)
{
	if (!Object)
		return;

	// Reverse-engineered HUD projection constants (from shader vertex debug
	// output): NDC.x = KX*X - BX, NDC.y = -KY*Y + BY, NDC.z = Z, w = 1. A
	// notransform (POSITIONT) VS re-applies this to its input, so we feed it the
	// inverse: from the camera clip-space position of a vertex we recover the
	// HUD-space X,Y,Z that reproduces it. The model then renders as a real 3D,
	// world-anchored object instead of a screen-glued billboard.
	const float KX = 0.0020038f, BX = 0.999f;
	const float KY = 0.003086f, BY = 0.9984f;

	const bool clip = bNotransform;
	for (auto it = Object->FirstMesh(); it != Object->LastMesh(); ++it)
	{
		CEditableMesh* M = *it;
		if (!M)
			continue;

		const u32 vc = M->GetVCount();
		if (!vc)
			continue;

		xr_vector<Fvector>& Orig = OrigPos[M];
		if (Orig.empty())
		{
			Orig.resize(vc);
			for (u32 i = 0; i < vc; ++i)
				Orig[i] = M->Vertices()[i];
		}

		if (clip)
		{
			for (u32 i = 0; i < vc; ++i)
			{
				Fvector4 c;
				WVP.transform(c, Fvector4(Orig[i].x, Orig[i].y, Orig[i].z, 1.f));
				const float invw = (fabsf(c.w) > EPS_L) ? 1.f / c.w : 0.f;
				const float ndcx = c.x * invw;
				const float ndcy = c.y * invw;
				const float ndcz = c.z * invw;
				M->Vertices()[i].set((ndcx + BX) / KX, (BY - ndcy) / KY, ndcz);
			}
			M->UnloadRenderBuffers();
			M->GenerateRenderBuffers();
			bWasClip = true;
		}
		else if (bWasClip)
		{
			for (u32 i = 0; i < vc; ++i)
				M->Vertices()[i] = Orig[i];
			M->UnloadRenderBuffers();
			M->GenerateRenderBuffers();
			bWasClip = false;
		}
	}

	// (Re)patch the geometry declarations: GenerateRenderBuffers rebuilt them and
	// reset dx10_dcl_code to the source declaration, so re-apply the patched one
	// to keep the active VS signature satisfied (CreateInputLayout would fail
	// otherwise, e.g. missing COLOR0).
	ReapplyDeclarations();
}

void CPreviewObject::ReapplyDeclarations(bool bLog)
{
	if (!Object) return;
	for (auto it = Object->FirstMesh(); it != Object->LastMesh(); ++it)
	{
		CEditableMesh* M = *it;
		if (!M) continue;
		SDeclaration* dcl = GetMeshDeclaration(M);
		if (dcl)
		{
			OriginalDecl = dcl->dx10_dcl_code_pristine;
			SetSourceDeclaration(OriginalDecl);
			ApplyToDeclaration(dcl, bLog);
		}
	}
}

SDeclaration* CPreviewObject::GetMeshDeclaration(CEditableMesh* M)
{
	// The SDeclaration lives on the render-layer geometry built from the editor
	// mesh (GenerateRenderBuffers creates it from the fixed editor vertex
	// layout). Resolve it here so ApplyToDeclaration() can patch dx10_dcl_code
	// to match the active VS signature (dx9->dx11 input-layout mismatch fix).
	if (!M) return nullptr;
	if (!M->m_RenderBuffers)
		M->GenerateRenderBuffers();
	if (!M->m_RenderBuffers)
		return nullptr;
	for (auto& kv : *M->m_RenderBuffers)
	{
		for (auto& rb : kv.second)
		{
			if (rb.pGeom)
				return rb.pGeom->dcl._get();
		}
	}
	return nullptr;
}
