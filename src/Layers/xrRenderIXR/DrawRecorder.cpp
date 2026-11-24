/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "DrawRecorder.h"

DrawRecorder Recorder;
thread_local DrawRecorder::List* DrawRecorder::Open = nullptr;

namespace
{
enum EKind : u8
{
	KindPass,
	KindScissor,
	KindNoScissor,
	KindWorld,
	KindView,
	KindCull,
	KindDepth,
	KindAlphaRef,
	KindVertices,
	KindDraw,
	KindQuads,
	KindCall
};

template <typename T>
u32 Keep(DrawRecorder::List& Into, const T& Value)
{
	const u32 At = u32(Into.Bytes.size());
	Into.Bytes.resize(At + sizeof(T));
	std::memcpy(Into.Bytes.data() + At, &Value, sizeof(T));
	return At;
}

template <typename T>
T Kept(const DrawRecorder::List& From, u32 At)
{
	T Value;
	std::memcpy(&Value, From.Bytes.data() + At, sizeof(T));
	return Value;
}

void Hold(DrawRecorder::List& Into, SGeometry* Geometry)
{
	if (Into.Geometries.empty() || Into.Geometries.back()._get() != Geometry)
	{
		Into.Geometries.emplace_back()._set(Geometry);
	}
}

void RunScissor(Irect* Rect)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	Cmd.SetScissor(Rect);
	Cmd.SetScissorOverride(Rect != nullptr);
}

void RunDraw(SGeometry* Geometry, ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 Offset)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	Cmd.SetGeometry(Geometry);
	RImplementation.Dynamic.Draw(Cmd, Topology, VertexCount, Offset);
}

void RunQuads(SGeometry* Geometry, u32 Quads, u32 Offset)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	Cmd.SetGeometry(Geometry);
	RImplementation.Dynamic.DrawQuads(Cmd, Quads, Offset);
}
} // namespace

void DrawRecorder::List::Clear()
{
	Commands.clear();
	Bytes.clear();
	Calls.clear();
	Elements.clear();
	Geometries.clear();
	IsRecorded = false;
}

DrawRecorder::Scope::Scope(List& Into) : Before(Open)
{
	OpenList(Into);
}

DrawRecorder::Scope::~Scope()
{
	Open = Before;
}

void DrawRecorder::OpenList(List& Into)
{
	Into.Clear();
	Into.IsRecorded = true;
	Open = &Into;
}

void DrawRecorder::CloseList()
{
	Open = nullptr;
}

void DrawRecorder::SetPass(ShaderElement* NewElement, u32 NewPass)
{
	Element = NewElement;
	Pass = NewPass;

	if (Open)
	{
		Open->Commands.push_back({ KindPass, NewPass, 0, 0, NewElement });

		if (Open->Elements.empty() || Open->Elements.back()._get() != NewElement)
		{
			Open->Elements.emplace_back()._set(NewElement);
		}

		return;
	}

	RImplementation.Imm().SetPass(NewElement, NewPass);
}

void DrawRecorder::SetScissor(const Irect* Rect)
{
	if (Open)
	{
		Open->Commands.push_back({ u8(Rect ? KindScissor : KindNoScissor), Rect ? Keep(*Open, *Rect) : 0u, 0, 0, nullptr });
		return;
	}

	Irect Copy = Rect ? *Rect : Irect{};
	RunScissor(Rect ? &Copy : nullptr);
}

void DrawRecorder::SetWorld(const Fmatrix& World)
{
	if (Open)
	{
		Open->Commands.push_back({ KindWorld, Keep(*Open, World), 0, 0, nullptr });
		return;
	}

	RImplementation.Imm().SetObjectTransform(World);
}

void DrawRecorder::SetView(const Fmatrix& View, const Fmatrix& Project)
{
	if (Open)
	{
		Open->Commands.push_back({ KindView, Keep(*Open, View), Keep(*Open, Project), 0, nullptr });
		return;
	}

	RImplementation.Imm().SetView(View, Project);
}

void DrawRecorder::SetCull(ERHI_CULLMODE Mode)
{
	if (Open)
	{
		Open->Commands.push_back({ KindCull, u32(Mode), 0, 0, nullptr });
		return;
	}

	RImplementation.Imm().SetCullMode(Mode);
}

void DrawRecorder::SetDepth(bool IsEnabled)
{
	if (Open)
	{
		Open->Commands.push_back({ KindDepth, u32(IsEnabled), 0, 0, nullptr });
		return;
	}

	RImplementation.Imm().SetDepthEnable(IsEnabled);
}

void DrawRecorder::SetAlphaRef(u32 Ref)
{
	if (Open)
	{
		Open->Commands.push_back({ KindAlphaRef, Ref, 0, 0, nullptr });
		return;
	}

	RImplementation.Imm().SetAlphaRef(Ref);
}

// A list's vertices wait in its bytes; the offset it gives counts from the lock, and Play adds the stream's own
void* DrawRecorder::Lock(u32 Count, EVertexFormat Format, u32& Offset)
{
	if (!Open)
	{
		return RImplementation.Dynamic.Lock(Count, Format, Offset);
	}

	Offset = 0;
	LockFormat = Format;
	LockCount = Count;
	LockStart = u32(Open->Bytes.size());
	Open->Bytes.resize(LockStart + size_t(Count) * VertexStride(Format));
	return Open->Bytes.data() + LockStart;
}

void DrawRecorder::Unlock(u32 Count)
{
	if (!Open)
	{
		RImplementation.Dynamic.Unlock(Count);
		return;
	}

	VERIFY(Count <= LockCount);
	Open->Bytes.resize(LockStart + size_t(Count) * VertexStride(LockFormat));
	Open->Commands.push_back({ KindVertices, LockStart, Count, u32(LockFormat), nullptr });
}

void DrawRecorder::Draw(SGeometry* Geometry, ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 Offset)
{
	if (Open)
	{
		Open->Commands.push_back({ KindDraw, u32(Topology), VertexCount, Offset, Geometry });
		Hold(*Open, Geometry);
		return;
	}

	RunDraw(Geometry, Topology, VertexCount, Offset);
}

void DrawRecorder::DrawQuads(SGeometry* Geometry, u32 Quads, u32 Offset)
{
	if (Open)
	{
		Open->Commands.push_back({ KindQuads, Quads, Offset, 0, Geometry });
		Hold(*Open, Geometry);
		return;
	}

	RunQuads(Geometry, Quads, Offset);
}

void DrawRecorder::Call(std::function<void()>&& What)
{
	if (Open)
	{
		Open->Commands.push_back({ KindCall, u32(Open->Calls.size()), 0, 0, nullptr });
		Open->Calls.push_back(std::move(What));
		return;
	}

	What();
}

CTexture* DrawRecorder::PassTexture() const
{
	const STextureList* Textures = Element && Pass < Element->passes.size() && Element->passes[Pass] ? Element->passes[Pass]->T._get() : nullptr;

	if (Textures)
	{
		for (const auto& [Slot, Texture] : *Textures)
		{
			if (Slot == 0)
			{
				return Texture._get();
			}
		}
	}

	return nullptr;
}

void DrawRecorder::Play(const List& From)
{
	VERIFY(!Open);
	GfxCmdList& Cmd = RImplementation.Imm();
	u32 Base = 0;

	for (const List::Command& Item : From.Commands)
	{
		switch (Item.Kind)
		{
		case KindPass:
			Element = static_cast<ShaderElement*>(Item.Pointer);
			Pass = Item.A;
			Cmd.SetPass(Element, Pass);
			break;
		case KindScissor:
		{
			Irect Rect = Kept<Irect>(From, Item.A);
			RunScissor(&Rect);
			break;
		}
		case KindNoScissor:
			RunScissor(nullptr);
			break;
		case KindWorld:
			Cmd.SetObjectTransform(Kept<Fmatrix>(From, Item.A));
			break;
		case KindView:
			Cmd.SetView(Kept<Fmatrix>(From, Item.A), Kept<Fmatrix>(From, Item.B));
			break;
		case KindCull:
			Cmd.SetCullMode(ERHI_CULLMODE(Item.A));
			break;
		case KindDepth:
			Cmd.SetDepthEnable(Item.A);
			break;
		case KindAlphaRef:
			Cmd.SetAlphaRef(Item.A);
			break;
		case KindVertices:
		{
			if (!Item.B)
			{
				break;
			}

			const EVertexFormat Format = EVertexFormat(Item.C);
			std::memcpy(RImplementation.Dynamic.Lock(Item.B, Format, Base), From.Bytes.data() + Item.A, size_t(Item.B) * VertexStride(Format));
			RImplementation.Dynamic.Unlock(Item.B);
			break;
		}
		case KindDraw:
			RunDraw(static_cast<SGeometry*>(Item.Pointer), ERHI_PRIMITIVE_TOPOLOGY(Item.A), Item.B, Base + Item.C);
			break;
		case KindQuads:
			RunQuads(static_cast<SGeometry*>(Item.Pointer), Item.A, Base + Item.B);
			break;
		case KindCall:
			From.Calls[Item.A]();
			break;
		}
	}
}
