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
#pragma once

class GfxCmdList;
struct SGeometry;
struct ShaderElement;
class CTexture;

// What the game draws by itself in a frame (its interface, the fonts, the debug lines, an icon) reaches the command list
// through here. With nothing open each call runs at once. With r_render_thread the game's thread opens a list for a
// place of the frame and makes its draws into it, and the render thread plays the list at that place: the game's draw
// code changes the game as it draws, so it cannot run beside the game's next frame, but its draws can.
// An offset Lock gives is good for the draws that follow it, until the next Lock
class DrawRecorder
{
public:
	struct List
	{
		struct Command
		{
			u8 Kind;
			u32 A;
			u32 B;
			u32 C;
			void* Pointer;
		};

		xr_vector<Command> Commands;
		xr_vector<u8> Bytes;
		xr_vector<std::function<void()>> Calls;
		// What the commands name, held until the list is recorded again: the game may let it go before the list is played
		xr_vector<ref_selement> Elements;
		xr_vector<ref_geom> Geometries;
		bool IsRecorded = false;

		void Clear();
	};

	// Opens a list on this thread for as long as it lives; the one that was open comes back after it
	struct Scope
	{
		explicit Scope(List& Into);
		~Scope();

		List* Before;
	};

	// The same without a scope: from the frame's begin to its end
	static void OpenList(List& Into);
	static void CloseList();

	static bool IsRecording() { return Open != nullptr; }
	void Play(const List& From);

	void SetPass(ShaderElement* Element, u32 Pass = 0);
	void SetScissor(const Irect* Rect);
	void SetWorld(const Fmatrix& World);
	void SetView(const Fmatrix& View, const Fmatrix& Project);
	void SetCull(ERHI_CULLMODE Mode);
	void SetDepth(bool IsEnabled);
	void SetAlphaRef(u32 Ref);
	void* Lock(u32 Count, EVertexFormat Format, u32& Offset);
	void Unlock(u32 Count);
	void Draw(SGeometry* Geometry, ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 Offset);
	void DrawQuads(SGeometry* Geometry, u32 Quads, u32 Offset);
	// What has no command of its own: runs now, or at its place when the list is played. It must hold what it reads
	void Call(std::function<void()>&& What);
	// The first texture of the pass set last
	CTexture* PassTexture() const;

private:
	static thread_local List* Open;

	ShaderElement* Element = nullptr;
	u32 Pass = 0;
	u32 LockStart = 0;
	u32 LockCount = 0;
	EVertexFormat LockFormat = EVertexFormat::Packed;
};

extern DrawRecorder Recorder;
