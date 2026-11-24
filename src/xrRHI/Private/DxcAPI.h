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
#include <stddef.h>

// This interface deliberately avoids both DXC and engine platform headers:
// DXC's native COM types differ from the ones provided by DXVK and DXMT.
class DxcObject final
{
public:
	DxcObject() = default;
	~DxcObject();
	DxcObject(const DxcObject&) = delete;
	DxcObject& operator=(const DxcObject&) = delete;

	void* Get() const { return Object; }
	void** Put();
	const void* Data() const;
	size_t Size() const;
	void Reset();

private:
	void* Object = nullptr;
};

namespace DxcAPI
{
int Create(void (*CreateFunction)(), DxcObject& Utils, DxcObject& Compiler);
int Compile(const DxcObject& Compiler, const void* Source, size_t Size, const wchar_t** Arguments, size_t ArgumentCount, DxcObject& Code, DxcObject& Errors);
int Reflect(const DxcObject& Utils, const void* Code, size_t Size, const void* InterfaceId, void** Reflection);
} // namespace DxcAPI
