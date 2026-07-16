#pragma once

#include "fixedvector.h"

const u32			sh_STAGE_MAX	= 4;
const u32			sh_PASS_MAX		= 8;

typedef char		sh_name			[64];

class ENGINE_API	CTexture;
class ENGINE_API	CXRShader;

typedef FixedVector<sh_name,sh_STAGE_MAX*sh_PASS_MAX>	tex_names;
typedef FixedVector<CTexture*,sh_STAGE_MAX>				tex_vector;
typedef FixedVector<tex_vector,sh_PASS_MAX>				tex_handles;

