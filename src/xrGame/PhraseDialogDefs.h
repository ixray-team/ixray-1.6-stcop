#pragma once

#include "../xrCore/intrusive_ptr.h"

class CPhraseDialog;

using DIALOG_SHARED_PTR = xr_shared_ptr<CPhraseDialog>;

#include "PhraseDialog.h"

using DIALOG_ID_VECTOR = xr_vector<shared_str>;
using DIALOG_ID_IT = DIALOG_ID_VECTOR::iterator;
