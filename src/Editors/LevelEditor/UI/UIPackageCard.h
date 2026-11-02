#pragma once

#include "../Packages/PackageCatalog.h"

namespace PackageCardUI
{
void DrawMetadata(const SdkPackages::PackageRow& Row, bool HasLicenseText, xr_string& Error);
void DrawDocument(const xr_string& Text, const char* EmptyMessage, bool IsMarkdown = true);
} // namespace PackageCardUI
