> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.3

# Personal navigation marker

## Overview

A personal navigation marker is a custom point on the PDA map that can be set as the current navigation target.

## How it works

1. The marker is stored as a `CMapLocation` of a custom type.
2. The active target is stored in `CMapManager`.
3. The map context menu provides actions to:
   1. set the personal navigation marker as the active target
   2. clear the active target
4. When the marker is deleted, the active target is cleared automatically.

## Usage

1. Open the PDA map.
2. Select a personal marker.
3. Open the context menu.
4. Choose set or clear navigation target.

Related material: [UI overview](ui-advanced-features.md).
