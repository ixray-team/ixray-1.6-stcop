# File Ignoring System
## Overview
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.1

`.xrignore` - file from the root directory, responsible for skipping reading various files

## Usage
```python
# Works by finding in the path
# If we ignore desk.dds, sorting will work by the principle *desk.dds

Thumbs.db
.svn

# Temp trash
/.git/
/.vs/
/temp/
```
