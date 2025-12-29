# **Wildcard Support in File Paths**

> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: IX-Ray Platform 1.2

## **Overview**

You can now use **wildcards** in configuration and binary files to group resources together. This allows you to reduce manual file listing and automate data loading.

## **Usage in Text Configs**

Previously, you had to explicitly specify each file:

```cpp
#include "weapons\w_ak74.ltx"
#include "weapons\w_ak74u.ltx"
#include "weapons\w_ak101.ltx"
```

Now you can replace the list with a **template**:

```cpp
#include "weapons\w_*.ltx"
```

→ This will include **all files** in the `weapons` folder that match the mask `w_*.ltx`

### **Additional Features in Text Configs**

- Recursion is supported:

```cpp
#include "weapons\*\*.ltx"
```

→ Will include all `.ltx` files in `weapons` and subfolders

## **Usage in Binary Files (OGF)**

Previously, models required full path specification:

```ini
"dynamics\weapons\wpn_hand\wpn_abakan_hud_animation.omf"
"dynamics\weapons\wpn_hand\wpn_ak74_hud_animation.omf"
"dynamics\weapons\wpn_hand\wpn_ak74u_hud_animation.omf"
```

Now you can use a mask:

```ini
"dynamics\weapons\wpn_hand\*.omf"
```

→ This will load **all `.omf` files** from the specified folder

### **Additional Features in Binary Files**

- Recursion is supported:

```ini
"dynamics\weapons\*\*.omf"
```

→ Will load all `.omf` files in `dynamics\weapons` and subfolders

## **Recommendations**

- ✔ **Use masks** to simplify configs when dealing with many files, or when developing an addon;
- ✔ **Check the list of loaded files** to avoid unwanted resources being included;
- ⚠ **Recursive masks** (`*`) may result in loading extra files — use them carefully;
- ✖ **Don't use masks** if you need strict control over the order or composition of files.
