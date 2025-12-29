# Game Localization
# **In-Game Language Switching**

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: IX-Ray Platform 1.3 <br>

## **Overview**

Added the ability to change the interface language, subtitles, and game text in real-time without restarting the game. By default, the game starts in Russian.

Language switching is available in two places:
*   **Main Menu**: Using toggle buttons in the lower part of the screen
*   **In Game**: Through the menu `Options` → `Game` → `Language`
*   **Console Command**: `language`

## **Locale Configuration**

Language settings are located in **`gamedata\configs\localization.ltx`**.

### **Main Parameters:**

```ini
[string_table] ; Main localization settings section
language        = rus ; Active language by default (priority)
languages       = rus, eng ; List of all available languages
```

### **Parameter Description:**

*   **`language`** (Default: `rus`)
    *   Defines the language that will be used when the game starts for the first time
    *   Allowed values: `rus`, `eng`, and any other added locales as they appear in `gamedata\configs\text\`

*   **`languages`**
    *   Contains a complete list of all languages available in the game, separated by commas
    *   Languages must be specified as they are named in their folders in `gamedata\configs\text\`
    *   The order of languages in this line **corresponds to the order** of switching between them in the game menu

## **Adding a New Language**

To add a new language to the game, follow these steps:

### **1. Prepare Translation Files**

*   Place translation files (`.xml`) in the appropriate folder inside:
    `gamedata\configs\text\LOCALE_NAME\`
    *   For example, for Polish: `gamedata\configs\text\pol\`
*   Make sure the files are saved [in the correct encoding](https://github.com/ixray-team/ixray-1.6-stcop/wiki/File-Encoding)

### **2. Add Fonts**

*   Select and add font files (`.ttf`) that support the necessary characters (e.g., Cyrillic, diacritical marks, etc.).
*   Place font files in the folder:
    `gamedata\fonts\LOCALE_NAME\`
    *   For example: `gamedata\fonts\pol\`

### **3. Register the Language in Configuration**

*   Add the name of the new locale (exactly as the folder is named in `configs\text\`) to the `languages` parameter in `localization.ltx`.

    **Example of adding Polish:**
    ```ini
    [string_table]
    language        = rus
    languages       = rus, eng, pol ; Polish is now available in the menu
    ```

## **Important Notes**

*   Changes in `localization.ltx` take effect after restarting the game.
*   After adding a new language, it will automatically appear in the selection menu and be available for switching.
*   For correct display, make sure that:
    1.  [Translation files have the correct encoding](https://github.com/ixray-team/ixray-1.6-stcop/wiki/File-Encoding)
    2.  Fonts support all necessary characters of the added language
    3.  The locale name in the config, text folder, and font folder **are identical**

## **See Also**

1. [Working with fonts in IX-Ray](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Fonts)
2. [File encodings](https://github.com/ixray-team/ixray-1.6-stcop/wiki/File-Encoding)
