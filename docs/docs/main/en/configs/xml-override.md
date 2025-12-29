# XMLOverride
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.0

## Introduction
**XMLOverride** - a system for customizing XML through a separate file without modifying the existing file. It looks as follows: 
```
mm_ui_test.xml         <-- Original
mod_ui_test_myedit.xml <-- Mod file override
```
The override file is constructed as follows:
>mod_(filename)_(editname).xml

Block overrides are done using the `override` attribute, which has three values:
* add
* replace
* remove

## Example
We have the following code in the original file: 
```xml
<w>
      <background x="0" y="0" width="1024" height="768">
	<auto_static x="500" y="130" width="345" height="160" stretch="1">
		<texture width="432" height="160">ui\video_voroni_crop</texture>
	</auto_static>
	<auto_static x="432" y="353" width="460" height="416" stretch="1">
		<texture x="0" y="0" width="576" height="416">ui\video_water_crop</texture>
	</auto_static>
	<auto_static x="0" y="0" width="104" height="768" stretch="1">
		<texture>ui_inGame2_left_widepanel</texture>
	</auto_static>
	<auto_static x="920" y="0" width="104" height="768" stretch="1">
		<texture>ui_inGame2_right_widepanel</texture>
	</auto_static>
      </background>
</w>
```
Let's say we want to override:
```xml
	<auto_static x="500" y="130" width="345" height="160" stretch="1">
		<texture width="432" height="160">ui\video_voroni_crop</texture>
	</auto_static>
```
Write the following in the mod file: 
```xml
<w>
      <background x="0" y="0" width="1024" height="768">
	<auto_static x="500" y="130" width="345" height="160" stretch="1" override="replace"> <--! Set override attribute to replace -->
		<texture width="432" height="160">ui\video_voroni_crop</texture> <--! You can set your own value here -->
	</auto_static>
      </background>
</w>
```
