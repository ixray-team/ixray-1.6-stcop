# Dialogs
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.3

## Focus on NPC Face

### **Overview**

This mechanic automatically changes the field of view (FOV) and applies a background blur effect (Depth of Field) to focus the player's attention on the NPC's face during dialogue.

![image](https://github.com/user-attachments/assets/3f9cf089-7d6d-4716-8aca-d86b30177992)

## **Usage**

The function settings are located in `engine_external.ltx` in the `[gameplay]` section
<br>
**Available parameters:**

```ini
; Dialogs
DialogFovScale = 0.75 ; FOV scaling coefficient. Lower value -> stronger "zoom" on face.
TalkDof = 0.0, 0.5, 5.0, 0.0 ; Background blur parameters (Depth of Field)
```

Here you can adjust the magnification strength and blur strength. If you set `DialogFovScale = 1`, the magnification will disappear
<br><br>
Besides, to disable focus for a specific NPC, just add `focus_on_npc = false` to the NPC spawn section
<br>
**Example:** 
```ini
[your_npc_spawn_section]
character_profile = ...
; ... other parameters ...
focus_on_npc = false ; This NPC will not activate camera focus
```
---

> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.3

## Dialog Phrase Icons
![image](https://github.com/user-attachments/assets/6bee91b3-9308-4ea1-8e81-98aa1fc034c3)

```xml
   <phrase id="0">
        <icon_name ltx="0">ui_inv_icon_health_restore_speed</icon_name> <!-- Dialog icon -->
        <text>zat_b22_stalker_medic_need_health_care_0</text>
        <next>1</next>
        <next>2</next>
   </phrase>
```
* If you want to use an item icon, then: 
```xml
   <icon_name ltx="1">bread</icon_name> <!-- Dialog icon -->
...
```
