# Preset User Parameters

> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: IX-Ray Platform 1.3

## **Overview**

Now you can set custom parameters that will be automatically applied every time the game starts.

**Why do you need this:**

If a modification uses specific parameters for lighting, bloom, tonemapping, and other graphics options — you no longer need to modify the engine, write scripts (as was done in AtmosFear 3), or edit graphics presets.
Additionally, it's convenient for debugging: you can create a helper addon with pre-configured parameters — for example, needed options, cheats, or logging.

## **Usage**

Create a **ixray_settings** subdirectory in the **configs** folder, and in it create a file `default_settings_MODNAME.ltx`. In this file, you need to list all parameters that should be applied every time the game starts. The file structure is completely analogous to **user.ltx**.

![image](https://github.com/user-attachments/assets/049e7085-79aa-4eb8-ab82-176276bcdbcf)

## **Recommendations**

- ✔ Use for setting base graphics options, such as lighting for weather modifications;
- ✔ Remember that the engine loads all files by mask `default_settings_*.ltx` in alphabetical order;
- ⚠ Settings from other addons may also override values;
- ✖ Don't use to override control keys — this may ruin the user experience.
