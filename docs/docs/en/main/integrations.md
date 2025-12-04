# Integrations
## RenderDoc
> [!IMPORTANT]  
> **Status**: Discontinued in 1.1 <br>
> **Minimum Version**: 1.0

![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/15de1c4a-cb08-4dd2-a64a-4939e316a1e9)

Support for work (dx11 only) with the ability to connect in real-time (run with the `-renderdoc` key). Also, when running with this parameter, the debug mode for shaders is enabled, allowing you to modify them in RenderDoc
> [!WARNING]  
> As of version 1.1, only launches from the program are supported.  <br>
> To use shader debug mode, use the `-dxdebug` key
## Discord 
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.0

![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/c84a6d44-7514-49ec-9a7c-53c064dbe190)

Basic Discord activity integration. The title can be specified in ```engine_external.ltx``` in the `[general]` section in the `title` parameter. The section uses StringTable, which allows you to make translations for different languages. 

## LuaPanda
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.0

**LuaPanda** - This is a VSCode plugin that allows you to debug Lua scripts. 
> [!WARNING]
> Integration is not available for Release configuration!

1. Install the plugin
2. Open the scripts folder as the root for VSCode 
3. Run the debugger 
4. Connect the game to VSCode 

![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/537e34a5-3403-49cc-a5fc-dd91d7bdec12)

## Optick
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.2

**Optick** - is a high-performance, low-overhead profiler for C++, designed for the game industry. It provides all the necessary tools for analyzing and optimizing application performance.
  
> [!WARNING]
> Integration is not available for Release configuration!

### Usage Instructions

1. Download and install [Optick](https://github.com/bommody/optick)
2. Run the Optick application
3. Run the game and go to the area that needs profiling.
  
Profiling can be done in two ways: through the Optick interface or directly in the game.
  
### Method 1: Profiling via Optick
  
1. In the Optick interface, click the Start Profiling Session button in the desired game scene
  
![image](https://github.com/user-attachments/assets/67753532-dd4e-4042-80fc-bf18222c13db)
  
2. Wait for connection — a Connecting message will appear
  
![image](https://github.com/user-attachments/assets/6d774793-d49a-4a18-831a-20fbad187715)

3. After capture is complete, information about the number of recorded frames will be displayed

![image](https://github.com/user-attachments/assets/92847568-c2c3-408e-8529-df2abb3b9ade)

4. To stop profiling, click the **Stop** button
  
### Method 2: Profiling via the Game Interface

1. Run the game;
2. Go to the scene that needs to be analyzed;
3. Activate debug mode: press `Alt + I` (English layout);
4. You will get a cursor, go to the **Tools** tab;
5. Click the **Optick Start Capture** button;
6. To finish profiling, use the **Optick Stop Capture** button in the same tab;
  
![image](https://github.com/user-attachments/assets/73fb8f9d-2732-4c38-9a04-201d2a585075)
  
7. A file named ixray-`date`-`time`-`username`.opt will appear in the game root;
8. Open the file in **Optick**
  
> [!WARNING]
> After capture is complete, the game may crash. This is acceptable behavior.
  
### Recommendations
  
* Long capture is not recommended — it may cause Optick to malfunction.
* Running Optick and the game as administrator allows you to get a more complete set of data for analysis.
