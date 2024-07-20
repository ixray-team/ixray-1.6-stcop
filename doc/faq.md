# Frequently Asked Questions

English | [Русский](./faq.rus.md)

List of frequently asked questions

## Questions and Answers

### What is IX-Ray and IX-Ray Team? Who are you?

IX-Ray is a project to improve the original X-Ray engine used for the initial trilogy of S.T.A.L.K.E.R. games. The IX-Ray Team is a team of IX-Ray project developers, consisting of former and current modders. For more details, see the project description.

### The developers also list ImeSense, what is that?

ImeSense is another of our more "general" teams focused on developing software and services beyond the scope of the S.T.A.L.K.E.R. game. The IX-Ray project is partially based on components, changes, and infrastructure from ImeSense.

### Why did the project take a long time to develop, but there was no information about it anywhere?

The project was developed for quite some time by small efforts without fuss and haste. Moreover, we did not aim to create premature hype without having some completed fixes and improvements at that moment. We did not want to make empty promises. Now it no longer makes sense to hide anything, and the project has accumulated a significant number of changes.

### How is your project different from Oxygen/CryRay/ClosedXRay/others?

We have repeatedly considered and were offered the opportunity to join one of the existing projects, but we encountered insurmountable obstacles and disagreements in the approach to development, release publication, etc., so we stayed on our own and developed our own project. The IX-Ray project is developed entirely by our own efforts, not relying on the "fixes" of existing projects, combining a balanced approach to making changes, and using the most modern available development tools. See also the main changes section and the detailed change list for more information.

### Why does your engine not support Windows XP?

The Windows XP operating system has not been supported by the manufacturer for a very long time, has extremely limited hardware support, and many problems that were fixed only later. The team almost immediately decided to eliminate a number of problems and useless limitations by abandoning support for an already little-used system.

### What renders and types of lighting are available?

All original renders are available. DirectX 10 works on a backward compatibility scheme with DirectX 11. The choice of DirectX 9 render has been simplified: now instead of 3 presets, 1 is available with __full__ functionality preservation of the previous ones.

### What is the status of Direct 9 render support?

At the moment, DirectX 9 is still supported by the project, but in the future, new features will not be added to the DirectX 9 render.

### Why do the FPS counter values displayed in the game not match the values from external programs?

At the moment, this is a temporary implementation, the counter can indeed be updated with a delay

### Why is fullscreen mode not working on Debug/Mixed config of the engine?

This is done intentionally to solve engine debugging problems (minimizing, etc) and since these builds are not intended for players

### What is Nvidia DLSS and AMD FSR2 in the context of this project?

Both technologies take an image with a lower resolution and scale it up to a higher resolution. They use advanced algorithms to make this scaling higher quality. DLSS uses artificial intelligence and deep learning to predict and improve image details, while FSR uses noise reduction and edge smoothing algorithms to achieve a similar result. As a result, these technologies provide a sharper and smoother image, improving graphics quality and game performance even on less powerful systems. If these scaling methods are used in native mode, they will work as anti-aliasing.

### Is multiplayer supported and is its development planned?

Unlike some other projects, we do not aim to completely bury the possibility of multiplayer gaming. At the moment, the engine supports multiplayer, but it still requires refinement.

### I don't see support for `component_name` or `feature_name` in the engine. What should I do?

We do not practice completely removing components without providing a full functional replacement.

### I found a bug, what should I do?

Fill out the [form](https://github.com/ixray-team/ixray-1.6-stcop/issues/new?assignees=&labels=bug&projects=&template=bug_report.yml) on GitHub or leave a detailed description of the problem on the [relevant channel](https://discord.com/channels/959159181053661244/1165620257436733540) of the Discord server.

### I have an idea and I think it might be useful to others. How can I report it?

Fill out the [form](https://github.com/ixray-team/ixray-1.6-stcop/issues/new?assignees=&labels=enhancement&projects=&template=feature_request.yml) on GitHub.

### I want to move to your engine, but I need `feature_name`, what should I do?

Also fill out the corresponding [form](https://github.com/ixray-team/ixray-1.6-stcop/issues/new?assignees=&labels=enhancement&projects=&template=feature_request.yml).

### The team's profile lists partners, how does it work?

All those teams listed in our profile directly work with us on making changes to the engine, and we also help them in developing their projects and porting them to IX-Ray.

### Besides the CoP engine, I saw forks of other parts of the game, what is their status?

Forks of the SoC and CS engines are still under development and require a huge number of changes, so there can be no talk of their release yet.

### Where can I see what tasks are planned to be performed?

On the corresponding [page](https://github.com/orgs/ixray-team/projects/7).

### You have so many features! But where is the documentation?

It is available on the [wiki](https://github.com/ixray-team/ixray-1.6-stcop/wiki).

### Is the unification of the SoC, CS, and CoP game modules planned?

As soon as possible.

### Is the adaptation of Gunslinger/StCop/IWP/my_favorite_weapon_pack planned?

Follow this information in the official sources.

### There is a `gamedata` folder in your repository, why is it there?

Some engine fixes and changes require corresponding changes in game resources. Moreover, there were a huge number of problems not only in the engine but also in the game files that required fixes for the game to work correctly or to properly support mods. The repository contains full gamedata (without assets), and only the modified files are in the releases.

### How can an ordinary player or modder help the project?

The best support is to help with testing and spreading information about the project. Create your modifications, builds, update old modifications, and let us know about it for publication on our resources.

### Will only your tools (editors, utilities) or third-party ones be suitable for the engine?

We did not break backward compatibility with the original game formats, so we can talk about full backward compatibility. Both our and third-party tools will be suitable.

### I launched DLSS/FSR and the image became too blurry or sharp. How to fix this?

There is a "Sharpness" slider in the graphics settings. Adjust it to a comfortable value.
