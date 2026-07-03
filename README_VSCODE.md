# Onboarding Guide: VS Code Fortran Development Setup

This repository uses **VS Code**, the **Modern Fortran** extension, and a localized **Conda** environment to manage the `fortls` language server and Python linting universally. Follow these steps to set up your VS Code CMake environment.

# Table of Contents

[Step 1: Install VS Code](#step-1-install-vs-code)

[Step 2: Install Miniconda](#step-2-install-miniconda)

[Step 3: Create the Shared Fortran-Tools Python Environment](#step-3-create-the-shared-fortran-tools-python-environment)

[Step 4: Open the Workspace in VS Code](#step-4-open-the-workspace-in-vs-code)

[Step 5. Configuration of CMake](#step-5-configuration-of-cmake)

[Step 6. Understanding Linting and Language Services](#step-6-understanding-linting-and-language-services)

[Step 7. Browsing Projects, Building Projects](#step-7-browsing-projects-building-projects)

[Step 8. Cleaning the projects](#step-8-cleaning-the-projects)

[Step 9. Building a project](#step-9-building-a-project)

[Step 10. What is the working directory and where is the compiled code?](#step-10-what-is-the-working-directory-and-where-is-the-compiled-code)

[Step 11. Starting the Debugger](#step-11-starting-the-debugger)

[Step 12. Opening Source Code and Setting Breakpoints](#step-12-opening-source-code-and-setting-breakpoints)

[13. A Few Hints on Getting Started with VS Code](#13-a-few-hints-on-getting-started-with-vs-code)

[14. Adding Files to a Project](#14-adding-files-to-a-project)

[15. Adjusting Compile Flags](#15-adjusting-compile-flags)

---
## Step 1. Install VS Code

If you don't already have Visual Studio Installed, [download and install](https://code.visualstudio.com/download). VS Code should be installed as a 'User', not as an administrator. Run the installation on the user account that will be used for working with this repository.

[Return to TOC](#table-of-contents)

## Step 2. Install Miniconda or Anaconda

Note: if you have Anaconda installed, you do not need to install Miniconda. Minconda is much smaller had has all the important needed code. If you don't already have Miniconda installed, download and install it using the default settings:

* **Windows Installer:** [Miniconda Windows 64-bit](https://docs.conda.io/en/latest/miniconda.html)
* *Note:* Install it as a _user_ on the same account as VS Code. Ensure it installs to the default directory (`%USERPROFILE%\Miniconda3`). The VS Code settings.json will look for fortls in this specific folder. If you have installed previously, the default folder would substitute the name Ananconda3 here and in subsequent instructions.

[Return to TOC](#table-of-contents)

## Step 3. Create the Shared Fortran-Tools Python Environment

Open your terminal (PowerShell or Command Prompt) and run the following command to create the fotran-tools needed for this repository (substitute Anaconda3 if you have that installed):

```cmd
"%USERPROFILE%\Miniconda3\Scripts\conda.exe" create --name fortran-tools python fortls -c conda-forge --yes

```
[Return to TOC](#table-of-contents)

## Step 4. Open the Workspace in VS Code

If you have installed Anaconda3 instead of Miniconda3, open .vscode/settings.json in a text editor and replace Miniconda3 with Anaconda3. Do no recommit the file if you have modified this single setting.

The configuration of this repo is intended to be used with either Visual Studio or Visual Studio Code. The implementation of VS Code relies on the environment variables configured in the integration of oneAPI with Visual Studio. **Thus the VS Code instance should always be started from the Intel oneAPI command prompt as given below.**

    **Navigate to the repo directory and run launch_vscode.bat**. The launch_vscode.bat automates steps 1-3

or

    1. Start the command prompt for Intel oneAPI for Visual Studio.
    2. cd to the repo location
    3. Launch VS Code with the command from the oneAPI prompt in the repo root by entering "Code ."

> The script file, or steps 1-3 should also be used for subsequent sessions for VS Code to use the preconfigured .json settings.

4. If you use other profiles in VSCode, it is recommended to create a profile for this folder so that settings.json is not modified by other extensions (such as Python). See [VS Help on Profiles](https://code.visualstudio.com/docs/configure/profiles).

    On the first launch, when prompted, install the recommended extensions:

    **Modern Fortran**

    **CMake (by Microsoft)**).

    The **C/C++** and **C/C++** extensions will be automatically installed as dependencies. If you miss the installation notification prompt, click the bell icon in the bottom right. You can also install these by clicking the building blocks button for extensions on the far left panel.

---

### Why this works seamlessly for the team:

* **Predictable Paths:** By ensuring everyone uses the default Miniconda installation path, `${env:USERPROFILE}/Miniconda3/...` (or Ananconda3) will resolve flawlessly on every single team member's Windows machine.
* **No Git Pollution:** Since the configuration is entirely portable, nobody will accidentally modify or commit their own absolute paths (like `C:/Users/your-name/...`) to the shared repository.

#### Details on CMake Extensions

What Each Extension Handles
* fortran-lang.linter (Modern Fortran): Drives your syntax highlighting, hover documentation, and automatically communicates with the fortls.exe server tracking the .git exclusions.
* fortls language server analyzes code for relationships.
* ms-vscode.cmake-tools (CMake Tools): Provides the status bar UI to easily toggle between your debug, release, and relwithdebinfo configuration presets with one click.
*  ms-vscode.cpptools (C/C++): Supplies the native cppvsdbg debug engine required to read the Visual Studio-style embedded debugging symbols generated by the Intel ifx compiler.

[Return to TOC](#table-of-contents)

## Step 5. Configuration of CMake

**Click the CMake Triangle icon in the left toolbar.**
* In the Configure field, edit and select the desired build configuration
* In the Build field, select the desired build project
* If the terminal is displayed (Ctrl + \` ) on the 'output' tab you will find that CMake immediately builds a cache and creates an 'out' folder. The 'out' folder can be safely deleted in File Explorer between sessions. (The terminal can be found on the 'View' menu or toggled using (Ctrl + \` ). The 'Terminal' menu will add additional terminal windows, or (Ctrl + Shift + \` ).


**That’s it!** Autocomplete, hover tooltips, and background code diagnostics will work out of the box without any local modifications.

[Return to TOC](#table-of-contents)

## Step 6. Understanding Linting and Language Services

When a FORTRAN file is opened, VS Code will run a linter and language service. Here is the breakdown of how **Linters** and **Language Services** differ, how they overlap, and how they actually work together to help you code.

---

### The Short Version

* **A Linter** is like an editor or a proofreader. It looks for stylistic issues, anti-patterns, and bad habits (e.g., "You have an unused variable here," or "Your indentation is inconsistent"). It enforces specific code quality rules.
* **A Language Service** is like a deeply knowledgeable assistant. It actually *understands* the architecture of your codebase (e.g., "What properties does this object have?", "Where is this function defined?", "What type of data does this variable hold?").

---

### Side-by-Side Comparison

---

| Feature | Linter (e.g., ifx, ESLint, Flake8, `fprettify`) | Language Service (e.g., fortls, gopls, Pyright) |
| --- | --- | --- |
| **Primary Goal** | Catch code smells, style violations, and potential bugs. | Provide code intelligence, navigation, and autocomplete. |
| **Scope** | Usually analyzes **one file at a time** line-by-line using abstract syntax trees. | Analyzes the **entire project graph**, understanding how files link together. |
| **Features** | Warning markers, formatting fixes, enforcing naming conventions. | Go-to-definition, find all references, safe renaming (refactoring), IntelliSense/autocomplete. |
| **Customization** | Highly configurable via a local config file (e.g., `.eslintrc`, `.yaml`) where you turn rules on/off. | Usually configured via compiler options, build paths, or workspace settings (like `CMakePresets.json`). |

---

When you open a code file in VS Code, both tools are running in the background simultaneously:

1. **The Language Service** reads your project structure, indexes your modules or files, and figures out how they depend on each other. When you type a function name, it provides the autocomplete dropdown. If you pass an integer to a function expecting a string, it flags a type error.
2. **The Linter** scans the text you just wrote. It notices that you left a trailing whitespace at the end of line 12, or that you used a deprecated syntax that works but isn't best practice. It overlays its own warnings right alongside the language service's errors.

Many language services can also run linting rules inherently (for instance, a language server might warn you about an unreferenced variable because it already tracked every reference in the project), but dedicated linters exist so teams can customize and enforce strict style rules across an entire engineering team.

The idenfied issues will be displayed in the 'Problems' tab of the terminal pane.

[Return to TOC](#table-of-contents)

## Step 7. Browsing Projects, Building Projects

This code distribution depends heavily on .json files to assure a similar development experience for all users. Only a subset of CMake integration settings are configured, and thus some of the available VS CMake controls are not used or addressed in these instructions.

**Open the Cmake panel** by clicking the CMake triangle in the left toolbar. The CMake 'Run and Debug' button in the toolbar is used during debugging. Debugging is explained below.

### The CMake Panels

#### Project Status

The Project Status frame is used to select the build configuration and build target.

Set the Build Configuration and Target Project:

    Project Status > Configure: This binds your Configuration/Build Type (e.g., debug, relwithdebinfo, release) directly from your CMakePresets.json. The relwithdebinfo is optimized to /O2 which can still be reliably debugged to assure that the optimization did not break anything. The release build configuration is optimized agressively to /O3 which vectorizes DO loops, which makes debugging impractical. However, testing of the release code against benchmarks is still necessary to make sure the final level of optimization did not break anything.

    Project Status > Build: This is where you click to change your active target from [all] to a specific asset like PGLdll or PGLEOS. It is important to build PGLdll components separately from the PGLEOS because the PGLdll components are configured to link dynamically whereas the PGLEOS is configured to link statically. The system can be easily confused by linking to intermediate files shared by the two methods if they don't have the correct build type within the intermediate files. Thus, don't use [all], and clean all projects when switching from a project that uses dynamic linking to a project that uses static linking.

    Project Status > Debug: This is where you click to select the target that will be launched by the Debug button.

#### Project Outline

The top level 'Project Outline' line includes a flyout menu when hovering.

    Project Outline > Configure Projects: Clears and rewrites the CMake cache. This can be used to 'refresh' the CMake settings.

    Project Outline > Configure all Projects with CMake Debugger: not used.

    Project Outline > Build Projects: Do not use at the overall project level since the dynamic/static build configurations of the subprojects conflict some common build folders are used.

    The following are available in the Project Outline flyout menu ...

    Project Outline > ... > Clean All Projects: Useful to purge all cached intermediate build files (.obj, .mod, etc.) and it important to use when switching back and forth between a project with dynamic linking (PGLDll and PGLDllTest) to a project with static linking (PGLEOS).

    Project Outline > ... > Clean Reconfigure All Projects: Cleans the CMake cache and refreshes across all projects.

##### Repo Listing

Hovering over MDNAProject provides the following links:

* MDNAproject > Configure: rewrites the CMake cache
* MDNAproject > Build: Don't use to avoild clashes with the different link modes in the repo
* MDNAproject > Clean: Cleans the previous compile/link for all projects in the repo

##### Subproject Listings

* Hovering over a subproject will provide pop-up icons to build or set a bookmark.
* Right-clicking a subproject provides shortcuts to build, open the CMakeLists, or set as the build target.
* Clicking the subproject accordion provides a project browser and code listing specific for the project. Using this menu for the project is the preferred method of browsing code because only the relevant source code is displayed.

You may click the build icon beside the subproject target. Once you set those two fields in the Project Outline frame, the main Build button on the bottom status bar dynamically binds itself to those exact choices.

At the bottom of the Project Outline frame are the overall CMakeLists.txt and CMakePresets.json. These hold the settings for the overall repo. These settings are inherited by the individual projects. CMake expects you to open the repo folder so that it reads these files and builds the repo CMake cache as soon as the folder is opened.

[Return to TOC](#table-of-contents)

## Step 8. Cleaning the projects

This step is not needed on the initial configuration, but it is needed to assure that the caches are not conflicting with previous projects.

**CMake and VS Code cache the project agressively. It is very important to perform a repo level 'clean' when switching between builds of the PGLDll and PGLEos/GammaPA projects.** PGLDll is built for dynamic linking and PGLEos and GammaPA are built for static linking (stand-alone app) so one build type can pollute the other due to the aggressive caching.

Occasionally, you may need to Reload the Window, which completely flushes the CMake settings in memory. To execute a reload, call up the VS Code command pallete using Cntl + Shift + P. Start typing 'Developer: Reload Window' and the dropdown will automatically filter.

**Developer: Reload Window** only restarts the **front-end UI layer** of VS Code. It is essentially like refreshing a tab in your web browser.

Here is exactly what happens behind the scenes when you trigger a reload versus what happens to your build system:

### 8.1. What a Window Reload Actually Does

* Shuts down and restarts the VS Code extension host process.
* Forces extensions like **Modern Fortran** and **CMake Tools** to reboot, re-read your `.vscode/settings.json`, and re-initialize their background language servers (`fortls.exe`).
* Clears out any stuck UI states, extension panics, or "server crashed 5 times" locking codes.

### 8.2. What Happens to CMake Natively

* **The CMake cache is entirely untouched.** * When CMake runs its initial configuration step, it writes all its discoveries—like your compiler paths, library configurations, and architecture settings—directly to a physical file on your hard drive (`out/build/debug/CMakeCache.txt`).
* When you reload the window, the newly booted CMake Tools extension simply looks at your project directory, sees that `CMakeCache.txt` already exists, and says, *"Great, the configuration is already done!"* It then loads that exact cached state right back into memory.

---

### The Cheat Sheet: When to use which command

Because they target completely different layers of your development environment, you have to use them strategically depending on what you just modified:

| Action You Just Took | What You Need to Run | Why? |
| --- | --- | --- |
| **Modified `.vscode/settings.json**` or updated a Python/Conda tool path. | `Developer: Reload Window` | Forces VS Code's UI to load your new editor settings. |
| **Modified `CMakePresets.json**` or changed underlying compiler flags/strategies. | `CMake: Delete Cache and Reconfigure` | Forces CMake to delete `CMakeCache.txt` on disk and re-run its compiler sanity checks. |
| **Added a brand new `.f90` source file** to a directory tracked by CMake. | `CMake: Configure` | Tells CMake to scan the directories again and update the build targets without wiping its whole memory. |

If CMake is totally confused, the best option is to close VS Code completely. With VS Code closed, delete the 'out' folder. Nothing important is deleted, this forces CMake to start a fresh configuration when VS Code reloads the folder. When you reopen VS Code, then CMake will reconstruct the 'out' folder.

[Return to TOC](#table-of-contents)

## Step 9. Building a project

**The !DEC$ export for masterDir,PGLinputDir must be toggled ON when compiling PGLDll and toggled OFF when compiling PGLEOS.** This inconvenience will be eliminated in the future.

The selected project build is displayed in the **Project Status>Build** field. That selection is bound to the 'Build' button in the toolbar at the bottom of the CMake Panel.

The build button at the bottom VS Code taskbar will trigger display of the 'OUTPUT' terminal where any compile/link issues are displayed.

[Return to TOC](#table-of-contents)

## Step 10. What is the working directory and where is the compiled code?

The working directory is the repo root so that the code can properly read/write to the 'Input' and 'Output' folders. When code is launched with the 'Debug' button, VS Code will properly use this working directory.

The compiled code is within the 'out/build/\<build config>/<project folder name>'. Using the VS Code launch button bypasses some settings and tries to run the code from this location, but then the 'Input' and 'Output' cannot be found. To run the project _outside_ of the CMake and VS Code environment, move the executable to the root of the repo, or create a folder location with the 'Input' and 'Output' as subfolders.

    When using PGLTest.exe in this way, due to dynamic linking, the PGLDll.dll needs to also be in the repo root. If on a machine without the oneAPI environment, additional runtime .dll files are needed from the oneAPI redist folder. A dependency app can be used to determine which .dll files are needed.

[Return to TOC](#table-of-contents)

## Step 11. Starting the Debugger

After the project is built successfully, use the 'Debug' bug icon in the bottom CMake toolbar to start the debug process.

**Note that if the focus is on the Output terminal, you will need to click the 'DEBUG CONSOLE' tab to see the console.**

When the debugger runs, a pop-up toolbar is displayed. If you close VS Code with the debugger running, or don't properly shutdown the debugger, the breakpoint and debugger session may be in the backgroud when a new debug session is started. Look in the bottom of the CMake panel to see debug sessions and close them if the debug toolbar or the displays appear to be 'messed up'.

[Return to TOC](#table-of-contents)

## Step 12. Opening Source Code and Setting Breakpoints

Browse in the CMake Project Outline frame to select a source code file to open. Expand the subproject name accordion and the 'References' list to find the code.

**Note that single clicking a file name triggers the 'preview' mode and the file name is displayed in italics. Single clicking a different file switches the preview to the new file but does not 'load' the file.**

Double click the file, or double-click the filename tab to open a file. Then click in the left line numbers to set a breakpoint.

Use the 'Run and Debug' toolbar button in the left VS Code toolbar to view the Breakpoints. Clicking a breakpoint will open the corresponding file at the breakpoint location.

When debugging the 'Run and Debug' window provides the local variables and provides a 'Watch Window' and 'Call Stack'. Clicking the Call Stack will switch to the variables at that point of the stack.

[Return to TOC](#table-of-contents)

## 13. A Few Hints on Getting Started with VS Code

### Folder View

The folder icon in the top of the left toolbar toggles a folder view. Single-clicking a file opens a preview window and the filename in the tab is displayed in italics. Double click a file name or the preview tab to open the file, and the filename in the tab will be displayed in regular font. The font color provides feedback on the state of a file. When editing a .json, the tab color indicates when all syntax is OK. Also look for squiggly lines which are warnings.

### Linting Cleanup

The project linting compiles to check syntax and creates '\*\_genmod.f90', '\*\_genmod.smod', and '\*.mod' files in the source code folder. These are hidden in the VS Code view but will be apparent when viewing in File Explorer. These are ignored by Git. A powershell script is provided to clean up the folders. To clean the folders, right-click on the powershell script 'clean_genmod.ps1' and select 'Run in Powershell'.

### Integration with Git

When viewing the file listings, the letter at the right of the panel shows if items are unmodified, etc. and hovering shows more details. The left toolbar includes a 'Git' button that opens a panel to show the history in the lower window and a summary of changes in the upper window.

Clicking an item with changes shows the 'diff' in the file window and provides a popup to 'undo' the changes in hunks.

Files can be staged using the GUI. The terminal accepts line git commands.

### VS Code Whitespace Settings for Coding

White spaces at the end of lines create confusion in revision control because if the amount of white space changes the line is marked as 'changed'. Also, files shoule end with a new line. These settings are preconfigured in the .vscode/settings.json.

### Filtering Problems

The Problems window (use Ctrl + \`) is very helpful when viewing code files when linting is enabled. However, warnings can be overwhelming especially when using implicit variable types. On the Problems tab, use the filter box to select which items are shown. Click an item to jump to the location.

Be patient when VS Code first loads before compiling because CMake will run the compiler to identify problems. Initally the problem count may be large, but it may gradually decrease as the linter finds dependent files, etc. as it works in the background.

### Searching within a subproject

CMake generates a file fortls_sources.txt. This is a scratch file - do not commit this file to the repo. Open in a text editor and copy the list of source files from the project that you wish to search. Use the magnifying glass in the left toolbar or Ctrl + Shift + F and paste the list into the box for 'files to include'. Then enter the search term in the top box.

### Command Palette

The command palette is opened using Ctrl + Shift + P. Commands are best entered by starting to type the command instead of browsing. Useful commands for this project are:

* CMake: Clean Rebuild
* CMake: Delete Cache and Reconfigure
* Fortran: Restart the Fortran Language Server - if the tool tips are not appearing.
* Developer: Reload Window - a clean reload of the .json files (But does not clean CMake FORTRAN build files)

The recently-used commands appear at the top of the dropdown.

### Autocomplete in terminal

By default, pressing TAB during a command typing will autocomplete with the first match, which is annoying. Type a few letters and then press Ctrl + Spacebar. VS Code documentation provides instructions to configure automatically.

### Viewing Command History

Within the terminal, command history is available.

* Use the up arrow to scroll through the history one command at a time.
* In the command window, use the shortcut keys Ctrl + Alt + R to bring up the same history as Ctrl + Shift + P -> Terminal: Run Recent command.
* History for the current session is available by typing 'history' in the command line.

[Return to TOC](#table-of-contents)

## 14. Adding Files to a Project

The project CMakelists.txt provides a list of files in the order of compiling and linking. To build the file list a powershell script scan-fortran.ps1 is provided to crawl the folders for module definitions and common blocks. Files with module definitions should be listed before they are used. CMake is 'smart' and recompiles only the set of files needed. If the fourth from the last file is edited, only the last four files will be compiled again before linking. If missing symbols are found when linking, check the file order. When the files are sorted by folder, the order that the files are added is also important.

[Return to TOC](#table-of-contents)

## 15. Adjusting Compile Flags

Compile flags are predominantly in CMakePresets.json. To view more warnings, edit to /warn:all. To enforce a specific FORTRAN standard, see the comment in the file.

Compile commands for fixed format .f/.for files are within the project CMakeLists.txt. Project-specific flags can be added immediately after the target_compile_options. Flags are appended, and subsequent flags will take precedence in the case of conflicts. The exact compile commands are in out/\<build config>/build.ninja and .ninja.log. Some flags may use Linux format dashes instead of the Windows format slash. CMake adjust the flags to the Windows format before passing to the compiler/linker.

[Return to TOC](#table-of-contents)
