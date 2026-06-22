# Configuration for debugging in Visual Studio
The debugger will need to dynamically link to some intel redistributables.

In the Solution Explorer window, if in the folder view, right click the topmost folder and switch to CMake Targets View.

Expand PGLdllTest Project
Right-click on PGLdllTest(executable), and select to create a debug configuration.
An editor window will open for launch.vs.json. Edit the evironmentVariables section to be as shown below. When saved, the file will reside inside the .vs folder.

{
  "version": "0.2.1",
  "defaults": {},
  "configurations": [
    {
      "type": "default",
      "project": "Main_PGLdllTest",
      "projectTarget": "PGLdllTest.exe",
      "name": "PGLdllTest.exe",
      "environmentVariables": {
        "PATH": "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/redist/intel64_win/compiler;%PATH%"
      }
    }
]
}

# Configuration for debugging in VS Code
This is handled with the launch.json file (currently untested)
