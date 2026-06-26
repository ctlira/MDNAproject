# Variable input/output
Example input files and templates are included in the Input folder. Two different files are loaded. The first file sets the run options as well as the sites and association parameters. The second file loads the physical model parameters for the model selected in the first input file. The use of two input files permits different physical models to be fitted to the same association parameters. The run options are described in the comments at the top of gamma_pa.f90.
# General comments on I/O
Notes from JRE
- Your “Get__” functions should be included in your NRTLPA.f90 file for reading the parameters. FuEsd23.f90 or FuSpeadMd23a.f90 provide examples of reading association parameters. --CTL - I have used my input files and provided a 'database' file in excel format that are saved as tab-separated files.
- Probably, sourceCodeUA/FuSpeadMd23a will be more interesting for you. Around line 268 of that code, you will see how site-based parameters are read and stored. Line 370 calls GetAssocBips. FYI: BIPs are “binary interaction parameters.” GetAssocBips() is defined in sourceCodeUA/Wertheim.f90 (wertheim23.f90?) because Wertheim.f90 can be called generically for any TPT1, specifically ESD and SPEADMD in PGLWrapper. --CTL - I do not have a large database of TPT parameters. The only ones that I have are for alcohols.
- input/BipDA.txt file lists the BIPs between donors and acceptors. (Acceptors and donors have the same BIPs.) input/ParmsHb4.txt defines the volumes and energies of the donors and acceptors. input/SiteParms2580.txt defines the site types (also defined in ParmsHb4, but maybe it’s better to see the big picture). -- CTL - my values are entered explicitly for each pair.
- It might be nice if your definitions of site types could be the same as SPEADMD, just to have one less degree of confusion, but I don’t feel strongly about it. If you have a good reason to add a siteType that is not already available in SPEADMD, let me know. I will probably want to add it. -- CTL - I don't have many sites and they are specific to the host though generic sites are available.
- For example, it would be logical (to me) if you referred to your list of bonding volumes and energies as ParmsHbNRTLPA.txt. BIPs for the physical interactions of NRTL could be called BipNRTLPA.txt. For cross-association BIPs, the filename BipDaNRTLPA.txt makes sense to me, but you may prefer BipDA_NRTLPA.txt. -- CTL comment: the user can choose the name. There are so many options available that the parameters depend on the rdf model assumed for association. Thus there is not a single file that will always be the same.

## Notes to transfer to Readme.md later

When distributing a Fortran application built with the Intel oneAPI compiler (`ifx`), determining exactly which runtime DLLs to bundle depends entirely on **how your code was compiled** and **which Intel libraries your code actually calls** (like math or parallel processing libraries).

Because your users won't have Intel oneAPI installed on their machines, you must distribute these dependencies side-by-side with your `.exe` and `.dll` files.

Here is the step-by-step process to find out exactly what your application needs.

---

## Step 1: Check your Compiler Flags (`/libs`)

How you compiled your binary dictates your baseline dependencies. Look at the `/libs` switch in your `CMakeLists.txt` or compiler configuration:

* **`/libs:static`**: The core Fortran runtime is embedded *inside* your executable. You do **not** need to distribute core runtime files like `libifcoremd.dll`.
* **`/libs:dll`**: Your application relies on dynamic linking. You **must** bundle the core dynamic libraries.

---

## Step 2: Identify the Core Fortran DLLs

If you are using dynamic linking (`/libs:dll`), a standard Fortran application typically requires these foundational files from the Intel `redist` folder:

| File Name | Purpose | Required For... |
| --- | --- | --- |
| **`libifcoremd.dll`** | Core Fortran Runtime | Every dynamic Fortran app (Release) |
| **`libifcoremdd.dll`** | Core Fortran Runtime | Every dynamic Fortran app (Debug) |
| **`libmmd.dll`** | Intel Math Library | Vectorized math functions (sin, cos, log) |
| **`svml_dispmd.dll`** | Short Vector Math Library | Optimized loop math calculations |

---

## Step 3: Check for Advanced Feature Dependencies

Depending on what your code does, you may need to bundle extra packages. Look through your project for these features:

* **OpenMP Parallelism (`/Qopenmp`):** If your code runs loops in parallel, you must include **`libiomp5md.dll`** (the Intel OpenMP runtime).
* **Intel MKL (Math Kernel Library):** If you link against BLAS, LAPACK, or FFT routines, you will need the MKL redistributables (e.g., `mkl_core.dll`, `mkl_intel_thread.dll`, `mkl_rt.dll`).

---

## Step 4: The Foolproof Way to Audit (Using Dependency Walker / Dependencies)

Instead of guessing, you can inspect your compiled `.exe` or `.dll` to see its exact runtime wishlist.

Windows has a built-in search hierarchy, and free tools can map it out visually:

1. Download a modern tool like **Dependencies** (a rewrite of the classic *Dependency Walker*).
2. Open your compiled `PGLdllTest.exe` or `PGLdll.dll` inside the tool.
3. Look at the module tree graph. It will list every DLL your binary touches.
4. Filter out standard Windows system files (like `kernel32.dll` or `ntdll.dll`). Any file starting with `libif...`, `libm...`, or `mkl...` is an Intel dependency that must be bundled.

---

## Where to Grab the Distribution Files

Never copy files from the `.../compiler/latest/lib` or `.../bin` folders for distribution. Intel explicitly provides a **`redist`** folder designed exactly for this purpose.

Go to your oneAPI installation path:
`C:\Program Files (x86)\Intel\oneAPI\compiler\latest\windows\redist\intel64_win\compiler\`

Copy the required `.dll` files directly from this directory and place them **in the exact same installation folder** as your application's executable. When your end-user launches the program, the Windows OS loader will look in the local application folder first, find the Intel runtimes, and boot up flawlessly.
