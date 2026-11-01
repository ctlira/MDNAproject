# Project Gamma_PA

This project is based on the PhD Theses of Aseel Bala and William Killian at Michigan State University with assistance of Renming Lui for programming. This code implements the TPT1 variation of Wertheim theory with different options for the RDF and different options for the physical interactions.

The project is currently programmed as a stand-alone code that will be integrated in the PGL menu system.

## Required Files

The stand-alone version requires
* An input file listing the species, the species parameter version to use, the file name for the association parameters to implement and the file name for the physical model parameters. The input file specifies the 'version' of the species parameter to use, the functional groups on each species and how many occurences of each group occur on the species. The file also includes the 'kop' option codes for how the case is run, and a listing of states to calculate. An example file can be found in ./tests/GPA-CaseStudyMeOH-CycloC6.txt.
* A library file with the species id numbers and RDF parameters. Variations are possible by creating a 'version' of the species with different parameter sets since literature constants differ. The species file is ./Input/GAMMAPA/GPA-libSpecies.txt.
* A library file with molar volume and molecular information (Bondi molecular size and surface area). The volume file is ./Input/GAMMAPA/GPA-libVuq.txt. Currently, saturated liquid volume parameters are included in this file, but the long term plan is to use generalized correlations.
* A library file that breaks the functional groups into association sites. For example, a 2B hydroxyl group includes one electron acceptor and one electron donor. The corresponding site id numbers and number of occurences in each group are stored in this file. This file is ./Input/GAMMAPA/GPA-libSites.txt.
* A library file with Association Strength parameters. Since these also differ in the literature, the specific file to be used is listed in the input file. An example file is ./Input/GAMMAPA/GPA-libAssocPCS-GS.txt.
* A library file with Physical Model parameters. Since these depend on the assocation strengths and the data used for fitting, the specific file to be used is listed in the input file. An example file is ./Input/GAMMAPA/GPA-NRTLPCS.txt

## Output

The output files are written to the ./Output folder. The output file echos the key settings including the association and physical parameters.

When kop(1) is greater than 0 a debug file is also written. Values of 0-3 are accepted with increasing debug information as documented in GammaPAmain.f90. At a debug level of 3, additional files are generated.

## File Format

All the input and output files are tab-delimited (except the state calculations explained below) to minimize confusion that might arise with comma separated values when species names have commas, such as with 1,4-dioxane.

Files can be edited in the following ways:
* Using Notepad or Notepad++. A plugin is available for Notepad++ to align tabs, Columns++. After the Columns++ plugin is installed, the tabs can be aligned using Plugins>Columns++>Elastic tabstops. Excel is easier to use with the described caveat.
* Excel can be used to open or edit files, but with a two major caveats. Excel will aggressively modify exponentials to three significant digits, resulting in a substantial loss of precision and also will insert extra tabs when saving. The loss of precision occurs whether the file is opened by Excel, or if the file is opened in a text editor and then copy/pasted into Excel. When the file is saved as .csv or .txt or 'copied' again to paste to a text file, only the displayed digits in the exponential number are transferred. The work-around is to:
  * Initiate a search and click the 'Format...' button. Select the Scientific category and set to two decimal places. Click OK.
  * Next click the 'Replace' tab and again select 'Format ...'. Select the Scientific category and set to five decimal places. Click OK.
  * Next click 'Replace All'. Then drag the cell widths as necessary to view the digits.
  * One more step is necessary to avoid changes in white space at the end of rows. Excel pads every row to have the same number of 'cells' as the widest row with tabs. Before committing to the repo, use one of the following tools:
    * Open the file in Notepad++ and select Edit>Blank Operations>Trim Trailing Space. or
    * Open the file in VS Code (without moving from the project directory) and then resave. The .vscode/settings.json includes lines that enforce file hygiene automatically for files within the repo folders edited in VS Code.

    The extra tabs will be removed even though they are not 'spaces'. If extra tabs are present, Git will tag the line as changed even though if no changes of importance are made.

* Output of calculated T,P,x,gamma,Hex states are written as a formatted ouput line that is only space delimited. The easiest way to convert these lines is to:
  * Open the output file in Excel.
  * Select the space-delimited state calculations rows at the end of the file.
  * Use Data>Text to Columns. Set as space delimited, collapsing multiple spaces.
  * Then apply the exponential/white space formatting procedure specified above.

## Benchmark Files

Benchmark files and documentation is included in ./Main_GammaPA/Wertheim_documentation.docx and ./tests/Gamma_PA/Werthiem_documentation_bench.docx. The later files uses 'random' parameters that are not realistic, but have been used to assure that the code always reproduces the same values after modification. Additional example files are provided in the ./tests/Gamma_PA folder, but be aware that they do not use realistic parameters.

## File Status

The code has not yet been modified for other physical models besides NRTL even though source code is provided for the physical models. These models will be implemented in the future.

To make the subroutines easier to find, they will be gradually be stored as individual files. However, using the VS Code option for Workflow (see the related Readme file) provides a rapid way to jump to the subroutine definition and then return to the line where the call occurs.

The documentation .docx files were developed for the Aspen implementation and have not yet been updated, though the benchmark values for activity coefficients have been verified as documented in the ./tests/Gamma_PA folder.
