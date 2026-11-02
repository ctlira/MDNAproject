# Instructions for Input Parameters
Please see the Readme.md file in the Main_GammaPA folder.

This folder holds:
* Input files with species and states to use.
* The following parameter library files:
  * GPA-libSpecies.txt - Equation of State dependent parameters. Different 'versions' can be added and specified in the input file. The file includes a listing of the associating functional groups to use for each species.
  * GPA-libSites.txt - The association sites for each functional group
  * GPA-libVuq.txt - Bondi volumes and surface areas for species
  * GPA-libAssoc\<EOS\>.txt - association constants for the RDF model given by the variable \<EOS\>. Some extra characters may also be appended to improve identification
  * GPA-\<PhysModel\>.txt - parameters for the physical interactions for \<PhysModel\>, such as NRTL, etc., with potentially some extra characters appended to improve identification.

## Configuring Input Parameter Files
Input parameter files must be tab delimited. This format makes them easy to read and avoids confusion with commas in literature references or comments contained within the files. Comments can be added on any line following the input values.

The input file uses the format (line and col numbers are only for clarity, not part of the input file):

| ln\col | A | B | C | D | E | F | G | H | I | J | K |
| -------- | - | - | - | - | - | - | - | - | - | - | - |
| 1 | 2 | nSpecies |  |  |  |  |  |  |  |  |  |
| 2 | GPA-libAssocPCS-GS\.txt | assoc param file |  |  |  |  |  |  |  |  |  |
| 3 | GPA-libNRTLPCS\.txt | phys param file |  |  |  |  |  |  |  |  |  |
| 4 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | kop index |
| 5 | dbg level | rdf | liq vol | att model | comb corr | comb |  |  |  |  | kop desc |
| 6 | 1 | 3 | 1 | 0 | 1 | 1 | 0 | 0 | 0 | 0 | kop |
| 7 | idTRC | version | nGroupTypes | Group1 | n1 | Group2 | n2 | Group3 | n3 |  |  |
| 8 | 715 | 1 | 0 |  |  |  |  |  |  |  |  |
| 9 | 5350 | 1 | 1 | 11401 | 1 |  |  |  |  |  |  |
| 10 | 2 | nStates |  |  |  |  |  |  |  |  |  |
| 11 | Kcalc | T | P | x |  |  |  |  |  |  |  |
| 12 | 3 | 323\.15 | 1 | 1 | 0 |  |  |  |  |  |  |
| 13 | 3 | 323\.15 | 1 | 0\.9 | 0\.1 |  |  |  |  |  |  |

Lines 4 and 5 are for reference, and should always be present. See documentation in GammaPaMain.f90 for the kop values.

The number of species is listed in A1 and that number of rows is expected starting a line 8. The species parameters for the listed version are processed from GPA-libSpecies.txt. The groups with associating sites are listed and processed from GPA-libSites.txt. For this case, methanol group 11401 will have one electron acceptor 21401 and one electron donor 51401. The association strength parameters are processed from the association parameter library indicated in A2 and the physical model parameters are processed from the library indicated in A3. The association strengths are identified by the site ids.

The number of states to be evaluated is indicated in the row following the last specie, in this case row 10 where 2 states are to be evaluted. The next row is a header row.

For each of the states to be evaluated includes the value of KCALC. KCALC = 1 evaluates only gammas. KCALC = 2 evaluates only enthalpy of mixing, and KCALC = 3 evaluates both.

The number of mole fractions should match the number of species. The mole fractions are normalized within the code.

## Preparing Library Files

Each of the libary file should be tab-delimited. When ID numbers are included, they should be sorted by the first column. When pair interactions are present, sort the file by the first column, then the second.

When editing by Excel, be sure to follow the exponential display procedures in the GammaPA Readme.md and avoid committing files with empty tabs at the end of rows.
