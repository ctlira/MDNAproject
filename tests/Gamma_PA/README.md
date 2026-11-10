# Case Study Documentation

The library files for these case studies are included in the folder in the event that the standard library files are modified.
* GPA-libSpecies.txt
* GPA-libVuq.txt
* GPA-libSites.txt

The input files for the case studies are:
* GPA-CaseStudyMeOH-CycloC6.txt - This is the case study for the distributed code described in ..\..\Main_GammaPA except that only activity coefficients are evaluated. The additional parameter files are:
    * GPA-libAssocPCS2a.txt
    * GPA-libNRTLPCS.txt
* GPA-CaseStudy1.txt - This is CaseStudy1 in Wertheim_documentation_Bench.docx. Only the gammas and Hex are evaluated, not any phase equilibria. The additional parameter files are:
    * GPA-libAssocVDW1.txt
    * GPA-libNRTL1a.txt - when including the att model
    * GPA-libNRTL1b.txt - when the att contribution is absent
* GPA-CaseStudy2a.txt - This is the test file for CaseStudy2iv-vii in Wertheim_documentation_Bench.docx by changing kop(2) and species order. The parameter files are:
    * GPA-libAssocCPA2a.txt - when using kop(2) = 2
    * GPA-libAssocPCS2a.txt - when using kop(2) = 3
    * GPA-libNRTL1b.txt
* GPA-CaseStudy2b.txt - This is the test file for CaseStudy2viii-x in Wertheim_documentation_Bench.docx by changing kop(2) and species order. The parameter files are:
    * GPA-libAssocCPA2a.txt - when using kop(2) = 2
    * GPA-libAssocPCS2a.txt - when using kop(2) = 3
    * GPA-libNRTL1b.txt
