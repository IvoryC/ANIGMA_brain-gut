# ANIGMA_brain-gut

Analysis of clinical data and 16S data from the ANIGMA cohort.

## See outputs

The scripts to create outputs and some input files are stored in the git repository itself.  The outputs they make, such as figures, are available as non-tracked attachements, see the releases page: [https://github.com/IvoryC/ANIGMA_brain-gut/releases](https://github.com/IvoryC/ANIGMA_brain-gut/releases). These can be downloaded to browse locally.

## Project File Structure

This project is designed to be compatible with specific software: Code Ocean and BioLockJ.

### Code Ocean

This project is designed to work with [Code Ocean](https://codeocean.com/).

See corresponding Code Ocean Capsule ["Anigma_brain-gut"](https://codeocean.com/capsule/7378534/tree).

To work with the Code Ocean platform, the top level directories are named to match the 4 "Core Files" folders:

 * `metadata`
 * `environment`
 * `data`
 * `code` (pretty much everything is in here)
 
**metadata**
 
_This directory has the metadata.yml generated by Code Ocean.  For data describing samples (ie metadata) see **code/analysis/input/meta**._
 
**environment**

_This directory has the files to create the environment to run the pipeline._  

This includes: 

  * the `Dockerfile` that is automatically generated by Code Ocean based on information entered in the Environment IDE. It is important to NOT EDIT this file except through the Code Ocean IDE.
  * the `postInstall` script that installs anything else required.
  * the `alternatives/docker/Dockerfile` which can be used locally to create a container for running the pipeline on a local machine.
  
_Note:_ Both Code Ocean and BioLockJ use docker containers.  When running on Code Ocean, the docker container is managed by Code Ocean which uses the container defined in `/environment/Dockerfile`, and BioLockJ runs the pipeline inside that container and does not know that it is operating inside a container.  If you choose to run the pipeline on a local machine you may choose to have BioLockJ manage docker containers locally, using the container(s) available thru the docker hub or defined in `/environment/alternatives/Dockerfile`. BioLockJ could also be used to run the pipeline on a local machine as long the required software is installed.  

To run locally, you will need the following:

R (version 4.3.0 was used)

Most modules require following R packages:
  * tidyr
  * dplyr
  * ggplot2
  * ggrepel

Rendering markdowns requires:
  * markdown

The ordination module additionally requires:
  * cowplot
  * ggsignif
  * phyloseq

Diversity module additionally requires:
  * vegan

**data**

_Code Ocean will store and manage large files that should not be comited in git. Here, we have the folder for reference, and the README in that folder will have instructions on how to acquire any data that needed to run the analysis that is not directly commited in git._


**code**

_This is the main folder.  This is the folder that Code Ocean expects all other git-committed files to be in._


For a compelling argument in favor of using Code Ocean, check out the 2021 editorial ["Promoting reproducibility with Code Ocean"](https://genomebiology.biomedcentral.com/articles/10.1186/s13059-021-02299-x) by Barbara Cheifet, an editor at _Genome Biology_.

### BioLockJ

[BioLockJ](https://biolockj-dev-team.github.io/BioLockJ/) is a pipeline manager. In this project, it takes a set of inputs and scripts and generates a new project space with a set structure. 

The project structure here matches the structure that BioLockJ creates, so relative file paths that work in a given run of the pipeline also work in this repository directly.  Thus, the `analsys` folder (that is, `code/analysis`) is equivalent to the top level of a pipeline run folder. 

The run specifications for BioLockJ are given in a BioLockJ config file, `anigma_stress.config`.

Note that module folders have numbers but these numbers are arbitrary. It keeps them in an intuitive order, but the numbers are not guaranteed to be the same in a given run.

For any given script, its working directory in a run will be `<pipeline>/<module>/<subdir>` . In a run, that "subdir" is usually the script subdirectory. In the project repository, the subdirectory is called "src". It is an arbitrary name, but the script should be in **a directory one level down from the module directory**. For example: 

 * project file: `code/analysis/04_Feature_Interactions/src/feature-interactions.Rmd`
 * becomes `anigma_stress_<date>/02_Feature_Interactions/resources/feature-interactions.Rmd` in a given BioLockJ run.

BioLockJ can copy any specified input files into the `input` folder for the run.  For any inputs that are stored in git, it is convenient to store them in `analysis/input` to mach the relative files paths of a run.

For more details about BioLockJ, see the [BioLockJ User Guide](https://biolockj-dev-team.github.io/BioLockJ/) and the README file in the `analysis` folder.

## Publication info

Eventually, this project will be published in a scientific journal.  Details will posted here when they become available.

