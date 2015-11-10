/** tutorial/Ex1
 * This example shows how to:
 * initialize a femus application;
 * define the multilevel-mesh object mlMsh;
 * read from the file ./input/square.neu the coarse-level mesh and associate it to mlMsh;
 * add in mlMsh uniform refined level-meshes;
 * define the multilevel-solution object mlSol associated to mlMsh;
 * add in mlSol different types of finite element solution variables;
 * initialize the solution varables;
 * define vtk and gmv writer objects associated to mlSol;
 * print vtk and gmv binary-format files in ./output directory.
 **/

#include "FemusInit.hpp"
#include "MultiLevelProblem.hpp"
#include "NumericVector.hpp"
#include "VTKWriter.hpp"
#include "GMVWriter.hpp"
#include "LinearImplicitSystem.hpp"
#include "adept.h"

using namespace femus;




int main(int argc, char** args) {

  // init Petsc-MPI communicator
  FemusInit mpinit(argc, args, MPI_COMM_WORLD);

  // define multilevel mesh
  MultiLevelMesh mlMsh;
  double scalingFactor = 1.;
  // read coarse level mesh and generate finers level meshes
  mlMsh.ReadCoarseMesh("./input/square.neu", "seventh", scalingFactor);
  /* "seventh" is the order of accuracy that is used in the gauss integration scheme
      probably in the furure it is not going to be an argument of this function   */
  unsigned numberOfUniformLevels = 3;
  unsigned numberOfSelectiveLevels = 0;
  mlMsh.RefineMesh(numberOfUniformLevels , numberOfUniformLevels + numberOfSelectiveLevels, NULL);
  mlMsh.PrintInfo();

  // define the multilevel solution and attach the mlMsh object to it
  MultiLevelSolution mlSol(&mlMsh);

  // add variables to mlSol
  mlSol.AddSolution("U", LAGRANGE, FIRST);
  
  mlSol.Initialize("All");    // initialize all varaibles to zero

 
 // attach the boundary condition function and generate boundary data
      mlSol.AttachSetBoundaryConditionFunction(SetBoundaryCondition);
      mlSol.GenerateBdc("u");

      // define the multilevel problem attach the mlSol object to it
      MultiLevelProblem mlProb(&mlSol);

      // add system Poisson in mlProb as a Linear Implicit System
      LinearImplicitSystem& system = mlProb.add_system < LinearImplicitSystem > ("Poisson");

      // add solution "u" to system
      system.AddSolutionToSystemPDE("u");

      // attach the assembling function to system
      system.SetAssembleFunction(AssemblePoissonProblem_AD);

      // initilaize and solve the system
      system.init();
      system.solve();
 
  
  
  
  
  
  

  // print solutions
  std::vector < std::string > variablesToBePrinted;
  variablesToBePrinted.push_back("U");
  variablesToBePrinted.push_back("P");
  variablesToBePrinted.push_back("T");

  VTKWriter vtkIO(&mlSol);
  vtkIO.write(DEFAULT_OUTPUTDIR, "biquadratic", variablesToBePrinted);

  GMVWriter gmvIO(&mlSol);
  variablesToBePrinted.push_back("all");
  gmvIO.SetDebugOutput(false);
  gmvIO.write(DEFAULT_OUTPUTDIR, "biquadratic", variablesToBePrinted);

  return 0;
}




