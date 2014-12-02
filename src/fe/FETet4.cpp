#include "FETet4.hpp"


namespace femus {




// =======================
FETet4::FETet4(std::vector<GeomEl> geomel_in) : FEElemBase(geomel_in) {
	    
	      _name[VV]="Tet_4";
	      _name[BB]="Tri_3"; 
             _pname[VV]="Tetrahedron";
	     _pname[BB]="Triangle"; 

	     _ndof[VV]=4;    
	     _ndof[BB]=3;
	  }
	  
// =======================
          FETet4::~FETet4() {    }

          
// =======================
      float FETet4::get_embedding_matrix(const uint a ,const uint b,const uint c )  {  return _embedding_matrix[a][b][c];  }
      

// =======================
//STATIC data member
	   const float FETet4::_embedding_matrix[8][4][4] =
 { // --------------------------------------------
    { // embedding matrix for child 0
      // 0    1    2    3  
      {1.0, 0.0, 0.0, 0.0}, // 0
      {0.5, 0.5, 0.0, 0.0}, // 1
      {0.5, 0.0, 0.5, 0.0}, // 2
      {0.5, 0.0, 0.0, 0.5}  // 3
    },
    { // embedding matrix for child 1
      // 0    1    2    3  
      {0.5, 0.5, 0.0, 0.0}, // 0
      {0.0, 1.0, 0.0, 0.0}, // 1
      {0.0, 0.5, 0.5, 0.0}, // 2
      {0.0, 0.5, 0.0, 0.5}  // 3
    },
    {// embedding matrix for child 2
      // 0    1    2    3  
      {0.5, 0.0, 0.5, 0.0}, // 0
      {0.0, 0.5, 0.5, 0.0}, // 1
      {0.0, 0.0, 1.0, 0.0}, // 2
      {0.0, 0.0, 0.5, 0.5}  // 3
    }, 
    {// embedding matrix for child 3
      // 0    1    2    3  
      {0.5, 0.0, 0.0, 0.5}, // 0
      {0.0, 0.5, 0.0, 0.5}, // 1
      {0.0, 0.0, 0.5, 0.5}, // 2
      {0.0, 0.0, 0.0, 1.0}  // 3
    },
    {  // embedding matrix for child 4
      // 0    1    2    3  
      {0.5, 0.5, 0.0, 0.0}, // 0
      {0.0, 0.5, 0.0, 0.5}, // 1
      {0.5, 0.0, 0.5, 0.0}, // 2
      {0.5, 0.0, 0.0, 0.5}  // 3
    },
    {// embedding matrix for child 5
      // 0    1    2    3  
      {0.5, 0.5, 0.0, 0.0}, // 0
      {0.0, 0.5, 0.5, 0.0}, // 1
      {0.5, 0.0, 0.5, 0.0}, // 2
      {0.0, 0.5, 0.0, 0.5}  // 3
    },
    { // embedding matrix for child 6
      // 0    1    2    3  
      {0.5, 0.0, 0.5, 0.0}, // 0
      {0.0, 0.5, 0.5, 0.0}, // 1
      {0.0, 0.0, 0.5, 0.5}, // 2
      {0.0, 0.5, 0.0, 0.5}  // 3
    },
  
    // embedding matrix for child 7
    {
      // 0    1    2    3  
      {0.5, 0.0, 0.5, 0.0}, // 0
      {0.0, 0.5, 0.0, 0.5}, // 1
      {0.0, 0.0, 0.5, 0.5}, // 2
      {0.5, 0.0, 0.0, 0.5}  // 3
    }
  };	   
  
  
   const double FETet4::_Prol[10*4/*NNDS*NNDSL*/] = { 
   1.,0.,0.,0.,
   0.,1.,0.,0.,
   0.,0.,1.,0.,
   0.,0.,0.,1.,
   0.5,0.5,0.,0.,
      0.,0.5,0.5,0.,
   0.5,0.,0.5,0.,
   0.5,0.,0.,0.5,
    0.,0.5,0.,0.5,
        0.,0.,0.5,0.5
   };  
  


} //end namespace femus


   