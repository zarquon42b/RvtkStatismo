#include "polyData2R.h"

Rcpp::List polyData2R(vtkSmartPointer<vtkPolyData> polydata) {
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  //polydata = reader->GetOutput();
  
  int np = polydata->GetNumberOfPoints();
  points=polydata->GetPoints();
  NumericMatrix vb(3,np);
  double point[3];
  for (int i=0; i< np;i++) {
    polydata->GetPoints()->GetPoint(i,point);
    for (int j=0; j<3; j++)
      vb(j,i) = point[j];
  }
  
  int h;
  vtkIdType npts=3,*pts; 
  int nit = polydata->GetNumberOfPolys();
  IntegerMatrix it(3,nit);
  vtkSmartPointer<vtkCellArray> oCellArr= vtkSmartPointer<vtkCellArray>::New();
  oCellArr=polydata->GetPolys();
  for (int i=0; i< nit;i++) {
    h = oCellArr->GetNextCell(npts,pts); 
    if(h == 0){
      break;
    } 
    if(npts == 3){
      it(0,i) = pts[0]+1;
      it(1,i) = pts[1]+1;
      it(2,i) = pts[2]+1; 
    }
  }
  List out = List::create(Named("vb")=vb,
		       Named("it")=it
		       );
  if (it.ncol() > 0)
    out.attr("class") = "mesh3d";
  return  out;
} 

 
