#include "polyData2R.h"
#include <vtkSmartPointer.h>
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
  std::vector<int> it;
  vtkSmartPointer<vtkCellArray> oCellArr= vtkSmartPointer<vtkCellArray>::New();
  polydata->GetPolys()->InitTraversal();
  while(polydata->GetPolys()->GetNextCell(npts,pts)) {
    //for (int i=0; i< nit;i++) {
    // h = oCellArr->GetNextCell(npts,pts); 
    //if(h == 0){
    //  break;
    //} 
    if(npts == 3){
      it.push_back(pts[0]);
      it.push_back(pts[1]);
      it.push_back(pts[2]); 
    }
  }
  int ll = it.size()/3;
  IntegerMatrix itout(3,ll,it.begin());
  List out = List::create(Named("vb")=vb,
		       Named("it")=itout
		       );
  if (it.size() > 0)
    out.attr("class") = "mesh3d";
  return  out;
} 

 
