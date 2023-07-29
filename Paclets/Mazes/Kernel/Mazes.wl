(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["PeterBurbery`Mazes`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


TriangularGridGraph;
GeneralizedTriangularGridGraph;
HexagonalGridGraph;
Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*Define your public and private symbols here:*)


HexagonalGridGraph[{Pattern[
     wide, 
Blank[Integer]] ? Positive, 
    Pattern[high, 
Blank[Integer]] ? Positive}, 
   opts : 
    OptionsPattern[Graph]] := Module[
       {cells, 
    edges, 
    vertices},
       cells = 
    Flatten[
             Table[
                  CirclePoints[
                       {
                            
        Sqrt[3] * (((2 * 
               j)
 + k) - 2),
                            (3 * 
           k) - 
         2
                        },
                       {2, Pi / 2},
                       6
                   ],
                  {j, 
       wide},
                  {k, 
       high}
              ],
             1
         ];
       edges = 
    Union[
             Map[Sort,
                  
      Flatten[Map[Partition[#, 2, 1, 1] &, 
        cells], 1]
              ]
         ];
       vertices 
= Union @ 
     Flatten[
edges, 1];
       IndexGraph[
            
    Graph[UndirectedEdge @@@ 
      edges,
                 
     opts,
                 
     VertexCoordinates -> 
      Thread[
vertices -> 
        vertices]

             ]
        ]
   ];

TriangularGridGraph[
   Pattern[n0, 
Blank[Integer]] ? NonNegative, 
   opts : 
    OptionsPattern[Graph]] := Module[
       {n = 
     n0 + 1, 
    edges, 
    tab},
       tab = 
    TakeList[
     Range[Binomial[
       n + 1, 
       2]], Range @ 
      n];
       edges = 
    MapApply[UndirectedEdge,
             Union[
                  Flatten[
                       Map[
                            Function[
                                 {
                                      
          Transpose[{Part[#, 1], Most[Part[#, 2]]}],
                                      
          Transpose[{Part[#, 1], Rest[Part[#, 2]]}]
                                  }
                             ],
                            
        Partition[
         tab, 2, 
         1]
                        ],
                       2
                   ],
                  
      Flatten[Map[Partition[#, 2, 1] &, 
        tab], 1]
              ]
         ];
       Graph[
    Flatten @ 
     Most @ tab,
            
    edges, 
    opts, 
    VertexCoordinates -> Flatten[
                  
      Table[{i - 
         j / 
          
          2, (-j)
 * Sqrt[3] / 2},
                       {j, 
        n},
                       {i, 
        j}
                   ],
                  1
              ]
        ]
   ];


GeneralizedTriangularGridGraph[
   input : 
{Pattern[m, 
Blank[]] ? 
      
PositiveIntegerQ, 
     Pattern[n, 
Blank[]] ? 
      
PositiveIntegerQ}, 
   opts : 
    OptionsPattern[Graph]] := Module[
       {
            
    
vertexCoordinates, 
    
triangularGridGraph, 
    vertices, 
    
vertexCoordinatesToVerticesAssociation,
            
    
topPartDeletedGraph, 
    subGraph, 
    verticesToKeep
        },
       
triangularGridGraph = 
    
TriangularGridGraph[
     input + 1];
       
vertexCoordinates = 
    
VertexCoordinateList[
     
triangularGridGraph];
       vertices 
= VertexList @ 
     
triangularGridGraph;
       
topPartDeletedGraph = VertexDelete[
             
     
triangularGridGraph,
             Flatten[
                  
      PositionLargest[
       
vertexCoordinates, 1, Function @ Order[Last @ #, Last @ #2]]
              ]
         ];
       
vertexCoordinatesToVerticesAssociation = 
    AssociationThread[
     
vertexCoordinates -> 
      vertices];
       
verticesToKeep = Lookup[
             
     
vertexCoordinatesToVerticesAssociation,
             
     Catenate @ 
      Values @ 
       Map[Most, 
        Map[SortBy[First], 
         GroupBy[Last][
          
vertexCoordinates]]]
         ];
       subGraph 
= Subgraph[
     
topPartDeletedGraph, 
     
verticesToKeep, 
     opts]
   ];
(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];