(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["PeterBurbery`NewMazePaclet`"];



(* ::Text:: *)
(*Declare your public symbols here:*)


TriangularGridGraph;

HexagonalGridGraph;

MakeMaze;

PositiveIntegerQ;

SolveMaze;

EquilateralTriangleGraph;

ReduceGraph;

VertexCoordinateList;

Begin["`Private`"];



(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*Define your public and private symbols here:*)


helperFunctionTriangularGraph[{wide_, high_}, opts : OptionsPattern[Graph
   ]] :=
   Module[{cells, edges, vertices},
      cells = Flatten[Table[CirclePoints[{Sqrt[3] * (((2 * j) + k) - 
         2), (3 * k) - 2}, {2, Pi / 2}, 3], {j, wide}, {k, high}], 1];
      edges = Union[Map[Sort, Flatten[Map[Partition[#, 2, 1, 1]&, cells
         ], 1]]];
      vertices = Union @ Flatten[edges, 1];
      IndexGraph[Graph[MapApply[UndirectedEdge, edges], opts, VertexCoordinates
          -> Thread[vertices -> vertices]]]
   ]

HexagonalGridGraph[{Pattern[wide, Blank[Integer]] ? Positive, Pattern[
   high, Blank[Integer]] ? Positive}, opts : OptionsPattern[Graph]] :=
   Module[{cells, edges, vertices},
      cells = Flatten[Table[CirclePoints[{Sqrt[3] * (((2 * j) + k) - 
         2), (3 * k) - 2}, {2, Pi / 2}, 6], {j, wide}, {k, high}], 1];
      edges = Union[Map[Sort, Flatten[Map[Partition[#, 2, 1, 1]&, cells
         ], 1]]];
      vertices = Union @ Flatten[edges, 1];
      IndexGraph[Graph[UndirectedEdge @@@ edges, opts, VertexCoordinates
          -> Thread[vertices -> vertices]]]
   ];

EquilateralTriangleGraph[Pattern[n0, Blank[Integer]] ? NonNegative, opts
    : OptionsPattern[Graph]] :=
   Module[{n = n0 + 1, edges, tab},
      tab = TakeList[Range[Binomial[n + 1, 2]], Range @ n];
      edges = MapApply[UndirectedEdge, Union[Flatten[Map[Function[{Transpose[
         {Part[#, 1], Most[Part[#, 2]]}], Transpose[{Part[#, 1], Rest[Part[#, 
         2]]}]}], Partition[tab, 2, 1]], 2], Flatten[Map[Partition[#, 2, 1]&, 
         tab], 1]]];
      Graph[Flatten @ Most @ tab, edges, opts, VertexCoordinates -> Flatten[
         Table[{i - j / 2, (-j) * Sqrt[3] / 2}, {j, n}, {i, j}], 1]]
   ];

TriangularGridGraph[input : {m_?PositiveIntegerQ, n_?PositiveIntegerQ
   }, opts : OptionsPattern[Graph]] :=
   Module[{vertexCoordinates, triangularGridGraph, vertices, vertexCoordinatesToVerticesAssociation,
       topPartDeletedGraph, subGraph, verticesToKeep},
      triangularGridGraph = helperFunctionTriangularGraph[input + 1];
         
      vertexCoordinates = VertexCoordinateList[triangularGridGraph];
      vertices = VertexList @ triangularGridGraph;
      topPartDeletedGraph = VertexDelete[triangularGridGraph, Flatten[
         PositionLargest[vertexCoordinates, 1, Function @ Order[Last @ #, Last
          @ #2]]]];
      vertexCoordinatesToVerticesAssociation = AssociationThread[vertexCoordinates
          -> vertices];
      verticesToKeep = Lookup[vertexCoordinatesToVerticesAssociation,
          Catenate @ Values @ Map[Most, Map[SortBy[First], GroupBy[Last][vertexCoordinates
         ]]]];
      subGraph = Subgraph[topPartDeletedGraph, verticesToKeep, opts]
   ];

PositiveIntegerQ[Pattern[n, Blank[]]] :=
   Apply[And, {IntegerQ[n], TrueQ[Element[n, PositiveIntegers]]}];

VertexCoordinateList[Pattern[g, Blank[]]] :=
   Part[GraphEmbedding @ g, Map[Last, Sort @ Transpose @ {VertexList 
      @ g, Range @ VertexCount @ g}]];

MakeMaze // ClearAll

MakeMaze::usage = "MakeMaze[g] makes a maze out of the graph g.";

MakeMaze[graph_?GraphQ] :=
   Module[{m, g},
      g = graph;
      m =
         Graph[
            VertexList[g]
            ,
            Module[{stack = {RandomChoice[VertexList[g]]}},
               Do[AnnotationValue[{g, v}, "Visited"] = False, {v, VertexList[
                  g]}];
               AnnotationValue[{g, First @ stack}, "Visited"] = True;
                  
               While[
                  Not[stack == {}]
                  ,
                  With[{v = First @ stack},
                     With[{adj = Cases[EdgeList[g, v \[UndirectedEdge]
                         _] /. {v \[UndirectedEdge] u_ :> u, u_ \[UndirectedEdge] v :> u}, x_
                         /; \[Not]AnnotationValue[{g, x}, "Visited"]]},
                        If[adj == {},
                           stack = Rest @ stack
                           ,
                           With[{w = RandomChoice[adj]},
                              PrependTo[stack, w];
                              AnnotationValue[{g, w}, "Visited"] = True
                                 ;
                              Sow[v \[UndirectedEdge] w]
                           ]
                        ]
                     ]
                  ]
               ]
            ] //
            Reap //
            Last //
            First
            ,
            VertexCoordinates -> GraphEmbedding[g]
         ]
   ]

ReduceGraph // ClearAll

ReduceGraph::usage = "ReduceGraph[ugraph] contracts vertices with a vertex degree of 2  in the undirected graph ugraph .\nReduceGraph[dgraph] contracts vertices with an in degree of 1 and out degree of 1 in the directed graph dgraph.";

ReduceGraph[graph_?DirectedGraphQ, opts : OptionsPattern[Graph]] :=
   Graph[
      Fold[VertexContract[#1, List[First[AdjacencyList[#1, #2]], #2]]&,
          graph, VertexList[graph, v_ /; And @@ {VertexInDegree[graph, v] === 
         1, VertexOutDegree[graph, v] === 1}]]
      ,
      opts
      ,
      VertexCoordinates -> Automatic
              (*If the user changes the value of VertexCoordinates \
         
since opts appears before this their specification will take effect.*)
   
      ,
      VertexLabels -> Automatic
   ]

ReduceGraph[graph_?UndirectedGraphQ, opts : OptionsPattern[Graph]] :=
   Graph[Fold[VertexContract[#1, List[First[AdjacencyList[#1, #2]], #2
      ]]&, graph, VertexList[graph, v_ /; And @@ {VertexDegree[graph, v] ===
       2}]], opts, VertexCoordinates -> Automatic, VertexLabels -> Automatic
      ]

SolveMaze // ClearAll

SolveMaze::usage = "SolveMaze[g] solves the maze represented by the graph g.";

SolveMaze[m_?GraphQ] :=
   HighlightGraph[m, PathGraph @ FindShortestPath[m, First @ VertexList[
      m], Last @ VertexList[m]]]



(* ::Section::Closed:: *)
(*Package Footer*)


End[];

EndPackage[];
