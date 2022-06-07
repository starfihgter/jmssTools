(* ::Package:: *)

Unprotect @@ Names["Tools`*"];
ClearAll @@ Names["Tools`*"];
PacletUninstall["ResourceFuncs"];
SetDirectory[NotebookDirectory[]];
PacletInstall["ResourceFuncs.paclet", ForceVersionInstall -> True];
Needs["ResourceFuncs`"];

BeginPackage["Tools`"]


TurningPointForm::usage = "For the expression: a \!\(\*SuperscriptBox[\(x\), \(2\)]\)+b x+c, CompleteTheSquare[x,a x^2+b x + c}] will return the equation in turning point form";
DetailedPlot::usage = "The same as Plot but with intercepts and intersections";
SolveTriangle::usage = "Finds the missing angles and side lengths of a triangle";
FTest::usage = "Finds whether equations using a given function are true";
TangentLine::usage = "Outputs the tangent to a curve at a given value";
CosRule::usage = "Uses cosine rule for side length";
CosRuleAngle::usage = "Uses cosine rule for angle";
SinRule::usage = "Uses sine rule for side";
SinRuleAngle::usage = "Uses sine rule for angle";
MatrixTransform::usage = "";
Surdify::usage = "Converts expressions with fractional powers to surd form.";
NormalLine::usage = "";
FindLine::usage = "";
RFTest::usage == "";
PolynomialDivide::usage == "Divides a polynomial. Why this isn't a built in function I don't know.";
StationaryPoints::usage == "Gives you some stationary points for a given function"
FunctionInfo::usage == "cool information it's very cool"
TrigRedefine::usage == "Transforms a trig function into an equivalent trig function"
NewtonIterative::Usage == "Performs Newton's Approximation to a defined number of interations"
QuickTransformMatrix::Usage == "Quickly provides a standard transformation Matrix, without fucking around with the basic math pallet."
InverseFunctionButBetter::Usage == "Provides all possible inverses for the user to determine which one is useful"
PlotIntegratedArea::Usage=="A nice graph to help visualise integrals"


DecimalPlaces = DecimalPlaces;
Labels = Labels;
Stationary = Stationary;
Intercepts = Intercepts;
Intersections = Intersections;
Discontinuities = Discontinuities;

isLoaded = True;
Begin["`Private`"]


(* ::Chapter:: *)
(*Alexi's additions (except for findline but i'm too lazy to move it )*)


FindLine[a_,b_,var_]:=Module[{y},y/.Solve[y-a[[2]]==((b[[2]]-a[[2]])/(b[[1]]-a[[1]]))(var - a[[1]]),y]//FullSimplify]


(* Alexi's Additions *)

SetAttributes[PolynomialDivide,HoldAll]
PolynomialDivide[numerator_,denominator_,var_]:= PolynomialQuotient[numerator,denominator,var]+PolynomialRemainder[numerator,denominator,var]/denominator

SetAttributes[StationaryPoints,HoldAll]
StationaryPoints[function_,var_]:= 
	Module[{x,y,coords,xcoord,secderiv},
	x= Solve[D[function,var]==0,var,Reals];
	y = function/.x;
	xcoord = var/.x;
     coords= Table[{xcoord[[i]],y[[i]],
     (* Sign test with accuracy of =-0.1, second deriv can have inconclusive outcomes *)
     If[ (function/.(var->(xcoord[[i]] - 0.1)))< y[[i]], (* left, smaller*)
		If[(function/.(var-> (xcoord[[i]] + 0.1)))< y[[i]],"Maximum", (* Right, smaller *)
			If[(function/.(var-> (xcoord[[i]] + 0.1)))> y[[i]],"S.P.I","This is a straight line"]], (*Right, Bigger *)
			If[(function/.(var-> (xcoord[[i]] + 0.1)))< y[[i]],"S.P.I", (*Right, Smaller *)
				If[(function/.(var-> (xcoord[[i]] + 0.1)))> y[[i]], "Minimum", "This is a straight line"]]] (*Right, Bigger *)
     },{i,Length[x]}]
	]
	





(* And these too *)


FunctionInfo[expr_,var_,y_]:=
TableForm[{
StringForm["Most-Simple form: ``",FullSimplify[expr]],
(*StringForm["Shape : ``", "Coming Soon"], *)
StringForm["x-intercepts: ``", Solve[expr == 0 ,var]],
StringForm["y-intercepts: ``", expr/.var -> 0],
StringForm["1st Derivative: ``", D[expr,var]],
StringForm["2nd Derivative: ``", D[expr,{var,2}]],
StringForm["Integral: ``", Integrate[expr,var]],
StringForm["Stationary Points: ``", StationaryPoints[expr,var]],
StringForm["Domain: ``", If[FunctionDomain[expr,var],"All Reals","Invalid",FunctionDomain[expr,var]]],
StringForm["Range: ``", If[FunctionRange[expr,var,y],"All Reals","Invalid",FunctionRange[expr,var,y]]]
(*StringForm["Amplitude and Period: ``", "Coming Soon"] *)
}]



SetAttributes[TrigRedefine, HoldAll]
TrigRedefine[expression_,targetFunction_,var_]:=
targetFunction[var]/.Solve[expression,var]


NewtonIterative[expr_,initalGuess_,numIts_,var_]:=For[approx=initalGuess;i=0,i<=numIts,i++;approx=approx-(expr/.var->approx)/(D[expr,var]/.var->approx),Print[approx]]


QuickTransformMatrix[a_,b_,c_,d_,x_,y_,h_,k_,hold_]:=If[hold==Hold,(MatrixForm[{{a,b},{c,d}} ] . MatrixForm[{{x},{y}}] )+ MatrixForm[{{h},{k}}],(({
 {a, b},
 {c, d}
}) . ({
 {x},
 {y}
}))+({
 {h},
 {k}
})]


InverseFunctionButBetter[expr_,x_,y_]:=Module[{adjustedExpr},adjustedExpr=expr/.x->y;
Solve[x==adjustedExpr,y,Reals]]


PlotIntegratedArea[function_,secondaryFunction_,{totalDomLower_,totalDomUpper_},{plotRangeLower_,plotRangeUpper_},{integralBoundLower_,integralBoundUpper_},var_]:=
Show[
	Plot[{function},{var,totalDomLower,totalDomUpper},PlotRange->{plotRangeLower,plotRangeUpper}],
Plot[{function,secondaryFunction},{var,integralBoundLower,integralBoundUpper},PlotRange->{plotRangeLower,plotRangeUpper},
Filling->{1->{2}}]]


(* ::Subtitle:: *)
(*Alexi Stuff Fin*)


(* ::Subsection::Closed:: *)
(*Surdify*)


SetAttributes[Surdify,HoldAll]
Surdify[x_]:=(x/.{\!\(\*
TagBox[
StyleBox[
RowBox[{"Power", "[", 
RowBox[{"n_", ",", 
RowBox[{"Rational", "[", 
RowBox[{"a_", ",", "b_"}], "]"}]}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)-> (Surd[n,b])^a})


(* ::Subsection::Closed:: *)
(*NormalLine*)


SetAttributes[NormalLine,HoldAll]

NormalLine[exp_,var_,point_]:=
	Module[{solutions,y},
	solutions = Solve[y - (exp/.{var-> point})==(-1/(D[exp,var]/.{var-> point}))(var - point),y];
	solutions = Table[y/.solutions[[i]],{i,Length[solutions]}];
	If[Length[solutions] ===1,solutions[[1]],solutions]
	]


(* ::Subsection::Closed:: *)
(*Matrix Transform*)


MatrixTransform[exp_,var_,dil_,trans_]:=
	Module[{sol,newx,newy,x,y},
		((newy/.Solve[((y==exp)/.Solve[({
 {newx},
 {newy}
})==dil . ({
 {var},
 {y}
})+trans,{var,y}]),{newy}])/.newx->var)[[1]]
	]


(* ::Subsection::Closed:: *)
(*CosRule*)


CosRule[b_,c_,A_]:=Sqrt[b^2+c^2-2*b*c*Cos[A]]


CosRuleAngle[a_,b_,c_]:= ArcCos[(a^2-b^2-c^2)/(-2 b c)]


SinRule[x_,y_]:= (y[[1]]*Sin[x])/Sin[y[[2]]]


SinRuleAngle[x_,y_]:= ArcSin[(x*Sin[y[[2]]])/y[[1]]]


(* ::Subsection::Closed:: *)
(*TangentLine*)


SetAttributes[TangentLine,HoldAll]

TangentLine[exp_,var_,point_]:=
	Module[{solutions,y},
	solutions = Solve[y - (exp/.{var-> point})==(D[exp,var]/.{var-> point})(var - point),y];
	solutions = Table[y/.solutions[[i]],{i,Length[solutions]}];
	If[Length[solutions] ===1,solutions[[1]],solutions]
	]


(* ::Subsection::Closed:: *)
(*RFTest*)


SetAttributes[RFTest, HoldAllComplete]
Options[RFTest] = {"Assumptions" -> {}};

InternalTest[exp_, cond_, var_, func_, asum_] :=
    Module[{solution,sss},	
     sss := Function[var,exp];
	solution = FullSimplify[ReleaseHold[cond]/.{func->sss},Assumptions ->asum];
	solution
    ]
    
SetAttributes[InternalTest, HoldAllComplete]
    



RFTest[exps_,cond_,var_,func_,opts:OptionsPattern[]]:=
	Module[{sols,funcs},
	funcs = If[Head@exps===List,exps,{exps}];
	Table[If[InternalTest[funcs[[i]]//Evaluate,cond,var,func,OptionValue["Assumptions"]]===True,StringForm["`` DOES satisfy condition",funcs[[i]]],StringForm["`` DOES NOT satisfy condition",funcs[[i]]]],{i,1,Length[funcs]}]//TableForm
	]


(* ::Subsection:: *)
(*FTest*)


(* ::Text:: *)
(*I still have no idea how this works*)


SetAttributes[FTest,HoldAllComplete];
Options[FTest] = {"Assumptions"->{}};

FTest[exp_,equations_,var_,func_,opts:OptionsPattern[]]:=
	Module[{solutions,f,eq,asum},
	asum = OptionValue["Assumptions"]//ReleaseHold;
	ClearAll[func];
	eq = If[Head@equations =!= List,{equations},equations];
	
	solutions = Table[func:=Function[var,exp];If[FullSimplify[ReleaseHold[eq[[i]]],Assumptions->asum]===True,ClearAll[func];StringForm["`` is True",eq[[i]]],ClearAll[func];StringForm["`` is False",eq[[i]]]],{i,1,Length[eq]}];
	ClearAll[func];
	TableForm[solutions]
	]


(* ::Subsection::Closed:: *)
(*SolveTriangle*)


Options[SolveTriangle]={"Degrees"-> True,"Area"-> False};
SetAttributes[SolveTriangle,HoldAll]
SolveTriangle[sides_,angles_,OptionsPattern[]]:=
	Module[{a,b,c,A,B,CC,solutions,vars},
	vars = {};
	a = sides[[1]];
	b = sides[[2]];
	c = sides[[3]];
	A = angles[[1]];
	B = angles[[2]];
	CC = angles[[3]];
	Table[
		If[Head@sides[[i]]===Symbol,vars = Append[vars,sides[[i]]]];
		If[Head@angles[[i]]===Symbol,vars = Append[vars,angles[[i]]]]
	,{i,1,3}];
	If[OptionValue["Degrees"],
	solutions = Solve[a/Sin[A \[Degree]]==b/Sin[B \[Degree]]&&b/Sin[B \[Degree]]==c/Sin[CC \[Degree]]&&0< A <180&&0 < B < 180&& 0 < CC < 180&&A + B + CC ==180  ,vars,Reals],
	solutions = Solve[a/Sin[A]==b/Sin[B ]&&b/Sin[B ]==c/Sin[CC]&&0< A <Pi&&0 < B < Pi&& 0 < CC < Pi&&A + B + CC ==Pi  ,vars,Reals]
	];
	If[OptionValue["Area"],(a/2*b*Sin[CC*If[OptionValue["Degrees"],Degree,1]])/.FullSimplify[solutions],solutions = FullSimplify[solutions]]
	

	]
	


SetAttributes[TurningPointForm, HoldAll]
TurningPointForm[expression_, var_] :=
    Module[{out,exp},
        exp = Expand[expression];
        x2co = Coefficient[expression, var, 2];
        xco = Coefficient[expression, var, 1];
        cons = Coefficient[expression, var, 0];
        out = x2co (var+xco/(2 x2co))^2+cons -(xco)^2/(4 x2co);
        out
    ]



(* ::Subsection::Closed:: *)
(*RestrictedInverse*)


SetAttributes[RestrictedInverse,HoldAll]

RestrictedInverse[variable_,expression_,condition_]:=
	InverseFunction[Function[variable, ConditionalExpression[expression, condition]]]


(* ::Subsection::Closed:: *)
(*DetailedPlot*)


(* ::Text:: *)
(*To anyone who may look at this in the future, my sincere apologies.*)


SetAttributes[DetailedPlot, HoldAll]
Options[DetailedPlot] = Flatten[Append[Options[Plot], {Labels -> True, DecimalPlaces -> False, Asymptotes -> True}]];

DetailedPlot[exp_, args__, opts:OptionsPattern[]] :=
	Module[{vAssGraph,funran,discon,vAsses,assVar, hAssGraph, y, currAss, hAsses, currAsses, ass, roundedPoint, mapOpts, otherExp, currentPoint, pointMap, currentKey, graph, pointsGraph, currentExpression, exps, whichLabels, currentSolutions, points, intersections, stationary, intercepts, ClosedHole, OpenHole, var, startPoint, endPoint, curSol},
		Off[Piecewise::pairs];
		var = args[[1]];
		
		startPoint = args[[2]];
		
		endPoint = args[[3]]; (*Draws the first graph*)
		
		hAsses = {}; vAsses = {};
		
		graph = Plot[exp, args, Evaluate[FilterRules[{opts}, Options[Plot]]]]; (*Redefines exp as a list*)
		
		exps = If[Head @ exp =!= List,
			{exp}
			,
			exp
		];
		
		mapOpts = <|stationary -> Tools`Stationary, intersections -> Tools`Intersections, intercepts -> Tools`Intercepts, OpenHole -> Discontinuities, ClosedHole -> Discontinuities|>;
		
		(*Point mapping*)
		
		pointMap = <|intercepts -> Blue, intersections -> Red, stationary -> Green, ClosedHole -> Black, OpenHole -> Gray|>;
		
		(* Defines the points association*)
		
		points = <|intercepts -> {}, stationary -> {}, intersections -> {}, OpenHole -> {}, ClosedHole -> {}|>; (*Loops through each graph*)
		
		Table[(* Sets the current expression *)
			currentExpression = exps[[i]]; (* Y intercepts*)
			
			Off[Power::infy];
			
			If[Element[currentExpression /. var -> 0, Reals],
				points[intercepts] = Append[points[intercepts], {0, currentExpression /. var -> 0}];
			];
			
			On[Power::infy];
			
			(*X intercepts*)
			
			currentSolutions = Solve[currentExpression == 0 && startPoint <= var <= endPoint, var, Reals];
			
			If[currentSolutions =!= {{}},
				Table[
					points[intercepts] = Append[points[intercepts], {var /. currentSolutions[[n]], 0}];, {n, Length[currentSolutions]}
				];
			];
			
			(* intersections *)
			
			Table[
				If[n =!= i,
					otherExp = exps[[n]];
					
					currentSolutions = Solve[otherExp == currentExpression && startPoint <= var <= endPoint, var];
					
					If[currentSolutions =!= {{}},
						Table[
							If[((currentExpression /. currentSolutions[[j]])// N) \[Element] Reals,
								points[intersections] = Append[points[intersections], {var, currentExpression} /. currentSolutions[[j]]]
							]
							, {j, 1, Length[currentSolutions]}
						];
					]
				]
				, {n, 1, Length[exps]}
			];
			
			(*stationary Points*)
			
			currentSolutions = Solve[(D[currentExpression, var] // Evaluate) == 0 && startPoint <= var <= endPoint, var, Reals];
			
			If[currentSolutions =!= {{}},
				Table[
				If[N[(currentExpression/.currentSolutions[[n]])]\[Element]Reals,
					points[stationary] = Append[points[stationary], {var /. currentSolutions[[n]], Simplify[currentExpression /. currentSolutions[[n]]]}];], {n, 1, Length[currentSolutions]}
				];
			]; (*Discontinuities*)
			
			If[Head @ exp === List,
				currentSolutions = ResourceFuncs`FunctionDiscontinuities[Extract[Hold[exp] /. {ConditionalExpression[x_, y_] :> Piecewise[{{x, y}}, Infinity], Piecewise[x_, Indeterminate] :> Piecewise[x, Infinity]}, {1, i}, Unevaluated], var, "Properties"]
				,
				currentSolutions = ResourceFuncs`FunctionDiscontinuities[Extract[Hold[{exp}] /. {ConditionalExpression[x_, y_] :> Piecewise[{{x, y}}, Infinity], Piecewise[x_, Indeterminate] :> Piecewise[x, Infinity]}, {1, i}, Unevaluated], var, "Properties"]
			];
			
			Table[
				curSol = currentSolutions[[n]];
				
				If[N[curSol[[2]][["LeftLimit"]]] \[Element] Reals,
					If[N[curSol[[2]][["LeftLimit"]]] === N[curSol[[2]][["ValueAtDiscontinuity"]]],
						points[ClosedHole] = Append[points[ClosedHole], {curSol[[1]][[2]], curSol[[2]][["ValueAtDiscontinuity"]]}]
						,
						points[OpenHole] = Append[points[OpenHole], {curSol[[1]][[2]], curSol[[2]][["LeftLimit"]]}]
					];
				];
				
				If[N[curSol[[2]][["RightLimit"]]] \[Element] Reals,
					If[N[curSol[[2]][["RightLimit"]]] === N[curSol[[2]][["ValueAtDiscontinuity"]]],
						points[ClosedHole] = Append[points[ClosedHole], {curSol[[1]][[2]], curSol[[2]][["ValueAtDiscontinuity"]]}]
						,
						points[OpenHole] = Append[points[OpenHole], {curSol[[1]][[2]], curSol[[2]][["RightLimit"]]}]
					];
				];
				, {n, 1, Length[currentSolutions]}
			];
			(*Cusp
									currentSolutions = Solve[Evaluate[D[currentExpression,var]] &&startPoint \[LessEqual] var \[LessEqual] endPoint,var];
									Print[currentSolutions];*)
			
			(* Asymptotes *)
			
			If[OptionValue[Asymptotes] === True,
				ass = ResourceFuncs`Asymptotes[currentExpression /. ConditionalExpression[x_, y_] :> x, var, y];
				
				(* Oblique asymptotes *)
				
				If[MemberQ[Keys[ass], "Oblique"],
					currAsses = ass["Oblique"];
					
					Table[
						hAsses = Append[hAsses, currAsses[[n]][[1]][[2]]];
						, {n, 1, Length[currAsses]}
					];
				];
				
				(*Horizontal*)
				
				If[MemberQ[Keys[ass], "Horizontal"],
					currAsses = ass["Horizontal"];
					
					Table[
						hAsses = Append[hAsses, currAsses[[n]][[1]][[2]]];
						
						assVar = currAsses[[n]][[2]][[1]]
						, {n, 1, Length[currAsses]}
					];
				];
				
				(* vertical *)
				funran = Reduce[Reduce[System`FunctionDiscontinuities[currentExpression,var]&&(startPoint-1)<=var<=(1+endPoint),var,Reals]//Simplify,var];
				discon = {};
				Table[
				Table[
					If[N[funran[[x,y]]]\[Element]Reals,AppendTo[discon,funran[[x,y]]]];
				,{y,1,Length[funran[[x]]]}];
				If[N[funran[[x]]]\[Element]Reals,AppendTo[discon,funran[[x]]]];
				,{x,1,Length[funran]}];
				
				Table[
					If[Or[
					(Limit[currentExpression,var->(discon[[n]]),Direction->1]//Abs)===Infinity,
					(Limit[currentExpression,var->(discon[[n]]),Direction->-1]//Abs)===Infinity
					],
					vAsses = Append[vAsses,discon[[n]]]
					];
					If[Limit[currentExpression,var->(discon[[n]]),Direction->1]//N === (currentExpression/.var->discon[[n]])//N && ((currentExpression/.var->discon[[n]])//N)\[Element]Reals,
						points[ClosedHole] = Append[points[ClosedHole],{discon[[n]],Limit[currentExpression,var->(discon[[n]]),Direction->1]}]
					,
						If[(Limit[currentExpression,var->(discon[[n]]),Direction->1]//N )\[Element]Reals,
							points[OpenHole] = Append[points[ClosedHole],{discon[[n]],Limit[currentExpression,var->(discon[[n]]),Direction->1]}]
						]
					];
					If[Limit[currentExpression,var->(discon[[n]]),Direction->-1]//N === (currentExpression/.var->discon[[n]])//N && ((currentExpression/.var->discon[[n]])//N)\[Element]Reals,
						points[ClosedHole] = Append[points[ClosedHole],{discon[[n]],Limit[currentExpression,var->(discon[[n]]),Direction->1]}]
					,
						If[(Limit[currentExpression,var->(discon[[n]]),Direction->-1]//N )\[Element]Reals,
							points[OpenHole] = Append[points[ClosedHole],{discon[[n]],Limit[currentExpression,var->(discon[[n]]),Direction->1]}]
						]
					];
				,{n,1,Length[discon]}];
			];
			, {i, 1, Length[exps]}
		];
		
		(*Drawing obliqueAssymptotes*)
		
		If[OptionValue[Asymptotes] === True,
			hAsses = hAsses /. assVar :> var;
			
			hAssGraph = Plot[Tooltip[hAsses // Evaluate], args, PlotStyle -> {{Red, Dashed}}] // Evaluate;
		
		(*Vertical asymptotes*)
		If[OptionValue[Asymptotes]===True,
			vAssGraph = Graphics[Table[
			{Red,Dashed,Tooltip[Line[{{vAsses[[i]],(PlotRange/.graph[[2]])[[2]][[1]]-100},{vAsses[[i]],(PlotRange/.graph[[2]])[[2]][[2]]+100}}],vAsses[[i]]]}
			,{i,1,Length[vAsses]}]]
		];
		];
		
		(* Creating Everything *)
		
		pointsGraph = Graphics[Table[
			currentKey = Keys[points][[i]];
			
			points[currentKey] = FullSimplify[DeleteDuplicates[points[currentKey]]];
			
			Table[
				currentPoint = points[currentKey][[n]];
				
				{PointSize[1 / 50], pointMap[currentKey], Tooltip[Point[currentPoint],
					If[OptionValue[DecimalPlaces] =!= False,
						roundedPoint = Round[currentPoint, 10^-OptionValue[DecimalPlaces]] // N
						,
						roundedPoint = currentPoint
					];
					
					roundedPoint
				], If[OptionValue[Labels] === True || MemberQ[OptionValue[Labels], mapOpts[currentKey]]||mapOpts[currentKey]===OptionValue[Labels],
					Text[roundedPoint, currentPoint, {-1, -1}]
				]}
				, {n, 1, Length[points[currentKey]]}
			]
			, {i, 1, Length[Keys[points]]}
		]];
		
		On[Piecewise::pairs];
		(*Formatting label*)
		
		If[OptionValue[Asymptotes] === True,
			Show[graph, vAssGraph,hAssGraph, graph, pointsGraph]
			,
			Show[graph, pointsGraph]
		]
	]


(* ::Subsection::Closed:: *)
(*Turning Point Form*)


TurningPointForm[var_, expression_] :=
    Module[{out,exp,x2co,xco,cons},
        exp = Expand[expression];
        x2co = Coefficient[expression, var, 2];
        xco = Coefficient[expression, var, 1];
        cons = Coefficient[expression, var, 0];
        out = x2co (var+xco/(2 x2co))^2+cons -(xco)^2/(4 x2co);
        out
    ]


(* ::Subsubsection:: *)
(**)


(* ::Subsection:: *)
(*End Statements*)


End[];

Protect @@ Names["Tools`*"];

EndPackage[]
