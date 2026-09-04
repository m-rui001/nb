(* ::Package:: *)

BeginPackage["FormatExpr`"];


FormatExpr;
SetAttributes[FormatExpr, HoldAll];


Begin["`Private`"];


FormatExpr[code_] := Module[{count = 0, str1, str2, str3, str4},
	str1 = "\"" <> StringReplace[ToString[code, InputForm], "\\\"" -> "\"\""] <> "\"";
	str2 = StringReplace[str1,
		sub1: RegularExpression["\"[^\"]*\""] :> StringReplace[sub1,{" " -> "",
			sub2: ("("|"["|"{"|"<|"|">|"|"}"|"]"|")"|","|";") :> Which[MatchQ[sub2, Alternatives["(", "[", "{", "<|"]], PreIncrement[count]; (sub2 <> "\n") <> StringRepeat["\t", count], MatchQ[sub2, Alternatives[")", "]", "}", "|>"]], PreDecrement[count]; sub2, MatchQ[sub2, Alternatives[",", ";"]], (sub2 <> "\n") <> StringRepeat["\t", count]]}]];
	str3 = StringTrim[str2, "\""];
	str4 = StringReplace[str3, "\"\"" -> "\\\""]];


End[];


EndPackage[];
