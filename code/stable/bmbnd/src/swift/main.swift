/** main.swift
    Controls the flow of the program
    - Authors: Jason Balaci
*/
var filename: String = CommandLine.arguments[0]
var a_0: Double
var a_1: Double
var a_2: Double
var a_3: Double
var L_B: Double
var E_B: Double
var I_B: Double
(a_0, a_1, a_2, a_3, L_B, E_B, I_B) = try get_input(filename)
var y_B: (Double, Float) -> Double = func_y_B(E_B)
try write_output(y_B)
