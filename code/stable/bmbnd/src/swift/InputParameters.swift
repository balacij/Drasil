/** InputParameters.swift
    Provides the function for reading inputs
    - Authors: Jason Balaci
*/
import Foundation

extension String: Error {}

/** Reads input from a file with the given file name
    - Parameter filename: name of the input file
    - Returns: coefficient of w_B's term of power 0 (N/m)
    - Returns: coefficient of w_B's term of power 1 (N/m^2)
    - Returns: coefficient of w_B's term of power 2 (N/m^3)
    - Returns: coefficient of w_B's term of power 3 (N/m^4)
    - Returns: length of the beam (m)
    - Returns: modulus of elasticity of the beam (Pa)
    - Returns: moment of second area of a cross-section of the beam (m)
*/
func get_input(_ filename: String) throws -> (Double, Double, Double, Double, Double, Double, Double) {
    var a_0: Double
    var a_1: Double
    var a_2: Double
    var a_3: Double
    var L_B: Double
    var E_B: Double
    var I_B: Double
    
    var infile: URL
    infile = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent(filename)
    var goolContents: [[String]]
    do {
        goolContents = try String(contentsOf: infile).components(separatedBy: "\n").map({(l: String) -> [String] in l.components(separatedBy: " ")})
    } catch {
        throw "Error reading from file."
    }
    a_0 = Double(goolContents[1][0])!
    a_1 = Double(goolContents[2][0])!
    a_2 = Double(goolContents[3][0])!
    a_3 = Double(goolContents[4][0])!
    L_B = Double(goolContents[5][0])!
    E_B = Double(goolContents[6][0])!
    I_B = Double(goolContents[7][0])!
    
    return (a_0, a_1, a_2, a_3, L_B, E_B, I_B)
}
