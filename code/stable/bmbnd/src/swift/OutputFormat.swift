/** OutputFormat.swift
    Provides the function for writing outputs
    - Authors: Jason Balaci
*/
import Foundation

/** Writes the output values to output.txt
    - Parameter y_B: deflection at a particular point along the beam (m)
*/
func write_output(_ y_B: (Double, Float) -> Double) throws -> Void {
    var outputfile: FileHandle
    do {
        outputfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("output.txt"))
    } catch {
        throw "Error opening file."
    }
    do {
        try outputfile.write(contentsOf: Data("y_B = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(y_B).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.close()
    } catch {
        throw "Error closing file."
    }
}
