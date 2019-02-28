import XCTest
import SwiftSyntax
import SwiftLang
@testable import SwiftEvolve

class ShuffleGenericRequirementsEvolutionTests: XCTestCase {
  var predictableRNG = PredictableGenerator(values: 1..<16)

  func testEvolution() throws {
    let code = try SyntaxParser.parse(source:
      """
      func foo<T>(_: T) where T: Hashable, T == Comparable {}
      """
    )
    let decl = code.filter(whereIs: FunctionDeclSyntax.self).first!
    let dc = DeclChain(decls: [code, decl])

    let evo = try ShuffleGenericRequirementsEvolution(
      for: decl.genericWhereClause!.requirementList, in: dc, using: &predictableRNG
    )

    XCTAssertEqual(evo?.mapping.count, 2)

    let evolved = evo?.evolve(decl.genericWhereClause!.requirementList)
    XCTAssertEqual(evolved.map(String.init(describing:)),
                   "T == Comparable , T: Hashable")
  }

  func testBypass() throws {
    let code = try SyntaxParser.parse(source:
      """
      func foo<T>(_: T) where T: Hashable, T == Comparable {}
      """
    )
    let decl = code.filter(whereIs: FunctionDeclSyntax.self).first!
    let dc = DeclChain(decls: [code, decl])

    XCTAssertThrowsError(
      try ShuffleGenericRequirementsEvolution(
        for: decl.genericWhereClause!, in: dc, using: &predictableRNG
      )
    ) { error in
      XCTAssertEqual(error as? EvolutionError, EvolutionError.unsupported)
    }
  }
}
