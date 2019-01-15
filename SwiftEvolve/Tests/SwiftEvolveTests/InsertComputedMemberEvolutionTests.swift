import XCTest
import SwiftSyntax
@testable import SwiftEvolve

class InsertComputedMemberEvolutionTests: XCTestCase {
  var predictableRNG = LinearCongruentialGenerator(seed: 42)

  func testInit() throws {
    let code = try SyntaxParser.parse(source:
      """
      enum Foo {
        case a
        case b
        func x() -> Int { return 0 }
      }
      """
    )
    let decl = code.filter(whereIs: EnumDeclSyntax.self).first!
    let dc = DeclContext(declarationChain: [code, decl])

    XCTAssertThrowsError(
      try InsertComputedMemberEvolution(
        for: decl, in: dc, using: &predictableRNG
      )
    ) { error in
      XCTAssertEqual(error as? EvolutionError, EvolutionError.unsupported)
    }

    guard let evo1 = try InsertComputedMemberEvolution(
      for: decl.members.members, in: dc, using: &predictableRNG
    ) else { XCTFail(); return }

    XCTAssertEqual(evo1.index, 3)
    XCTAssertEqual(evo1.name, "__swiftEvolveInserted8023441616847907134")
    XCTAssertEqual(evo1.labeledParameters, [])
    XCTAssertEqual(evo1.memberKind, .variable)
    XCTAssertEqual(evo1.staticKeyword, .instance)
    XCTAssertEqual(evo1.accessLevel, .private)
    XCTAssertEqual(evo1.isConvenience, false)

    guard let evo2 = try InsertComputedMemberEvolution(
      for: decl.members.members, in: dc, using: &predictableRNG
    ) else { XCTFail(); return }

    XCTAssertEqual(evo2.index, 1)
    XCTAssertEqual(evo2.name, "__swiftEvolveInserted12391666931450987487")
    XCTAssertEqual(evo2.labeledParameters, [true, true, true, false, false])
    XCTAssertEqual(evo2.memberKind, .subscription)
    XCTAssertEqual(evo2.staticKeyword, .instance)
    XCTAssertEqual(evo2.accessLevel, .private)
    XCTAssertEqual(evo2.isConvenience, false)


    guard let evo3 = try InsertComputedMemberEvolution(
      for: decl.members.members, in: dc, using: &predictableRNG
    ) else { XCTFail(); return }
    XCTAssertEqual(evo3.index, 3)
    XCTAssertEqual(evo3.name, "__swiftEvolveInserted5723979029937790549")
    XCTAssertEqual(evo3.labeledParameters, [])
    XCTAssertEqual(evo3.memberKind, .function)
    XCTAssertEqual(evo3.staticKeyword, .instance)
    XCTAssertEqual(evo3.accessLevel, .fileprivate)
    XCTAssertEqual(evo3.isConvenience, false)
  }

  func testEvolve() throws {
    let code = try SyntaxParser.parse(source:
      """
      enum Foo {
        case a
        case b
        func x() -> Int { return 0 }
      }
      """
    )
    let decl = code.filter(whereIs: EnumDeclSyntax.self).first!

    let evo1 = InsertComputedMemberEvolution(
      index: 3,
      name: "__swiftEvolveInserted8023441616847907134",
      labeledParameters: [],
      memberKind: .variable,
      staticKeyword: .instance,
      accessLevel: .fileprivate,
      isConvenience: false
    )
    let evolved1 = evo1.evolve(decl.members.members)
    XCTAssertEqual(String(describing: evolved1), """

        case a
        case b
        func x() -> Int { return 0 }

      // Synthesized by InsertComputedMemberEvolution
      fileprivate var __swiftEvolveInserted8023441616847907134: Any {
        fatalError("Resilience failure: Called a computed member inserted during later evolution!")
      }

      """)

    let evo2 = InsertComputedMemberEvolution(
      index: 1,
      name: "__swiftEvolveInserted12391666931450987487",
      labeledParameters: [true, true, true, false, false],
      memberKind: .subscription,
      staticKeyword: .instance,
      accessLevel: .public,
      isConvenience: false
    )
    let evolved2 = evo2.evolve(decl.members.members)
    XCTAssertEqual(String(describing: evolved2), """

        case a

      // Synthesized by InsertComputedMemberEvolution
      public subscript(__swiftEvolveInserted12391666931450987487 _: Any, __swiftEvolveInserted12391666931450987487 _: Any, __swiftEvolveInserted12391666931450987487 _: Any, _ _: Any, _ _: Any) -> Any {
        fatalError("Resilience failure: Called a computed member inserted during later evolution!")
      }

        case b
        func x() -> Int { return 0 }
      """)

    let evo3 = InsertComputedMemberEvolution(
      index: 3,
      name: "__swiftEvolveInserted5723979029937790549",
      labeledParameters: [],
      memberKind: .function,
      staticKeyword: .instance,
      accessLevel: .fileprivate,
      isConvenience: false
    )
    let evolved3 = evo3.evolve(decl.members.members)
    XCTAssertEqual(String(describing: evolved3), """

        case a
        case b
        func x() -> Int { return 0 }

      // Synthesized by InsertComputedMemberEvolution
      fileprivate func __swiftEvolveInserted5723979029937790549() -> Any {
        fatalError("Resilience failure: Called a computed member inserted during later evolution!")
      }

      """)
  }
}
