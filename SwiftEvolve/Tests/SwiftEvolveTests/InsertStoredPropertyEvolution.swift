import XCTest
import SwiftSyntax
@testable import SwiftEvolve

class InsertStoredPropertyEvolutionTests: XCTestCase {
  var predictableRNG = LinearCongruentialGenerator(seed: 42)

  func testInit() throws {
    let code = try SyntaxParser.parse(source:
      """
      struct Struct {
        var a: Int
        var b: String
        func x() -> Int { return 0 }
      }
      class Class {
        var a: Int
        var b: String
        func x() -> Int { return 0 }
      }
      """
    )

    do {
      let decl = code.filter(whereIs: StructDeclSyntax.self).first!
      let dc = DeclChain(decls: [code, decl])

      XCTAssertThrowsError(
        try InsertStoredPropertyEvolution(
          for: decl, in: dc, using: &predictableRNG
        )
      )

      guard let evo = try InsertStoredPropertyEvolution(
        for: decl.members.members, in: dc, using: &predictableRNG
      ) else {
        XCTFail()
        return
      }

      XCTAssertContains(evo.index, 0..<3)
      XCTAssert(evo.name.starts(with: "__swiftEvolveInserted"))
      XCTAssertContains(evo.accessLevel, [.private, .fileprivate, .internal])
    }

    do {
      let decl = code.filter(whereIs: ClassDeclSyntax.self).first!
      let dc = DeclChain(decls: [code, decl])

      XCTAssertThrowsError(
        try InsertStoredPropertyEvolution(
          for: decl, in: dc, using: &predictableRNG
        )
      )

      guard let evo = try InsertStoredPropertyEvolution(
        for: decl.members.members, in: dc, using: &predictableRNG
        ) else {
          XCTFail()
          return
      }

      XCTAssertContains(evo.index, 0..<3)
      XCTAssert(evo.name.starts(with: "__swiftEvolveInserted"))
      XCTAssertContains(evo.accessLevel, [.private, .fileprivate, .internal])
    }
  }

  func testInitIneligible() throws {
    let code = try SyntaxParser.parse(source:
      """
      @_fixed_layout struct FixedLayoutStruct {
        var a: Int
        var b: String
        func x() -> Int { return 0 }
      }
      @_fixed_layout class FixedLayoutClass {
        var a: Int
        var b: String
        func x() -> Int { return 0 }
      }
      enum Enum {
        case a (Int)
        case b (String)
        func x() -> Int { return 0 }
      }
      protocol Protocol {
        var a: Int { get }
        var b: String { get }
        func x() -> Int
      }
      extension ExtendingType {
        var a: Int { return 0 }
        var b: String { return "" }
        func x() -> Int { return 0 }
      }
      """
    )

    func checkDecl<T: Decl & DeclWithMembers>(
      ofType type: T.Type,
      file: StaticString = #file,
      line: UInt = #line
    ) {
      let decl = code.filter(whereIs: type).first!
      let dc = DeclChain(decls: [code, decl])

      XCTAssertThrowsError(
        try InsertStoredPropertyEvolution(
          for: decl.members.members, in: dc, using: &predictableRNG
        ),
        "Should throw for \(decl.descriptiveName)",
        file: file, line: line
      ) { error in
        XCTAssertEqual(
          error as? EvolutionError,
          EvolutionError.unsupported,
          "Should throw unsupported error for \(decl.descriptiveName)",
          file: file, line: line
        )
      }
    }

    checkDecl(ofType: StructDeclSyntax.self)
    checkDecl(ofType: ClassDeclSyntax.self)
    checkDecl(ofType: EnumDeclSyntax.self)
    checkDecl(ofType: ProtocolDeclSyntax.self)
    checkDecl(ofType: ExtensionDeclSyntax.self)
  }

  func testEvolve() throws {
    let code = try SyntaxParser.parse(source:
      """
      struct Struct {
        var a: Int
        var b: String
        func x() -> Int { return 0 }
      }
      class Class {
        var a: Int
        var b: String
        func x() -> Int { return 0 }
      }
      """
    )

    do {
      let decl = code.filter(whereIs: StructDeclSyntax.self).first!
      let evo = InsertStoredPropertyEvolution(
        index: 1, name: "__swiftEvolveInserted0", accessLevel: .private
      )
      let evolved = evo.evolve(decl.members.members)
      XCTAssertEqual(
        String(describing: evolved),
        """

          var a: Int

        // Synthesized by InsertStoredPropertyEvolution
        private var __swiftEvolveInserted0: Any="Synthesized by InsertStoredPropertyEvolution"
          var b: String
          func x() -> Int { return 0 }
        """)
    }

    do {
      let decl = code.filter(whereIs: ClassDeclSyntax.self).first!
      let evo = InsertStoredPropertyEvolution(
        index: 3, name: "__swiftEvolveInserted42", accessLevel: .internal
      )
      let evolved = evo.evolve(decl.members.members)
      XCTAssertEqual(
        String(describing: evolved),
        """

          var a: Int
          var b: String
          func x() -> Int { return 0 }

        // Synthesized by InsertStoredPropertyEvolution
        internal var __swiftEvolveInserted42: Any="Synthesized by InsertStoredPropertyEvolution"
        """)
    }
  }
}

func XCTAssertContains<C>(
  _ elem: C.Element, _ collection: C,
  _ message: String? = nil, file: StaticString = #file, line: UInt = #line
) where C: Collection, C.Element: Equatable {
  let failureString = "\(elem) not contained in \(collection)"
  let fullMessage = message == nil ? failureString : "\(message!): \(failureString)"

  XCTAssert(collection.contains(elem), fullMessage, file: file, line: line)
}
