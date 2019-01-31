import XCTest
import SwiftSyntax
@testable import SwiftEvolve

class DeclChainTests: XCTestCase {
  func testMaximumAccessLevelBasic() throws {
    let code = try SyntaxParser.parse(source:
      """
      internal struct Washington {}
      enum Jefferson {}
      private protocol Burr {}
      open class Hamilton {}

      // No where clauses
      extension Hamilton {}

      // Extended type more restricted
      extension Burr where T: Hamilton {}

      // Where clause more restricted
      extension Hamilton where U: Jefferson {}

      // Multiple where clauses
      extension Hamilton where T: Burr, U: Jefferson {}

      // Generic type included
      extension Hamilton where T: Washington<Burr> {}
      extension Hamilton where T: Washington<Hamilton<Jefferson, Burr>> {}
      """
    )

    func makeDeclChain(_ decl: Decl) -> DeclChain {
      return DeclChain(decls: [code, decl])
    }

    let explicitlyInternal = code.filter(whereIs: StructDeclSyntax.self).first!
    XCTAssertEqual(makeDeclChain(explicitlyInternal).maximumAccessLevel,
                   .internal)

    let implicitlyInternal = code.filter(whereIs: EnumDeclSyntax.self).first!
    XCTAssertEqual(makeDeclChain(implicitlyInternal).maximumAccessLevel,
                   .internal)

    let explicitlyPrivate = code.filter(whereIs: ProtocolDeclSyntax.self).first!
    XCTAssertEqual(makeDeclChain(explicitlyPrivate).maximumAccessLevel,
                   .private)

    let explicitlyOpen = code.filter(whereIs: ClassDeclSyntax.self).first!
    XCTAssertEqual(makeDeclChain(explicitlyOpen).maximumAccessLevel,
                   .open)

    let extensions = code.filter(whereIs: ExtensionDeclSyntax.self)
    XCTAssertEqual(makeDeclChain(extensions[0]).maximumAccessLevel,
                   .open)
    XCTAssertEqual(makeDeclChain(extensions[1]).maximumAccessLevel,
                   .private)
    XCTAssertEqual(makeDeclChain(extensions[2]).maximumAccessLevel,
                   .internal)
    XCTAssertEqual(makeDeclChain(extensions[3]).maximumAccessLevel,
                   .private)
    XCTAssertEqual(makeDeclChain(extensions[4]).maximumAccessLevel,
                   .private)
  }

  func testNestedExtensionTypes() throws {
    let code = try SyntaxParser.parse(source:
      """
      class MyClass: NSObject {}
      extension MyClass { struct Nested {} }
      extension MyClass.Nested { enum NestedDeeper {} }
      """
    )
    let dc = DeclChain(decls: [code])

    XCTIfLet(dc.lookupQualified("MyClass")) { MyClass in
      XCTAssertTrue(MyClass.last is ClassDeclSyntax)

      XCTIfLet(MyClass.lookupQualified("Nested")) { MyClass_Nested in
        XCTAssertTrue(MyClass_Nested.last is StructDeclSyntax)

        XCTIfLet(MyClass_Nested.lookupQualified("NestedDeeper")) { MyClass_Nested_NestedDeeper in
          XCTAssertTrue(MyClass_Nested_NestedDeeper.last is EnumDeclSyntax)
        }
      }
    }
  }

  func testIfConfig() throws {
    let code = try SyntaxParser.parse(source:
      """
      #if _runtime(objc)
      class MyClass: NSObject {}
      extension MyClass { struct Nested {} }
      extension MyClass.Nested { enum NestedDeeper {} }
      #endif
      """
    )
    let dc = DeclChain(decls: [code])

    XCTIfLet(dc.lookupQualified("MyClass")) { MyClass in
      XCTAssertTrue(MyClass.last is ClassDeclSyntax)

      XCTIfLet(MyClass.lookupQualified("Nested")) { MyClass_Nested in
        XCTAssertTrue(MyClass_Nested.last is StructDeclSyntax)

        XCTIfLet(MyClass_Nested.lookupQualified("NestedDeeper")) { MyClass_Nested_NestedDeeper in
          XCTAssertTrue(MyClass_Nested_NestedDeeper.last is EnumDeclSyntax)
        }
      }
    }
  }

  func testIsInitialized() throws {
    let code = try SyntaxParser.parse(source:
      """
      var a: Int
      var b = 1
      var c: Int?
      """
    )
    let vars = code.filter(whereIs: VariableDeclSyntax.self)

    XCTAssertFalse(vars[0].boundProperties[0].hasInitializer)
    XCTAssertTrue(vars[1].boundProperties[0].hasInitializer)
    XCTAssertFalse(vars[2].boundProperties[0].hasInitializer)
  }
}

func XCTIfLet<T>(
  _ value: T?,
  _ message: String = "Value should not be nil",
  file: StaticString = #file,
  line: UInt = #line,
  then body: (T) throws -> Void
) rethrows {
  if let value = value {
    try body(value)
  }
  else {
    XCTFail(message, file: file, line: line)
  }
}
