import XCTest
import SwiftSyntax
@testable import SwiftEvolve

class DeclContextTests: XCTestCase {
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

    func makeDeclContext(_ decl: Decl) -> DeclContext {
      return DeclContext(declarationChain: [code, decl])
    }

    let explicitlyInternal = code.filter(whereIs: StructDeclSyntax.self).first!
    XCTAssertEqual(makeDeclContext(explicitlyInternal).maximumAccessLevel,
                   .internal)

    let implicitlyInternal = code.filter(whereIs: EnumDeclSyntax.self).first!
    XCTAssertEqual(makeDeclContext(implicitlyInternal).maximumAccessLevel,
                   .internal)

    let explicitlyPrivate = code.filter(whereIs: ProtocolDeclSyntax.self).first!
    XCTAssertEqual(makeDeclContext(explicitlyPrivate).maximumAccessLevel,
                   .private)

    let explicitlyOpen = code.filter(whereIs: ClassDeclSyntax.self).first!
    XCTAssertEqual(makeDeclContext(explicitlyOpen).maximumAccessLevel,
                   .open)

    let extensions = code.filter(whereIs: ExtensionDeclSyntax.self)
    XCTAssertEqual(makeDeclContext(extensions[0]).maximumAccessLevel,
                   .open)
    XCTAssertEqual(makeDeclContext(extensions[1]).maximumAccessLevel,
                   .private)
    XCTAssertEqual(makeDeclContext(extensions[2]).maximumAccessLevel,
                   .internal)
    XCTAssertEqual(makeDeclContext(extensions[3]).maximumAccessLevel,
                   .private)
    XCTAssertEqual(makeDeclContext(extensions[4]).maximumAccessLevel,
                   .private)
  }
}
