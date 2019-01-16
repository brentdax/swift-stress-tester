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

    XCTAssertThrowsError(
      try InsertComputedUnnamedMemberEvolution(
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

    guard let evo2 = try InsertComputedMemberEvolution(
      for: decl.members.members, in: dc, using: &predictableRNG
    ) else { XCTFail(); return }

    XCTAssertEqual(evo2.index, 1)
    XCTAssertEqual(evo2.name, "__swiftEvolveInserted12391666931450987487")
    XCTAssertEqual(evo2.labeledParameters, [true, true, false, false])
    XCTAssertEqual(evo2.memberKind, .function)
    XCTAssertEqual(evo2.staticKeyword, .instance)
    XCTAssertEqual(evo2.accessLevel, .private)

    guard let evo3 = try InsertComputedUnnamedMemberEvolution(
      for: decl.members.members, in: dc, using: &predictableRNG
    ) else { XCTFail(); return }
    XCTAssertEqual(evo3.index, 3)
    XCTAssertEqual(evo3.name, "__swiftEvolveInserted5723979029937790549")
    XCTAssertEqual(evo3.labeledParameters, [true])
    XCTAssertEqual(evo3.memberKind, .initializer)
    XCTAssertEqual(evo3.accessLevel, .internal)
    XCTAssertEqual(evo3.isConvenience, false)

    guard let evo4 = try InsertComputedUnnamedMemberEvolution(
      for: decl.members.members, in: dc, using: &predictableRNG
    ) else { XCTFail(); return }
    XCTAssertEqual(evo4.index, 3)
    XCTAssertEqual(evo4.name, "__swiftEvolveInserted8751954781424914954")
    XCTAssertEqual(evo4.labeledParameters, [true, false, false, false])
    XCTAssertEqual(evo4.memberKind, .subscription)
    XCTAssertEqual(evo4.accessLevel, .private)
    XCTAssertEqual(evo4.isConvenience, false)
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
      accessLevel: .fileprivate
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
      labeledParameters: [true, true, false, false],
      memberKind: .function,
      staticKeyword: .instance,
      accessLevel: .private
    )
    let evolved2 = evo2.evolve(decl.members.members)
    XCTAssertEqual(String(describing: evolved2), """

        case a

      // Synthesized by InsertComputedMemberEvolution
      private func __swiftEvolveInserted12391666931450987487(label _: Any, label _: Any, _ _: Any, _ _: Any) -> Any {
        fatalError("Resilience failure: Called a computed member inserted during later evolution!")
      }

        case b
        func x() -> Int { return 0 }
      """)

    let evo3 = InsertComputedUnnamedMemberEvolution(
      index: 3,
      name: "__swiftEvolveInserted5723979029937790549",
      labeledParameters: [true],
      memberKind: .initializer,
      accessLevel: .internal,
      isConvenience: false
    )
    let evolved3 = evo3.evolve(decl.members.members)
    XCTAssertEqual(String(describing: evolved3), """

        case a
        case b
        func x() -> Int { return 0 }

      // Synthesized by InsertComputedUnnamedMemberEvolution
      internal init(__swiftEvolveInserted5723979029937790549 _: Any) {
        fatalError("Resilience failure: Called a computed member inserted during later evolution!")
      }

      """)

    let evo4 = InsertComputedUnnamedMemberEvolution(
      index: 3,
      name: "__swiftEvolveInserted8751954781424914954",
      labeledParameters: [true, false, false, false],
      memberKind: .subscription,
      accessLevel: .private,
      isConvenience: false
    )
    let evolved4 = evo4.evolve(decl.members.members)
    XCTAssertEqual(String(describing: evolved4), """

        case a
        case b
        func x() -> Int { return 0 }

      // Synthesized by InsertComputedUnnamedMemberEvolution
      private subscript(__swiftEvolveInserted8751954781424914954 _: Any, _ _: Any, _ _: Any, _ _: Any) -> Any {
        fatalError("Resilience failure: Called a computed member inserted during later evolution!")
      }

      """)

    let evo5 = InsertComputedUnnamedMemberEvolution(
      index: 0,
      name: "__swiftEvolveInserted5723979029937790549",
      labeledParameters: [true],
      memberKind: .initializer,
      accessLevel: .open,   // These two values are invalid
      isConvenience: true   // in an enum, but who cares?
    )
    let evolved5 = evo5.evolve(decl.members.members)
    XCTAssertEqual(String(describing: evolved5), """


      // Synthesized by InsertComputedUnnamedMemberEvolution
      open convenience init(__swiftEvolveInserted5723979029937790549 _: Any) {
        fatalError("Resilience failure: Called a computed member inserted during later evolution!")
      }

        case a
        case b
        func x() -> Int { return 0 }
      """)

  }

  func testMustBeConvenience() throws {
    let code = try SyntaxParser.parse(source:
      """
      class Bar {}
      enum Foo {
        class Nested {}
      }
      extension Foo {}
      extension Foo.Nested {}

      /// An `NSArray` with Swift-native reference counting and contiguous
      /// storage.
      ///
      /// NOTE: older runtimes called this
      /// _SwiftNativeNSArrayWithContiguousStorage. The two must coexist, so
      /// it was renamed. The old name must not be used in the new runtime.
      @_fixed_layout
      @usableFromInline
      internal class __SwiftNativeNSArrayWithContiguousStorage
        : __SwiftNativeNSArray { // Provides NSArray inheritance and native refcounting

        @inlinable
        @nonobjc internal override init() {}

        @inlinable
        deinit {}

        // Operate on our contiguous storage
        internal func withUnsafeBufferOfObjects<R>(
          _ body: (UnsafeBufferPointer<AnyObject>) throws -> R
        ) rethrows -> R {
          _internalInvariantFailure(
            "Must override withUnsafeBufferOfObjects in derived classes")
        }
      }

      // Implement the APIs required by NSArray
      extension __SwiftNativeNSArrayWithContiguousStorage : _NSArrayCore {
        @objc internal var count: Int {
          return withUnsafeBufferOfObjects { $0.count }
        }

        @objc(objectAtIndex:)
        internal func objectAt(_ index: Int) -> AnyObject {
          return withUnsafeBufferOfObjects {
            objects in
            _precondition(
              _isValidArraySubscript(index, count: objects.count),
              "Array index out of range")
            return objects[index]
          }
        }

        @objc internal func getObjects(
          _ aBuffer: UnsafeMutablePointer<AnyObject>, range: _SwiftNSRange
        ) {
          return withUnsafeBufferOfObjects {
            objects in
            _precondition(
              _isValidArrayIndex(range.location, count: objects.count),
              "Array index out of range")

            _precondition(
              _isValidArrayIndex(
                range.location + range.length, count: objects.count),
              "Array index out of range")

            if objects.isEmpty { return }

            // These objects are "returned" at +0, so treat them as pointer values to
            // avoid retains. Copy bytes via a raw pointer to circumvent reference
            // counting while correctly aliasing with all other pointer types.
            UnsafeMutableRawPointer(aBuffer).copyMemory(
              from: objects.baseAddress! + range.location,
              byteCount: range.length * MemoryLayout<AnyObject>.stride)
          }
        }

        @objc(countByEnumeratingWithState:objects:count:)
        internal func countByEnumerating(
          with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
          objects: UnsafeMutablePointer<AnyObject>?, count: Int
        ) -> Int {
          var enumerationState = state.pointee

          if enumerationState.state != 0 {
            return 0
          }

          return withUnsafeBufferOfObjects {
            objects in
            enumerationState.mutationsPtr = _fastEnumerationStorageMutationsPtr
            enumerationState.itemsPtr =
              AutoreleasingUnsafeMutablePointer(objects.baseAddress)
            enumerationState.state = 1
            state.pointee = enumerationState
            return objects.count
          }
        }

        @objc(copyWithZone:)
        internal func copy(with _: _SwiftNSZone?) -> AnyObject {
          return self
        }
      }
      """
    )
    let dcs = code.filter(whereIs: DeclSyntax.self).map {
      DeclContext(declarationChain: reconstructDeclChain(at: $0))
    }

    // 0 is the SourceFileSyntax.
    XCTAssertFalse(MemberKind.initializer.mustBeConvenience(for: dcs[0]))

    XCTAssertTrue(MemberKind.initializer.mustBeConvenience(for: dcs[1]))
    XCTAssertFalse(MemberKind.initializer.mustBeConvenience(for: dcs[2]))
    XCTAssertTrue(MemberKind.initializer.mustBeConvenience(for: dcs[3]))
    XCTAssertFalse(MemberKind.initializer.mustBeConvenience(for: dcs[4]))
    XCTAssertTrue(MemberKind.initializer.mustBeConvenience(for: dcs[5]))

    XCTAssertTrue(MemberKind.initializer.mustBeConvenience(for: dcs[6]))
    XCTAssertTrue(MemberKind.initializer.mustBeConvenience(for: dcs[10]))
  }

  func testMustBeConvenienceRegression() throws {
    let code = try SyntaxParser.parse(URL(fileURLWithPath: "/Volumes/DocumentsHD/Code/open-swift-alt/swift/stdlib/public/core/DictionaryBridging.swift"))
    XCTAssertNotNil(code.lookupDirect("__CocoaDictionary"))
  }
}

func reconstructDeclChain(at node: Syntax) -> [Decl] {
  return sequence(first: node) { $0.parent }.compactMap { $0 as? Decl }.reversed()
}
