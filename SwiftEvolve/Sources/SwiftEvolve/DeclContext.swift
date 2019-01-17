// SwiftEvolveKit/Decl.swift - Logic for resilient declarations
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// -----------------------------------------------------------------------------
///
/// This file specifies which declarations may be resilient and how to tell if
/// they are resilient.
///
// -----------------------------------------------------------------------------

import SwiftSyntax

struct DeclContext {
  private(set) var name: String

  var declarationChain: [Decl] = [] {
    didSet { name = makeName(from: declarationChain) }
  }

  init(declarationChain: [Decl] = []) {
    self.declarationChain = declarationChain
    name = makeName(from: declarationChain)
  }

  /// - Complexity: O(N)
  init(at node: Syntax) {
    self.init(declarationChain: reconstructDeclarationChain(at: node))
  }

  var rootContext: DeclContext? {
    return declarationChain.first.map { DeclContext(declarationChain: [$0]) }
  }

  /// If `last` is an extension, returns the `Decl` for the extended type;
  /// otherwise returns `last`.
  var extendedDeclContext: DeclContext? {
    switch last! {
    case let ext as ExtensionDeclSyntax:
      if let extendedDC = rootContext?.lookupQualified(ext.extendedType) {
        return extendedDC
      }
      return nil

    default:
      return self
    }
  }

  /// If `last` is an extension, returns the `Decl` for the extended type;
  /// otherwise returns `last`.
  var extendedDecl: Decl? {
    return extendedDeclContext?.last
  }
}

extension DeclContext: CustomStringConvertible {
  var description: String {
    return name
  }

  var isResilient: Bool {
    // Defaults to true because a source file is resilient.
    return last?.isResilient ?? true
  }

  var isStored: Bool {
    return last?.isStored ?? false
  }

  var maximumAccessLevel: AccessLevel {
    guard let ext = last as? ExtensionDeclSyntax else {
      // Well, this is easy!
      return last!.formalAccessLevel
    }

    // Extensions are trickier. We need the least of the extended type and all
    // types involved in generic constraints.

    /// Collects all nominal types explicitly mentioned in a syntax tree,
    /// ignoring sugar. For instance, in `[Foo]` it would only include `Foo`,
    /// not `Array`.
    class NominalTypeSyntaxCollector: SyntaxVisitor {
      var types: [TypeSyntax]
      private lazy var genericFinder = GenericArgumentClauseFinder(collector: self)

      init(types: [TypeSyntax]) {
        self.types = types
      }

      // We want to collect only SimpleTypeIdentifierSyntax and
      // MemberTypeIdentifierSyntax. When we do, we want to collect all nominal
      // types in generic argument clauses but ignore all others, so we ask the
      // genericFinder to walk the children of the node instead of doing it
      // ourselves; it will call us back to walk any generic argument clauses
      // it finds.
      //
      // We should walk into all other syntax, including type sugar, generic
      // type syntax, and structural types, looking for nominal types within
      // them.

      override func visit(_ node: SimpleTypeIdentifierSyntax) -> SyntaxVisitorContinueKind {
        types.append(node)
        node.walk(genericFinder)
        return .skipChildren
      }

      override func visit(_ node: MemberTypeIdentifierSyntax) -> SyntaxVisitorContinueKind {
        types.append(node)
        node.walk(genericFinder)
        return .skipChildren
      }

      private class GenericArgumentClauseFinder: SyntaxVisitor {
        unowned let collector: NominalTypeSyntaxCollector

        init(collector: NominalTypeSyntaxCollector) {
          self.collector = collector
        }

        override func visit(_ node: GenericArgumentClauseSyntax) -> SyntaxVisitorContinueKind {
          node.walk(collector)
          return .skipChildren
        }
      }
    }

    let collector = NominalTypeSyntaxCollector(types: [ ext.extendedType ])
    ext.genericWhereClause?.walk(collector)

    return collector.types.compactMap {
      lookupUnqualified($0)?.maximumAccessLevel
    }.min()
      // If no resolvable types are involved, assume they are imported or
      // otherwise unrestricted.
      ?? .open
  }
}

extension DeclContext {
  func `is`(at node: Syntax) -> Bool {
    return !isEmpty && last! == node
  }

  var last: Decl? {
    return declarationChain.last
  }

  var isEmpty: Bool {
    return declarationChain.isEmpty
  }

  mutating func append(_ node: Decl) {
    declarationChain.append(node)
  }

  func appending(_ node: Decl) -> DeclContext {
    var copy = self
    copy.append(node)
    return copy
  }

  mutating func removeLast() {
    declarationChain.removeLast()
  }
  
  func removingLast() -> DeclContext {
    var copy = self
    copy.removeLast()
    return copy
  }
}

enum AccessLevel: String, Codable, Comparable {
  case `private`, `fileprivate`, `internal`, `public`, `open`

  private var rank: Int {
    switch self {
    case .private:
      return 0
    case .fileprivate:
      return 1
    case .internal:
      return 2
    case .public:
      return 3
    case .open:
      return 4
    }
  }

  static func < (lhs: AccessLevel, rhs: AccessLevel) -> Bool {
    return lhs.rank < rhs.rank
  }
}

extension DeclModifierSyntax {
  var accessLevel: AccessLevel? {
    return AccessLevel(rawValue: self.name.text)
  }
}

protocol Decl: DeclSyntax, MayContainSiblingNominalDecls {
  var name: String { get }

  var isResilient: Bool { get }
  var isStored: Bool { get }

  var modifiers: ModifierListSyntax? { get }
}

extension Decl {
  var isResilient: Bool { return true }
  var isStored: Bool { return false }

  var formalAccessLevel: AccessLevel {
    return modifiers?.lazy.compactMap { $0.accessLevel }.first ?? .internal
  }
}

extension SourceFileSyntax: Decl {
  var name: String { return "(file)" }

  var modifiers: ModifierListSyntax? { return nil }
}

extension ClassDeclSyntax: Decl {
  var name: String {
    return identifier.text
  }

  var isResilient: Bool {
    return !attributes.contains(named: "_fixed_layout")
  }
}

extension StructDeclSyntax: Decl {
  var name: String {
    return identifier.text
  }

  var isResilient: Bool {
    return !attributes.contains(named: "_fixed_layout")
  }
}

extension EnumDeclSyntax: Decl {
  var name: String {
    return identifier.text
  }

  var isResilient: Bool {
    return !attributes.contains(named: "_frozen")
  }
}

extension ProtocolDeclSyntax: Decl {
  var name: String {
    return identifier.text
  }
}

extension ExtensionDeclSyntax: Decl {
  var name: String {
    return "(extension \(extendedType.typeText))"
  }
}

extension TypealiasDeclSyntax: Decl {
  var name: String {
    return identifier.text
  }
}

extension AssociatedtypeDeclSyntax: Decl {
  var name: String {
    return identifier.text
  }
}

extension FunctionDeclSyntax: Decl {}

extension InitializerDeclSyntax: Decl {}

extension SubscriptDeclSyntax: Decl {}

extension PatternSyntax {
  var boundIdentifiers: [(name: TokenSyntax, type: TypeSyntax?)] {
    switch self {
    case let self as IdentifierPatternSyntax:
      return [(self.identifier, nil)]

    case let self as AsTypePatternSyntax:
      let subnames = self.pattern.boundIdentifiers
      if let tupleType = self.type as? TupleTypeSyntax {
        return zip(subnames.map { $0.name }, tupleType.elements.map { $0.type })
          .map { ($0.0, $0.1) }
      }
      else {
        assert(subnames.count == 1)
        return [ (subnames[0].name, self.type) ]
      }

    case let self as TuplePatternSyntax:
      return self.elements.flatMap { $0.pattern.boundIdentifiers }

    case is WildcardPatternSyntax:
      return []

    default:
      return [(SyntaxFactory.makeUnknown("<unknown>"), nil)]
    }
  }
}

extension PatternBindingSyntax {
  var boundIdentifiers: [(name: TokenSyntax, type: TypeSyntax?)] {
    return pattern.boundIdentifiers.map {
      ($0.name, typeAnnotation?.type ?? $0.type)
    }
  }
}

extension VariableDeclSyntax: Decl {
  struct BoundProperty: CustomStringConvertible, TextOutputStreamable {
    var name: TokenSyntax
    var type: TypeSyntax?
    var isInitialized: Bool
    
    init(boundIdentifier: (name: TokenSyntax, type: TypeSyntax?), isInitialized: Bool) {
      self.name = boundIdentifier.name
      self.type = boundIdentifier.type
      self.isInitialized = isInitialized
    }
    
    func write<Target>(to target: inout Target) where Target: TextOutputStream {
      name.write(to: &target)
      if let type = type {
        ": ".write(to: &target)
        type.write(to: &target)
      }
      if isInitialized {
        " = <value>".write(to: &target)
      }
    }
    
    var description: String {
      var str = ""
      write(to: &str)
      return str
    }
  }
  
  var name: String {
    let list = boundProperties
    if list.count == 1 { return list.first!.name.text }
    let nameList = list.map { $0.name.text }
    return "(\( nameList.joined(separator: ", ") ))"
  }

  var boundProperties: [BoundProperty] {
    return Array(
      bindings.lazy
        .flatMap {
          zip(
            $0.boundIdentifiers,
            repeatElement($0.initializer != nil, count: .max)
          )
        }
        .map { BoundProperty(boundIdentifier: $0.0, isInitialized: $0.1) }
    )
  }

  // FIXME: Is isResilient == true correct?

  var isStored: Bool {
    // FIXME: It's wrong to describe the whole decl as stored or not stored;
    // each individual binding (or, arguably, each individual bound property)
    // is stored or not stored.
    return bindings.allSatisfy { binding in
      switch binding.accessor {
      case is CodeBlockSyntax:
        // There's a computed getter.
        return false

      case let accessorBlock as AccessorBlockSyntax:
        // Check the individual accessors.
        return accessorBlock.accessors.allSatisfy { accessor in
          switch accessor.accessorKind.text {
          case "willSet", "didSet":
            // These accessors are allowed on stored properties.
            return true
          default:
            // All other accessors are assumed to make this computed.
            return false
          }
        }

      default:
        // This binding doesn't include any computed getters.
        return true
      }
    }
  }
}

extension EnumCaseElementSyntax {
  var name: String {
    let params: String
    if let paramList = associatedValue?.parameterList {
      params = paramList.map {
        "\($0.firstName?.text ?? "_"):"
      }.joined()
    }
    else {
      params = ""
    }
    return "\(identifier.text)(\( params ))"
  }
}

extension EnumCaseDeclSyntax: Decl {
  var name: String {
    if elements.count == 1 {
      return elements.first!.name
    }
    else {
      return "(" + elements.map { $0.name }.joined(separator: ", ") + ")"
    }
  }

  var isStored: Bool {
    return true
  }
}

extension IfConfigDeclSyntax {
  var containsStoredMembers: Bool {
    return clauses.contains { clause in
      guard let members = clause.elements as? MemberDeclListSyntax else {
        return false
      }

      return members.contains { memberItem in
        switch memberItem.decl {
        case let nestedIfConfig as IfConfigDeclSyntax:
          return nestedIfConfig.containsStoredMembers

        case let member as Decl:
          return member.isStored

        default:
          return false
        }
      }
    }
  }
}

// MARK: - Helpers

extension Optional where Wrapped == AttributeListSyntax {
  func contains(named name: String) -> Bool {
    return self?.contains { $0.attributeName.text == name } ?? false
  }
}

extension Optional where Wrapped == ModifierListSyntax {
  func contains(named name: String) -> Bool {
    return self?.contains { $0.name.text == name } ?? false
  }
}

private func makeName(from declarationChain: [Decl]) -> String {
  return declarationChain.map { $0.name }.joined(separator: ".")
}

func reconstructDeclarationChain(at node: Syntax) -> [Decl] {
  return sequence(first: node) { $0.parent }.compactMap { $0 as? Decl }.reversed()
}
