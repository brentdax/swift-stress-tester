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
/// This file abstracts over the various kinds of declarations in the syntax
/// tree and adds helpers for analyzing them.
///
// -----------------------------------------------------------------------------

import SwiftSyntax

/// A syntax node which can participate in a DeclChain.
protocol Decl: DeclSyntax {
  /// A name for this declaration that can be used to describe its place in the
  /// declaration context hierarchy.
  var descriptiveName: String { get }

  var modifiers: ModifierListSyntax? { get }
}

/// A declaration of a named source entity.
protocol ValueDecl: Decl {
  /// Names for this declaration that would be used in syntax.
  var syntacticNames: [String] { get }

  var isResilient: Bool { get }

  var isStored: Bool { get }
}

extension Decl {
  var formalAccessLevel: AccessLevel {
    return modifiers?.lazy.compactMap { $0.accessLevel }.first ?? .internal
  }

  var staticKeyword: StaticKeyword {
    return modifiers?.lazy.compactMap { $0.staticKeyword }.first ?? .instance
  }
}

extension ValueDecl {
  var syntacticNames: [String] {
    return [descriptiveName]
  }

  var isResilient: Bool { return true }
  var isStored: Bool { return false }
}

extension ValueDecl where Self: DeclWithParameters {
  var syntacticNames: [String] {
    return [descriptiveName, baseName]
  }
}

enum AccessLevel: String, Codable, Comparable, Hashable {
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

enum StaticKeyword: String, Codable, Hashable {
  case instance, `static`, `class`
}

extension DeclModifierSyntax {
  var accessLevel: AccessLevel? {
    return AccessLevel(rawValue: self.name.text)
  }
  var staticKeyword: StaticKeyword? {
    return StaticKeyword(rawValue: self.name.text)
  }
}

extension SourceFileSyntax: Decl {
  var descriptiveName: String { return "(file)" }

  var modifiers: ModifierListSyntax? { return nil }
}

extension ClassDeclSyntax: ValueDecl {
  var descriptiveName: String {
    return identifier.text
  }

  var isResilient: Bool {
    return !attributes.contains(named: "_fixed_layout")
  }
}

extension StructDeclSyntax: ValueDecl {
  var descriptiveName: String {
    return identifier.text
  }

  var isResilient: Bool {
    return !attributes.contains(named: "_fixed_layout")
  }
}

extension EnumDeclSyntax: ValueDecl {
  var descriptiveName: String {
    return identifier.text
  }

  var isResilient: Bool {
    return !attributes.contains(named: "_frozen")
  }
}

extension ProtocolDeclSyntax: ValueDecl {
  var descriptiveName: String {
    return identifier.text
  }
}

extension ExtensionDeclSyntax: Decl {
  var descriptiveName: String {
    return "(extension \(extendedType.typeText))"
  }
}

extension TypealiasDeclSyntax: ValueDecl {
  var descriptiveName: String {
    return identifier.text
  }
}

extension AssociatedtypeDeclSyntax: ValueDecl {
  var descriptiveName: String {
    return identifier.text
  }
}

extension FunctionDeclSyntax: ValueDecl {}

extension InitializerDeclSyntax: ValueDecl {}

extension SubscriptDeclSyntax: ValueDecl {}

extension VariableDeclSyntax: ValueDecl {
  var descriptiveName: String {
    let list = syntacticNames
    if list.count == 1 {
      return list.first!
    }
    else {
      return "(\( list.joined(separator: ", ") ))"
    }
  }

  var syntacticNames: [String] {
    return boundProperties.map { $0.name.text }
  }

  var isStored: Bool {
    // FIXME: It's wrong to describe the whole decl as stored or not stored;
    // each individual binding (or, arguably, each individual bound property)
    // is stored or not stored.
    return bindings.contains { $0.isStored }
  }
}

extension EnumCaseDeclSyntax: ValueDecl {
  var descriptiveName: String {
    let list = syntacticNames
    if list.count == 1 {
      return list.first!
    }
    else {
      return "(" + list.joined(separator: ", ") + ")"
    }
  }

  var syntacticNames: [String] {
    return elements.map { $0.name }
  }

  var isStored: Bool {
    return true
  }
}

// MARK: Infrastructure for analyzing VariableDeclSyntax

extension VariableDeclSyntax {
  struct BoundProperty: CustomStringConvertible, TextOutputStreamable {
    var name: TokenSyntax
    var type: TypeSyntax?
    var isInitialized: Bool

    init(boundIdentifier: (name: TokenSyntax, type: TypeSyntax?), hasInitializer: Bool) {
      self.name = boundIdentifier.name
      self.type = boundIdentifier.type
      self.isInitialized = hasInitializer
                        || boundIdentifier.type is OptionalTypeSyntax
                        || boundIdentifier.type is
                                ImplicitlyUnwrappedOptionalTypeSyntax
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

  var boundProperties: [BoundProperty] {
    return Array(
      bindings.lazy
        .flatMap {
          zip(
            $0.boundIdentifiers,
            repeatElement($0.initializer != nil, count: .max)
          )
        }
        .map { BoundProperty(boundIdentifier: $0.0, hasInitializer: $0.1) }
    )
  }
}

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

  var isStored: Bool {
    switch accessor {
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

// MARK: Infrastructure for analyzing enum cases

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

        case let member as ValueDecl:
          return member.isStored

        default:
          return false
        }
      }
    }
  }
}

extension DeclChain {
  var isResilient: Bool {
    // Defaults to true because a source file is resilient.
    return (last as? ValueDecl)?.isResilient ?? true
  }

  var isStored: Bool {
    return (last as? ValueDecl)?.isStored ?? false
  }

  var maximumAccessLevel: AccessLevel {
    guard let ext = last as? ExtensionDeclSyntax else {
      // Well, this is easy!
      return last!.formalAccessLevel
    }

    // Extensions are trickier. We need the least of the extended type and all
    // types involved in generic constraints.
    var types = [ ext.extendedType ]
    if let whereClause = ext.genericWhereClause {
      types += NominalTypeReferenceFinder(in: whereClause).found
    }

    return types.compactMap { lookupUnqualified($0)?.maximumAccessLevel }.min()
      // If no resolvable types are involved, we can assume that they at least
      // come from outside the file, so they are internal or greater.
      ?? .internal
  }
}

// MARK: Syntax tree analysis

/// Collects all nominal types explicitly mentioned in a syntax tree,
/// ignoring sugar. For instance, in `[Foo]` it would only include `Foo`,
/// not `Array`.
fileprivate struct NominalTypeReferenceFinder: SyntaxVisitor {
  var found: [TypeSyntax] = []

  init(in node: Syntax) {
    node.walk(&self)
  }
  
  mutating func appendAndFindInGenericArguments(_ node: TypeSyntax) {
    found.append(node)
    
    var genericFinder = GenericArgumentClauseFinder()
    node.walk(&genericFinder)
    found += genericFinder.found
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

  mutating func visit(_ node: SimpleTypeIdentifierSyntax) -> SyntaxVisitorContinueKind {
    appendAndFindInGenericArguments(node)
    return .skipChildren
  }

  mutating func visit(_ node: MemberTypeIdentifierSyntax) -> SyntaxVisitorContinueKind {
    appendAndFindInGenericArguments(node)
    return .skipChildren
  }

  private struct GenericArgumentClauseFinder: SyntaxVisitor {
    var found: [TypeSyntax] = []

    init() {}

    mutating func visit(_ node: GenericArgumentClauseSyntax) -> SyntaxVisitorContinueKind {
      found += NominalTypeReferenceFinder(in: node).found
      return .skipChildren
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
