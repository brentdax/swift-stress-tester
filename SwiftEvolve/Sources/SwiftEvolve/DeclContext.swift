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
import Foundation

private func makeName(from declarationChain: [Decl]) -> String {
  return declarationChain.map { $0.name }.joined(separator: ".")
}

struct DeclContext {
  private(set) var name: String

  var declarationChain: [Decl] = [] {
    didSet { name = makeName(from: declarationChain) }
  }

  init(declarationChain: [Decl] = []) {
    self.declarationChain = declarationChain
    name = makeName(from: declarationChain)
  }

  /// Looks up `name` in the last declaration in `declarationChain`.
  ///
  /// - Note: This won't work for let (x, y) declarations; that's fine for our
  ///         use cases.
  func lookupDirect(_ name: String) -> DeclContext? {
    guard let decl = last else {
      return nil
    }
    return decl.lookupDirect(name).map(self.appending(_:))
  }
  
  func lookupDirect(_ identifier: TokenSyntax) -> DeclContext? {
    return lookupDirect(identifier.text)
  }

  /// Looks up `name` in the declarations in `declarationChain`, from last to
  /// first.
  ///
  /// - Note: This won't work for let (x, y) declarations; that's fine for our
  ///         use cases.
  func lookupUnqualified(_ name: String) -> DeclContext? {
    guard !isEmpty else { return nil }
    return lookupDirect(name) ?? removingLast().lookupUnqualified(name)
  }
  
  func lookupUnqualified(_ identifier: TokenSyntax) -> DeclContext? {
    return lookupUnqualified(identifier.text)
  }

  /// If `last` is an extension, returns the `Decl` for the extended type;
  /// otherwise returns `last`.
  var extendedDecl: Decl? {
    // Print statements are here to help diagnose a bug.
    print("Called DeclContext(\(self)).extendedDecl")
    switch last! {
    case let ext as ExtensionDeclSyntax:
      if let extendedDC = ext.extendedType.lookup(in: self) {
        print("  extension of \(extendedDC) (\(type(of: extendedDC.last!)))")
        return extendedDC.last!
      }
      print("  extension of (extended type not found)")
      return nil

    default:
      print("  not extension (\(type(of: last!)))")
      return last!
    }
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
      $0.lookup(in: self)?.maximumAccessLevel
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

protocol Decl: DeclSyntax {
  var name: String { get }
  var lookupName: String? { get }

  var isResilient: Bool { get }
  var isStored: Bool { get }

  var modifiers: ModifierListSyntax? { get }
  
  func lookupDirect(_ name: String) -> Decl?
}

extension Decl {
  var name: String { return lookupName! }
  var isResilient: Bool { return true }
  var isStored: Bool { return false }

  var formalAccessLevel: AccessLevel {
    return modifiers?.lazy.compactMap { $0.accessLevel }.first ?? .internal
  }
}

extension Decl where Self: DeclWithMembers {
  func lookupDirect(_ name: String) -> Decl? {
    for item in members.members {
      switch item.decl {
      case let member as Decl:
        if member.lookupName == name {
          return member
        }

      case let ifConfig as IfConfigDeclSyntax:
        if let decl = ifConfig.lookupDirect(name) {
          return decl
        }

      default:
        continue
      }
    }
    return nil
  }
}

extension IfConfigDeclSyntax {
  func lookupDirect(_ name: String) -> Decl? {
    // HACK: As a simplifying assumption, we assume that if the same symbol is
    // declared in several different clauses of a #if, the declarations will
    // be similar enough to substitute them. For instance, a type will not be
    // a class in one and a struct in another, or @_fixed_layout in one and
    // resilient in the other.
    //
    // If this assumption proves not to be true in practice, we can represent
    // and handle #if in a more sophisticated way in the future.
    for clause in clauses {
      switch clause.elements {
      case let statementHolder as WithStatementsSyntax:
        if let decl = statementHolder.lookupDirect(name) {
          return decl
        }

      case let codeList as CodeBlockItemListSyntax:
        if let decl = codeList.lookupDirect(name) {
          return decl
        }

      default:
        log(type: .debug, "Not sure how to handle IfConfigDeclSyntax clause elements of type \(type(of: clause.elements))")
      }
    }
    return nil
  }
}

extension Decl where Self: AbstractFunctionDecl {
  func lookupDirect(_ name: String) -> Decl? {
    return body?.lookupDirect(name)
  }
}

extension CodeBlockItemListSyntax {
  func lookupDirect(_ name: String) -> Decl? {
    for item in self {
      switch item.item {
      case let decl as Decl:
        if decl.name == name {
          return decl
        }

      case let ifConfig as IfConfigDeclSyntax:
        if let decl = ifConfig.lookupDirect(name) {
          return decl
        }

      default:
        continue
      }
    }
    return nil
  }
}

extension WithStatementsSyntax {
  func lookupDirect(_ name: String) -> Decl? {
    return statements.lookupDirect(name)
  }
}

extension SourceFileSyntax: Decl {
  var name: String { return "(file)" }
  var lookupName: String? { return nil }

  var modifiers: ModifierListSyntax? { return nil }
}

extension ClassDeclSyntax: Decl {
  var lookupName: String? {
    return identifier.text
  }

  var isResilient: Bool {
    return !attributes.contains(named: "_fixed_layout")
  }
}

extension StructDeclSyntax: Decl {
  var lookupName: String? {
    return identifier.text
  }

  var isResilient: Bool {
    return !attributes.contains(named: "_fixed_layout")
  }
}

extension EnumDeclSyntax: Decl {
  var lookupName: String? {
    return identifier.text
  }

  var isResilient: Bool {
    return !attributes.contains(named: "_frozen")
  }
}

extension ProtocolDeclSyntax: Decl {
  var lookupName: String? {
    return identifier.text
  }
}

extension ExtensionDeclSyntax: Decl {
  var name: String {
    return "(extension \(extendedType.typeText))"
  }
  var lookupName: String? {
    return nil
  }
}

extension TypealiasDeclSyntax: Decl {
  var lookupName: String? {
    return identifier.text
  }
  
  func lookupDirect(_ name: String) -> Decl? {
    fatalError("Not implemented: \(type(of: self)).lookupDirect(_:)")
  }
}

extension AssociatedtypeDeclSyntax: Decl {
  var lookupName: String? {
    return identifier.text
  }
  
  func lookupDirect(_ name: String) -> Decl? {
    fatalError("Not implemented: \(type(of: self)).lookupDirect(_:)")
  }
}

extension FunctionDeclSyntax: Decl {}

extension InitializerDeclSyntax: Decl {}

extension SubscriptDeclSyntax: Decl {
  func lookupDirect(_ name: String) -> Decl? {
    fatalError("Not implemented: \(type(of: self)).lookupDirect(_:)")
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
  var lookupName: String? { return nil }

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
  
  func lookupDirect(_ name: String) -> Decl? {
    return nil
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
  var lookupName: String? { return nil }

  var isStored: Bool {
    return true
  }

  func lookupDirect(_ name: String) -> Decl? {
    return nil
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

