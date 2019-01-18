// SwiftEvolveKit/NameLookup.swift - Logic for looking up names
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
/// This file implements limited name lookup support for swift-evolve. It
/// doesn't implement Swift's full semantics, but it's usually capable of
/// matching a type reference to a ValueDecl node.
///
// -----------------------------------------------------------------------------

import SwiftSyntax

// MARK: External API. This is what most code outside this file should use.

extension DeclChain {
  /// Looks up `name` in the last declaration in `decls`.
  ///
  /// - Parameter name: The name of the declaration to look up.
  /// - Returns: The first child declaration of `last` to match `name`, or
  ///            `nil` if none were found.
  func lookupQualified(_ name: String) -> DeclChain? {
    guard let decl = last as? DeclContext else {
      return nil
    }
    // FIXME: This ought to look up `name` in superclasses and protocol
    // extensions if `last` doesn't contain `name`.
    return decl.lookupDirect(name).map(self.appending(_:))
  }

  /// Looks up `identifier` in the last declaration in `decls`.
  ///
  /// - Parameter identifier: A token containing the name of the declaration to
  ///             look up.
  /// - Returns: The first child declaration of `last` to match `identifier`, or
  ///            `nil` if none were found.
  func lookupQualified(_ identifier: TokenSyntax) -> DeclChain? {
    return lookupQualified(identifier.text)
  }

  /// Looks up `typeSyntax` in the last declaration in `decls`.
  ///
  /// - Parameter typeSyntax: A `TypeSyntax` instance identifying a type to look
  ///              up. This may be a `MemberTypeIdentifierSyntax`.
  /// - Returns: The first child declaration of `last` to match `typeSyntax`, or
  ///            `nil` if none were found.
  func lookupQualified(_ typeSyntax: TypeSyntax) -> DeclChain? {
    switch typeSyntax {
    case let ts as SimpleTypeIdentifierSyntax:
      return lookupQualified(ts.name)

    case let ts as MemberTypeIdentifierSyntax:
      return lookupQualified(ts.baseType)?.lookupQualified(ts.name)

    default:
      return nil
    }
  }

  /// Looks up `name` in the declarations in `decls`, from last to
  /// first.
  ///
  /// - Parameter name: The name of the declaration to look up.
  /// - Returns: The first child declaration of `last` to match `typeSyntax`, or
  ///            `nil` if none were found.
  func lookupUnqualified(_ name: String) -> DeclChain? {
    guard !isEmpty else { return nil }
    return lookupQualified(name) ?? removingLast().lookupUnqualified(name)
  }

  /// Looks up `identifier` in the declarations in `decls`, from last
  /// to first.
  ///
  /// - Parameter identifier: A token containing the name of the declaration to
  ///             look up.
  /// - Returns: The first child declaration of `last` to match `identifier`, or
  ///            `nil` if none were found.
  func lookupUnqualified(_ identifier: TokenSyntax) -> DeclChain? {
    return lookupUnqualified(identifier.text)
  }

  /// Looks up `typeSyntax` in the declarations in `decls`, from last
  /// to first.
  ///
  /// - Parameter typeSyntax: A `TypeSyntax` instance identifying a type to look
  ///              up. This may be a `MemberTypeIdentifierSyntax`.
  /// - Returns: The first child declaration of `last` to match `identifier`, or
  ///            `nil` if none were found.
  func lookupUnqualified(_ typeSyntax: TypeSyntax) -> DeclChain? {
    switch typeSyntax {
    case let ts as SimpleTypeIdentifierSyntax:
      return lookupUnqualified(ts.name)

    case let ts as MemberTypeIdentifierSyntax:
      return lookupUnqualified(ts.baseType)?.lookupQualified(ts.name)

    default:
      return nil
    }
  }
}

// MARK: Name matching.

/// A declaration of some sort of named source entity.
protocol ValueDecl: Decl {
  /// Returns true if `self` should be returned for a lookup of `name`.
  func matches(_ name: String) -> Bool
}

extension ValueDecl {
  func matches(_ name: String) -> Bool {
    return self.syntacticNames.contains(name)
  }
}

extension ClassDeclSyntax: ValueDecl {}
extension StructDeclSyntax: ValueDecl {}
extension EnumDeclSyntax: ValueDecl {}
extension ProtocolDeclSyntax: ValueDecl {}
extension TypealiasDeclSyntax: ValueDecl {}
extension AssociatedtypeDeclSyntax: ValueDecl {}
extension ValueDecl where Self: DeclWithParameters {}
extension FunctionDeclSyntax: ValueDecl {}
extension InitializerDeclSyntax: ValueDecl {}
extension SubscriptDeclSyntax: ValueDecl {}
extension VariableDeclSyntax: ValueDecl {}
extension EnumCaseDeclSyntax: ValueDecl {}

// MARK: Defining which decls have child decls, and how to locate them.

/// A source entity which acts as a namespace for declarations. A direct name
/// lookup in one DeclContext will never find a DeclValue in a different
/// DeclContext, except for extensions.
protocol DeclContext {
  /// Looks for a child of `self` with the name `name`.
  func lookupDirect(_ name: String) -> ValueDecl?
}

extension DeclContext where Self: DeclWithMembers {
  func lookupDirect(_ name: String) -> ValueDecl? {
    // Check all of this type's member lists to see if any of them have the
    // declaration.
    for context in ExtensionFinder(of: self).found {
      // Search all nodes under the decl, ignoring nodes under DeclContexts.
      if let decl = ValueDeclFinder(name, in: context).found.first {
        return decl
      }
    }

    return nil
  }
}

// FIXME: Do we really want this?
extension DeclContext where Self: AbstractFunctionDecl {
  func lookupDirect(_ name: String) -> ValueDecl? {
    return ValueDeclFinder(name, in: self).found.first
  }
}

// FIXME: Do we really want this?
extension VariableDeclSyntax: DeclContext {
  func lookupDirect(_ name: String) -> ValueDecl? {
    return nil
  }
}

private func lookupDirectUnimplemented(parent: Decl, name: String) -> ValueDecl? {
  let parentName = DeclChain(at: parent).descriptiveName
  log(type: .error, #"Not implemented: \#(type(of: parent)).lookupDirect("\#(name)") called on \#(parentName)"#)
  return nil
}

extension TypealiasDeclSyntax: DeclContext {
  func lookupDirect(_ name: String) -> ValueDecl? {
    return lookupDirectUnimplemented(parent: self, name: name)
  }
}

extension AssociatedtypeDeclSyntax: DeclContext {
  func lookupDirect(_ name: String) -> ValueDecl? {
    return lookupDirectUnimplemented(parent: self, name: name)
  }
}

extension SubscriptDeclSyntax {
  func lookupDirect(_ name: String) -> ValueDecl? {
    return lookupDirectUnimplemented(parent: self, name: name)
  }
}

extension SourceFileSyntax: DeclContext {
  func lookupDirect(_ name: String) -> ValueDecl? {
    return ValueDeclFinder(name, in: self).found.first
  }
}

// MARK: Extension handling

/// A visitor which finds a ValueDecl child of a DeclContext with a given name.
fileprivate class ValueDeclFinder: SyntaxVisitor {
  private let syntacticName: String
  let parent: DeclContext & Syntax
  var found: [ValueDecl] = []

  init(_ syntacticName: String, in parent: DeclContext & Syntax) {
    self.parent = parent
    self.syntacticName = syntacticName

    super.init()

    parent.walk(self)
  }

  private func process(_ node: Syntax) -> SyntaxVisitorContinueKind {
    if let decl = node as? ValueDecl, decl.matches(syntacticName) {
      found.append(decl)
    }
    if node is DeclContext && node != parent {
      // We shouldn't look in here; it's a different namespace.
      return .skipChildren
    }
    return .visitChildren
  }

  // This list is a superset of the nodes we actually care about today.

  override func visit(_ node: TypealiasDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: AssociatedtypeDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: ClassDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: StructDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: ProtocolDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: ExtensionDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: SourceFileSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: FunctionDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: InitializerDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: DeinitializerDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: SubscriptDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: ImportDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: AccessorDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: VariableDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: EnumCaseDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: EnumDeclSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: GenericParameterSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  override func visit(_ node: FunctionParameterSyntax) -> SyntaxVisitorContinueKind {
    return process(node)
  }

  // In the future, we might also need something for the let variables in
  // if/guard/while/for statements and pattern matches.
}

/// A visitor which finds all the extensions matching the declaration containing
/// a given node.
fileprivate class ExtensionFinder: SyntaxVisitor {
  /// The decl we're looking for extensions on.
  private let lookingFor: String

  /// The result list. Includes the original decl and all found extensions.
  fileprivate var found: [DeclContext & Syntax]

  /// The context of the decl we're currently visiting.
  private var current: DeclChain

  fileprivate init(of node: Syntax) {
    current = DeclChain(decls: [])

    guard let target = DeclChain(at: node).extendedDeclChain else {
      lookingFor = current.extendedTypeDescriptiveName
      found = []

      super.init()

      return
    }

    lookingFor = target.extendedTypeDescriptiveName
    found = [target.last as! DeclContext & Syntax]

    super.init()

    target.fileChain?.last?.walk(self)
  }

  override func visitPre(_ node: Syntax) {
    guard let decl = node as? Decl else {
      return
    }

    current.append(decl)
  }

  override func visitPost(_ node: Syntax) {
    guard let decl = node as? Decl else {
      return
    }
    precondition(current.last == decl)
    current.removeLast()
  }

  override func visit(_ node: ExtensionDeclSyntax) -> SyntaxVisitorContinueKind {
    // FIXME: Kind of gross, but at least it's not circular.
    if current.extendedTypeDescriptiveName == lookingFor {
      found.append(node)
    }

    // Extensions cannot contain extensions, so we don't need to look at
    // the children at this time.
    return .skipChildren
  }

  override func shouldVisit(_ kind: SyntaxKind) -> Bool {
    switch kind {
    // We want to visit extensions, of course.
    case .extensionDecl:
      return true

    // Skip over some declarations which cannot contain extensions. This
    // isn't required for correctness, it's just for speed.
    case .typealiasDecl,
         .associatedtypeDecl,
         .poundErrorDecl,
         .poundWarningDecl,
         .poundSourceLocation,
         .classDecl,
         .structDecl,
         .protocolDecl,
         .functionDecl,
         .initializerDecl,
         .deinitializerDecl,
         .subscriptDecl,
         .importDecl,
         .accessorDecl,
         .variableDecl,
         .enumCaseDecl,
         .enumDecl,
         .operatorDecl,
         .precedenceGroupDecl:
      return false

    // Assume we should visit everything else.
    default:
      return true
    }
  }
}
