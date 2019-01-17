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
/// This file implements limited name lookup support for swift-evolve.
///
// -----------------------------------------------------------------------------

import SwiftSyntax

protocol MayContainSiblingNominalDecls: Syntax {
  /// Implements name lookup within the parent context against this node. That
  /// is, if `self` or some child of `self` should be returned from
  /// `lookupDirect(_:)` on its `DeclContext`, returns that node.
  func lookupFlat(_ name: String) -> Decl?
}

protocol MayContainChildNominalDecls: Syntax {
  /// Looks for a child of `self` with the name `name`.
  func lookupDirect(_ name: String) -> Decl?
}

extension DeclContext {
  /// Looks up `name` in the last declaration in `declarationChain`.
  func lookupQualified(_ name: String) -> DeclContext? {
    guard let decl = last as? MayContainChildNominalDecls else {
      return nil
    }
    return decl.lookupDirect(name).map(self.appending(_:))
  }

  func lookupQualified(_ identifier: TokenSyntax) -> DeclContext? {
    return lookupQualified(identifier.text)
  }

  func lookupQualified(_ typeSyntax: TypeSyntax) -> DeclContext? {
    switch typeSyntax {
    case let ts as SimpleTypeIdentifierSyntax:
      return lookupQualified(ts.name)

    case let ts as MemberTypeIdentifierSyntax:
      return lookupQualified(ts.baseType)?.lookupQualified(ts.name)

    default:
      return nil
    }
  }

  /// Looks up `name` in the declarations in `declarationChain`, from last to
  /// first.
  func lookupUnqualified(_ name: String) -> DeclContext? {
    guard !isEmpty else { return nil }
    return lookupQualified(name) ?? removingLast().lookupUnqualified(name)
  }

  func lookupUnqualified(_ identifier: TokenSyntax) -> DeclContext? {
    return lookupUnqualified(identifier.text)
  }

  func lookupUnqualified(_ typeSyntax: TypeSyntax) -> DeclContext? {
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

// MARK: Decl sibling lookup
//
// This is basically how various declarations match themselves against a name.

// The basic implementation for most decls:
extension Decl /*: MayContainSiblingNominalDecls */ {
  func lookupFlat(_ name: String) -> Decl? {
    if name == self.name { return self }
    return nil
  }
}

// Types with multiple names per declaration:
extension VariableDeclSyntax {
  func lookupFlat(_ name: String) -> Decl? {
    // If any name in this decl matches, return this decl.
    return boundProperties.contains { $0.name.text == name } ? self : nil
  }
}

extension EnumCaseDeclSyntax {
  func lookupFlat(_ name: String) -> Decl? {
    // If any name in this decl matches, return this decl.
    return elements.contains { $0.name == name } ? self : nil
  }
}

extension Decl where Self: DeclWithParameters, Self: MayContainSiblingNominalDecls {
  func lookupFlat(_ name: String) -> Decl? {
    // Can be named by either base name or compound name.
    return baseName == name || self.name == name ? self : nil
  }
}

// Types without utterable names:
extension SourceFileSyntax {
  func lookupFlat(_ name: String) -> Decl? {
    // These are impossible to name, even though they are Decl enough to need
    // to be in a DeclContext.
    return nil
  }
}

extension ExtensionDeclSyntax {
  func lookupFlat(_ name: String) -> Decl? {
    // The names of extensions cannot be directly uttered; their children get
    // merged into the types they extend.
    return nil
  }
}

// Syntax nodes which can contain declarations belonging to the parent type:
extension MemberDeclListSyntax: MayContainSiblingNominalDecls {
  func lookupFlat(_ name: String) -> Decl? {
    for item in self {
      if let decl = (item.decl as? MayContainSiblingNominalDecls)?.lookupFlat(name) {
        return decl
      }
    }
    return nil
  }
}

extension IfConfigDeclSyntax: MayContainSiblingNominalDecls {
  func lookupFlat(_ name: String) -> Decl? {
    // HACK: As a simplifying assumption, we assume that if the same symbol is
    // declared in several different clauses of a #if, the declarations will
    // be similar enough to substitute them. For instance, a type will not be
    // a class in one and a struct in another, or @_fixed_layout in one and
    // resilient in the other.
    //
    // If this assumption proves not to be true in practice, we can represent
    // and handle #if in a more sophisticated way in the future.
    for clause in clauses {
      guard let elements = clause.elements as? MayContainSiblingNominalDecls else {
        log(type: .error, "Not sure how to handle IfConfigDeclSyntax clause elements of type \(type(of: clause.elements))")
        continue
      }
      if let decl = elements.lookupFlat(name) {
        return decl
      }
    }
    return nil
  }
}

extension CodeBlockItemListSyntax: MayContainSiblingNominalDecls {
  func lookupFlat(_ name: String) -> Decl? {
    for item in self {
      guard let statement = item.item as? MayContainSiblingNominalDecls else {
        log(type: .error, "Not sure how to handle CodeBlockItemListSyntax clause elements of type \(type(of: item.item))")
        continue
      }
      if let decl = statement.lookupFlat(name) {
        return decl
      }
    }
    return nil
  }
}

extension WithStatementsSyntax where Self: MayContainSiblingNominalDecls {
  func lookupFlat(_ name: String) -> Decl? {
    return statements.lookupFlat(name)
  }
}

extension CodeBlockSyntax: MayContainSiblingNominalDecls {}
extension ClosureExprSyntax: MayContainSiblingNominalDecls {}
extension SourceFileSyntax: MayContainSiblingNominalDecls {}
extension SwitchCaseSyntax: MayContainSiblingNominalDecls {}

// MARK: Decl child lookup

extension DeclWithMembers /*: MayContainChildNominalDecls */ where Self: Decl {
  func lookupDirect(_ name: String) -> Decl? {
    // Check all of this type's member lists to see if any of them have the
    // declaration.
    for memberList in collectMemberLists() {
      if let decl = memberList.lookupFlat(name) {
        return decl
      }
    }

    return nil
  }
}

extension Decl where Self: AbstractFunctionDecl {
  func lookupDirect(_ name: String) -> Decl? {
    return body?.lookupFlat(name)
  }
}

extension VariableDeclSyntax {
  func lookupDirect(_ name: String) -> Decl? {
    return nil
  }
}

extension EnumCaseDeclSyntax {
  func lookupDirect(_ name: String) -> Decl? {
    return nil
  }
}

private func lookupDirectUnimplemented(parent: Decl, name: String) -> Decl? {
  let parentName = DeclContext(at: parent).name
  log(type: .error, #"Not implemented: \#(type(of: parent)).lookupDirect("\#(name)") called on \#(parentName)"#)
  return nil
}

extension TypealiasDeclSyntax: MayContainChildNominalDecls {
  func lookupDirect(_ name: String) -> Decl? {
    return lookupDirectUnimplemented(parent: self, name: name)
  }
}

extension AssociatedtypeDeclSyntax: MayContainChildNominalDecls {
  func lookupDirect(_ name: String) -> Decl? {
    return lookupDirectUnimplemented(parent: self, name: name)
  }
}

extension SubscriptDeclSyntax {
  func lookupDirect(_ name: String) -> Decl? {
    return lookupDirectUnimplemented(parent: self, name: name)
  }
}

extension SourceFileSyntax: MayContainChildNominalDecls {
  func lookupDirect(_ name: String) -> Decl? {
    return statements.lookupFlat(name)
  }
}

// MARK: Extension handling

extension DeclWithMembers {
  /// Returns the base declaration plus all extensions matching the given
  /// Decl.
  ///
  /// - Parameter decl: A type declaration of some kind. This could be
  ///             either the base declaration or an extension of the type.
  /// - Returns: A list of DeclContexts in the same source file as `decl`
  ///            which match that type.
  fileprivate func collectMemberLists() -> [MemberDeclListSyntax] {
    return ExtensionFinder(self).found.compactMap {
      ($0.last as? DeclWithMembers)?.members.members
    }
  }
}

extension DeclContext {
  /// Returns the name of the declaration as if it were defined directly in the
  /// base type, even if it's actually within an extension.
  var extendedTypeName: String {
    return declarationChain.flatMap { decl -> [Substring] in
      if let ext = decl as? ExtensionDeclSyntax {
        return ext.extendedType.typeText.split(separator: ".")
      }
      else {
        return [decl.name[...]]
      }
    }.joined(separator: ".")
  }
}

/// A visitor which finds all the extensions matching the declaration containing
/// a given node.
fileprivate class ExtensionFinder: SyntaxVisitor {
  /// The decl we're looking for extensions on.
  private let lookingFor: DeclContext

  /// The result list. Includes the original decl and all found extensions.
  fileprivate var found: [DeclContext]

  /// The context of the decl we're currently visiting.
  private var current: DeclContext

  fileprivate init(_ node: Syntax) {
    current = DeclContext(declarationChain: [])

    guard let target = DeclContext(at: node).extendedDeclContext else {
      lookingFor = current
      found = []

      super.init()

      return
    }

    lookingFor = target
    found = [target]

    super.init()

    target.rootContext?.last?.walk(self)
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

  override func visit(_: ExtensionDeclSyntax) -> SyntaxVisitorContinueKind {
    // FIXME: Kind of gross, but at least it's not circular.
    if lookingFor.extendedTypeName == current.extendedTypeName {
      found.append(current)
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
