// SwiftEvolveKit/SyntaxExtensions.swift - Miscellaneous SwiftSyntax extensions
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
/// This file includes various convenience methods and abstractions used
/// in other files to work with the SwiftSyntax tree.
///
// -----------------------------------------------------------------------------

import SwiftSyntax
import Foundation

protocol DeclWithMembers: DeclSyntax, DeclContext {
  var members: MemberDeclBlockSyntax { get }
  func withMembers(_ newChild: MemberDeclBlockSyntax?) -> Self
}

extension ClassDeclSyntax: DeclWithMembers {}
extension StructDeclSyntax: DeclWithMembers {}
extension EnumDeclSyntax: DeclWithMembers {}
extension ProtocolDeclSyntax: DeclWithMembers {}
extension ExtensionDeclSyntax: DeclWithMembers {}

protocol DeclWithParameters: DeclSyntax {
  var baseName: String { get }
  
  var parameters: ParameterClauseSyntax { get }
  func withParameters(_ parameters: ParameterClauseSyntax?) -> Self
}

extension DeclWithParameters {
  var descriptiveName: String {
    let parameterNames = parameters.parameterList.map { param in
      "\(param.firstName?.text ?? "_"):"
    }
    return "\( baseName )(\( parameterNames.joined() ))"
  }
}

protocol AbstractFunctionDecl: DeclWithParameters {
  var body: CodeBlockSyntax? { get }
  func withBody(_ body: CodeBlockSyntax?) -> Self
}

extension InitializerDeclSyntax: AbstractFunctionDecl {
  var baseName: String { return "init" }
}

extension FunctionDeclSyntax: AbstractFunctionDecl {
  var baseName: String {
    return identifier.text
  }

  var parameters: ParameterClauseSyntax {
    return signature.input
  }

  func withParameters(_ parameters: ParameterClauseSyntax?) -> FunctionDeclSyntax {
    return withSignature(signature.withInput(parameters))
  }
}

extension SubscriptDeclSyntax: DeclWithParameters {
  var baseName: String { return "subscript" }

  var parameters: ParameterClauseSyntax {
    return indices
  }

  func withParameters(_ parameters: ParameterClauseSyntax?) -> SubscriptDeclSyntax {
    return withIndices(parameters)
  }
}

extension SourceLocation: CustomStringConvertible {
  public var description: String {
    return "\(file):\(line):\(column)"
  }
}

func == (lhs: Syntax?, rhs: Syntax?) -> Bool {
  switch (lhs, rhs) {
  case (nil, nil):
    return true
  case (nil, _?), (_?, nil):
    return false
  case (let lhs?, let rhs?):
    return lhs == rhs
  }
}

func != (lhs: Syntax?, rhs: Syntax?) -> Bool {
  return !(lhs == rhs)
}

extension DeclChain {
  var typeSyntax: TypeSyntax {
    let name = SyntaxFactory.makeIdentifier(last!.syntacticNames.first!)
    let parent = removingLast()
    
    if parent.decls.allSatisfy({ $0 is SourceFileSyntax }) {
      // Base case
      return SyntaxFactory.makeSimpleTypeIdentifier(
        name: name,
        genericArgumentClause: nil
      )
    }
    
    return SyntaxFactory.makeMemberTypeIdentifier(
      baseType: parent.typeSyntax,
      period: SyntaxFactory.makePeriodToken(),
      name: name,
      genericArgumentClause: nil
    )
  }
}

extension TypeSyntax {
  func absolute(in dc: DeclChain) -> TypeSyntax {
    guard let resolved = dc.lookupUnqualified(self) else {
      return self
    }
    if let typealiasDecl = resolved.last as? TypealiasDeclSyntax {
      return typealiasDecl.initializer!.value
        .absolute(in: resolved.removingLast())
    }
    return resolved.typeSyntax
  }

  func isFunctionType(in dc: DeclChain) -> Bool {
    let abs = absolute(in: dc)
    
    switch abs {
    case is FunctionTypeSyntax:
      return true

    case let abs as AttributedTypeSyntax:
      return abs.baseType.isFunctionType(in: dc)

    default:
      return false
    }
  }
}

extension TypeSyntax {
  var typeText: String {
    var formatter = TokenTextFormatter()
    walk(&formatter)
    return formatter.text
  }
}

extension TokenKind {
  var needsSpace: Bool {
    if isKeyword { return true }
    switch self {
    case .identifier, .dollarIdentifier, .integerLiteral, .floatingLiteral, .yield:
      return true
    default:
      return false
    }
  }
}

fileprivate struct TokenTextFormatter: SyntaxVisitor {
  var previous: TokenKind?
  var text: String = ""

  mutating func visit(_ token: TokenSyntax) -> SyntaxVisitorContinueKind {
    switch token.tokenKind {
    case .comma:
      text += ", "
    case .colon:
      text += ": "
    case .arrow:
      text += " -> "
    case _ where token.tokenKind.needsSpace && (previous?.needsSpace ?? false):
      text += " " + token.text
    case _:
      text += token.text
    }
    previous = token.tokenKind
    return .skipChildren
  }
}

extension SyntaxCollection {
  var first: Element? {
    return self.first(where:{_ in true})
  }
}
