// SwiftEvolveKit/DeclChain.swift - DeclChain type
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
/// This file implements DeclChain, a type which stores and analyzes the
/// ancestry of a Decl.
///
// -----------------------------------------------------------------------------

import SwiftSyntax

struct DeclChain {
  private(set) var descriptiveName: String

  var decls: [Decl] = [] {
    didSet { descriptiveName = makeName(from: decls) }
  }

  init(decls: [Decl] = []) {
    self.decls = decls
    descriptiveName = makeName(from: decls)
  }

  /// - Complexity: O(N)
  init(at node: Syntax) {
    self.init(decls: reconstructDeclarationChain(at: node))
  }

  var fileChain: DeclChain? {
    return decls.first.map { DeclChain(decls: [$0]) }
  }

  /// If `last` is an extension, returns the `Decl` for the extended type;
  /// otherwise returns `last`.
  var extendedDeclChain: DeclChain? {
    switch last! {
    case let ext as ExtensionDeclSyntax:
      if let extendedDC = fileChain?.lookupQualified(ext.extendedType) {
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
    return extendedDeclChain?.last
  }

  /// Returns the name of the declaration as if it were defined directly in the
  /// base type, even if it's actually within an extension.
  var extendedTypeDescriptiveName: String {
    return decls.map { decl -> String in
      if let ext = decl as? ExtensionDeclSyntax {
        return ext.extendedType.typeText
      }
      else {
        return decl.descriptiveName
      }
    }.joined(separator: ".")
  }
}

extension DeclChain: CustomStringConvertible {
  var description: String {
    return descriptiveName
  }
}

extension DeclChain {
  func `is`(at node: Syntax) -> Bool {
    return !isEmpty && last! == node
  }

  var last: Decl? {
    return decls.last
  }

  var isEmpty: Bool {
    return decls.isEmpty
  }

  mutating func append(_ node: Decl) {
    decls.append(node)
  }

  func appending(_ node: Decl) -> DeclChain {
    var copy = self
    copy.append(node)
    return copy
  }

  mutating func removeLast() {
    decls.removeLast()
  }
  
  func removingLast() -> DeclChain {
    var copy = self
    copy.removeLast()
    return copy
  }
}

private func makeName(from decls: [Decl]) -> String {
  return decls.map { $0.descriptiveName }.joined(separator: ".")
}

func reconstructDeclarationChain(at node: Syntax) -> [Decl] {
  return sequence(first: node) { $0.parent }.compactMap { $0 as? Decl }.reversed()
}
