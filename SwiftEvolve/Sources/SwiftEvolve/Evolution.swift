// SwiftEvolveKit/Evolution.swift - Rules for mechanically evolving declarations
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
/// This file specifies and implements many ABI-compatible mechanical
/// transformations we can perform on various resilient declarations.
///
// -----------------------------------------------------------------------------

import SwiftSyntax

/// Errors with recognized meaning for evolution initialization.
enum EvolutionError: Error {
  /// The evolution does not know how to handle this node. If this is a
  /// prerequisite evolution, the evolution following it cannot be performed.
  case unsupported
}

/// An Evolution is a mechanically-implementable transformation of code. Each
/// evolution knows which declarations it can be applied to; to see if it can
/// be applied to a given declaration, try to create an instance with
/// `init(for:in:using:)`.
protocol Evolution: Codable {
  /// Attempts to create a random instance of the given evolution on `node`.
  ///
  /// - Parameter node: The syntax node we're looking to evolve.
  /// - Parameter decl: The declaration the syntax node is in.
  /// - Parameter rng: A random number generator the evolution can use to
  ///             make decisions pseudo-randomly.
  /// - Throws: Usually an `EvolutionError.unsupported` if the evolution
  ///           cannot be safely applied to `node`. Other errors can also be
  ///           thrown and will pass through all the initialization machinery.
  /// - Returns: The evolution instance, or `nil` if applying the evolution
  ///            would be a no-op.
  init?<G>(for node: Syntax, in decl: DeclChain, using rng: inout G) throws
    where G: RandomNumberGenerator

  /// Creates instances of any evolutions that need to be applied to `node`
  /// before `self` is applied.
  ///
  /// - Parameter node: The syntax node we're looking to evolve.
  /// - Parameter decl: The declaration the syntax node is in.
  /// - Parameter rng: A random number generator the evolution can use to
  ///             make decisions pseudo-randomly.
  /// - Throws: Usually an `EvolutionError.unsupported` if the evolution
  ///           cannot be safely applied to `node`.
  /// - Returns: An array of evolutions which should be applied before `self`.
  ///
  /// - Note: Implementations should usually make the prerequisite evolutions
  ///         by calling `makeWithPrerequisites(for:in:using:)`.
  func makePrerequisites<G>(
    for node: Syntax, in decl: DeclChain, using rng: inout G
  ) throws -> [Evolution] where G: RandomNumberGenerator

  /// Applies the evolution to `node`.
  ///
  /// - Parameter node: The node to evolve.
  /// - Returns: An evolved version of `decl`.
  /// - Precondition: `decl` represents the same code passed to
  ///                 `init(decl:using:)`.
  func evolve(_ node: Syntax) -> Syntax

  var kind: AnyEvolution.Kind { get }
}

extension Evolution {
  /// Creates an array containing an instance of the evolution and all
  /// evolutions that need to be applied along with it.
  ///
  /// - Parameter node: The syntax node we're looking to evolve.
  /// - Parameter decl: The declaration the syntax node is in.
  /// - Parameter rng: A random number generator the evolution can use to
  ///             make decisions pseudo-randomly.
  /// - Throws: Usually an `EvolutionError.unsupported` if the evolution
  ///           cannot be safely applied to `node`.
  /// - Returns: An array of evolutions which all need to be applied together,
  ///            or `nil` if the evolution would be a no-op.
  static func makeWithPrerequisites<G>(
    for node: Syntax, in decl: DeclChain, using rng: inout G
  ) throws -> [Evolution]? where G: RandomNumberGenerator {
    guard let evo = try self.init(for: node, in: decl, using: &rng) else {
      return nil
    }
    let prereqs = try evo.makePrerequisites(for: node, in: decl, using: &rng)
    return prereqs + [evo]
  }

  func makePrerequisites<G>(
    for node: Syntax, in decl: DeclChain, using rng: inout G
  ) throws -> [Evolution] where G: RandomNumberGenerator {
    return []
  }
}

extension AnyEvolution {
  enum Kind: String, Codable, CaseIterable {
    case shuffleMembers
    case synthesizeMemberwiseInitializer
    case shuffleGenericRequirements
    case insertComputedMember
    case insertComputedUnnamedMember

    var type: Evolution.Type {
      switch self {
      case .shuffleMembers:
        return ShuffleMembersEvolution.self
      case .synthesizeMemberwiseInitializer:
        return SynthesizeMemberwiseInitializerEvolution.self
      case .shuffleGenericRequirements:
        return ShuffleGenericRequirementsEvolution.self
      case .insertComputedMember:
        return InsertComputedMemberEvolution.self
      case .insertComputedUnnamedMember:
        return InsertComputedUnnamedMemberEvolution.self
      }
    }
  }
}

/// An evolution which rearranges the members of a type.
struct ShuffleMembersEvolution: Evolution {
  /// The members to be shuffled. Any indices not in this list should be moved
  /// to the end and kept in the same order.
  var mapping: [Int]
  var kind: AnyEvolution.Kind { return .shuffleMembers }
}

/// An evolution which makes an implicit struct initializer explicit.
struct SynthesizeMemberwiseInitializerEvolution: Evolution {
  struct StoredProperty: Codable, CustomStringConvertible {
    var name: String
    var type: String
    
    var description: String {
      return "\(name): \(type)"
    }
  }
  
  var inits: [[StoredProperty]]
  
  var kind: AnyEvolution.Kind { return .synthesizeMemberwiseInitializer }
}

/// An evolution which shuffles the constraints in a generic where clause.
struct ShuffleGenericRequirementsEvolution: Evolution {
  var mapping: [Int]
  var kind: AnyEvolution.Kind { return .shuffleGenericRequirements }
}

// An evolution which adds a computed func or var to a concrete type.
struct InsertComputedMemberEvolution: Evolution {
  var index: Int
  var name: String
  var labeledParameters: [Bool]
  var memberKind: MemberKind
  var staticKeyword: StaticKeyword
  var accessLevel: AccessLevel

  var kind: AnyEvolution.Kind { return .insertComputedMember }
}

// An evolution which adds a computed init or  to a concrete type.
struct InsertComputedUnnamedMemberEvolution: Evolution {
  var index: Int
  var name: String
  var labeledParameters: [Bool]
  var memberKind: MemberKind
  var accessLevel: AccessLevel
  var isConvenience: Bool

  var kind: AnyEvolution.Kind { return .insertComputedUnnamedMember }
}

// MARK: Implementations

extension ShuffleMembersEvolution {
  init?<G>(for node: Syntax, in decl: DeclChain, using rng: inout G) throws
    where G: RandomNumberGenerator
  {
    guard
      let membersList = node as? MemberDeclListSyntax
    else { throw EvolutionError.unsupported }
    let members = Array(membersList)

    func shouldShuffleMember(at i: Int) -> Bool {
      guard let memberDecl = members[i].decl as? Decl else {
        // Don't know what this is, so conservatively leave it alone.
        return false
      }
      return decl.isResilient || !decl.appending(memberDecl).isStored
    }
    let indicesByShuffling: [Bool: [Int]] =
      Dictionary(grouping: members.indices, by: shouldShuffleMember(at:))

    let mapping = indicesByShuffling[true, default: []].shuffled(using: &rng)

    if mapping.count <= 1 { return nil }

    self.init(mapping: mapping)
  }

  func makePrerequisites<G>(
    for node: Syntax, in decl: DeclChain, using rng: inout G
  ) throws -> [Evolution] where G : RandomNumberGenerator {
    return [
      try SynthesizeMemberwiseInitializerEvolution
        .makeWithPrerequisites(for: node, in: decl, using: &rng)
    ].compactMap { $0 }.flatMap { $0 }
  }

  func evolve(_ node: Syntax) -> Syntax {
    let members = Array(node as! MemberDeclListSyntax)

    let inMapping = Set(mapping)
    let missing = members.indices.filter { !inMapping.contains($0) }
    let fullMapping = mapping + missing

    return SyntaxFactory.makeMemberDeclList(fullMapping.map { members[$0] })
  }
}

extension SynthesizeMemberwiseInitializerEvolution {
  init?<G>(for node: Syntax, in decl: DeclChain, using rng: inout G) throws
    where G : RandomNumberGenerator
  {
    guard let members = node as? MemberDeclListSyntax else {
      throw EvolutionError.unsupported
    }
    guard
      decl.last is StructDeclSyntax,
      members.parent is MemberDeclBlockSyntax
    else {
      return nil
    }

    var hasDefault = true
    var hasMemberwise = true
    var hasConditionalStoredProperties = false
    var properties: [StoredProperty] = []

    for membersItem in members {
      switch membersItem.decl {
      case let ifConfig as IfConfigDeclSyntax:
        if ifConfig.containsStoredMembers {
          // We would need to generate separate inits for each version. Maybe
          // someday, but not today.
          hasConditionalStoredProperties = true
        }

      case is InitializerDeclSyntax:
        // If we declare an explicit init, we don't have implicit ones
        // FIXME: Do we need to look into IfConfigDecls for these, too? That
        // would be ludicrous.
        return nil

      case let member as VariableDeclSyntax where member.isStored:
        // We definitely care about stored properties.
        for prop in member.boundProperties {
          if let type = prop.type {
            var typeName = type.typeText
            if type.isFunctionType(in: decl) {
              typeName = "@escaping \(typeName)"
            }

            properties.append(StoredProperty(
              name: prop.name.text,
              type: typeName
            ))
          } else {
            hasMemberwise = false
          }

          if !prop.isInitialized {
            hasDefault = false
          }
        }

      default:
        // Consistency check: This isn't somehow stored, is it?
        if let member = membersItem.decl as? ValueDecl {
          assert(!member.isStored, "\(member.descriptiveName) is a stored non-property???")
        }

        // If not, then we don't care.
        continue
      }
    }

    if hasConditionalStoredProperties {
      throw EvolutionError.unsupported
    }
    
    var inits: [[StoredProperty]] = []
    if hasDefault {
      inits.append([])
    }
    if hasMemberwise && !properties.isEmpty {
      inits.append(properties)
    }
    
    if inits.isEmpty {
      return nil
    }

    self.init(inits: inits)
  }

  func evolve(_ node: Syntax) -> Syntax {
    let members = node as! MemberDeclListSyntax
    
    return inits.reduce(members) { members, properties in
      let parameters = properties.mapToFunctionParameterClause {
        SyntaxFactory.makeFunctionParameter(
          attributes: nil,
          firstName: SyntaxFactory.makeIdentifier($0.name),
          secondName: nil,
          colon: SyntaxFactory.makeColonToken(trailingTrivia: [.spaces(1)]),
          type: SyntaxFactory.makeTypeIdentifier($0.type),
          ellipsis: nil,
          defaultArgument: nil,
          trailingComma: nil
        )
      }
      
      let body = properties.mapToCodeBlock { prop in
        ExprSyntaxTemplate.makeExpr(withVars: "self", prop.name) {
          _self, arg in _self[dot: prop.name] ^= arg
        }
      }
      
      let newInitializer = SyntaxFactory.makeInitializerDecl(
        attributes: nil,
        modifiers: nil,
        initKeyword: SyntaxFactory.makeInitKeyword(
          leadingTrivia: [
            .newlines(2),
            .lineComment("// Synthesized by SynthesizeMemberwiseInitializerEvolution"),
            .newlines(1)
          ],
          trailingTrivia: []
        ),
        optionalMark: nil,
        genericParameterClause: nil,
        parameters: parameters,
        throwsOrRethrowsKeyword: nil,
        genericWhereClause: nil,
        body: body
      )
      
      return members.appending(MemberDeclListItemSyntax {
        $0.useDecl(newInitializer)
      })
    }
  }
}

extension ShuffleGenericRequirementsEvolution {
  init?<G>(for node: Syntax, in decl: DeclChain, using rng: inout G) throws
    where G: RandomNumberGenerator
  {
    guard
      let requirementsList = node as? GenericRequirementListSyntax
    else { throw EvolutionError.unsupported }
    let requirements = Array(requirementsList)

    let indices = requirements.indices

    let mapping = indices.shuffled(using: &rng)

    if mapping.count <= 1 { return nil }

    self.init(mapping: mapping)
  }

  func evolve(_ node: Syntax) -> Syntax {
    let requirements = Array(node as! GenericRequirementListSyntax)

    precondition(requirements.count == mapping.count,
                 "ShuffleGenericRequirementsEvolution mapping does not match node it's being applied to")

    return SyntaxFactory.makeGenericRequirementList(
      mapping.map { requirements[$0] }.withCorrectTrailingCommas()
    )
  }
}

enum MemberKind: String, CaseIterable, Codable {
  case function, variable, subscription, initializer

  mutating func mustBeConvenience(for dc: DeclChain) -> Bool {
    guard self == .initializer else {
      return false
    }
    guard let base = dc.extendedDecl else {
      // HACK: If we can't find the decl, we don't know whether we need the
      // keyword or not. Change it to a different decl.
      log(type: .error, "can't find base decl for \(dc); '''fixing''' the member we might generate")
      self = .subscription
      return false
    }
    return base is ClassDeclSyntax
  }

  func makeDeclSyntax(
    modifiers: ModifierListSyntax,
    name: TokenSyntax,
    parameterLabels: [TokenSyntax],
    body: CodeBlockSyntax
  ) -> Decl {
    let parameters = parameterLabels.mapToFunctionParameterClause {
      SyntaxFactory.makeFunctionParameter(
        attributes: nil,
        firstName: $0.withTrailingTrivia([.spaces(1)]),
        secondName: SyntaxFactory.makeIdentifier("_"),
        colon: SyntaxFactory.makeColonToken(trailingTrivia: [.spaces(1)]),
        type: SyntaxFactory.makeAnyTypeIdentifier(),
        ellipsis: nil,
        defaultArgument: nil,
        trailingComma: nil
      )
    }

    switch self {
    case .variable:
      assert(parameterLabels.isEmpty)

      return SyntaxFactory.makeVariableDecl(
        attributes: nil,
        modifiers: modifiers,
        letOrVarKeyword: SyntaxFactory.makeVarKeyword(trailingTrivia: [.spaces(1)]),
        bindings: SyntaxFactory.makePatternBindingList([
          SyntaxFactory.makePatternBinding(
            pattern: SyntaxFactory.makeIdentifierPattern(identifier: name),
            typeAnnotation: SyntaxFactory.makeTypeAnnotation(
              colon: SyntaxFactory.makeColonToken(trailingTrivia: [.spaces(1)]),
              type: SyntaxFactory.makeAnyTypeIdentifier()
            ),
            initializer: nil,
            accessor: body,
            trailingComma: nil
          )
        ])
      )

    case .function:
      return SyntaxFactory.makeFunctionDecl(
        attributes: nil,
        modifiers: modifiers,
        funcKeyword: SyntaxFactory.makeFuncKeyword(trailingTrivia: [.spaces(1)]),
        identifier: name,
        genericParameterClause: nil,    // FIXME: We should make generics too
        signature: SyntaxFactory.makeFunctionSignature(
          input: parameters,
          throwsOrRethrowsKeyword: nil,
          output: SyntaxFactory.makeReturnClause(
            arrow: SyntaxFactory.makeArrowToken(
              leadingTrivia: .spaces(1), trailingTrivia: .spaces(1)
            ),
            returnType: SyntaxFactory.makeAnyTypeIdentifier()
          )
        ),
        genericWhereClause: nil,
        body: body
      )

    case .initializer:
      return SyntaxFactory.makeInitializerDecl(
        attributes: nil,
        modifiers: modifiers,
        initKeyword: SyntaxFactory.makeInitKeyword(),
        optionalMark: nil,
        genericParameterClause: nil,    // FIXME: We should make generics too
        parameters: parameters,
        throwsOrRethrowsKeyword: nil,
        genericWhereClause: nil,
        body: body
      )

    case .subscription:
      return SyntaxFactory.makeSubscriptDecl(
        attributes: nil,
        modifiers: modifiers,
        subscriptKeyword: SyntaxFactory.makeSubscriptKeyword(),
        genericParameterClause: nil,    // FIXME: We should make generics too
        indices: parameters,
        result: SyntaxFactory.makeReturnClause(
          arrow: SyntaxFactory.makeArrowToken(
            leadingTrivia: .spaces(1), trailingTrivia: .spaces(1)
          ),
          returnType: SyntaxFactory.makeAnyTypeIdentifier()
        ),
        genericWhereClause: nil,
        accessor: body
      )
    }
  }
}

enum StaticKeyword: String, Codable {
  case instance, `static`, `class`

  static func allCases(
    for decl: Decl
  ) -> [StaticKeyword] {
    if decl is ClassDeclSyntax {
      return [.instance, .static, .class]
    }
    return [.instance, .static]
  }

  func makeToken() -> TokenSyntax? {
    switch self {
    case .class:
      return SyntaxFactory.makeClassKeyword()
    case .static:
      return SyntaxFactory.makeStaticKeyword()
    case .instance:
      return nil
    }
  }
}

extension AccessLevel {
  static func allCases(for dc: DeclChain) -> [AccessLevel] {
    let maximum = dc.maximumAccessLevel
    return [.private, .fileprivate, .internal, .public].filter {
      $0 <= maximum
    }
  }

  func makeToken() -> TokenSyntax {
    switch self {
    case .private:
      return SyntaxFactory.makePrivateKeyword()
    case .fileprivate:
      return SyntaxFactory.makeFileprivateKeyword()
    case .internal:
      return SyntaxFactory.makeInternalKeyword()
    case .public:
      return SyntaxFactory.makePublicKeyword()
    case .open:
      // open isn't a keyword! Surprise!
      return SyntaxFactory.makeIdentifier("open")
    }
  }
}

extension InsertComputedMemberEvolution {
  init?<G>(for node: Syntax, in decl: DeclChain, using rng: inout G) throws
    where G: RandomNumberGenerator
  {
    guard
      !(decl.last is ProtocolDeclSyntax),
      let members = (node as? MemberDeclListSyntax).map(Array.init(_:))
    else { throw EvolutionError.unsupported }

    let kind = [MemberKind.function, .variable].randomElement(using: &rng)!
    let randomPart = rng.next(upperBound: UInt.max)

    let labeled: [Bool]
    if kind == .function {
      let arity = Int.random(in: 0..<10, using: &rng)
      labeled = (0..<arity).map { _ in Bool.random(using: &rng) }
    }
    else {
      labeled = []
    }

    self.init(
      index: Int.random(
        in: members.startIndex ... members.endIndex,
        using: &rng
      ),
      name: "__swiftEvolveInserted\(randomPart)",
      labeledParameters: labeled,
      memberKind: kind,
      staticKeyword: StaticKeyword.allCases(for: decl.last!)
        .randomElement(using: &rng)!,
      accessLevel: AccessLevel.allCases(for: decl)
        .randomElement(using: &rng)!
    )
  }

  func evolve(_ node: Syntax) -> Syntax {
    let members = node as! MemberDeclListSyntax

    let modifierKeywords = [accessLevel.makeToken(), staticKeyword.makeToken()]

    let modifiers = SyntaxFactory.makeModifierList(
      modifierKeywords.compactMap { $0 }
      .map { modifierName in
        SyntaxFactory.makeDeclModifier(
          name: modifierName.withTrailingTrivia([.spaces(1)]),
          detailLeftParen: nil,
          detail: nil,
          detailRightParen: nil
        )
      }
    )

    // Minor hack: We know we won't want to analyze this body, so we express it
    // as an unknown token rather than figure out how to generate the exact
    // nodes we'd want.
    let body = [
      #"fatalError("Resilience failure: Called a computed member inserted during later evolution!")"#
    ].mapToCodeBlock {
      SyntaxFactory.makeUnknown(
        $0,
        leadingTrivia: [.spaces(1)],
        trailingTrivia: [.spaces(1)]
      )
    }

    let newMember = memberKind.makeDeclSyntax(
      modifiers: modifiers,
      name: SyntaxFactory.makeIdentifier(name),
      parameterLabels: labeledParameters.map {
        SyntaxFactory.makeIdentifier($0 ? "label" : "_")
      },
      body: body
    )

    var membersCopy = Array(members)
    membersCopy.insert(
      SyntaxFactory.makeMemberDeclListItem(
        decl: newMember, semicolon: nil
      ).prependingTrivia([
        .newlines(2),
        .lineComment("// Synthesized by InsertComputedMemberEvolution"),
        .newlines(1)
      ]),
      at: index
    )

    return SyntaxFactory.makeMemberDeclList(membersCopy)
  }
}

extension InsertComputedUnnamedMemberEvolution {
  init?<G>(for node: Syntax, in decl: DeclChain, using rng: inout G) throws
    where G: RandomNumberGenerator
  {
    guard
      !(decl.last is ProtocolDeclSyntax),
      let members = (node as? MemberDeclListSyntax).map(Array.init(_:))
    else { throw EvolutionError.unsupported }

    var kind = [MemberKind.initializer, .subscription].randomElement(using: &rng)!
    let isConvenience = kind.mustBeConvenience(for: decl)
    let randomPart = rng.next(upperBound: UInt.max)

    let arity = Int.random(in: 0..<10, using: &rng)
    let labeled: [Bool] = [true] + (0..<arity).map { _ in Bool.random(using: &rng) }

    self.init(
      index: Int.random(
        in: members.startIndex ... members.endIndex,
        using: &rng
      ),
      name: "__swiftEvolveInserted\(randomPart)",
      labeledParameters: labeled,
      memberKind: kind,
      accessLevel: AccessLevel.allCases(for: decl)
        .randomElement(using: &rng)!,
      isConvenience: isConvenience
    )
  }

  func makePrerequisites<G>(
    for node: Syntax, in decl: DeclChain, using rng: inout G
    ) throws -> [Evolution] where G : RandomNumberGenerator {
    guard memberKind == .initializer else {
      return []
    }
    // If we insert a new init, it might prevent a memberwise init from being
    // synthesized.
    return [
      try SynthesizeMemberwiseInitializerEvolution
        .makeWithPrerequisites(for: node, in: decl, using: &rng)
      ].compactMap { $0 }.flatMap { $0 }
  }

  func evolve(_ node: Syntax) -> Syntax {
    let members = node as! MemberDeclListSyntax

    let modifierKeywords = [
      accessLevel.makeToken(),
      isConvenience ? SyntaxFactory.makeIdentifier("convenience") : nil
    ]

    let modifiers = SyntaxFactory.makeModifierList(
      modifierKeywords.compactMap { $0 }
        .map { modifierName in
          SyntaxFactory.makeDeclModifier(
            name: modifierName.withTrailingTrivia([.spaces(1)]),
            detailLeftParen: nil,
            detail: nil,
            detailRightParen: nil
          )
      }
    )

    // Minor hack: We know we won't want to analyze this body, so we express it
    // as an unknown token rather than figure out how to generate the exact
    // nodes we'd want.
    let body = [
      #"fatalError("Resilience failure: Called a computed member inserted during later evolution!")"#
    ].mapToCodeBlock {
      SyntaxFactory.makeUnknown(
        $0,
        leadingTrivia: [.spaces(1)],
        trailingTrivia: [.spaces(1)]
      )
    }

    let newMember = memberKind.makeDeclSyntax(
      modifiers: modifiers,
      name: SyntaxFactory.makeIdentifier(name),
      parameterLabels: labeledParameters.map {
        SyntaxFactory.makeIdentifier($0 ? name : "_")
      },
      body: body
    )

    var membersCopy = Array(members)
    membersCopy.insert(
      SyntaxFactory.makeMemberDeclListItem(
        decl: newMember, semicolon: nil
        ).prependingTrivia([
          .newlines(2),
          .lineComment("// Synthesized by InsertComputedUnnamedMemberEvolution"),
          .newlines(1)
          ]),
      at: index
    )

    return SyntaxFactory.makeMemberDeclList(membersCopy)
  }
}
