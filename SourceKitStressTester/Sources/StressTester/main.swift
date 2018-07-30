//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import SwiftSyntax
import SwiftLang

func main() throws {
  // Divide arguments into source files and compiler arguments
  let parts = CommandLine.arguments[1...].split(separator: "--", maxSplits: 1,
                                                omittingEmptySubsequences: false)
  guard parts.count >= 2 else {
    log(.usage)
    exit(EXIT_FAILURE)
  }

  // Stress test the files and handle errors
  do {
    try stressTest(files: Array(parts[0]), compilerArgs: Array(parts[1]))
  } catch let error as StressTestError {
    log(error)
    exit(EXIT_FAILURE)
  }
}

/// Invokes a range of SourceKit requests on each of the passed files in the
/// provided order, throwing a StressTestError for the first issue encountered.
func stressTest(files: [String], compilerArgs: [String]) throws {
  let connection = SourceKitdService()

  for (index, file) in files.enumerated() {
    let document = SourceKitDocument(file, args: compilerArgs, connection: connection)
    let documentInfo = try document.open()

    // code completion is expensive, so support limiting the number of completions
    let completions = limit(documentInfo.completionOffsets, to: "SK_STRESS_CODECOMPLETE_LIMIT")
    let isTruncated = completions.count < documentInfo.completionOffsets.count

    log("""
    [\(index + 1)/\(files.count)] Stress testing \(file):
       \(documentInfo.lineCount) lines of code
       \(documentInfo.cursorInfoPositions.count) CursorInfo requests
       \(documentInfo.rangeInfoRanges.count) RangeInfo requests
       \(documentInfo.completionOffsets.count) CodeComplete requests\(isTruncated ? " (limited to \(completions.count))" : "")
    """)

    for position in documentInfo.cursorInfoPositions {
      _ = try document.cursorInfo(position: position)
    }
    for range in documentInfo.rangeInfoRanges {
      _ = try document.rangeInfo(offset: range.offset, length: range.length)
    }
    for offsets in completions.chunked(32) {
      try processAsync(offsets, numWorkers: 4) {
          _ = try document.codeComplete(offset: $0)
      }
    }

    try document.close()
  }
}

func limit<T: Collection>(_ collection: T, to envVariable: String) -> T.SubSequence {
  guard let value = ProcessInfo.processInfo.environment[envVariable],
    let limit = Int(value) else { return collection[...] }
  return collection.prefix(limit)
}

struct SourceKitDocument {
  let file: String
  let args: [String]
  let connection: SourceKitdService

  init(_ file: String, args: [String], connection: SourceKitdService) {
    self.file = file
    self.args = args
    self.connection = connection
  }

  func open() throws -> DocumentInfo {
    let request = SourceKitdRequest(uid: .request_EditorOpen)

    request.addParameter(.key_SourceFile, value: file)
    request.addParameter(.key_Name, value: file)
    request.addParameter(.key_EnableSyntaxMap, value: 1)
    request.addParameter(.key_SyntacticOnly, value: 1)
    request.addParameter(.key_EnableSyntaxTree, value: 1)

    let compilerArgs = request.addArrayParameter(.key_CompilerArgs)
    for arg in args { compilerArgs.add(arg) }

    let response = connection.sendSyn(request: request)
    try throwIfInvalid(response, info: .editorOpen(file: file))

    let encodedSyntax = response.value.getString(.key_SerializedSyntaxTree)
    guard let sourceFileSyntax = try? SourceFileSyntax.decodeSourceFileSyntax(encodedSyntax) else {
      throw StressTestError.errorDecodingSyntaxTree(request: .editorOpen(file: file), response: response.description)
    }

    let lineStartOffsets = LineStartOffsets(for: sourceFileSyntax.description)
    let locationCollector = PositionAndRangeCollector(with: lineStartOffsets)
    locationCollector.visit(sourceFileSyntax)

    return locationCollector.documentInfo
  }

  func close() throws {
    let request = SourceKitdRequest(uid: .request_EditorClose)
    request.addParameter(.key_SourceFile, value: file)
    request.addParameter(.key_Name, value: file)

    let response = connection.sendSyn(request: request)
    try throwIfInvalid(response, info: .editorClose(file: file))
  }

  func rangeInfo(offset: Int, length: Int) throws -> SourceKitdResponse {
    let request = SourceKitdRequest(uid: .request_RangeInfo)

    request.addParameter(.key_SourceFile, value: file)
    request.addParameter(.key_Offset, value: offset)
    request.addParameter(.key_Length, value: length)
    request.addParameter(.key_RetrieveRefactorActions, value: 1)

    let compilerArgs = request.addArrayParameter(.key_CompilerArgs)
    for arg in args { compilerArgs.add(arg) }

    let response = connection.sendSyn(request: request)
    let requestInfo = RequestInfo.rangeInfo(file: file, offset: offset, length: length, args: args)
    try throwIfInvalid(response, info: requestInfo)

    return response
  }

  func cursorInfo(position: Position) throws -> SourceKitdResponse {
    let request = SourceKitdRequest(uid: .request_CursorInfo)

    request.addParameter(.key_SourceFile, value: file)
    request.addParameter(.key_Offset, value: position.offset)
    request.addParameter(.key_RetrieveRefactorActions, value: 1)

    let compilerArgs = request.addArrayParameter(.key_CompilerArgs)
    for arg in args { compilerArgs.add(arg) }

    let response = connection.sendSyn(request: request)
    let requestInfo = RequestInfo.cursorInfo(file: file, offset: position.offset, args: args)
    try throwIfInvalid(response, info: requestInfo)

    if let typeName = response.value.getOptional(.key_TypeName)?.getString(), typeName.contains("<<error type>>") {
      log(.errorTypeInResponse(request: requestInfo, response: response.value.description))
    }

    let symbolName = response.value.getOptional(.key_Name)?.getString()

    if let actions = response.value.getOptional(.key_RefactorActions)?.getArray() {
      for i in 0 ..< actions.count {
        let action = actions.getDictionary(i)
        let actionName = action.getString(.key_ActionName)
        guard actionName != "Global Rename" else { continue }
        let kind = action.getUID(.key_ActionUID)
        let request = SourceKitdRequest(uid: .request_SemanticRefactoring)

        request.addParameter(.key_ActionUID, value: kind)
        request.addParameter(.key_SourceFile, value: file)
        request.addParameter(.key_Line, value: position.line)
        request.addParameter(.key_Column, value: position.column)
        if let symbolName = symbolName, actionName == "Local Rename" {
          request.addParameter(.key_Name, value: symbolName)
        }
        let compilerArgs = request.addArrayParameter(.key_CompilerArgs)
        for arg in args { compilerArgs.add(arg) }

        let response = connection.sendSyn(request: request)
        let requestInfo = RequestInfo.semanticRefactoring(kind: actionName, file: file, offset: position.offset, args: args)
        try throwIfInvalid(response, info: requestInfo)
      }
    }

    return response
  }

  func codeComplete(offset: Int) throws -> SourceKitdResponse {
    let request = SourceKitdRequest(uid: .request_CodeComplete)

    request.addParameter(.key_SourceFile, value: file)
    request.addParameter(.key_Offset, value: offset)

    let compilerArgs = request.addArrayParameter(.key_CompilerArgs)
    for arg in args { compilerArgs.add(arg) }

    let response = connection.sendSyn(request: request)
    let requestInfo = RequestInfo.codeComplete(file: file, offset: offset, args: args)
    try throwIfInvalid(response, info: requestInfo)

    return response
  }

  private func throwIfInvalid(_ response: SourceKitdResponse, info: RequestInfo) throws {
    if response.isCompilerCrash || response.isConnectionInterruptionError {
      throw StressTestError.crashResponse(request: info)
    }
    // FIXME: We don't supply a valid new name for initializer calls for local
    // rename requests. Ignore these errors for now.
    if response.isError, !response.description.contains("does not match the arity of the old name") {
      throw StressTestError.errorResponse(request: info, response: response.description)
    }
  }
}

struct LineStartOffsets {
  let lineEndOffsets: [Int]
  let lineCount: Int

  init(for content: String) {
    lineEndOffsets = content.description.utf8
      .enumerated()
      .filter { $0.1 == UInt8(ascii: "\n") }
      .map { $0.0 + 1 }
    lineCount = lineEndOffsets.count
  }

  subscript(line: Int) -> Int {
    let prevLineIndex = line - 2;
    guard prevLineIndex >= 0 else { return 0 }
    return lineEndOffsets[prevLineIndex]
  }

  func column(for offset: Int, on line: Int) -> Int {
    return offset - self[line] + 1
  }
}

class PositionAndRangeCollector: SyntaxVisitor {
  var completionOffsets = [Int]()
  var cursorInfoPositions = [Position]()
  var rangeInfoRanges = [RangeInfo]()
  let lineStartOffsets: LineStartOffsets

  init(with lineOffsets: LineStartOffsets) {
    lineStartOffsets = lineOffsets
  }

  var documentInfo: DocumentInfo {
    return DocumentInfo(completionOffsets: completionOffsets,
                        cursorInfoPositions: cursorInfoPositions,
                        rangeInfoRanges: rangeInfoRanges,
                        lineCount: lineStartOffsets.lineCount)
  }

  override func visit(_ token: SwiftSyntax.TokenSyntax) {
    guard isTokenKindOfInterest(token.tokenKind) else { return }
    let pos = token.positionAfterSkippingLeadingTrivia
    let range = RangeInfo(start: Position(offset: pos.byteOffset, line: pos.line,
                                          column: lineStartOffsets.column(for: pos.byteOffset, on: pos.line)),
                          length: token.byteSizeAfterTrimmingTrivia)

    if shouldAddStartOffset(for: token.tokenKind) {
      cursorInfoPositions.append(range.start)
      completionOffsets.append(range.start.offset)
    }
    if shouldAddEndOffset(for: token.tokenKind) {
      completionOffsets.append(range.endOffset)
    }
  }

  override func visitPost(_ node: Syntax) {
    guard node.numberOfChildren > 0 else { return }
    let pos = node.positionAfterSkippingLeadingTrivia
    let range = RangeInfo(start: Position(offset: pos.byteOffset, line: pos.line,
                                          column: lineStartOffsets.column(for: pos.byteOffset, on: pos.line)),
                          length: node.byteSizeAfterTrimmingTrivia)
    guard range.length > 0, rangeInfoRanges.last != range else { return }
    rangeInfoRanges.append(range)
  }

  private func isTokenKindOfInterest(_ kind: TokenKind) -> Bool {
    return shouldAddStartOffset(for: kind) || shouldAddEndOffset(for: kind)
  }

  private func shouldAddStartOffset(for kind: TokenKind) -> Bool {
    switch kind {
    case .identifier: fallthrough
    case .dollarIdentifier:
      return true
    default:
      return false
    }
  }

  private func shouldAddEndOffset(for kind: TokenKind) -> Bool {
    switch kind {
    case .identifier: fallthrough
    case .dollarIdentifier: fallthrough
    case .rightParen: fallthrough
    case .rightBrace: fallthrough
    case .rightSquareBracket:
      return true
    default:
      return false
    }
  }
}

struct DocumentInfo {
  let completionOffsets: [Int]
  let cursorInfoPositions: [Position]
  let rangeInfoRanges: [RangeInfo]
  let lineCount: Int
}

struct Position: Equatable {
  let offset: Int
  let line: Int
  let column: Int
}

struct RangeInfo: Equatable {
  let start: Position
  let length: Int
  var offset: Int { return start.offset }
  var endOffset: Int { return start.offset + length }
}

/// Stores information on each SourceKit request
enum RequestInfo {
  case editorOpen(file: String)
  case editorClose(file: String)
  case cursorInfo(file: String, offset: Int, args: [String])
  case rangeInfo(file: String, offset: Int, length: Int, args: [String])
  case codeComplete(file: String, offset: Int, args: [String])
  case semanticRefactoring(kind: String, file: String, offset: Int, args: [String])
}

extension RequestInfo: CustomStringConvertible {
  var description: String {
    switch self {
    case .editorOpen(let file):
      return "SourceKit request EditorOpen on \(file)"
    case .editorClose(let file):
      return "SourceKit request EditorClose on \(file)"
    case .cursorInfo(let file, let offset, let args):
      return "SourceKit request CursorInfo in \(file) at offset \(offset) with args: \(args.joined(separator: " "))"
    case .rangeInfo(let file, let offset, let length, let args):
      return "SourceKit request RangeInfo in \(file) at offset \(offset) for length \(length) with args: \(args.joined(separator: " "))"
    case .codeComplete(let file, let offset, let args):
      return "SourceKit request CodeComplete in \(file) at offset \(offset) with args: \(args.joined(separator: " "))"
    case .semanticRefactoring(let kind, let file, let offset, let args):
      return "SourceKit request SemanticRefactoring (\(kind)) in \(file) at offset \(offset) with args: \(args.joined(separator: " "))"
    }
  }
}

enum StressTestError: Error {
  case usage
  case crashResponse(request: RequestInfo)
  case errorResponse(request: RequestInfo, response: String)
  case errorTypeInResponse(request: RequestInfo, response: String)
  case missingExpectedResult(expected: String, request: RequestInfo, response: String)
  case errorDecodingSyntaxTree(request: RequestInfo, response: String)
}

/// Divide the given worklist evenly among a number of workers. If any failures
/// are encountered, the worklist is reprocessed sequentially to ensure the
/// error thrown corresponds to the item in the list that triggered the issue.
func processAsync<T>(_ worklist: [T], numWorkers: Int = 4,
                             execute: @escaping (T) throws -> Void) throws {
  guard !worklist.isEmpty else { return }

  let group = DispatchGroup()
  let batchSize = Swift.max(worklist.count / numWorkers, 1)
  var anyFailed = false

  for batch in worklist.chunked(batchSize) {
    group.enter()
    DispatchQueue.global().async {
      do {
        try batch.lazy.forEach(execute)
      } catch {
        DispatchQueue.global().sync{ anyFailed = true }
      }
      group.leave()
    }
  }
  group.wait()

  guard anyFailed else { return }

  for item in worklist {
    try execute(item)
  }
}

func log(_ message: String) {
  let prefix = "[sk-stress-test]"
  var standardError = FileHandle.standardError
  print("\(prefix) \(message)\n", to: &standardError)
}

func log(_ error: StressTestError) {
  switch error {
  case .usage:
    log("usage: sk-stress-test <source file>... -- <compiler args>")
  case .crashResponse(let request):
    log("error: crashed invoking \(request)")
  case .errorResponse(let request, let response):
    log("""
      error: failed invoking \(request).
      Received response: \(response)
      """)
  case .errorTypeInResponse(let request, _):
    log("warning: error type in response of \(request)")
  case .missingExpectedResult(let expected, let request, _):
    log("warning: missing expected result '\(expected)' in response to \(request)")
  case .errorDecodingSyntaxTree(request: let request, response: let response):
    log("""
      error: failed decoding syntax tree in response of \(request)"
      Received response: \(response)
      """)
  }
}

// Allow standardError/standardOutput to be passed as a target to print()
extension FileHandle : TextOutputStream {
  public func write(_ string: String) {
    guard let data = string.data(using: .utf8) else { return }
    self.write(data)
  }
}

extension Collection where Index == Int {
  func chunked(_ chunkSize: Int) -> [[Element]] {
    return stride(from: 0, to: self.count, by: chunkSize).map {
      Array(self[$0 ..< Swift.min($0 + chunkSize, self.count)])
    }
  }
}

try main()