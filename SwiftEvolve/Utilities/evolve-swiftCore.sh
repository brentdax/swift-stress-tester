#!/bin/bash

: ${SWIFT_CONFIG:=RelWithDebInfoAssert}

# Note: this script is especially terrible; it probably needs to invoke ninja
# to figure out the commands it ought to use, then modify them.

until [ -e build ]; do
  if [[ "$(pwd)" == "/" ]]; then
    echo "Can't find build directory"
    exit 1
  fi
  cd ..
done

ROOT="$(pwd)"
BUILD=$ROOT/build/Ninja-$SWIFT_CONFIG
BUILD_SWIFT=$BUILD/swift-macosx-x86_64
EVOLVE=$ROOT/swift-stress-tester/SwiftEvolve

function gensha() {
  find $BUILD_SWIFT -type f -print0 | sort -z | xargs -0 shasum
}

#gensha >$EVOLVE/before.sha

cd $EVOLVE && env PATH="$BUILD/swiftpm-macosx-x86_64/x86_64-apple-macosx/debug:$PATH" swift run swift-evolve --replace --rules=$EVOLVE/Utilities/swiftCore-exclude.json $ROOT/swift/stdlib/public/core/*.swift \
&& cd $ROOT/swift && utils/build-script --release-debuginfo --debug-swift-stdlib -t

# Currently this is hacked to simply build Swift with a debug stdlib while I
# shake out non-resilience-related issues.




#&& cd $BUILD_SWIFT/stdlib/public/core && /usr/bin/python $ROOT/swift/utils/line-directive @$BUILD_SWIFT/stdlib/public/core/kk7SI.txt -- $BUILD_SWIFT/./bin/swiftc -c -sdk /Applications/Xcode-Everglades.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk -target x86_64-apple-macosx10.9 -resource-dir $BUILD_SWIFT/./lib/swift -F /Applications/Xcode-Everglades.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk/../../../Developer/Library/Frameworks -O -g -D INTERNAL_CHECKS_ENABLED -D SWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS -I $BUILD_SWIFT/./lib/swift/macosx/x86_64 -module-cache-path $BUILD_SWIFT/./module-cache -no-link-objc-runtime -Xfrontend -enable-resilience -Xfrontend -enable-sil-ownership -Xfrontend -enable-mandatory-semantic-arc-opts -Xfrontend -enforce-exclusivity=unchecked -nostdimport -parse-stdlib -module-name Swift -Xfrontend -group-info-path -Xfrontend $ROOT/swift/stdlib/public/core/GroupInfo.json -swift-version 5 -warn-swift3-objc-inference-complete -Xfrontend -verify-syntax-tree -Xllvm -sil-inline-generics -Xllvm -sil-partial-specialization -Xcc -DswiftCore_EXPORTS -warn-implicit-overrides -module-link-name swiftCore -force-single-frontend-invocation -Xcc -D__SWIFT_CURRENT_DYLIB=swiftCore -parse-as-library -o $BUILD_SWIFT/stdlib/public/core/macosx/x86_64/Swift.o @$BUILD_SWIFT/stdlib/public/core/kk7SI.txt \
#&& cd $BUILD_SWIFT && $BUILD/llvm-macosx-x86_64/./bin/clang++ -Wno-unknown-warning-option -Werror=unguarded-availability-new -fno-stack-protector -fPIC -fvisibility-inlines-hidden -Werror=date-time -Werror=unguarded-availability-new -std=c++11 -Wall -Wextra -Wno-unused-parameter -Wwrite-strings -Wcast-qual -Wmissing-field-initializers -Wcovered-switch-default -Wno-class-memaccess -Wnon-virtual-dtor -Wdelete-non-virtual-dtor -Wstring-conversion -fdiagnostics-color -Werror=switch -Wdocumentation -Wimplicit-fallthrough -Wunreachable-code -Woverloaded-virtual -DOBJC_OLD_DISPATCH_PROTOTYPES=0 -fno-sanitize=all -DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1 -O2 -isysroot /Applications/Xcode-Everglades.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk -dynamiclib -Wl,-headerpad_max_install_names -all_load -target x86_64-apple-macosx10.9 -isysroot /Applications/Xcode-Everglades.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk -arch x86_64 -F /Applications/Xcode-Everglades.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk/../../../Developer/Library/Frameworks -mmacosx-version-min=10.9 -Wl,-sectcreate,__TEXT,__info_plist,$BUILD_SWIFT/stdlib/public/core/Info.plist -Wl,-application_extension  "-L$BUILD_SWIFT/./lib/swift/macosx/x86_64" "-L$BUILD_SWIFT/./bin/../lib/swift/macosx/x86_64" "-L$BUILD_SWIFT/./bin/../lib/swift/macosx" -o lib/swift/macosx/x86_64/libswiftCore.dylib -install_name @rpath/libswiftCore.dylib stdlib/public/core/macosx/x86_64/Swift.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/AnyHashableSupport.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Array.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Casting.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/CompatibilityOverride.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/CygwinPort.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Demangle.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Enum.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ErrorObjectConstants.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ErrorObjectNative.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Errors.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ErrorDefaultImpls.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Exclusivity.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ExistentialContainer.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Heap.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/HeapObject.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ImageInspectionMachO.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ImageInspectionELF.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ImageInspectionCOFF.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/KeyPaths.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/KnownMetadata.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/LLVMSupport.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Metadata.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/MetadataLookup.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/MutexPThread.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/MutexWin32.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Numeric.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Once.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/Portability.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ProtocolConformance.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/RefCount.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/RuntimeInvocationsTracking.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/SwiftDtoa.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/__/__/__/lib/Demangling/OldDemangler.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/__/__/__/lib/Demangling/Demangler.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/__/__/__/lib/Demangling/NodePrinter.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/__/__/__/lib/Demangling/Context.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/__/__/__/lib/Demangling/ManglingUtils.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/__/__/__/lib/Demangling/Punycode.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/__/__/__/lib/Demangling/NodeDumper.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ErrorObject.mm.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/SwiftObject.mm.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/SwiftValue.mm.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ReflectionMirror.mm.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/ObjCRuntimeGetImageNameFromClass.mm.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/__/__/__/lib/Demangling/OldRemangler.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/__/__/__/lib/Demangling/Remangler.cpp.o stdlib/public/runtime/CMakeFiles/swiftRuntime-macosx-x86_64.dir/__/__/__/lib/Demangling/TypeDecoder.cpp.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/Assert.cpp.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/CommandLine.cpp.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/GlobalObjects.cpp.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/LibcShims.cpp.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/Random.cpp.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/Stubs.cpp.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/ThreadLocalStorage.cpp.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/MathStubs.cpp.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/Availability.mm.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/FoundationHelpers.mm.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/OptionalBridgingHelper.mm.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/Reflection.mm.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/SwiftNativeNSXXXBaseARC.m.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/8/SwiftNativeNSXXXBase.mm.o stdlib/public/stubs/CMakeFiles/swiftStdlibStubs-macosx-x86_64.dir/UnicodeNormalization.cpp.o -L$BUILD/llvm-macosx-x86_64/./lib -framework Foundation -framework CoreFoundation -licucore \
#&& cd $BUILD_SWIFT/stdlib/public/core && /Applications/Xcode-Everglades.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/lipo -create -output $BUILD_SWIFT/./lib/swift/macosx/libswiftCore.dylib $BUILD_SWIFT/lib/swift/macosx/x86_64/libswiftCore.dylib && codesign -f -s - $BUILD_SWIFT/./lib/swift/macosx/libswiftCore.dylib

#gensha >$EVOLVE/before.sha
#diff $EVOLVE/before.sha $EVOLVE/after.sha
