;"use strict";
var___95__prelude = do_import("standardGrace", gracecode_standardGrace);
if (typeof gctCache !== "undefined")
  gctCache["standardInput"] = "classes:\nconfidential:\ndialect:\n standardGrace\nfresh-methods:\nmodules:\n collectionsPrelude\n standardGrace\npath:\n /dev/stdin\npublic:\npublicMethodTypes:\ntypes:\n";
if (typeof originalSourceLines !== "undefined") {
  originalSourceLines["standardInput"] = [
 ];
}
function gracecode_standardInput() {
  setModuleName("standardInput");
  importedModules["standardInput"] = this;
  var module$standardInput = this;
  this.definitionModule = "standardInput";
  this.definitionLine = 0;
  var var_prelude = var___95__prelude;
  this.closureKeys = this.closureKeys || [];
  this.closureKeys.push("outer_standardInput_0");
  this.outer_standardInput_0 = var_prelude;
  // Dialect "standardGrace"
  var_prelude = do_import("standardGrace", gracecode_standardGrace);
  this.outer = var_prelude;
  return this;
}
if (typeof global !== "undefined")
  global.gracecode_standardInput = gracecode_standardInput;
if (typeof window !== "undefined")
  window.gracecode_standardInput = gracecode_standardInput;
gracecode_standardInput.imports = ["standardGrace"];
