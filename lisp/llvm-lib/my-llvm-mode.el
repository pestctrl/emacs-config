;;; llvm-mode.el --- Major mode for the LLVM assembler language.

;; Maintainer:  The LLVM team, http://llvm.org/
;; Version: 1.0

;;; Commentary:

;; Major mode for editing LLVM IR files.

;;; Code:

(defvar llvm-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?% "-" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?: "-" table)
    (modify-syntax-entry ?\; "< " table)
    (modify-syntax-entry ?\n "> " table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23" table)
    ;; (setq llvm-mode-syntax-table table)
    table
    )
  "Syntax table used while in LLVM mode.")

(defvar llvm-font-lock-keyword-words
  `(
    ;; Attributes
    (,(regexp-opt
       '("alwaysinline" "argmemonly" "allocsize" "builtin" "cold" "convergent" "dereferenceable" "dereferenceable_or_null" "hot" "inaccessiblememonly"
         "inaccessiblemem_or_argmemonly" "inalloca" "inlinehint" "jumptable" "minsize" "mustprogress" "naked" "nobuiltin" "nonnull"
         "nocallback" "nocf_check" "noduplicate" "nofree" "noimplicitfloat" "noinline" "nomerge" "nonlazybind" "noprofile" "noredzone" "noreturn"
         "norecurse" "nosync" "noundef" "nounwind" "nosanitize_bounds" "nosanitize_coverage" "null_pointer_is_valid" "optforfuzzing" "optnone" "optsize" "preallocated" "readnone" "readonly" "returned" "returns_twice"
         "shadowcallstack" "speculatable" "speculative_load_hardening" "ssp" "sspreq" "sspstrong" "safestack" "sanitize_address" "sanitize_hwaddress" "sanitize_memtag"
         "sanitize_thread" "sanitize_memory" "strictfp" "swifterror" "uwtable" "vscale_range" "willreturn" "writeonly" "immarg")
       'symbols)
     . font-lock-constant-face)
    ;; Keywords
    (,(regexp-opt
        '(;; Toplevel entities
          "declare" "define" "module" "target" "source_filename" "global" "constant" "const" "alias" "ifunc" "comdat"
          "attributes" "uselistorder" "uselistorder_bb"
          ;; Linkage types
          "private" "internal" "weak" "weak_odr" "linkonce" "linkonce_odr" "available_externally" "appending" "common" "extern_weak" "external"
          "uninitialized" "implementation" "..."
          ;; Values
          "true" "false" "null" "undef" "zeroinitializer" "none" "c" "asm" "blockaddress" "poison"

          ;; Calling conventions
          "ccc" "fastcc" "coldcc" "webkit_jscc" "anyregcc" "preserve_mostcc" "preserve_allcc"
          "cxx_fast_tlscc" "swiftcc" "tailcc" "swifttailcc" "cfguard_checkcc"
          ;; Visibility styles
          "default" "hidden" "protected"
          ;; DLL storages
          "dllimport" "dllexport"
          ;; Thread local
          "thread_local" "localdynamic" "initialexec" "localexec"
          ;; Runtime preemption specifiers
          "dso_preemptable" "dso_local" "dso_local_equivalent"

          "gc" "atomic" "no_cfi" "volatile" "personality" "prologue" "section") 'symbols) . font-lock-keyword-face)
    ;; Arithmetic and Logical Operators
    (,(regexp-opt '("add" "sub" "mul" "sdiv" "udiv" "urem" "srem" "and" "or" "xor"
                     "setne" "seteq" "setlt" "setgt" "setle" "setge") 'symbols) . font-lock-keyword-face)
    ;; Floating-point operators
    (,(regexp-opt '("fadd" "fsub" "fneg" "fmul" "fdiv" "frem") 'symbols) . font-lock-keyword-face)
    ;; Special instructions
    (,(regexp-opt '("phi" "tail" "call" "select" "to" "shl" "lshr" "ashr" "fcmp" "icmp" "va_arg" "landingpad" "freeze") 'symbols) . font-lock-keyword-face)
    ;; Control instructions
    (,(regexp-opt '("ret" "br" "switch" "invoke" "resume" "unwind" "unreachable" "indirectbr" "callbr") 'symbols) . font-lock-keyword-face)
    ;; Memory operators
    (,(regexp-opt '("malloc" "alloca" "free" "load" "store" "getelementptr" "fence" "cmpxchg" "atomicrmw") 'symbols) . font-lock-keyword-face)
    ;; Casts
    (,(regexp-opt '("bitcast" "inttoptr" "ptrtoint" "trunc" "zext" "sext" "fptrunc" "fpext" "fptoui" "fptosi" "uitofp" "sitofp" "addrspacecast") 'symbols) . font-lock-keyword-face)
    ;; Vector ops
    (,(regexp-opt '("extractelement" "insertelement" "shufflevector") 'symbols) . font-lock-keyword-face)
    ;; Aggregate ops
    (,(regexp-opt '("extractvalue" "insertvalue") 'symbols) . font-lock-keyword-face)
    ;; Metadata types
    (,(regexp-opt '("distinct") 'symbols) . font-lock-keyword-face)
    ;; Atomic memory ordering constraints
    (,(regexp-opt '("unordered" "monotonic" "acquire" "release" "acq_rel" "seq_cst") 'symbols) . font-lock-keyword-face)
    ;; Fast-math flags
    (,(regexp-opt '("nnan" "ninf" "nsz" "arcp" "contract" "afn" "reassoc" "fast") 'symbols) . font-lock-keyword-face)
    ;; Use-list order directives
    (,(regexp-opt '("uselistorder" "uselistorder_bb") 'symbols) . font-lock-keyword-face)
    (,(rx "@" symbol-start (+? (or (syntax symbol) (syntax word))) symbol-end) . font-lock-function-name-face)
    ;; Integer literals
    ("\\b[-]?[0-9]+\\b" . font-lock-preprocessor-face)
    ))

(defvar llvm-pre-opcode-attributes
  (rx symbol-start
      (or "nnan" "ninf" "nsz" "arcp" "contract" "afn" "reassoc" "fast" "nsw" "nuw" "mc-replicate")
      symbol-end))

(defvar llvm-font-lock-keywords
  `(
    ;; Labels
    (,(rx symbol-start
          (group
           (+ (or alphanumeric (any "._"))))
          ":")
     . font-lock-function-name-face)
    ;; Labels
    (,(rx symbol-start
          (group "label")
          " "
          (group "%" (+ (or alphanumeric (any "._")))))
     (1 font-lock-type-face) (2 font-lock-function-name-face))
    ;; phi clauses
    (,(rx "[" (* " ") (+ (not ",")) "," (* " ")
          (group "%" (+ (or alphanumeric (any "_."))))
          (* " ") "]")
     (1 font-lock-function-name-face))
    ;; Virtual registers with type attributes
    (,(rx symbol-start "%" (+ (or alphanumeric (any "_.")))
          ":"
          (+ (or alphanumeric (any "_."))))
     . font-lock-variable-name-face)
    ;; Variables
    (,(rx (any "%$") symbol-start (+? (or (syntax symbol) (syntax word))) symbol-end) . font-lock-variable-name-face)
    ;; Attributes
    (,(rx "!" (+ (or alphanumeric (any "._")))) . 'shadow)
    ;; Machine Opcodes
    (,(rx "= "
          (* (regexp llvm-pre-opcode-attributes) " ")
          (group (+ (or alphanumeric (any "._")))))
     . (1 font-lock-keyword-face))
    (,(rx line-start
          (optional (+ digit) "B")
          (+ whitespace)
          (group (+ (or alphanumeric (any "._"))))
          (+ (not (any "=" "\n")))
          line-end)
     . (1 font-lock-keyword-face))
    ;; Types
    (,(rx symbol-start
          (or "void"
              "i1" "i8" "i16" "i32" "i64" "i128"
              "half" "bfloat" "float" "double" "fp128"
              "ptr"
              "x86_fp80" "x86_mmx" "x86_amx" "ppc_fp128"
              "type" "label" "opaque" "token")
          symbol-end)
     . font-lock-type-face)
    ;; Other Attributes
    (,(rx (or "renamable" "killed " "implicit-def" "implicit" "debug-location" "nsw" "nuw" "align" "dead" "early-clobber")) . 'shadow)

    ,@llvm-font-lock-keyword-words

    (,(rx "$" (+ (or (syntax symbol) (syntax word))) symbol-end) . 'font-lock-variable-use-face)

    ;; Floating point constants
    ("\\b[-+]?[0-9]+.[0-9]*\\([eE][-+]?[0-9]+\\)?\\b" . font-lock-preprocessor-face)
    ;; Hex constants
    ("\\b0x[0-9A-Fa-f]+\\b" . font-lock-preprocessor-face)
    ;; Integer literals
    ("\\b[-]?[0-9]+\\b" . font-lock-preprocessor-face)
    )
  "Syntax highlighting for LLVM.")

;;;###autoload
(define-derived-mode llvm-mode prog-mode "LLVM"
  "Major mode for editing LLVM source files.
\\{llvm-mode-map}
  Runs `llvm-mode-hook' on startup."
  (setq font-lock-defaults `(llvm-font-lock-keywords))
  (setq-local comment-start ";"))

;; Associate .ll files with llvm-mode
;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.ll\\'" 'llvm-mode))

(provide 'my-llvm-mode)

;;; my-llvm-mode.el ends here
