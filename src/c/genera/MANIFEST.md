# C Runtime Manifest

Build: `gcc -O3 -march=native -nostdlib -static -o gna src/c/genera.c`
Binary: ~128KB. Freestanding: no libc, no CRT.

## Architecture

```
genera.c → lang/lang.c → std/std.c → sys/sys.c
                                         ├── types.c      (portable types, memcpy/memset)
                                         ├── linux/x86_64.c (syscall ABI, _start)
                                         ├── api.c        (OS wrappers, file I/O, terminal, perf)
                                         └── x86.c        (instruction encoding)
                                      std/
                                         ├── fmt.c        (OutBuf, pf, ANSI, CSI, pretty-print)
                                         ├── mem.c        (Arena allocator)
                                         ├── str.c        (Str, StrId, intern, StrBuild)
                                         ├── val.c        (NaN-boxed Val, 12 types in 64 bits)
                                         ├── arr.c        (DynArray, HashMap)
                                         ├── tap.c        (DISPATCH, obs bitmask, trace ring, rdtsc)
                                         └── cmd.c        (command registry, key input, line editor, REPL, base_init)
                                      lang/
                                         ├── proto.c      (Cons, protocol dispatch X-macros)
                                         ├── coll.c       (pmap HAMT, pvec trie, atom, sig dispatch)
                                         ├── grammar.c    (universal parser, bitmask views, entity bridge, symbols)
                                         ├── eval.c       (Env, eval/apply, special forms, builtins, printer)
                                         ├── jit.c        (GNode → x86-64 JIT)
                                         ├── cc.c         (GNode → C source emitter, gcc driver)
                                         ├── image.c      (call graph, program database)
                                         ├── test.c       (~325 tests)
                                         ├── bench.c      (benchmarks)
                                         ├── watch.c      (inotify file watcher)
                                         ├── repl.c       (extended REPL commands)
                                         └── cli.c        (CLI argument dispatch)
```

Init order: `base_init() → proto_init() → grammar_init() → coll_init() → coll_register_protos() → eval_init()`

## Arena Lifetime Rules

Three arenas. The choice is about **lifetime**, not performance.

| Arena | Lifetime | Reset | Use for |
|-------|----------|-------|---------|
| `g_perm` | Process | Never | Builtins, interned strings, grammar nodes, bitmasks, protocol tables |
| `g_req` | Per-eval | `arena_reset(&g_req)` between top-level evals | Cons cells, closures, eval results, CPVec/CPMap handles, Str values |
| `g_temp` | Per-operation | Reset inside `gram_parse()`, scratch | Parser stack temporaries, scratch buffers |

**Rules:**
- `FnObj` for **builtins** → `g_perm` (lives forever, referenced by env)
- `FnObj` for **closures** → `g_req` (created during eval, survives until next top-level reset)
- `Env *` from `env_create` → `g_req` (lexical scope, dies with eval cycle)
- `Cons *` from `cons_new` → `g_req` (always — cons cells are eval-time data)
- `CPVec *`, `CPMap *` handles → `g_req` (the handle; coll pool is separate `sys_alloc`)
- `Str *` for user strings → `g_req` (the Str struct + bytes)
- Interned `Str` data → `g_perm` (via `str_dup(&g_perm, ...)` inside `str_intern`)
- Grammar `GNode[]`, bitmasks → `g_perm` (via `gram_new`)
- Image entries → `g_image[]` static array (not arena-backed)
- Coll pools (nodes, leaves, smalls, vnodes, vleaves) → `sys_alloc` (not arena-backed)

**Rule of thumb:** If it outlives a single `eval()` call, it goes in `g_perm`. If it's created during eval and consumed before the next top-level form, `g_req`. If it's truly scratch, `g_temp`.

## Val Type System

`Val` = `u64` (NaN-boxed). 12 types, 48-bit payload.

| Tag | Constructor | Predicate | Extractor | Payload |
|-----|-------------|-----------|-----------|---------|
| f64 | `val_f64(d)` | `val_is_f64(v)` | `val_as_f64(v)` | IEEE 754 double (no NaN tag) |
| nil | `val_nil()` / `NIL` | `val_is_nil(v)` | — | Singleton `0x7FF8...` |
| bool | `val_bool(b)` / `val_true()` / `val_false()` | `val_is_bool(v)` | `val_as_bool(v)` | 0 or 1 |
| int | `val_int(n)` | `val_is_int(v)` | `val_as_int(v)` | Sign-extended 48-bit i64 |
| sym | `val_sym(id)` | `val_is_sym(v)` | `val_as_sym(v)` | StrId (u32) |
| kw | `val_kw(id)` | `val_is_kw(v)` | `val_as_kw(v)` | StrId (u32) |
| str | `val_str(s)` | `val_is_str(v)` | `val_as_str(v)` | Str* pointer |
| pmap | `val_pmap(p)` | `val_is_pmap(v)` | `val_as_pmap(v)` | CPMap* pointer |
| pvec | `val_pvec(p)` | `val_is_pvec(v)` | `val_as_pvec(v)` | CPVec* pointer |
| fn | `val_fn(p)` | `val_is_fn(v)` | `val_as_fn(v)` | FnObj* pointer |
| cons | `val_cons(p)` | `val_is_cons(v)` | `val_as_cons(v)` | Cons* pointer |

`val_truthy(v)`: false only for nil and false. Everything else is truthy.
`val_tag_idx(v)`: returns 0-10 index for protocol dispatch tables.

## API by Layer

### sys/ — Hardware Boundary

```c
// types.c — portable types, no deps
u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 usize bool
ALIGN_UP(x,a)  LIKELY(x)  UNLIKELY(x)  POPCOUNT(x)  CTZ(x)  CLZ(x)
ALWAYS_INLINE  NOINLINE  ALIGNED(n)  MIN(a,b)  MAX(a,b)  CLAMP(x,lo,hi)
memcpy memset memmove memcmp strlen strcmp  // provided for -nostdlib
va_list va_start va_end va_arg              // compiler builtins

// linux/x86_64.c — raw syscalls (ABI: rax=NR, rdi/rsi/rdx/r10/r8/r9)
_sc1 _sc2 _sc3 _sc4 _sc6                   // syscall wrappers
sys_exit(code)                              // exit_group

// api.c — OS wrappers
sys_read(fd, buf, n) → i64                  // read bytes
sys_write(fd, buf, n) → i64                 // write bytes
sys_open(path, flags, mode) → int           // open file
sys_close(fd) → int
sys_lseek(fd, off, whence) → i64
sys_mmap(addr, len, prot, flags, fd, off) → void*
sys_munmap(addr, len) → int
sys_alloc(size) → void*                     // page-aligned RW
sys_free(ptr, size)
sys_alloc_exec(size) → void*                // page-aligned RWX (JIT)
sys_free_exec(ptr, size)
sys_time_ns() → u64                         // CLOCK_MONOTONIC
sys_fork() → int
sys_pipe(fds) → int
sys_dup2(old, new) → int
sys_execve(path, argv, envp) → int
sys_waitpid(pid, status, opts) → int
sys_run(path, argv) → int                   // fork+exec+wait, returns exit code
sys_run_capture(path, argv, out, cap, len) → int  // fork+exec+capture stdout
FileData sys_read_file(path, alloc_fn) → {data, len}
sys_write_file(path, data, len) → bool
sys_isatty(fd) → bool
sys_term_width() → u32
sys_term_size(rows*, cols*)
sys_term_raw()                              // raw mode (char-at-a-time)
sys_term_cooked()                           // restore canonical mode
sys_inotify_init() → int
sys_inotify_add_watch(fd, path, mask) → int
sys_poll(fds, nfds, timeout_ms) → int
sys_nanosleep(ns) → int
perf_init() → PerfCounters                  // open hardware perf counters
perf_start(pc)  perf_stop(pc)  perf_close(pc)
perf_read(fd) → u64

// x86.c — instruction encoding (for JIT)
// CodeBuf, x86_* encoding functions (see jit.c for usage)
```

### std/ — Standard Library (no libc)

```c
// fmt.c — output + formatting
OutBuf {buf, pos, cap}
buf_c(b, char)                              // append char
buf_s(b, str)                               // append C string
buf_n(b, str, n)                            // append n bytes
buf_u(b, u64)  buf_i(b, i64)  buf_x(b, u64)  buf_hex(b, u64)
buf_f1(b, f64)                              // "12.3" (1 decimal place)
buf_flush(b, fd)  buf_reset(b)
buf_vfmt(b, fmt, va)  buf_fmt(b, fmt, ...)  // mini printf: %s %d %u %x %c %lld %llu %llx
pf(fmt, ...)                                // printf → stdout (auto-flush)
pfc(code)                                   // buffer ANSI code if g_color
print_flush()
buf_pad(b, ch, n)                           // repeat char
buf_rjust_i(b, val, width)                  // right-justify integer
buf_ljust_s(b, str, width)                  // left-justify string
buf_rjust_s(b, str, width)
buf_hr(b, width)                            // horizontal rule
buf_row(b, vals, widths, ncols)             // table row
buf_elapsed(b, ns)                          // "12.3 ms" style
buf_bar(b, val, max, width, color)          // proportional bar
buf_bytes(b, n)                             // "1.2 MB" style
buf_label(b, label, val)  buf_label_i(b, label, i64)
csi(a, b, code)                             // CSI sequence primitive
cur_up(n) cur_down(n) cur_fwd(n) cur_back(n) cur_col(c) cur_goto(r,c)
erase_eol() erase_line() erase_screen()
cur_save() cur_restore() cur_hide() cur_show()
screen_alt() screen_main() scroll_set(t,b) scroll_reset()
buf_fg(b, n)  buf_bg(b, n)                 // 256-color
buf_fg_rgb(b, r, g, bl)  buf_bg_rgb(b, r, g, bl)  // 24-bit RGB
// Colors: C_RESET C_BOLD C_DIM C_RED C_GREEN C_YELLOW C_BLUE C_CYAN C_MAGENTA C_GRAY
hash32(u32) → u32                           // hash function
now_ns = sys_time_ns                        // alias

// mem.c — arena bump allocator
Arena {current, default_size, name, alloc_count, alloc_total, live_bytes, high_water, ...}
ArenaMark {arena, block, used}
arena_create(default_size) → Arena
arena_alloc(a, size, align) → void*         // ~2ns hot path
arena_push(a, T) → T*                      // typed alloc (macro)
arena_push_n(a, T, n) → T*                 // typed array alloc (macro)
arena_reset(a)                              // free all but first block, reset live_bytes
arena_destroy(a)                            // free everything
arena_begin_temp(a) → ArenaMark             // save point
arena_end_temp(mark)                        // restore to save point
arena_snap(a) → ArenaSnap                   // {alloc_count, alloc_total} for delta
g_temp g_req g_perm                         // global arenas (see lifetime rules)

// str.c — fat strings + interning
Str {data, len}                             // u8*, u32
StrId = u32                                 // interned string index
STR_EMPTY  STR_LIT("foo")                  // compile-time Str from literal
str_eq(a, b) → bool
str_slice(s, start, len) → Str
str_hash(s) → u32                           // FNV-1a
str_dup(arena, s) → Str                     // arena copy
str_find(s, ch) → u32                       // index or s.len
str_rfind(s, ch) → u32
str_after(s, ch) → u32                      // index past first occurrence, or 0
str_after_last(s, ch) → u32
str_contains(s, ch) → bool
str_starts_with(s, pre) → bool
str_ends_with(s, suf) → bool
str_trim(s) → Str                           // whitespace trim
str_cat(arena, x, y) → Str                  // concatenate
StrBuild {data, len, cap, arena}
strbuild(arena, cap) → StrBuild
sb_byte(sb, ch)  sb_str(sb, Str)  sb_cstr(sb, cstr)
sb_finish(sb) → Str
str_intern(s) → StrId                       // intern: O(1) compare after
str_from_id(id) → Str                       // resolve interned
INTERN("foo")                               // macro: str_intern(STR_LIT("foo"))

// val.c — NaN-boxed values (see Val Type System table above)
Val = u64
val_nil() val_bool(b) val_true() val_false() val_int(n) val_sym(id) val_kw(id)
val_str(Str*) val_pmap(void*) val_pvec(void*) val_fn(void*) val_cons(void*) val_f64(d)
val_is_nil val_is_bool val_is_int val_is_sym val_is_kw val_is_str
val_is_pmap val_is_pvec val_is_fn val_is_cons val_is_f64
val_as_bool val_as_int val_as_sym val_as_kw val_as_str
val_as_pmap val_as_pvec val_as_fn val_as_cons val_as_f64
val_truthy(v) → bool

// arr.c — dynamic array + hashmap
arr_push(a, val, arena)  arr_pop(a)  arr_last(a)  arr_clear(a)  arr_count(a)
HashMap {keys, vals, cap, count, mask, arena}
hashmap_create(arena, cap) → HashMap
hashmap_get(m, key:u32, out*) → bool        // key = StrId typically
hashmap_put(m, key:u32, val:Val)
hashmap_del(m, key:u32) → bool

// tap.c — observation + tracing
DISPATCH(kind, name, depth)                 // hot path: obs + ring buffer (~2ns tier 1)
OBS(name)                                   // obs only (no ring buffer entry)
rdtsc() → u64                              // cheapest timestamp
obs_reset()  obs_hit(name) → bool  obs_dump(max)  obs_collect(hits, cap) → u32
tap_on()  tap_off()  tap_reset()
trace_count() → u32  trace_at(ago) → TraceEv*
trace_dump(max)  trace_print(max)
tap_to_bitmask(kind, mask, mw)              // trace → bitmask view
tap_share() → int                           // shared memory for cross-process
tap_observe()                               // observer loop (noreturn)
tap_done()  tap_unshare()
g_obs_level                                 // 1=counts, 2=+timing

// cmd.c — commands + REPL + init
cmd_register(name, fn, help)
repl()                                      // interactive or piped
base_init()                                 // MUST call first: arenas, intern, print, color
base_cleanup()
read_key() → int                            // raw terminal key input
// Key constants: KEY_UP KEY_DOWN KEY_LEFT KEY_RIGHT KEY_ENTER KEY_BACKSPACE KEY_CTRL_*
```

### lang/ — The Language

```c
// proto.c — cons cells + polymorphic dispatch
Cons {car:Val, cdr:Val}
NIL = val_nil()
cons_new(a, d) → Val                        // alloc on g_req
car(v) → Val  cdr(v) → Val                 // v must be TAG_CONS
list_len(v) → u32
// Protocol dispatch (3 tiers):
//   t_first(v) t_rest(v) t_seq(v) t_count(v)           — tier 1: table ~2ns
//   p_first(v) p_rest(v) p_seq(v) p_count(v)           — tier 2: fast-path ~0.1ns
//   p_get(coll, key) p_assoc(coll, key, val)            — polymorphic
//   p_first_rest(v, *first, *rest) → bool               — tier 3: fused
extend(P_method, type_idx, fn)              // extend protocol at runtime
proto_init()                                // fill defaults + register impls
// Val contracts for protocols:
//   p_first(v:cons|pvec|nil) → Val
//   p_rest(v:cons|pvec|nil) → Val (cons→cons, pvec→cons list, nil→nil)
//   p_get(coll:pmap|pvec|nil, key:kw|sym|int) → Val|nil
//   p_assoc(coll:pmap|pvec, key:kw|sym|int, val:any) → pmap|pvec

// coll.c — persistent data structures + sig
CPMap {count, root, txn, is_small}          // persistent hash-array mapped trie
cpmap_empty() → CPMap
cpmap_put(m, key:u32, val:Val) → CPMap      // key = StrId; returns NEW map
cpmap_get(m, key:u32, out*) → bool
cpmap_count(m) → u32
cpmap_foreach(m, fn, ctx)                   // iterate: fn(key:u32, val:Val, ctx*)
cpmap_transient(m) → CPMap                  // mutable copy for batch writes
cpmap_put_t(m, key:u32, val:Val) → CPMap    // transient put (mutates in place)
cpmap_persistent(m) → CPMap                 // freeze back to immutable

CPVec {count, shift, root, tail, tail_len}  // persistent 32-way trie with tail
cpvec_empty() → CPVec
cpvec_get(v, i:u32) → Val                  // O(log32 n)
cpvec_append(v, val:Val) → CPVec            // returns NEW vec
cpvec_count(v) → u32

CAtom {version, value}
catom_new(val) → CAtom
catom_deref(a*) → Val
catom_reset(a*, val) → Val
catom_cas(a*, expected, new_val) → bool

pvec_to_list(v:Val) → Val                   // pvec → cons list (for param bindings)
coll_init()  coll_cleanup()  coll_reset()
coll_register_protos()                      // wire pmap/pvec into protocol tables

// sig — dispatch primitive
SigFn = Val (*)(Val data, void *ctx)
sig_on(name:StrId, fn:SigFn, flags:u8)     // register handler
sig_get(name:StrId) → SigFn|NULL            // lookup
sig_is_special(name:StrId) → bool           // SIG_SPECIAL flag
// Signal protocol (side-channel):
//   g_signal: SIGNAL_NONE=0, SIGNAL_RECUR=1, SIGNAL_ERROR=2
//   g_signal_val: error/recur payload
//   g_depth: call stack depth (max DEPTH_MAX=1024)
// After eval(), ALWAYS check g_signal. If set, propagate immediately.

// grammar.c — universal parser + analysis
Lang {name, cc[256], open[], close[], ...}  // language spec
lang_lisp(l*)  lang_c(l*)  lang_bf(l*)      // fill spec for each language
GNode {start, len, kind, depth, parent, child, next, end}  // 28 bytes
// NK_ROOT NK_LIST NK_VEC NK_MAP NK_QUOTE NK_IDENT NK_NUM NK_STR_NODE NK_OP NK_KW NK_OTHER
Gram {src, src_len, nodes, n, cap, mw, m[], v[], bind[], scope[], val[], analyzed}
gram_new(cap) → Gram                        // alloc on g_perm
gram_parse(g*, lang*, src, len)             // tokenize + structure in one pass
gram_index(g*)                              // build bitmask indexes
gram_analyze(g*)                            // scope + type + flow passes
gram_print(g*, id)                          // debug: print tree
gram_render(g*, lang*)                      // round-trip: tree → surface
gram_render_outline(g*)                     // indented outline
gram_render_buf(g*, lang*, outbuf*)         // render to buffer
gram_annotate(g*, hit_mask, counts)         // runtime-annotated source
gram_save(g*, path) → bool                  // serialize to file
gram_load(g*, path) → bool                  // deserialize from file
gram_read(source) → Val                     // parse single expr → Val (via entity bridge)
// Query API:
gn_child(g,i) gn_next(g,i) gn_parent(g,i) gn_kind(g,i) gn_end(g,i)
gn_text(g,i) → Str  gn_intern(g,i) → StrId
gn_count(g,i) → u32  gn_nth(g,i,n) → u32
view_is(g, vid, i) → bool                  // is node i in view?
view_has(g, vid, i) → bool                 // any descendant in view?
view_count(g, vid, i) → u32                // count descendants in view
// Semantic views: V_DEF V_REF V_CALL V_TAIL V_PURE V_CONST V_DEAD V_INT V_VEC V_MAP V_FN
// Bitmask ops: BM_SET BM_GET bm_pop bm_and bm_or bm_next bm_any_range bm_pop_range
// Well-known symbols: S_NIL S_TRUE S_FALSE S_DEF S_DEFN S_FN S_IF S_LET S_DO S_LOOP S_RECUR ...
// Classification: is_special(s) is_builtin(s) is_pure_bi(s) is_int_ret(s)
entity_to_val(g*, id) → Val                 // GNode → Val cons list (internal)
world_step(source, analyze) → Gram*         // parse into world, optionally analyze

// eval.c — tree-walking evaluator
Env {bindings:HashMap, parent:Env*}
env_create(arena*, parent) → Env*
env_set(env*, name:StrId, val:Val)
env_get(env*, name:StrId, out*) → bool      // walks parent chain
FnObj {type, union{builtin{fn,name}, closure{params,body,env}}}
// type: FN_BUILTIN=0, FN_CLOSURE=1, FN_MACRO=2
make_builtin(name:StrId, fn:BuiltinFn) → Val  // alloc on g_perm
make_closure(params:Val, body:Val, env*) → Val // alloc on g_req
eval_node(g*, id, env*) → Val              // GNode tree walk (primary evaluator)
eval(form:Val, env*) → Val                  // Val tree walk (closures, macros)
eval_string(s, env*) → Val                  // world_step + eval_node all forms
apply_fn(fn:Val, args:Val) → Val            // call function with arg list
pr_val(v:Val)                               // print to g_pr_buf (plain text)
print_val(v:Val) → char*                    // pr_val + return buffer
pp_val(v:Val)                               // pretty-print to g_print_buf (colored)
eval_init()                                 // create g_global_env, register special forms + builtins
// Special forms on sig: quote def defn fn if do let and or cond when loop recur defmacro
// Builtins on env: + - * / mod = < > <= >= not inc dec zero? pos? neg?
//   cons first rest list count nil? cons? int? fn? symbol?
//   println pr-str map filter reduce apply
//   get assoc conj nth vec hash-map keys vals contains? empty? into str
//   vector? map? keyword? string? macro? range
//   transient persistent! assoc! gensym macroexpand-1

// image.c — program database
ImageEntry {defined, code_offset, n_params, params, body, calls[32], callers[32], ...}
image_build()                               // scan g_defns → fill g_image + reverse index
image_get(name:StrId) → ImageEntry*|NULL
image_defined(name:StrId) → bool
image_arity(name:StrId) → u32
image_calls(name:StrId, n*) → StrId*        // what does this function call?
image_callers(name:StrId, n*) → StrId*      // who calls this function?
image_offset(name:StrId) → i32              // JIT code offset (-1 if none)

// jit.c — GNode → x86-64
jit_run(source:char*) → i64                 // parse + compile + execute
jit_run_gram(g*) → i64                      // compile + execute from Gram

// cc.c — GNode → C source
cc_emit(source:char*)                       // parse + emit C to g_out
compile_and_capture(source) → int           // emit + gcc + run, output in g_captured

// cli.c
cli_run(argc, argv) → int                   // dispatch: test|bench|eval|jit|emit|run|parse|render|trace|tap|obs|observe|save|load|watch
```

## Globals Inventory

| Global | Type | File | Purpose |
|--------|------|------|---------|
| `g_perm` | Arena | mem.c | Permanent allocations (never reset) |
| `g_req` | Arena | mem.c | Per-request allocations (reset between evals) |
| `g_temp` | Arena | mem.c | Scratch (reset inside gram_parse) |
| `g_intern` | InternTable | str.c | String interning table (64K capacity) |
| `g_print_buf` | OutBuf | fmt.c | Global output buffer (8KB, auto-flush via pf) |
| `g_color` | bool | fmt.c | TTY color detection |
| `g_coll` | struct | coll.c | Collection pool state (nodes, leaves, smalls, vnodes, vleaves) |
| `g_sig_table` | SigFn[256] | coll.c | sig dispatch table (StrId → handler) |
| `g_sig_flags` | u8[256] | coll.c | sig flags (SIG_SPECIAL) |
| `g_signal` | u8 | coll.c | Condition protocol: 0=none, 1=recur, 2=error |
| `g_signal_val` | Val | coll.c | Signal payload |
| `g_depth` | u32 | coll.c | Call stack depth (max 1024) |
| `g_obs_mask` | u64[1024] | tap.c | Which StrIds were dispatched (64K bits) |
| `g_obs_count` | u32[65536] | tap.c | Dispatch count per StrId |
| `g_obs_ns` | u64[65536] | tap.c | Self-time ticks per StrId (tier 2) |
| `g_obs_alloc` | u64[65536] | mem.c | Bytes allocated per StrId |
| `g_alloc_tag` | u32 | mem.c | Current dispatch name (set by DISPATCH) |
| `g_obs_total` | u32 | tap.c | Total dispatches |
| `g_obs_level` | u8 | tap.c | 1=counts, 2=+timing |
| `g_trace` | TraceEv* | tap.c | Ring buffer (4096 events, 16 bytes each) |
| `g_trace_pos` | u32 | tap.c | Ring buffer write position |
| `g_trace_total` | u32 | tap.c | Total events ever written |
| `g_image` | ImageEntry[4096] | image.c | Program database (call graph) |
| `g_global_env` | Env* | eval.c | Top-level environment (aliased from g_world_env) |
| `g_world_env` | void* | cmd.c | World env root (for undo/redo) |
| `g_world` | World | grammar.c | Versioned world (gram + version + epoch) |
| `g_code` | CodeBuf | jit.c | Executable code buffer (mmap'd) |
| `g_out` | OutBuf | cc.c | C emitter output buffer (1MB) |
| `g_cmds` | Cmd[64] | cmd.c | Command registry |
| `g_ed` | struct | cmd.c | Line editor state |
| `g_raw_mode` | bool | api.c | Terminal raw mode flag |
| `g_orig_term` | Termios | api.c | Saved terminal state |
| `g_gensym_counter` | u32 | eval.c | Gensym counter |

## Rosetta Stone

```c
// === Parse → Eval → Print ===

// 1. Parse source into world (one function — the step)
Gram *g = world_step("(+ 1 2)", false);

// 2. Eval — walks GNodes directly (no entity_to_val copy)
g_signal = SIGNAL_NONE; g_depth = 0;
Val result = eval_node(g, g->nodes[0].child, g_global_env);
if (g_signal) { g_signal = SIGNAL_NONE; /* handle error */ }

// 4. Print (plain text → g_pr_buf)
const char *text = print_val(result);  // "3"
pf("%s\n", text);

// 5. Pretty-print (colored → g_print_buf → stdout)
pp_val(result); pf("\n");

// === Shorthand: eval_string does all of the above ===
Val r = eval_string("(+ 1 2)", g_global_env);

// === Build persistent data ===
CPMap *m = arena_push(&g_req, CPMap);      // handle on g_req
*m = cpmap_empty();
*m = cpmap_put(*m, INTERN("x"), val_int(42));  // key = interned StrId
Val map_val = val_pmap(m);                 // wrap as Val

// Batch writes: transient for performance
CPMap *tm = arena_push(&g_req, CPMap);
*tm = cpmap_transient(*m);
for (int i = 0; i < 1000; i++)
    *tm = cpmap_put_t(*tm, some_key, some_val);  // mutates in place
*tm = cpmap_persistent(*tm);               // freeze

// === Register new builtin ===
static Val my_fn(Val args) {               // args = cons list of evaluated arguments
    Val x = car(args);                     // first arg
    if (!val_is_int(x)) return NIL;        // type check
    return val_int(val_as_int(x) * 2);     // return int
}
env_set(g_global_env, INTERN("double"), make_builtin(INTERN("double"), my_fn));

// === Register new special form ===
static Val my_sf(Val rest, void *ctx) {    // rest = unevaluated args, ctx = Env*
    Env *env = (Env *)ctx;
    Val first_arg = eval(car(rest), env);  // evaluate manually
    if (g_signal) return NIL;              // ALWAYS propagate signals
    return first_arg;
}
sig_on(INTERN("my-form"), my_sf, SIG_SPECIAL);

// === Signal protocol ===
// After ANY eval() call:
Val result = eval(form, env);
if (g_signal) {
    // SIGNAL_RECUR: only caught by sf_loop
    // SIGNAL_ERROR: propagate up, catch at top level
    g_signal = SIGNAL_NONE;  // clear at top level only
    return NIL;
}
```

## Naming Conventions

| Prefix | Domain | Examples |
|--------|--------|---------|
| `sys_` | OS/hardware | `sys_alloc`, `sys_write`, `sys_time_ns` |
| `arena_` | Memory allocator | `arena_alloc`, `arena_reset`, `arena_push` |
| `str_` / `sb_` | Strings | `str_intern`, `str_eq`, `sb_byte` |
| `buf_` | Output formatting | `buf_s`, `buf_u`, `buf_elapsed` |
| `val_` | NaN-boxed values | `val_int`, `val_is_sym`, `val_as_fn` |
| `hashmap_` | HashMap ops | `hashmap_get`, `hashmap_put` |
| `arr_` | DynArray ops | `arr_push`, `arr_count` |
| `pf` / `pfc` | Print shortcuts | `pf("hello\n")`, `pfc(C_RED)` |
| `p_` | Protocol tier 2 | `p_first`, `p_get`, `p_assoc` |
| `t_` | Protocol tier 1 | `t_first`, `t_rest` |
| `cpmap_` / `cpvec_` | Collections | `cpmap_put`, `cpvec_append` |
| `catom_` | Atom | `catom_deref`, `catom_reset` |
| `sig_` | Dispatch primitive | `sig_on`, `sig_get` |
| `gram_` / `gn_` | Grammar | `gram_parse`, `gn_text`, `gn_child` |
| `bm_` / `BM_` | Bitmask ops | `bm_pop`, `BM_SET`, `BM_GET` |
| `view_` | Semantic views | `view_is`, `view_has`, `view_count` |
| `env_` | Eval environment | `env_create`, `env_set`, `env_get` |
| `eval` / `apply_fn` | Evaluator | `eval`, `eval_body`, `apply_fn` |
| `pr_` / `pp_` | Printer | `pr_val` (plain), `pp_val` (colored) |
| `image_` | Program database | `image_get`, `image_calls` |
| `sf_` | Special forms | `sf_if`, `sf_let`, `sf_loop` |
| `bi_` | Builtins | `bi_add`, `bi_map`, `bi_reduce` |
| `cmd_` | REPL commands | `cmd_register`, `cmd_help` |
| `obs_` | Observation | `obs_reset`, `obs_dump`, `obs_hit` |
| `tap_` | Tracing | `tap_on`, `tap_off`, `tap_share` |
| `trace_` | Trace ring | `trace_dump`, `trace_at` |
| `perf_` | HW counters | `perf_init`, `perf_start` |
| `csi` / `cur_` | Terminal | `csi(a,b,code)`, `cur_up(n)` |
| `ed_` | Line editor | `ed_splice`, `ed_undo`, `ed_redo` |
| `S_` | Well-known StrIds | `S_DEF`, `S_IF`, `S_NIL` |
| `TK_` | Trace kind StrIds | `TK_CALL`, `TK_EVAL`, `TK_JIT` |
| `TI_` | Type tag indices | `TI_CONS`, `TI_PMAP`, `TI_F64` |
| `NK_` | Node kinds | `NK_LIST`, `NK_IDENT`, `NK_NUM` |
| `V_` | View IDs | `V_DEF`, `V_TAIL`, `V_PURE` |
| `C_` | ANSI color codes | `C_RED`, `C_BOLD`, `C_RESET` |
