<?php

require __DIR__ . '/src/php/clojure/bootstrap.php';

ns(\Clojure\Php\Sym::create('clojure.lang.protocols'), 'Core protocols - the foundation of Clojure\'s abstractions.
   These replace clojure.lang.I* Java interfaces.');
call_user_func('\Clojure\Php\defProtocol', 'ISeqable', array(\Clojure\Php\vec('-seq')));
function _seq($__L_o, $__L_args2918) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ISeqable', '-seq', $__L_o, $__L_args2918);
}
call_user_func('\Clojure\Php\defProtocol', 'ISeq', array(\Clojure\Php\vec('-first', '-rest')));
function _first($__L_coll, $__L_args2919) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ISeq', '-first', $__L_coll, $__L_args2919);
}
function _rest($__L_coll, $__L_args2920) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ISeq', '-rest', $__L_coll, $__L_args2920);
}
call_user_func('\Clojure\Php\defProtocol', 'INext', array(\Clojure\Php\vec('-next')));
function _next($__L_coll, $__L_args2921) {
return call_user_func('\Clojure\Php\protocolDispatch', 'INext', '-next', $__L_coll, $__L_args2921);
}
call_user_func('\Clojure\Php\defProtocol', 'ICounted', array(\Clojure\Php\vec('-count')));
function _count($__L_coll, $__L_args2922) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ICounted', '-count', $__L_coll, $__L_args2922);
}
call_user_func('\Clojure\Php\defProtocol', 'IIndexed', array(\Clojure\Php\vec('-nth')));
function _nth($__L_coll, $__L_args2923) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IIndexed', '-nth', $__L_coll, $__L_args2923);
}
call_user_func('\Clojure\Php\defProtocol', 'ILookup', array(\Clojure\Php\vec('-lookup')));
function _lookup($__L_o, $__L_args2924) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ILookup', '-lookup', $__L_o, $__L_args2924);
}
call_user_func('\Clojure\Php\defProtocol', 'IAssociative', array(\Clojure\Php\vec('-contains-key?', '-assoc')));
function _contains_key_QMARK_($__L_coll, $__L_args2925) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IAssociative', '-contains-key?', $__L_coll, $__L_args2925);
}
function _assoc($__L_coll, $__L_args2926) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IAssociative', '-assoc', $__L_coll, $__L_args2926);
}
call_user_func('\Clojure\Php\defProtocol', 'IMap', array(\Clojure\Php\vec('-dissoc')));
function _dissoc($__L_coll, $__L_args2927) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IMap', '-dissoc', $__L_coll, $__L_args2927);
}
call_user_func('\Clojure\Php\defProtocol', 'ISet', array(\Clojure\Php\vec('-disjoin')));
function _disjoin($__L_coll, $__L_args2928) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ISet', '-disjoin', $__L_coll, $__L_args2928);
}
call_user_func('\Clojure\Php\defProtocol', 'IStack', array(\Clojure\Php\vec('-peek', '-pop')));
function _peek($__L_coll, $__L_args2929) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IStack', '-peek', $__L_coll, $__L_args2929);
}
function _pop($__L_coll, $__L_args2930) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IStack', '-pop', $__L_coll, $__L_args2930);
}
call_user_func('\Clojure\Php\defProtocol', 'ICollection', array(\Clojure\Php\vec('-conj')));
function _conj($__L_coll, $__L_args2931) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ICollection', '-conj', $__L_coll, $__L_args2931);
}
call_user_func('\Clojure\Php\defProtocol', 'IEmptyableCollection', array(\Clojure\Php\vec('-empty')));
function _empty($__L_coll, $__L_args2932) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IEmptyableCollection', '-empty', $__L_coll, $__L_args2932);
}
call_user_func('\Clojure\Php\defProtocol', 'IEquiv', array(\Clojure\Php\vec('-equiv')));
function _equiv($__L_o, $__L_args2933) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IEquiv', '-equiv', $__L_o, $__L_args2933);
}
call_user_func('\Clojure\Php\defProtocol', 'IHash', array(\Clojure\Php\vec('-hash')));
function _hash($__L_o, $__L_args2934) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IHash', '-hash', $__L_o, $__L_args2934);
}
call_user_func('\Clojure\Php\defProtocol', 'IMeta', array(\Clojure\Php\vec('-meta')));
function _meta($__L_o, $__L_args2935) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IMeta', '-meta', $__L_o, $__L_args2935);
}
call_user_func('\Clojure\Php\defProtocol', 'IWithMeta', array(\Clojure\Php\vec('-with-meta')));
function _with_meta($__L_o, $__L_args2936) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IWithMeta', '-with-meta', $__L_o, $__L_args2936);
}
call_user_func('\Clojure\Php\defProtocol', 'IFn', array(\Clojure\Php\vec('-invoke')));
function _invoke($__L_this, $__L_args2937) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IFn', '-invoke', $__L_this, $__L_args2937);
}
call_user_func('\Clojure\Php\defProtocol', 'IReduce', array(\Clojure\Php\vec('-reduce')));
function _reduce($__L_coll, $__L_args2938) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IReduce', '-reduce', $__L_coll, $__L_args2938);
}
call_user_func('\Clojure\Php\defProtocol', 'IReduceInit', array(\Clojure\Php\vec('-reduce-init')));
function _reduce_init($__L_coll, $__L_args2939) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IReduceInit', '-reduce-init', $__L_coll, $__L_args2939);
}
call_user_func('\Clojure\Php\defProtocol', 'IKVReduce', array(\Clojure\Php\vec('-kv-reduce')));
function _kv_reduce($__L_coll, $__L_args2940) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IKVReduce', '-kv-reduce', $__L_coll, $__L_args2940);
}
call_user_func('\Clojure\Php\defProtocol', 'IEditableCollection', array(\Clojure\Php\vec('-as-transient')));
function _as_transient($__L_coll, $__L_args2941) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IEditableCollection', '-as-transient', $__L_coll, $__L_args2941);
}
call_user_func('\Clojure\Php\defProtocol', 'ITransientCollection', array(\Clojure\Php\vec('-conj!', '-persistent!')));
function _conj_BANG_($__L_tcoll, $__L_args2942) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientCollection', '-conj!', $__L_tcoll, $__L_args2942);
}
function _persistent_BANG_($__L_tcoll, $__L_args2943) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientCollection', '-persistent!', $__L_tcoll, $__L_args2943);
}
call_user_func('\Clojure\Php\defProtocol', 'ITransientAssociative', array(\Clojure\Php\vec('-assoc!')));
function _assoc_BANG_($__L_tcoll, $__L_args2944) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientAssociative', '-assoc!', $__L_tcoll, $__L_args2944);
}
call_user_func('\Clojure\Php\defProtocol', 'ITransientMap', array(\Clojure\Php\vec('-dissoc!')));
function _dissoc_BANG_($__L_tcoll, $__L_args2945) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientMap', '-dissoc!', $__L_tcoll, $__L_args2945);
}
call_user_func('\Clojure\Php\defProtocol', 'ITransientVector', array(\Clojure\Php\vec('-assoc-n!', '-pop!')));
function _assoc_n_BANG_($__L_tcoll, $__L_args2946) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientVector', '-assoc-n!', $__L_tcoll, $__L_args2946);
}
function _pop_BANG_($__L_tcoll, $__L_args2947) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientVector', '-pop!', $__L_tcoll, $__L_args2947);
}
call_user_func('\Clojure\Php\defProtocol', 'ITransientSet', array(\Clojure\Php\vec('-disjoin!')));
function _disjoin_BANG_($__L_tcoll, $__L_args2948) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientSet', '-disjoin!', $__L_tcoll, $__L_args2948);
}
call_user_func('\Clojure\Php\defProtocol', 'IDeref', array(\Clojure\Php\vec('-deref')));
function _deref($__L_o, $__L_args2949) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IDeref', '-deref', $__L_o, $__L_args2949);
}
call_user_func('\Clojure\Php\defProtocol', 'IDerefWithTimeout', array(\Clojure\Php\vec('-deref-with-timeout')));
function _deref_with_timeout($__L_o, $__L_args2950) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IDerefWithTimeout', '-deref-with-timeout', $__L_o, $__L_args2950);
}
call_user_func('\Clojure\Php\defProtocol', 'IReset', array(\Clojure\Php\vec('-reset!')));
function _reset_BANG_($__L_o, $__L_args2951) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IReset', '-reset!', $__L_o, $__L_args2951);
}
call_user_func('\Clojure\Php\defProtocol', 'ISwap', array(\Clojure\Php\vec('-swap!')));
function _swap_BANG_($__L_o, $__L_args2952) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ISwap', '-swap!', $__L_o, $__L_args2952);
}
call_user_func('\Clojure\Php\defProtocol', 'IAtom', array(\Clojure\Php\vec('-compare-and-set!')));
function _compare_and_set_BANG_($__L_o, $__L_args2953) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IAtom', '-compare-and-set!', $__L_o, $__L_args2953);
}
call_user_func('\Clojure\Php\defProtocol', 'IRef', array(\Clojure\Php\vec('-set!')));
function _set_BANG_($__L_o, $__L_args2954) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IRef', '-set!', $__L_o, $__L_args2954);
}
call_user_func('\Clojure\Php\defProtocol', 'IWatchable', array(\Clojure\Php\vec('-add-watch', '-remove-watch')));
function _add_watch($__L_o, $__L_args2955) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IWatchable', '-add-watch', $__L_o, $__L_args2955);
}
function _remove_watch($__L_o, $__L_args2956) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IWatchable', '-remove-watch', $__L_o, $__L_args2956);
}
call_user_func('\Clojure\Php\defProtocol', 'IValidatable', array(\Clojure\Php\vec('-set-validator!', '-get-validator')));
function _set_validator_BANG_($__L_o, $__L_args2957) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IValidatable', '-set-validator!', $__L_o, $__L_args2957);
}
function _get_validator($__L_o, $__L_args2958) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IValidatable', '-get-validator', $__L_o, $__L_args2958);
}
call_user_func('\Clojure\Php\defProtocol', 'IPending', array(\Clojure\Php\vec('-realized?')));
function _realized_QMARK_($__L_o, $__L_args2959) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IPending', '-realized?', $__L_o, $__L_args2959);
}
call_user_func('\Clojure\Php\defProtocol', 'INamed', array(\Clojure\Php\vec('-name', '-namespace')));
function _name($__L_o, $__L_args2960) {
return call_user_func('\Clojure\Php\protocolDispatch', 'INamed', '-name', $__L_o, $__L_args2960);
}
function _namespace($__L_o, $__L_args2961) {
return call_user_func('\Clojure\Php\protocolDispatch', 'INamed', '-namespace', $__L_o, $__L_args2961);
}
call_user_func('\Clojure\Php\defProtocol', 'IPrintWithWriter', array(\Clojure\Php\vec('-pr-writer')));
function _pr_writer($__L_o, $__L_args2962) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IPrintWithWriter', '-pr-writer', $__L_o, $__L_args2962);
}
call_user_func('\Clojure\Php\defProtocol', 'IComparable', array(\Clojure\Php\vec('-compare')));
function _compare($__L_x, $__L_args2963) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IComparable', '-compare', $__L_x, $__L_args2963);
}
call_user_func('\Clojure\Php\defProtocol', 'ISorted', array(\Clojure\Php\vec('-sorted-seq', '-sorted-seq-from', '-entry-key', '-comparator')));
function _sorted_seq($__L_coll, $__L_args2964) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ISorted', '-sorted-seq', $__L_coll, $__L_args2964);
}
function _sorted_seq_from($__L_coll, $__L_args2965) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ISorted', '-sorted-seq-from', $__L_coll, $__L_args2965);
}
function _entry_key($__L_coll, $__L_args2966) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ISorted', '-entry-key', $__L_coll, $__L_args2966);
}
function _comparator($__L_coll, $__L_args2967) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ISorted', '-comparator', $__L_coll, $__L_args2967);
}
call_user_func('\Clojure\Php\defProtocol', 'IReversible', array(\Clojure\Php\vec('-rseq')));
function _rseq($__L_coll, $__L_args2968) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IReversible', '-rseq', $__L_coll, $__L_args2968);
}
call_user_func('\Clojure\Php\defProtocol', 'IChunk', array(\Clojure\Php\vec('-drop-first')));
function _drop_first($__L_coll, $__L_args2969) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IChunk', '-drop-first', $__L_coll, $__L_args2969);
}
call_user_func('\Clojure\Php\defProtocol', 'IChunkedSeq', array(\Clojure\Php\vec('-chunked-first', '-chunked-rest')));
function _chunked_first($__L_coll, $__L_args2970) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IChunkedSeq', '-chunked-first', $__L_coll, $__L_args2970);
}
function _chunked_rest($__L_coll, $__L_args2971) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IChunkedSeq', '-chunked-rest', $__L_coll, $__L_args2971);
}
call_user_func('\Clojure\Php\defProtocol', 'IChunkedNext', array(\Clojure\Php\vec('-chunked-next')));
function _chunked_next($__L_coll, $__L_args2972) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IChunkedNext', '-chunked-next', $__L_coll, $__L_args2972);
}
call_user_func('\Clojure\Php\defProtocol', 'IIterable', array(\Clojure\Php\vec('-iterator')));
function _iterator($__L_coll, $__L_args2973) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IIterable', '-iterator', $__L_coll, $__L_args2973);
}
call_user_func('\Clojure\Php\defProtocol', 'ISequential', array(\Clojure\Php\vec()));
call_user_func('\Clojure\Php\defProtocol', 'IRecord', array(\Clojure\Php\vec()));
call_user_func('\Clojure\Php\defProtocol', 'IMapEntry', array(\Clojure\Php\vec('-key', '-val')));
function _key($__L_coll, $__L_args2974) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IMapEntry', '-key', $__L_coll, $__L_args2974);
}
function _val($__L_coll, $__L_args2975) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IMapEntry', '-val', $__L_coll, $__L_args2975);
}
call_user_func('\Clojure\Php\defProtocol', 'IVolatile', array(\Clojure\Php\vec('-vreset!')));
function _vreset_BANG_($__L_o, $__L_args2976) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IVolatile', '-vreset!', $__L_o, $__L_args2976);
}
ns(\Clojure\Php\Sym::create('clojure.lang.array'), 'Portable array operations.

   This namespace provides platform-independent array operations.
   Each platform (JVM, PHP, JS, etc.) implements the protocols
   defined here via clojure.{platform}.array namespaces.

   Maps to:
   - JVM: java.lang.reflect.Array, native arrays
   - PHP: array functions
   - JS: Array, TypedArrays');
call_user_func('\Clojure\Php\defProtocol', 'IArray', array(\Clojure\Php\vec('-aget', '-aset', '-alength', '-array-type')));
function _aget($__L_arr, $__L_args2977) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IArray', '-aget', $__L_arr, $__L_args2977);
}
function _aset($__L_arr, $__L_args2978) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IArray', '-aset', $__L_arr, $__L_args2978);
}
function _alength($__L_arr, $__L_args2979) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IArray', '-alength', $__L_arr, $__L_args2979);
}
function _array_type($__L_arr, $__L_args2980) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IArray', '-array-type', $__L_arr, $__L_args2980);
}
call_user_func('\Clojure\Php\defProtocol', 'IMutableBuffer', array(\Clojure\Php\vec('-buf-add', '-buf-empty?', '-buf-size', '-buf-to-array', '-buf-clear')));
function _buf_add($__L_buf, $__L_args2981) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IMutableBuffer', '-buf-add', $__L_buf, $__L_args2981);
}
function _buf_empty_QMARK_($__L_buf, $__L_args2982) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IMutableBuffer', '-buf-empty?', $__L_buf, $__L_args2982);
}
function _buf_size($__L_buf, $__L_args2983) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IMutableBuffer', '-buf-size', $__L_buf, $__L_args2983);
}
function _buf_to_array($__L_buf, $__L_args2984) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IMutableBuffer', '-buf-to-array', $__L_buf, $__L_args2984);
}
function _buf_clear($__L_buf, $__L_args2985) {
return call_user_func('\Clojure\Php\protocolDispatch', 'IMutableBuffer', '-buf-clear', $__L_buf, $__L_args2985);
}
call_user_func('\Clojure\Php\defProtocol', 'ArrayEngine', array(\Clojure\Php\vec('-make-array', '-make-array-2d', '-make-array-nd', '-int-array', '-long-array', '-float-array', '-double-array', '-byte-array', '-char-array', '-boolean-array', '-short-array', '-object-array', '-to-array', '-to-array-2d', '-array-seq', '-array-seq-offset', '-aclone', '-acopy', '-afill', '-afill-range', '-array?', '-array-class', '-component-type', '-asort', '-asort-comparator', '-shuffle', '-make-buffer', '-make-buffer-sized')));
function _make_array($__L_e, $__L_args2986) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-make-array', $__L_e, $__L_args2986);
}
function _make_array_2d($__L_e, $__L_args2987) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-make-array-2d', $__L_e, $__L_args2987);
}
function _make_array_nd($__L_e, $__L_args2988) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-make-array-nd', $__L_e, $__L_args2988);
}
function _int_array($__L_e, $__L_args2989) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-int-array', $__L_e, $__L_args2989);
}
function _long_array($__L_e, $__L_args2990) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-long-array', $__L_e, $__L_args2990);
}
function _float_array($__L_e, $__L_args2991) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-float-array', $__L_e, $__L_args2991);
}
function _double_array($__L_e, $__L_args2992) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-double-array', $__L_e, $__L_args2992);
}
function _byte_array($__L_e, $__L_args2993) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-byte-array', $__L_e, $__L_args2993);
}
function _char_array($__L_e, $__L_args2994) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-char-array', $__L_e, $__L_args2994);
}
function _boolean_array($__L_e, $__L_args2995) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-boolean-array', $__L_e, $__L_args2995);
}
function _short_array($__L_e, $__L_args2996) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-short-array', $__L_e, $__L_args2996);
}
function _object_array($__L_e, $__L_args2997) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-object-array', $__L_e, $__L_args2997);
}
function _to_array($__L_e, $__L_args2998) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-to-array', $__L_e, $__L_args2998);
}
function _to_array_2d($__L_e, $__L_args2999) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-to-array-2d', $__L_e, $__L_args2999);
}
function _array_seq($__L_e, $__L_args3000) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-array-seq', $__L_e, $__L_args3000);
}
function _array_seq_offset($__L_e, $__L_args3001) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-array-seq-offset', $__L_e, $__L_args3001);
}
function _aclone($__L_e, $__L_args3002) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-aclone', $__L_e, $__L_args3002);
}
function _acopy($__L_e, $__L_args3003) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-acopy', $__L_e, $__L_args3003);
}
function _afill($__L_e, $__L_args3004) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-afill', $__L_e, $__L_args3004);
}
function _afill_range($__L_e, $__L_args3005) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-afill-range', $__L_e, $__L_args3005);
}
function _array_QMARK_($__L_e, $__L_args3006) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-array?', $__L_e, $__L_args3006);
}
function _array_class($__L_e, $__L_args3007) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-array-class', $__L_e, $__L_args3007);
}
function _component_type($__L_e, $__L_args3008) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-component-type', $__L_e, $__L_args3008);
}
function _asort($__L_e, $__L_args3009) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-asort', $__L_e, $__L_args3009);
}
function _asort_comparator($__L_e, $__L_args3010) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-asort-comparator', $__L_e, $__L_args3010);
}
function _shuffle($__L_e, $__L_args3011) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-shuffle', $__L_e, $__L_args3011);
}
function _make_buffer($__L_e, $__L_args3012) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-make-buffer', $__L_e, $__L_args3012);
}
function _make_buffer_sized($__L_e, $__L_args3013) {
return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-make-buffer-sized', $__L_e, $__L_args3013);
}
$GLOBALS['_STAR_array_engine_STAR_'] = null;
$_STAR_array_engine_STAR_ = &$GLOBALS['_STAR_array_engine_STAR_'];
function make_array($__L_type, $__L_length) {
return _make_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_type, $__L_length);
}
function int_array($__L_size_or_seq) {
return _int_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_size_or_seq);
}
function long_array($__L_size_or_seq) {
return _long_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_size_or_seq);
}
function float_array($__L_size_or_seq) {
return _float_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_size_or_seq);
}
function double_array($__L_size_or_seq) {
return _double_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_size_or_seq);
}
function byte_array($__L_size_or_seq) {
return _byte_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_size_or_seq);
}
function char_array($__L_size_or_seq) {
return _char_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_size_or_seq);
}
function boolean_array($__L_size_or_seq) {
return _boolean_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_size_or_seq);
}
function short_array($__L_size_or_seq) {
return _short_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_size_or_seq);
}
function object_array($__L_size_or_seq) {
return _object_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_size_or_seq);
}
function aget($__L_arr, $__L_idx) {
return _aget($__L_arr, $__L_idx);
}
function aset($__L_arr, $__L_idx, $__L_val) {
_aset($__L_arr, $__L_idx, $__L_val);
return $__L_val;
}
function alength($__L_arr) {
return _alength($__L_arr);
}
function to_array($__L_coll) {
return _to_array($GLOBALS['_STAR_array_engine_STAR_'], $__L_coll);
}
function to_array_2d($__L_coll) {
return _to_array_2d($GLOBALS['_STAR_array_engine_STAR_'], $__L_coll);
}
function into_array($__L_coll) {
return into_array(null, $__L_coll);
}
function array_seq($__L_arr) {
return _array_seq($GLOBALS['_STAR_array_engine_STAR_'], $__L_arr);
}
function aclone($__L_arr) {
return _aclone($GLOBALS['_STAR_array_engine_STAR_'], $__L_arr);
}
function acopy($__L_src, $__L_src_pos, $__L_dest, $__L_dest_pos, $__L_length) {
return _acopy($GLOBALS['_STAR_array_engine_STAR_'], $__L_src, $__L_src_pos, $__L_dest, $__L_dest_pos, $__L_length);
}
function afill($__L_arr, $__L_val) {
return _afill($GLOBALS['_STAR_array_engine_STAR_'], $__L_arr, $__L_val);
}
function array_QMARK_($__L_x) {
return _array_QMARK_($GLOBALS['_STAR_array_engine_STAR_'], $__L_x);
}
function amap($__L_f, $__L_arr) {
$__L_len = alength($__L_arr);
$__L_ret = aclone($__L_arr);
$__L_i = 0;
 while(true) { if (($__L_i < $__L_len)) { aset($__L_ret, $__L_i, call_user_func($__L_f, aget($__L_arr, $__L_i)));
$__recur_0 = ($__L_i + 1); $__L_i = $__recur_0; continue;}
 break; }
return $__L_ret;
}
function areduce($__L_f, $__L_init, $__L_arr) {
$__L_len = alength($__L_arr);
$__L_i = 0;
$__L_acc = $__L_init;
 while(true) { if (($__L_i < $__L_len)) { $__recur_0 = ($__L_i + 1); $__recur_1 = call_user_func($__L_f, $__L_acc, aget($__L_arr, $__L_i)); $__L_i = $__recur_0; $__L_acc = $__recur_1; continue;} else { return $__L_acc;} break; }

}
function afilter($__L_pred, $__L_arr) {
return into_array(filter($__L_pred, array_seq($__L_arr)));
}
function asort($__L_arr) {
return _asort($GLOBALS['_STAR_array_engine_STAR_'], $__L_arr);
}
function shuffle($__L_coll) {
return _shuffle($GLOBALS['_STAR_array_engine_STAR_'], $__L_coll);
}
function make_buffer() {
return _make_buffer($GLOBALS['_STAR_array_engine_STAR_']);
}
function buf_add($__L_buf, $__L_val) {
return _buf_add($__L_buf, $__L_val);
}
function buf_empty_QMARK_($__L_buf) {
return _buf_empty_QMARK_($__L_buf);
}
function buf_size($__L_buf) {
return _buf_size($__L_buf);
}
function buf_to_array($__L_buf) {
return _buf_to_array($__L_buf);
}
function buf_clear($__L_buf) {
return _buf_clear($__L_buf);
}
ns(\Clojure\Php\Sym::create('clojure.php.array'), 'PHP implementation of array protocols.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.array'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('arr')]));
call_user_func('\Clojure\Php\defType', 'MutableBuffer', array('arr'), array('arr'), array('-buf-add', (function($__L_this, $__L_val) { aget($__L_this->arr, count($__L_this->arr), $__L_val);
return $__L_this;}), '-buf-empty?', (function($__L__) { return (0 === count($__L__->arr));}), '-buf-size', (function($__L__) { return count($__L__->arr);}), '-buf-to-array', (function($__L__) { return $__L__->arr;}), '-buf-clear', (function($__L_this) { $__L_this->arr = array();
return $__L_this;})));
function __GT_MutableBuffer($__L_arr) {
return call_user_func('\Clojure\Php\createType', 'MutableBuffer', $__L_arr);
}
$GLOBALS['engine'] = call_user_func('\Clojure\Php\reify', array('-make-array', (function($__L__, $__L_type, $__L_length) { return array_fill(0, $__L_length, null);}), '-make-array-2d', (function($__L__, $__L_type, $__L_dim1, $__L_dim2) { $__L_outer = array_fill(0, $__L_dim1, null);
dotimes(\Clojure\Php\vec($GLOBALS['i'], $__L_dim1), aset($__L_outer, $GLOBALS['i'], array_fill(0, $__L_dim2, null)));
return $__L_outer;}), '-make-array-nd', (function($__L__, $__L_type, $__L_dims) { if (\Clojure\Php\equals(1, \Clojure\Php\count_($__L_dims))) { return array_fill(0, \Clojure\Php\first($__L_dims), null);} else { $__L_outer = array_fill(0, \Clojure\Php\first($__L_dims), null);
dotimes(\Clojure\Php\vec($GLOBALS['i'], \Clojure\Php\first($__L_dims)), aset($__L_outer, $GLOBALS['i'], _make_array_nd($__L__, $__L_type, \Clojure\Php\rest($__L_dims))));
return $__L_outer;}}), '-int-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0);} else { return array_values(to_array($__L_size_or_seq));}}), '-long-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0);} else { return array_values(to_array($__L_size_or_seq));}}), '-float-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0.0);} else { return array_values(to_array($__L_size_or_seq));}}), '-double-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0.0);} else { return array_values(to_array($__L_size_or_seq));}}), '-byte-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0);} else { return array_values(to_array($__L_size_or_seq));}}), '-char-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, '');} else { return array_values(to_array($__L_size_or_seq));}}), '-boolean-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, false);} else { return array_values(to_array($__L_size_or_seq));}}), '-short-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0);} else { return array_values(to_array($__L_size_or_seq));}}), '-object-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, null);} else { return array_values(to_array($__L_size_or_seq));}}), '-to-array', (function($__L__, $__L_coll) {  while(true) { if (is_array($__L_coll)) { return array_values($__L_coll);} else { $__L_arr = array();
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ($__L_s) { aget($__L_arr, count($__L_arr), \Clojure\Php\first($__L_s));
$__recur_0 = \Clojure\Php\next_($__L_s); $__L_s = $__recur_0; continue;}
 break; }
return $__L_arr;} break; }}), '-to-array-2d', (function($__L__, $__L_coll) {  while(true) { $__L_arr = array();
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ($__L_s) { aget($__L_arr, count($__L_arr), _to_array($__L__, \Clojure\Php\first($__L_s)));
$__recur_0 = \Clojure\Php\next_($__L_s); $__L_s = $__recur_0; continue;}
 break; }
return $__L_arr; break; }}), '-array-seq', (function($__L__, $__L_arr) { if (($__L_arr ? (count($__L_arr) > 0) : false)) { return \Clojure\Php\seq(array_values($__L_arr));}}), '-array-seq-offset', (function($__L__, $__L_arr, $__L_offset) { if (($__L_arr ? ($__L_offset < count($__L_arr)) : false)) { return \Clojure\Php\seq(array_slice($__L_arr, $__L_offset));}}), '-aclone', (function($__L__, $__L_arr) { return array_values($__L_arr);}), '-acopy', (function($__L__, $__L_src, $__L_src_pos, $__L_dest, $__L_dest_pos, $__L_length) { return dotimes(\Clojure\Php\vec($GLOBALS['i'], $__L_length), aset($__L_dest, ($__L_dest_pos + $GLOBALS['i']), aget($__L_src, ($__L_src_pos + $GLOBALS['i']))));}), '-afill', (function($__L__, $__L_arr, $__L_val) { $__L_len = count($__L_arr);
return dotimes(\Clojure\Php\vec($GLOBALS['i'], $__L_len), aset($__L_arr, $GLOBALS['i'], $__L_val));}), '-afill-range', (function($__L__, $__L_arr, $__L_start, $__L_end, $__L_val) {  while(true) { $__L_i = $__L_start;
 while(true) { if (($__L_i < $__L_end)) { aset($__L_arr, $__L_i, $__L_val);
$__recur_0 = ($__L_i + 1); $__L_i = $__recur_0; continue;} break; }
 break; }}), '-array?', (function($__L__, $__L_x) { return is_array($__L_x);}), '-array-class', (function($__L__, $__L_type) { return null;}), '-component-type', (function($__L__, $__L_arr) { return null;}), '-asort', (function($__L__, $__L_arr) { sort($__L_arr);
return $__L_arr;}), '-asort-comparator', (function($__L__, $__L_arr, $__L_comparator) { usort($__L_arr, $__L_comparator);
return $__L_arr;}), '-shuffle', (function($__L__, $__L_coll) { $__L_arr = _to_array($__L__, $__L_coll);
shuffle($__L_arr);
return \Vec::create($__L_arr);}), '-make-buffer', (function($__L__) { return __GT_MutableBuffer(array());}), '-make-buffer-sized', (function($__L__, $__L_capacity) { return __GT_MutableBuffer(array());})));
$engine = &$GLOBALS['engine'];
function init_BANG_() {
return $GLOBALS['_STAR_array_engine_STAR_'] = $GLOBALS['engine'];
}
init_BANG_();
ns(\Clojure\Php\Sym::create('clojure.lang.kernel'), 'Kernel types - the minimal platform-provided types.

   These are the only types that need platform-specific implementations.
   Everything else is built in pure Clojure on top of these.

   Platform implementors provide:
   - Cons: a pair (head, tail)
   - Sym: a symbol (ns, name)
   - Kw: a keyword (ns, name), interned
   - Atom: a mutable reference (val)

   The compiler emits appropriate constructors/field access for each platform.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')]));
call_user_func('\Clojure\Php\defType', 'Cons', array('head', 'tail', '_meta'), array(), array('-first', (function($__L__) { return $__L__->head;}), '-rest', (function($__L__) { if (($__L__->tail === null)) { return \Clojure\Php\emptyList();} else { return $__L__->tail;}}), '-next', (function($__L__) { if (($__L__->tail === null)) { return null;} else { return _seq($__L__->tail);}}), '-seq', (function($__L_this) { return $__L_this;}), '-count', (function($__L_this) {  while(true) { $__L_s = $__L_this;
$__L_c = 0;
 while(true) { if (($__L_s === null)) { return $__L_c;} else { $__recur_0 = _next($__L_s); $__recur_1 = ($__L_c + 1); $__L_s = $__recur_0; $__L_c = $__recur_1; continue;} break; }
 break; }}), '-conj', (function($__L_this, $__L_o) { return __GT_Cons($__L_o, $__L_this, $__L_this->_meta);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-equiv', (function($__L_this, $__L_other) {  while(true) { if (satisfies_QMARK_($GLOBALS['ISequential'], $__L_other)) { $__L_s1 = $__L_this;
$__L_s2 = _seq($__L_other);
 while(true) { if (($__L_s1 === null)) { return ($__L_s2 === null);} else { if (($__L_s2 === null)) { return false;} else { if (\Clojure\Php\equals(_first($__L_s1), _first($__L_s2))) { $__recur_0 = _next($__L_s1); $__recur_1 = _next($__L_s2); $__L_s1 = $__recur_0; $__L_s2 = $__recur_1; continue;} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}} break; }
} else { return false;} break; }}), '-hash', (function($__L_this) {  while(true) { $__L_s = $__L_this;
$__L_h = 1;
 while(true) { if (($__L_s === null)) { return $__L_h;} else { $__recur_0 = _next($__L_s); $__recur_1 = ((31 * $__L_h) + hash(_first($__L_s))); $__L_s = $__recur_0; $__L_h = $__recur_1; continue;} break; }
 break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_Cons($__L__->head, $__L__->tail, $__L_m);})));
function __GT_Cons($__L_head, $__L_tail, $__L__meta) {
return call_user_func('\Clojure\Php\createType', 'Cons', $__L_head, $__L_tail, $__L__meta);
}
call_user_func('\Clojure\Php\defType', 'Sym', array('ns', 'name', '_meta', '_hash'), array(), array('-namespace', (function($__L__) { return $__L__->ns;}), '-name', (function($__L__) { return $__L__->name;}), '-equiv', (function($__L__, $__L_other) { if (instance_QMARK_($GLOBALS['Sym'], $__L_other)) { if (\Clojure\Php\equals($__L__->ns, $__L_other->ns)) { return \Clojure\Php\equals($__L__->name, $__L_other->name);} else { return false;}} else { return false;}}), '-hash', (function($__L__) { if (($__L__->_hash === null)) { $__L_h = (hash($__L__->name) + (31 * hash($__L__->ns)));
$__L__->_hash = $__L_h;
return $__L_h;} else { return $__L__->_hash;}}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_Sym($__L__->ns, $__L__->name, $__L_m, $__L__->_hash);}), '-invoke', (function($__L_this, $__L_coll) { return _lookup($__L_coll, $__L_this);}), '-invoke', (function($__L_this, $__L_coll, $__L_not_found) { return _lookup($__L_coll, $__L_this, $__L_not_found);})));
function __GT_Sym($__L_ns, $__L_name, $__L__meta, $__L__hash) {
return call_user_func('\Clojure\Php\createType', 'Sym', $__L_ns, $__L_name, $__L__meta, $__L__hash);
}
function symbol($__L_name) {
if (instance_QMARK_($GLOBALS['Sym'], $__L_name)) { return $__L_name;} else { $__L_idx = (is_string($__L_name) ? $__L_name->indexOf('/') : false);
if (($__L_idx ? ($__L_idx > 0) : false)) { return __GT_Sym(subs($__L_name, 0, $__L_idx), subs($__L_name, ($__L_idx + 1)), null, null);} else { return __GT_Sym(null, $__L_name, null, null);}}
}
$GLOBALS['keyword_cache'] = atom(\Clojure\Php\hashMap());
$keyword_cache = &$GLOBALS['keyword_cache'];
call_user_func('\Clojure\Php\defType', 'Kw', array('ns', 'name', '_hash'), array(), array('-namespace', (function($__L__) { return $__L__->ns;}), '-name', (function($__L__) { return $__L__->name;}), '-equiv', (function($__L_this, $__L_other) { return ($__L_this === $__L_other);}), '-hash', (function($__L__) { if (($__L__->_hash === null)) { $__L_h = (hash($__L__->name) + (31 * hash($__L__->ns)) + 2654435769);
$__L__->_hash = $__L_h;
return $__L_h;} else { return $__L__->_hash;}}), '-invoke', (function($__L_this, $__L_coll) { return _lookup($__L_coll, $__L_this);}), '-invoke', (function($__L_this, $__L_coll, $__L_not_found) { return _lookup($__L_coll, $__L_this, $__L_not_found);})));
function __GT_Kw($__L_ns, $__L_name, $__L__hash) {
return call_user_func('\Clojure\Php\createType', 'Kw', $__L_ns, $__L_name, $__L__hash);
}
function keyword($__L_name) {
if (instance_QMARK_($GLOBALS['Kw'], $__L_name)) { return $__L_name;} else { $__L_idx = (is_string($__L_name) ? $__L_name->indexOf('/') : false);
if (($__L_idx ? ($__L_idx > 0) : false)) { return keyword(subs($__L_name, 0, $__L_idx), subs($__L_name, ($__L_idx + 1)));} else { return keyword(null, $__L_name);}}
}
call_user_func('\Clojure\Php\defType', 'Atom', array('val', '_meta', 'validator', 'watches'), array('val', 'validator', 'watches'), array('-deref', (function($__L__) { return $__L__->val;}), '-reset!', (function($__L_this, $__L_new_value) {  while(true) { if ($__L_this->validator) { if (call_user_func($__L_this->validator, $__L_new_value)) { null;
} else { throw ex_info('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('value'), $__L_new_value));
}
}
$__L_old_value = $__L_this->val;
$__L_this->val = $__L_new_value;
if ($__L_this->watches) { $__L_s3015 = \Clojure\Php\seq($__L_this->watches);
 while(true) { if ($__L_s3015) { $__L___dest_1 = \Clojure\Php\first($__L_s3015);
$__L_k = \Clojure\Php\nth($__L___dest_1, 0);
$__L_f = \Clojure\Php\nth($__L___dest_1, 1);
call_user_func($__L_f, $__L_k, $__L_this, $__L_old_value, $__L_new_value);
$__recur_0 = \Clojure\Php\next_($__L_s3015); $__L_s3015 = $__recur_0; continue;}
 break; }
}
return $__L_new_value; break; }}), '-swap!', (function($__L_this, $__L_f) { return _reset_BANG_($__L_this, call_user_func($__L_f, $__L_this->val));}), '-swap!', (function($__L_this, $__L_f, $__L_a) { return _reset_BANG_($__L_this, call_user_func($__L_f, $__L_this->val, $__L_a));}), '-swap!', (function($__L_this, $__L_f, $__L_a, $__L_b) { return _reset_BANG_($__L_this, call_user_func($__L_f, $__L_this->val, $__L_a, $__L_b));}), '-swap!', (function($__L_this, $__L_f, $__L_a, $__L_b, $__L_xs) { return _reset_BANG_($__L_this, apply($__L_f, $__L_this->val, $__L_a, $__L_b, $__L_xs));}), '-compare-and-set!', (function($__L_this, $__L_oldval, $__L_newval) { if (\Clojure\Php\equals($__L_this->val, $__L_oldval)) { _reset_BANG_($__L_this, $__L_newval);
return true;} else { return false;}}), '-add-watch', (function($__L__, $__L_key, $__L_f) { return $__L__->watches = \Clojure\Php\assoc((call_user_func(function() { $__L_or__3016 = $__L__->watches; if ($__L_or__3016) { return $__L_or__3016;} else { return \Clojure\Php\hashMap();} })), $__L_key, $__L_f);}), '-remove-watch', (function($__L__, $__L_key) { return $__L__->watches = \Clojure\Php\dissoc($__L__->watches, $__L_key);}), '-set-validator!', (function($__L__, $__L_f) { if ($__L_f) { if (call_user_func($__L_f, $__L__->val)) { null;
} else { throw ex_info('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('value'), $__L__->val));
}
}
return $__L__->validator = $__L_f;}), '-get-validator', (function($__L__) { return $__L__->validator;}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_Atom($__L__->val, $__L_m, $__L__->validator, $__L__->watches);})));
function __GT_Atom($__L_val, $__L__meta, $__L_validator, $__L_watches) {
return call_user_func('\Clojure\Php\createType', 'Atom', $__L_val, $__L__meta, $__L_validator, $__L_watches);
}
function atom($__L_val) {
return __GT_Atom($__L_val, null, null, null);
}
call_user_func('\Clojure\Php\defType', 'EmptyList', array('_meta'), array(), array('-first', (function($__L__) { return null;}), '-rest', (function($__L__) { return \Clojure\Php\emptyList();}), '-next', (function($__L__) { return null;}), '-seq', (function($__L__) { return null;}), '-count', (function($__L__) { return 0;}), '-conj', (function($__L__, $__L_o) { return __GT_Cons($__L_o, null, $__L__->_meta);}), '-empty', (function($__L_this) { return $__L_this;}), '-equiv', (function($__L__, $__L_other) { if (satisfies_QMARK_($GLOBALS['ISequential'], $__L_other)) { return (_seq($__L_other) === null);} else { return false;}}), '-hash', (function($__L__) { return 1;}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_EmptyList($__L_m);})));
function __GT_EmptyList($__L__meta) {
return call_user_func('\Clojure\Php\createType', 'EmptyList', $__L__meta);
}
$GLOBALS['EMPTY_LIST'] = __GT_EmptyList(null);
$EMPTY_LIST = &$GLOBALS['EMPTY_LIST'];
ns(\Clojure\Php\Sym::create('clojure.lang.RT'), 'Runtime functions - portable implementation matching JVM clojure.lang.RT.

   Provides the same API as the JVM RT class so core.cljc works unchanged.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
function seq($__L_coll) {
if ($__L_coll) { return _seq($__L_coll);}
}
function first($__L_coll) {
if (($__L_coll === null)) { return null;} else { if (satisfies_QMARK_($GLOBALS['ISeq'], $__L_coll)) { return _first($__L_coll);} else { $__L_s = \Clojure\Php\seq($__L_coll);
if ($__L_s) { return _first($__L_s);}}}
}
function rest($__L_coll) {
if (($__L_coll === null)) { return \Clojure\Php\emptyList();} else { if (satisfies_QMARK_($GLOBALS['ISeq'], $__L_coll)) { return _rest($__L_coll);} else { $__L_s = \Clojure\Php\seq($__L_coll);
if ($__L_s) { return _rest($__L_s);} else { return \Clojure\Php\emptyList();}}}
}
function next($__L_coll) {
if (($__L_coll === null)) { return null;} else { if (satisfies_QMARK_($GLOBALS['INext'], $__L_coll)) { return _next($__L_coll);} else { return \Clojure\Php\seq(\Clojure\Php\rest($__L_coll));}}
}
function cons($__L_x, $__L_coll) {
return __GT_Cons($__L_x, \Clojure\Php\seq($__L_coll), null);
}
function conj() {
return \Clojure\Php\vec();
}
function list() {
return $GLOBALS['EMPTY_LIST'];
}
function count($__L_coll) {
if (($__L_coll === null)) { return 0;} else { if (satisfies_QMARK_($GLOBALS['ICounted'], $__L_coll)) { return _count($__L_coll);} else { $__L_s = \Clojure\Php\seq($__L_coll);
$__L_n = 0;
 while(true) { if ($__L_s) { $__recur_0 = \Clojure\Php\next_($__L_s); $__recur_1 = ($__L_n + 1); $__L_s = $__recur_0; $__L_n = $__recur_1; continue;} else { return $__L_n;} break; }
}}
}
function empty_QMARK_($__L_coll) {
return (!\Clojure\Php\seq($__L_coll));
}
function nth($__L_coll, $__L_n) {
if (($__L_coll === null)) { throw ex_info('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n));
} else { if (satisfies_QMARK_($GLOBALS['IIndexed'], $__L_coll)) { return _nth($__L_coll, $__L_n);} else { $__L_s = \Clojure\Php\seq($__L_coll);
$__L_i = $__L_n;
 while(true) { if ($__L_s) { if (($__L_i === 0 || $__L_i === 0.0)) { return \Clojure\Php\first($__L_s);} else { $__recur_0 = \Clojure\Php\next_($__L_s); $__recur_1 = ($__L_i - 1); $__L_s = $__recur_0; $__L_i = $__recur_1; continue;}} else { throw ex_info('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n));
} break; }
}}
}
function get($__L_m, $__L_k) {
if ($__L_m) { if (satisfies_QMARK_($GLOBALS['ILookup'], $__L_m)) { return _lookup($__L_m, $__L_k);}}
}
function contains_QMARK_($__L_coll, $__L_k) {
if (($__L_coll === null)) { return false;} else { if (satisfies_QMARK_($GLOBALS['IAssociative'], $__L_coll)) { return _contains_key_QMARK_($__L_coll, $__L_k);} else { return (!(\Clojure\Php\get_($__L_coll, $__L_k, \Clojure\Php\Kw::createNs('user', 'not-found')) === \Clojure\Php\Kw::createNs('user', 'not-found')));}}
}
function assoc($__L_m, $__L_k, $__L_v) {
if (($__L_m === null)) { return \Clojure\Php\hashMap($__L_k, $__L_v);} else { return _assoc($__L_m, $__L_k, $__L_v);}
}
function dissoc($__L_m) {
return $__L_m;
}
function peek($__L_coll) {
if ($__L_coll) { return _peek($__L_coll);}
}
function pop($__L_coll) {
if ($__L_coll) { return _pop($__L_coll);}
}
function deref($__L_ref) {
return _deref($__L_ref);
}
function reset_BANG_($__L_atom, $__L_new_value) {
return _reset_BANG_($__L_atom, $__L_new_value);
}
function swap_BANG_($__L_atom, $__L_f) {
return _swap_BANG_($__L_atom, $__L_f);
}
function compare_and_set_BANG_($__L_atom, $__L_old, $__L_new) {
return _compare_and_set_BANG_($__L_atom, $__L_old, $__L_new);
}
function meta($__L_obj) {
if (satisfies_QMARK_($GLOBALS['IMeta'], $__L_obj)) { return _meta($__L_obj);}
}
function with_meta($__L_obj, $__L_m) {
return _with_meta($__L_obj, $__L_m);
}
function vary_meta($__L_obj, $__L_f, $__L_args) {
return with_meta($__L_obj, apply($__L_f, meta($__L_obj), $__L_args));
}
function name($__L_x) {
if (is_string($__L_x)) { return $__L_x;} else { return _name($__L_x);}
}
function namespace($__L_x) {
return _namespace($__L_x);
}
function _EQ_($__L_x) {
return true;
}
function hash($__L_x) {
if (($__L_x === null)) { return 0;} else { if (satisfies_QMARK_($GLOBALS['IHash'], $__L_x)) { return _hash($__L_x);} else { throw ex_info('No hash implementation', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('value'), $__L_x));
}}
}
function reduce($__L_f, $__L_coll) {
if (satisfies_QMARK_($GLOBALS['IReduce'], $__L_coll)) { return _reduce($__L_coll, $__L_f);} else { $__L_s = \Clojure\Php\seq($__L_coll);
if ($__L_s) { return reduce($__L_f, \Clojure\Php\first($__L_s), \Clojure\Php\next_($__L_s));} else { return call_user_func($__L_f);}}
}
function reduce_kv($__L_f, $__L_init, $__L_coll) {
if (satisfies_QMARK_($GLOBALS['IKVReduce'], $__L_coll)) { return _kv_reduce($__L_coll, $__L_f, $__L_init);} else { return reduce((function($__L_acc, $__L___dest_4) use (&$__L_init, &$__L_coll, &$__L_f) { $__L___dest_5 = $__L___dest_4;
$__L_k = \Clojure\Php\nth($__L___dest_5, 0);
$__L_v = \Clojure\Php\nth($__L___dest_5, 1);
return call_user_func($__L_f, $__L_acc, $__L_k, $__L_v);}), $__L_init, $__L_coll);}
}
call_user_func('\Clojure\Php\defType', 'Reduced', array('val'), array(), array('-deref', (function($__L__) { return $__L__->val;})));
function __GT_Reduced($__L_val) {
return call_user_func('\Clojure\Php\createType', 'Reduced', $__L_val);
}
function reduced($__L_x) {
return __GT_Reduced($__L_x);
}
function reduced_QMARK_($__L_x) {
return instance_QMARK_($GLOBALS['Reduced'], $__L_x);
}
function unreduced($__L_x) {
if (reduced_QMARK_($__L_x)) { return deref($__L_x);} else { return $__L_x;}
}
function ensure_reduced($__L_x) {
if (reduced_QMARK_($__L_x)) { return $__L_x;} else { return reduced($__L_x);}
}
function second($__L_coll) {
return \Clojure\Php\first(\Clojure\Php\next_($__L_coll));
}
function ffirst($__L_coll) {
return \Clojure\Php\first(\Clojure\Php\first($__L_coll));
}
function nfirst($__L_coll) {
return \Clojure\Php\next_(\Clojure\Php\first($__L_coll));
}
function fnext($__L_coll) {
return \Clojure\Php\first(\Clojure\Php\next_($__L_coll));
}
function nnext($__L_coll) {
return \Clojure\Php\next_(\Clojure\Php\next_($__L_coll));
}
function last($__L_coll) {
$__L_s = \Clojure\Php\next_($__L_coll);
if ($__L_s) { $__recur_0 = $__L_s; $__L_coll = $__recur_0; continue;} else { return \Clojure\Php\first($__L_coll);}
}
function butlast($__L_coll) {
$__L_ret = \Clojure\Php\vec();
$__L_s = $__L_coll;
 while(true) { if (\Clojure\Php\next_($__L_s)) { $__recur_0 = \Clojure\Php\conj($__L_ret, \Clojure\Php\first($__L_s)); $__recur_1 = \Clojure\Php\next_($__L_s); $__L_ret = $__recur_0; $__L_s = $__recur_1; continue;} else { return \Clojure\Php\seq($__L_ret);} break; }

}
function seq_QMARK_($__L_x) {
return satisfies_QMARK_($GLOBALS['ISeq'], $__L_x);
}
function seqable_QMARK_($__L_x) {
return satisfies_QMARK_($GLOBALS['ISeqable'], $__L_x);
}
function sequential_QMARK_($__L_x) {
return satisfies_QMARK_($GLOBALS['ISequential'], $__L_x);
}
function counted_QMARK_($__L_coll) {
return satisfies_QMARK_($GLOBALS['ICounted'], $__L_coll);
}
function indexed_QMARK_($__L_coll) {
return satisfies_QMARK_($GLOBALS['IIndexed'], $__L_coll);
}
function reversible_QMARK_($__L_coll) {
return satisfies_QMARK_($GLOBALS['IReversible'], $__L_coll);
}
function sorted_QMARK_($__L_coll) {
return satisfies_QMARK_($GLOBALS['ISorted'], $__L_coll);
}
function associative_QMARK_($__L_coll) {
return satisfies_QMARK_($GLOBALS['IAssociative'], $__L_coll);
}
ns(\Clojure\Php\Sym::create('clojure.lang.numbers'), 'Arithmetic operations using platform primitives.

   The compiler emits appropriate operations for each platform:
   - JVM: use primitives or clojure.lang.Numbers
   - PHP: use native PHP operators
   - JS: use native JS operators
   - Rust: use native Rust operators

   These functions define the semantics. The emitter handles optimization.');
function _PLUS_() {
return 0;
}
function _($__L_x) {
return (-$__L_x);
}
function _STAR_() {
return 1;
}
function _SLASH_($__L_x) {
return (1 / $__L_x);
}
function inc($__L_x) {
return ($__L_x + 1);
}
function dec($__L_x) {
return ($__L_x - 1);
}
function _LT_($__L__) {
return true;
}
function _GT_($__L__) {
return true;
}
function _LT__EQ_($__L__) {
return true;
}
function _GT__EQ_($__L__) {
return true;
}
function _EQ__EQ_($__L__) {
return true;
}
function zero_QMARK_($__L_x) {
return _EQ__EQ_($__L_x, 0);
}
function pos_QMARK_($__L_x) {
return ($__L_x > 0);
}
function neg_QMARK_($__L_x) {
return ($__L_x < 0);
}
function even_QMARK_($__L_n) {
return (($__L_n % 2) === 0 || ($__L_n % 2) === 0.0);
}
function odd_QMARK_($__L_n) {
return (!(($__L_n & 1) === 0));
}
function max($__L_x) {
return $__L_x;
}
function min($__L_x) {
return $__L_x;
}
function quot($__L_num, $__L_div) {
return quot($__L_num, $__L_div);
}
function rem($__L_num, $__L_div) {
return ($__L_num % $__L_div);
}
function mod($__L_num, $__L_div) {
return ($__L_num % $__L_div);
}
function bit_and($__L_x, $__L_y) {
return ($__L_x & $__L_y);
}
function bit_or($__L_x, $__L_y) {
return ($__L_x | $__L_y);
}
function bit_xor($__L_x, $__L_y) {
return ($__L_x ^ $__L_y);
}
function bit_not($__L_x) {
return bit_not($__L_x);
}
function bit_shift_left($__L_x, $__L_n) {
return ($__L_x << $__L_n);
}
function bit_shift_right($__L_x, $__L_n) {
return ($__L_x >> $__L_n);
}
function unsigned_bit_shift_right($__L_x, $__L_n) {
return unsigned_bit_shift_right($__L_x, $__L_n);
}
function bit_and_not($__L_x, $__L_y) {
return ($__L_x & bit_not($__L_y));
}
function bit_clear($__L_x, $__L_n) {
return ($__L_x & bit_not((1 << $__L_n)));
}
function bit_set($__L_x, $__L_n) {
return ($__L_x | (1 << $__L_n));
}
function bit_flip($__L_x, $__L_n) {
return ($__L_x ^ (1 << $__L_n));
}
function bit_test($__L_x, $__L_n) {
return (!(($__L_x & (1 << $__L_n)) === 0 || ($__L_x & (1 << $__L_n)) === 0.0));
}
function abs($__L_n) {
if (($__L_n < 0)) { return (-$__L_n);} else { return $__L_n;}
}
ns(\Clojure\Php\Sym::create('clojure.lang.seqs'), 'Sequence operations - the heart of Clojure.

   These functions operate lazily over any seqable collection,
   providing the functional programming core of Clojure.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')], [\Clojure\Php\Sym::create('clojure.lang.lazy'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('lazy')]));
function map($__L_f) {
return (function($__L_rf) use (&$__L_f) { return (function(...$__args) use (&$__L_rf, &$__L_f) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; return call_user_func($__L_rf, $__L_result, call_user_func($__L_f, $__L_input)); } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function mapcat($__L_f) {
return comp(map($__L_f), $GLOBALS['cat']);
}
function filter($__L_pred) {
return (function($__L_rf) use (&$__L_pred) { return (function(...$__args) use (&$__L_rf, &$__L_pred) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; if (call_user_func($__L_pred, $__L_input)) { return call_user_func($__L_rf, $__L_result, $__L_input);} else { return $__L_result;} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function remove($__L_pred) {
return filter(complement($__L_pred));
}
function keep($__L_f) {
return (function($__L_rf) use (&$__L_f) { return (function(...$__args) use (&$__L_rf, &$__L_f) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_v = call_user_func($__L_f, $__L_input);
if (($__L_v === null)) { return $__L_result;} else { return call_user_func($__L_rf, $__L_result, $__L_v);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function keep_indexed($__L_f) {
return (function($__L_rf) use (&$__L_f) { $__L_i = volatile_BANG_(-1);
return (function(...$__args) use (&$__L_i, &$__L_rf, &$__L_f) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_v = call_user_func($__L_f, vswap_BANG_($__L_i, $GLOBALS['inc']), $__L_input);
if (($__L_v === null)) { return $__L_result;} else { return call_user_func($__L_rf, $__L_result, $__L_v);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function take($__L_n) {
return (function($__L_rf) use (&$__L_n) { $__L_nv = volatile_BANG_($__L_n);
return (function(...$__args) use (&$__L_rf, &$__L_n, &$__L_nv) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_n = deref($__L_nv);
$__L_nn = vswap_BANG_($__L_nv, $GLOBALS['dec']);
$__L_result = (($__L_n > 0) ? call_user_func($__L_rf, $__L_result, $__L_input) : $__L_result);
if ((!($__L_nn > 0))) { return ensure_reduced($__L_result);} else { return $__L_result;} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function take_while($__L_pred) {
return (function($__L_rf) use (&$__L_pred) { return (function(...$__args) use (&$__L_rf, &$__L_pred) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; if (call_user_func($__L_pred, $__L_input)) { return call_user_func($__L_rf, $__L_result, $__L_input);} else { return reduced($__L_result);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function take_nth($__L_n) {
return (function($__L_rf) use (&$__L_n) { $__L_i = volatile_BANG_(-1);
return (function(...$__args) use (&$__L_i, &$__L_rf, &$__L_n) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_idx = vswap_BANG_($__L_i, $GLOBALS['inc']);
if ((($__L_idx % $__L_n) === 0 || ($__L_idx % $__L_n) === 0.0)) { return call_user_func($__L_rf, $__L_result, $__L_input);} else { return $__L_result;} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function drop($__L_n) {
return (function($__L_rf) use (&$__L_n) { $__L_nv = volatile_BANG_($__L_n);
return (function(...$__args) use (&$__L_rf, &$__L_n, &$__L_nv) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_n = deref($__L_nv);
vswap_BANG_($__L_nv, $GLOBALS['dec']);
if (($__L_n > 0)) { return $__L_result;} else { return call_user_func($__L_rf, $__L_result, $__L_input);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function drop_while($__L_pred) {
return (function($__L_rf) use (&$__L_pred) { $__L_dv = volatile_BANG_(true);
return (function(...$__args) use (&$__L_rf, &$__L_pred, &$__L_dv) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_drop_QMARK_ = deref($__L_dv);
if (($__L_drop_QMARK_ ? call_user_func($__L_pred, $__L_input) : false)) { return $__L_result;} else { vreset_BANG_($__L_dv, null);
return call_user_func($__L_rf, $__L_result, $__L_input);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function drop_last($__L_coll) {
return drop_last(1, $__L_coll);
}
function take_last($__L_n, $__L_coll) {
$__L_s = \Clojure\Php\seq($__L_coll);
$__L_lead = \Clojure\Php\seq(drop($__L_n, $__L_coll));
 while(true) { if ($__L_lead) { $__recur_0 = \Clojure\Php\next_($__L_s); $__recur_1 = \Clojure\Php\next_($__L_lead); $__L_s = $__recur_0; $__L_lead = $__recur_1; continue;} else { return $__L_s;} break; }

}
function split_at($__L_n, $__L_coll) {
return \Clojure\Php\vec(take($__L_n, $__L_coll), drop($__L_n, $__L_coll));
}
function split_with($__L_pred, $__L_coll) {
return \Clojure\Php\vec(take_while($__L_pred, $__L_coll), drop_while($__L_pred, $__L_coll));
}
function partition($__L_n, $__L_coll) {
return partition($__L_n, $__L_n, $__L_coll);
}
function partition_all($__L_n) {
return (function($__L_rf) use (&$__L_n) { $__L_a = (new java.util.ArrayList($__L_n));
return (function(...$__args) use (&$__L_a, &$__L_rf, &$__L_n) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_a->add($__L_input);
if (\Clojure\Php\equals($__L_n, $__L_a->size())) { $__L_v = \Vec::create($__L_a->toArray());
$__L_a->clear();
return call_user_func($__L_rf, $__L_result, $__L_v);} else { return $__L_result;} } else if ($__n == 1) { $__L_result = $__args[0]; $__L_result = ($__L_a->isEmpty() ? $__L_result : (call_user_func(function() { $__L_v = \Vec::create($__L_a->toArray()); $__L_a->clear();
return unreduced(call_user_func($__L_rf, $__L_result, $__L_v)); })));
return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function partition_by($__L_f) {
return (function($__L_rf) use (&$__L_f) { $__L_a = (new java.util.ArrayList());
$__L_pv = volatile_BANG_(\Clojure\Php\Kw::createNs('user', 'none'));
return (function(...$__args) use (&$__L_a, &$__L_rf, &$__L_pv, &$__L_f) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_pval = deref($__L_pv);
$__L_val = call_user_func($__L_f, $__L_input);
vreset_BANG_($__L_pv, $__L_val);
if ((call_user_func(function() { $__L_or__3031 = ($__L_pval === \Clojure\Php\Kw::createNs('user', 'none')); if ($__L_or__3031) { return $__L_or__3031;} else { return \Clojure\Php\equals($__L_val, $__L_pval);} }))) { $__L_a->add($__L_input);
return $__L_result;} else { $__L_v = \Vec::create($__L_a->toArray());
$__L_a->clear();
$__L_ret = call_user_func($__L_rf, $__L_result, $__L_v);
if (reduced_QMARK_($__L_ret)) { null;
} else { $__L_a->add($__L_input);
}
return $__L_ret;} } else if ($__n == 1) { $__L_result = $__args[0]; $__L_result = ($__L_a->isEmpty() ? $__L_result : (call_user_func(function() { $__L_v = \Vec::create($__L_a->toArray()); $__L_a->clear();
return unreduced(call_user_func($__L_rf, $__L_result, $__L_v)); })));
return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function concat() {
return null;
}
$GLOBALS['cat'] = 'A transducer that concatenates the contents of each input.';
$cat = &$GLOBALS['cat'];
function interleave() {
return \Clojure\Php\emptyList();
}
function interpose($__L_sep) {
return (function($__L_rf) use (&$__L_sep) { $__L_started = volatile_BANG_(false);
return (function(...$__args) use (&$__L_started, &$__L_sep, &$__L_rf) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; if (deref($__L_started)) { $__L_sepr = call_user_func($__L_rf, $__L_result, $__L_sep);
if (reduced_QMARK_($__L_sepr)) { return $__L_sepr;} else { return call_user_func($__L_rf, $__L_sepr, $__L_input);}} else { vreset_BANG_($__L_started, true);
return call_user_func($__L_rf, $__L_result, $__L_input);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function map_indexed($__L_f) {
return (function($__L_rf) use (&$__L_f) { $__L_i = volatile_BANG_(-1);
return (function(...$__args) use (&$__L_i, &$__L_rf, &$__L_f) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; return call_user_func($__L_rf, $__L_result, call_user_func($__L_f, vswap_BANG_($__L_i, $GLOBALS['inc']), $__L_input)); } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function zipmap($__L_keys, $__L_vals) {
$__L_map = \Clojure\Php\hashMap();
$__L_ks = \Clojure\Php\seq($__L_keys);
$__L_vs = \Clojure\Php\seq($__L_vals);
 while(true) { if (($__L_ks ? $__L_vs : false)) { $__recur_0 = \Clojure\Php\assoc($__L_map, \Clojure\Php\first($__L_ks), \Clojure\Php\first($__L_vs)); $__recur_1 = \Clojure\Php\next_($__L_ks); $__recur_2 = \Clojure\Php\next_($__L_vs); $__L_map = $__recur_0; $__L_ks = $__recur_1; $__L_vs = $__recur_2; continue;} else { return $__L_map;} break; }

}
function flatten($__L_x) {
return filter(complement($GLOBALS['sequential_QMARK_']), \Clojure\Php\rest(tree_seq($GLOBALS['sequential_QMARK_'], $GLOBALS['seq'], $__L_x)));
}
function tree_seq($__L_branch_QMARK_, $__L_children, $__L_root) {
$__L_walk = (function($__L_node) use (&$__L_branch_QMARK_, &$__L_children, &$__L_root) { return lazy::lazy-seq*((function() use (&$__L_branch_QMARK_, &$__L_children, &$__L_node, &$__L_root) { return __GT_Cons($__L_node, (call_user_func($__L_branch_QMARK_, $__L_node) ? mapcat($GLOBALS['walk'], call_user_func($__L_children, $__L_node)) : null), null);}));});
return call_user_func($__L_walk, $__L_root);
}
function distinct() {
return (function($__L_rf) { $__L_seen = volatile_BANG_(\Clojure\Php\hashSet());
return (function(...$__args) use (&$__L_rf, &$__L_seen) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; if (\Clojure\Php\contains(deref($__L_seen), $__L_input)) { return $__L_result;} else { vswap_BANG_($__L_seen, $GLOBALS['conj'], $__L_input);
return call_user_func($__L_rf, $__L_result, $__L_input);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function dedupe() {
return (function($__L_rf) { $__L_pv = volatile_BANG_(\Clojure\Php\Kw::createNs('user', 'none'));
return (function(...$__args) use (&$__L_rf, &$__L_pv) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_prior = deref($__L_pv);
vreset_BANG_($__L_pv, $__L_input);
if (\Clojure\Php\equals($__L_prior, $__L_input)) { return $__L_result;} else { return call_user_func($__L_rf, $__L_result, $__L_input);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });});
}
function group_by($__L_f, $__L_coll) {
return persistent_BANG_(reduce((function($__L_ret, $__L_x) use (&$__L_coll, &$__L_f) { $__L_k = call_user_func($__L_f, $__L_x);
return assoc_BANG_($__L_ret, $__L_k, \Clojure\Php\conj(\Clojure\Php\get_($__L_ret, $__L_k, \Clojure\Php\vec()), $__L_x));}), transient(\Clojure\Php\hashMap()), $__L_coll));
}
function frequencies($__L_coll) {
return persistent_BANG_(reduce((function($__L_counts, $__L_x) use (&$__L_coll) { return assoc_BANG_($__L_counts, $__L_x, (\Clojure\Php\get_($__L_counts, $__L_x, 0) + 1));}), transient(\Clojure\Php\hashMap()), $__L_coll));
}
function some($__L_pred, $__L_coll) {
$__L_when_let__3036 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3036) { $__L_s = $__L_when_let__3036;
$__L_or__3037 = call_user_func($__L_pred, \Clojure\Php\first($__L_s));
if ($__L_or__3037) { return $__L_or__3037;} else { $__recur_0 = $__L_pred; $__recur_1 = \Clojure\Php\next_($__L_s); $__L_pred = $__recur_0; $__L_coll = $__recur_1; continue;}}
}
function every_QMARK_($__L_pred, $__L_coll) {
if ((\Clojure\Php\seq($__L_coll) === null)) { return true;} else { if (call_user_func($__L_pred, \Clojure\Php\first($__L_coll))) { $__recur_0 = $__L_pred; $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_pred = $__recur_0; $__L_coll = $__recur_1; continue;} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}
}
function not_every_QMARK_($__L_pred, $__L_coll) {
return (!every_QMARK_($__L_pred, $__L_coll));
}
function not_any_QMARK_($__L_pred, $__L_coll) {
return (!some($__L_pred, $__L_coll));
}
function sort($__L_coll) {
return sort($GLOBALS['compare'], $__L_coll);
}
function sort_by($__L_keyfn, $__L_coll) {
return sort_by($__L_keyfn, $GLOBALS['compare'], $__L_coll);
}
function reverse($__L_coll) {
return reduce($GLOBALS['conj'], \Clojure\Php\emptyList(), $__L_coll);
}
function shuffle($__L_coll) {
$__L_al = (new java.util.ArrayList(to_array($__L_coll)));
java.util.Collections::shuffle($__L_al);
return \Vec::create($__L_al->toArray());
}
function rand_nth($__L_coll) {
return \Clojure\Php\nth($__L_coll, rand_int(\Clojure\Php\count_($__L_coll)));
}
function dorun($__L_coll) {
$__L_when_let__3039 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3039) { $__L_s = $__L_when_let__3039;
$__recur_0 = \Clojure\Php\next_($__L_s); $__L_coll = $__recur_0; continue;}
}
function doall($__L_coll) {
dorun($__L_coll);
return $__L_coll;
}
function into() {
return \Clojure\Php\vec();
}
function empty($__L_coll) {
if (satisfies_QMARK_($GLOBALS['IEmptyableCollection'], $__L_coll)) { return _empty($__L_coll);}
}
function not_empty($__L_coll) {
if (\Clojure\Php\seq($__L_coll)) { return $__L_coll;}
}
ns(\Clojure\Php\Sym::create('clojure.lang.lazy'), 'Lazy sequence implementations.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
call_user_func('\Clojure\Php\defType', 'LazySeq', array('fn', 'sv', 's', '_meta'), array('fn', 'sv', 's'), array('-seq', (function($__L_this) {  while(true) { if ($__L_this->fn) { $__L_this->sv = apply($GLOBALS['fn']);
$__L_this->fn = null;
}
if ($__L_this->sv) { $__L_ls = $__L_this->sv;
$__L_this->sv = null;
$__L_ls = $__L_ls;
 while(true) { if (instance_QMARK_($GLOBALS['LazySeq'], $__L_ls)) { $__recur_0 = $__L_ls->sv; $__L_ls = $__recur_0; continue;} else { $__L_this->s = $__L_ls;
$__L_this->s;
}
 break; }
}
return $__L_this->s; break; }}), '-first', (function($__L_this) { _seq($__L_this);
if ($__L_this->s) { return _first($__L_this->s);}}), '-rest', (function($__L_this) { _seq($__L_this);
if ($__L_this->s) { return _rest($__L_this->s);} else { return \Clojure\Php\emptyList();}}), '-next', (function($__L_this) { _seq($__L_this);
if ($__L_this->s) { return _next($__L_this->s);}}), '-conj', (function($__L_this, $__L_o) { return __GT_Cons($__L_o, $__L_this, null);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-count', (function($__L_this) {  while(true) { $__L_s = _seq($__L_this);
$__L_c = 0;
 while(true) { if ($__L_s) { $__recur_0 = _next($__L_s); $__recur_1 = ($__L_c + 1); $__L_s = $__recur_0; $__L_c = $__recur_1; continue;} else { return $__L_c;} break; }
 break; }}), '-equiv', (function($__L_this, $__L_other) {  while(true) { if (satisfies_QMARK_($GLOBALS['ISequential'], $__L_other)) { $__L_s1 = _seq($__L_this);
$__L_s2 = _seq($__L_other);
 while(true) { if (($__L_s1 === null)) { return ($__L_s2 === null);} else { if (($__L_s2 === null)) { return false;} else { if (\Clojure\Php\equals(_first($__L_s1), _first($__L_s2))) { $__recur_0 = _next($__L_s1); $__recur_1 = _next($__L_s2); $__L_s1 = $__recur_0; $__L_s2 = $__recur_1; continue;} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}} break; }
} else { return false;} break; }}), '-hash', (function($__L_this) {  while(true) { $__L_s = _seq($__L_this);
$__L_h = 1;
 while(true) { if ($__L_s) { $__recur_0 = _next($__L_s); $__recur_1 = ((31 * $__L_h) + hash(_first($__L_s))); $__L_s = $__recur_0; $__L_h = $__recur_1; continue;} else { return $__L_h;} break; }
 break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_LazySeq($__L__->fn, $__L__->sv, $__L__->s, $__L_m);}), '-realized?', (function($__L__) { return ($__L__->fn === null);})));
function __GT_LazySeq($__L_fn, $__L_sv, $__L_s, $__L__meta) {
return call_user_func('\Clojure\Php\createType', 'LazySeq', $__L_fn, $__L_sv, $__L_s, $__L__meta);
}
function lazy_seq_STAR_($__L_f) {
return __GT_LazySeq($__L_f, null, null, null);
}
call_user_func('\Clojure\Php\defType', 'Delay', array('fn', 'val', 'exception'), array('fn', 'val', 'exception'), array('-deref', (function($__L_this) { if ($__L_this->fn) { try { return $__L_this->val = apply($GLOBALS['fn']); } catch (\Throwable $__L_e) { return $__L_this->exception = $__L_e; }
$__L_this->fn = null;
}
if ($__L_this->exception) { throw $__L_this->exception;
} else { return $__L_this->val;}}), '-realized?', (function($__L__) { return ($__L__->fn === null);})));
function __GT_Delay($__L_fn, $__L_val, $__L_exception) {
return call_user_func('\Clojure\Php\createType', 'Delay', $__L_fn, $__L_val, $__L_exception);
}
function delay_STAR_($__L_f) {
return __GT_Delay($__L_f, null, null);
}
function delay_QMARK_($__L_x) {
return instance_QMARK_($GLOBALS['Delay'], $__L_x);
}
function force($__L_x) {
if (delay_QMARK_($__L_x)) { return _deref($__L_x);} else { return $__L_x;}
}
call_user_func('\Clojure\Php\defType', 'Volatile', array('val'), array('val'), array('-deref', (function($__L__) { return $__L__->val;}), '-vreset!', (function($__L__, $__L_new_val) { $__L__->val = $__L_new_val;
return $__L_new_val;})));
function __GT_Volatile($__L_val) {
return call_user_func('\Clojure\Php\createType', 'Volatile', $__L_val);
}
function volatile_BANG_($__L_val) {
return __GT_Volatile($__L_val);
}
function volatile_QMARK_($__L_x) {
return instance_QMARK_($GLOBALS['Volatile'], $__L_x);
}
function vreset_BANG_($__L_vol, $__L_new_val) {
return _vreset_BANG_($__L_vol, $__L_new_val);
}
function vswap_BANG_($__L_vol, $__L_f, $__L_args) {
return vreset_BANG_($__L_vol, apply($__L_f, deref($__L_vol), $__L_args));
}
call_user_func('\Clojure\Php\defType', 'Range', array('start', 'end', 'step', '_meta'), array(), array('-seq', (function($__L_this) { if (($__L_this->step > 0)) { if (($__L_this->start < $__L_this->end)) { return $__L_this;}} else { if (($__L_this->start > $__L_this->end)) { return $__L_this;}}}), '-first', (function($__L__) { return $__L__->start;}), '-rest', (function($__L__) { $__L_next_val = ($__L__->start + $__L__->step);
if (($__L__->step > 0)) { if (($__L_next_val < $__L__->end)) { return __GT_Range($__L_next_val, $__L__->end, $__L__->step, $__L__->_meta);} else { return \Clojure\Php\emptyList();}} else { if (($__L_next_val > $__L__->end)) { return __GT_Range($__L_next_val, $__L__->end, $__L__->step, $__L__->_meta);} else { return \Clojure\Php\emptyList();}}}), '-next', (function($__L_this) { $__L_r = _rest($__L_this);
if (_seq($__L_r)) { return $__L_r;}}), '-count', (function($__L__) { if ((call_user_func(function() { $__L_or__3040 = (($__L__->step > 0) ? ($__L__->start < $__L__->end) : false); if ($__L_or__3040) { return $__L_or__3040;} else { if (($__L__->step < 0)) { return ($__L__->start > $__L__->end);} else { return false;}} }))) { $__L_diff = ($__L__->end - $__L__->start);
return ((int)Math::ceil(($__L_diff / $__L__->step)));} else { return 0;}}), '-nth', (function($__L_this, $__L_n) { $__L_val = ($__L_this->start + ($__L_n * $__L_this->step));
if (($__L_this->step > 0)) { if ((($__L_val >= $__L_this->start) ? ($__L_val < $__L_this->end) : false)) { return $__L_val;} else { throw ex_info('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n));
}} else { if ((($__L_val <= $__L_this->start) ? ($__L_val > $__L_this->end) : false)) { return $__L_val;} else { throw ex_info('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n));
}}}), '-nth', (function($__L_this, $__L_n, $__L_not_found) { $__L_val = ($__L_this->start + ($__L_n * $__L_this->step));
if (($__L_this->step > 0)) { if ((($__L_val >= $__L_this->start) ? ($__L_val < $__L_this->end) : false)) { return $__L_val;} else { return $__L_not_found;}} else { if ((($__L_val <= $__L_this->start) ? ($__L_val > $__L_this->end) : false)) { return $__L_val;} else { return $__L_not_found;}}}), '-conj', (function($__L_this, $__L_o) { return __GT_Cons($__L_o, $__L_this, null);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-equiv', (function($__L_this, $__L_other) {  while(true) { if (satisfies_QMARK_($GLOBALS['ISequential'], $__L_other)) { $__L_s1 = _seq($__L_this);
$__L_s2 = _seq($__L_other);
 while(true) { if (($__L_s1 === null)) { return ($__L_s2 === null);} else { if (($__L_s2 === null)) { return false;} else { if (\Clojure\Php\equals(_first($__L_s1), _first($__L_s2))) { $__recur_0 = _next($__L_s1); $__recur_1 = _next($__L_s2); $__L_s1 = $__recur_0; $__L_s2 = $__recur_1; continue;} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}} break; }
} else { return false;} break; }}), '-hash', (function($__L_this) {  while(true) { $__L_s = _seq($__L_this);
$__L_h = 1;
 while(true) { if ($__L_s) { $__recur_0 = _next($__L_s); $__recur_1 = ((31 * $__L_h) + hash(_first($__L_s))); $__L_s = $__recur_0; $__L_h = $__recur_1; continue;} else { return $__L_h;} break; }
 break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_Range($__L__->start, $__L__->end, $__L__->step, $__L_m);}), '-reduce', (function($__L_this, $__L_f) {  while(true) { $__L_acc = $__L_this->start;
$__L_val = ($__L_this->start + $__L_this->step);
 while(true) { if (($__L_this->step > 0)) { if (($__L_val < $__L_this->end)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L_val);
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_val + $__L_this->step); $__L_acc = $__recur_0; $__L_val = $__recur_1; continue;}} else { return $__L_acc;}} else { if (($__L_val > $__L_this->end)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L_val);
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_val + $__L_this->step); $__L_acc = $__recur_0; $__L_val = $__recur_1; continue;}} else { return $__L_acc;}} break; }
 break; }}), '-reduce-init', (function($__L__, $__L_f, $__L_init) {  while(true) { $__L_acc = $__L_init;
$__L_val = $__L__->start;
 while(true) { if (($__L__->step > 0)) { if (($__L_val < $__L__->end)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L_val);
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_val + $__L__->step); $__L_acc = $__recur_0; $__L_val = $__recur_1; continue;}} else { return $__L_acc;}} else { if (($__L_val > $__L__->end)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L_val);
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_val + $__L__->step); $__L_acc = $__recur_0; $__L_val = $__recur_1; continue;}} else { return $__L_acc;}} break; }
 break; }})));
function __GT_Range($__L_start, $__L_end, $__L_step, $__L__meta) {
return call_user_func('\Clojure\Php\createType', 'Range', $__L_start, $__L_end, $__L_step, $__L__meta);
}
function range() {
return __GT_Range(0, Infinity, 1, null);
}
call_user_func('\Clojure\Php\defType', 'Repeat', array('val', 'count', '_meta'), array(), array('-seq', (function($__L_this) { if ((call_user_func(function() { $__L_or__3041 = ($__L_this->count === null); if ($__L_or__3041) { return $__L_or__3041;} else { return ($__L_this->count > 0);} }))) { return $__L_this;}}), '-first', (function($__L__) { return $__L__->val;}), '-rest', (function($__L__) { if (($__L__->count === null)) { return __GT_Repeat($__L__->val, null, $__L__->_meta);} else { if (($__L__->count > 1)) { return __GT_Repeat($__L__->val, ($__L__->count - 1), $__L__->_meta);} else { return \Clojure\Php\emptyList();}}}), '-next', (function($__L_this) { $__L_r = _rest($__L_this);
if (_seq($__L_r)) { return $__L_r;}}), '-count', (function($__L__) { if (($__L__->count === null)) { throw ex_info('Cannot count infinite repeat', \Clojure\Php\hashMap());
} else { return $__L__->count;}}), '-conj', (function($__L_this, $__L_o) { return __GT_Cons($__L_o, $__L_this, null);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_Repeat($__L__->val, $__L__->count, $__L_m);}), '-reduce', (function($__L__, $__L_f) {  while(true) { if (($__L__->count === null)) { $__L_acc = $__L__->val;
 while(true) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L__->val);
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = $__L_ret; $__L_acc = $__recur_0; continue;} break; }
} else { $__L_acc = $__L__->val;
$__L_n = ($__L__->count - 1);
 while(true) { if (($__L_n > 0)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L__->val);
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_n - 1); $__L_acc = $__recur_0; $__L_n = $__recur_1; continue;}} else { return $__L_acc;} break; }
} break; }}), '-reduce-init', (function($__L__, $__L_f, $__L_init) {  while(true) { if (($__L__->count === null)) { $__L_acc = $__L_init;
 while(true) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L__->val);
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = $__L_ret; $__L_acc = $__recur_0; continue;} break; }
} else { $__L_acc = $__L_init;
$__L_n = $__L__->count;
 while(true) { if (($__L_n > 0)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L__->val);
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_n - 1); $__L_acc = $__recur_0; $__L_n = $__recur_1; continue;}} else { return $__L_acc;} break; }
} break; }})));
function __GT_Repeat($__L_val, $__L_count, $__L__meta) {
return call_user_func('\Clojure\Php\createType', 'Repeat', $__L_val, $__L_count, $__L__meta);
}
function repeat($__L_x) {
return __GT_Repeat($__L_x, null, null);
}
call_user_func('\Clojure\Php\defType', 'Iterate', array('f', 'val', '_meta'), array(), array('-seq', (function($__L_this) { return $__L_this;}), '-first', (function($__L__) { return $__L__->val;}), '-rest', (function($__L__) { return __GT_Iterate($__L__->f, call_user_func($__L__->f, $__L__->val), $__L__->_meta);}), '-next', (function($__L_this) { return _rest($__L_this);}), '-conj', (function($__L_this, $__L_o) { return __GT_Cons($__L_o, $__L_this, null);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_Iterate($__L__->f, $__L__->val, $__L_m);}), '-reduce-init', (function($__L__, $__L_rf, $__L_init) {  while(true) { $__L_acc = $__L_init;
$__L_v = $__L__->val;
 while(true) { $__L_ret = call_user_func($__L_rf, $__L_acc, $__L_v);
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = call_user_func($__L__->f, $__L_v); $__L_acc = $__recur_0; $__L_v = $__recur_1; continue;} break; }
 break; }})));
function __GT_Iterate($__L_f, $__L_val, $__L__meta) {
return call_user_func('\Clojure\Php\createType', 'Iterate', $__L_f, $__L_val, $__L__meta);
}
function iterate($__L_f, $__L_x) {
return __GT_Iterate($__L_f, $__L_x, null);
}
call_user_func('\Clojure\Php\defType', 'Cycle', array('all', 'current', '_meta'), array(), array('-seq', (function($__L_this) { if (_seq($__L_this->all)) { return $__L_this;}}), '-first', (function($__L__) { return _first($__L__->current);}), '-rest', (function($__L__) { $__L_nxt = _next($__L__->current);
if ($__L_nxt) { return __GT_Cycle($__L__->all, $__L_nxt, $__L__->_meta);} else { return __GT_Cycle($__L__->all, _seq($__L__->all), $__L__->_meta);}}), '-next', (function($__L_this) { return _rest($__L_this);}), '-conj', (function($__L_this, $__L_o) { return __GT_Cons($__L_o, $__L_this, null);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_Cycle($__L__->all, $__L__->current, $__L_m);})));
function __GT_Cycle($__L_all, $__L_current, $__L__meta) {
return call_user_func('\Clojure\Php\createType', 'Cycle', $__L_all, $__L_current, $__L__meta);
}
function cycle($__L_coll) {
$__L_s = \Clojure\Php\seq($__L_coll);
if ($__L_s) { return __GT_Cycle($__L_s, $__L_s, null);}
}
ns(\Clojure\Php\Sym::create('clojure.lang.PersistentVector'), 'Persistent Vector - matches JVM clojure.lang.PersistentVector API.

   32-way branching trie (HAMT) implementation providing:
   - O(log32 n) access, update, and append
   - Structural sharing for efficiency
   - Transient support for batch operations', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')], [\Clojure\Php\Sym::create('clojure.lang.array'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('arr')]));
$GLOBALS['BITS'] = 5;
$BITS = &$GLOBALS['BITS'];
$GLOBALS['WIDTH'] = 32;
$WIDTH = &$GLOBALS['WIDTH'];
$GLOBALS['MASK'] = 31;
$MASK = &$GLOBALS['MASK'];
call_user_func('\Clojure\Php\defType', 'VectorNode', array('edit', 'arr'), array(), array());
function __GT_VectorNode($__L_edit, $__L_arr) {
return call_user_func('\Clojure\Php\createType', 'VectorNode', $__L_edit, $__L_arr);
}
$GLOBALS['EMPTY_NODE'] = __GT_VectorNode(null, object_array($GLOBALS['WIDTH']));
$EMPTY_NODE = &$GLOBALS['EMPTY_NODE'];
function new_path($__L_edit, $__L_level, $__L_node) {
if (($__L_level === 0 || $__L_level === 0.0)) { return $__L_node;} else { $__L_ret = __GT_VectorNode($__L_edit, object_array($GLOBALS['WIDTH']));
aset($__L_ret->arr, 0, new_path($__L_edit, ($__L_level - $GLOBALS['BITS']), $__L_node));
return $__L_ret;}
}
function push_tail($__L_level, $__L_parent, $__L_tail_node) {
$__L_subidx = (unsigned_bit_shift_right(($GLOBALS['cnt'] - 1), $__L_level) & $GLOBALS['MASK']);
$__L_ret = __GT_VectorNode($__L_parent->edit, aclone($__L_parent->arr));
$__L_node_to_insert = (\Clojure\Php\equals($__L_level, $GLOBALS['BITS']) ? $__L_tail_node : (call_user_func(function() { $__L_child = aget($__L_parent->arr, $__L_subidx); if ($__L_child) { return push_tail(($__L_level - $GLOBALS['BITS']), $__L_child, $__L_tail_node);} else { return new_path($__L_parent->edit, ($__L_level - $GLOBALS['BITS']), $__L_tail_node);} })));
aset($__L_ret->arr, $__L_subidx, $__L_node_to_insert);
return $__L_ret;
}
function do_assoc($__L_level, $__L_node, $__L_i, $__L_val) {
$__L_ret = __GT_VectorNode($__L_node->edit, aclone($__L_node->arr));
if (($__L_level === 0 || $__L_level === 0.0)) { aset($__L_ret->arr, ($__L_i & $GLOBALS['MASK']), $__L_val);
return $__L_ret;} else { $__L_subidx = (unsigned_bit_shift_right($__L_i, $__L_level) & $GLOBALS['MASK']);
aset($__L_ret->arr, $__L_subidx, do_assoc(($__L_level - $GLOBALS['BITS']), aget($__L_node->arr, $__L_subidx), $__L_i, $__L_val));
return $__L_ret;}
}
function array_for($__L_vec, $__L_i) {
if ((($__L_i >= 0) ? ($__L_i < $__L_vec->cnt) : false)) { if (($__L_i >= tail_off($__L_vec))) { return $__L_vec->tail;} else { $__L_node = $__L_vec->root;
$__L_level = $__L_vec->shift;
 while(true) { if (($__L_level > 0)) { $__recur_0 = aget($__L_node->arr, (unsigned_bit_shift_right($__L_i, $__L_level) & $GLOBALS['MASK'])); $__recur_1 = ($__L_level - $GLOBALS['BITS']); $__L_node = $__recur_0; $__L_level = $__recur_1; continue;} else { return $__L_node->arr;} break; }
}} else { throw ex_info('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('index'), $__L_i, \Clojure\Php\Kw::create('count'), $__L_vec->cnt));
}
}
function tail_off($__L_vec) {
$__L_cnt = $__L_vec->cnt;
if (($__L_cnt < $GLOBALS['WIDTH'])) { return 0;} else { return (unsigned_bit_shift_right(($__L_cnt - 1), $GLOBALS['BITS']) << $GLOBALS['BITS']);}
}
$GLOBALS['EMPTY_VECTOR'] = null;
$EMPTY_VECTOR = &$GLOBALS['EMPTY_VECTOR'];
call_user_func('\Clojure\Php\defType', 'PersistentVector', array('cnt', 'shift', 'root', 'tail', '_meta', '_hash'), array(), array('-count', (function($__L__) { return $__L__->cnt;}), '-nth', (function($__L_this, $__L_n) { return aget(array_for($__L_this, $__L_n), ($__L_n & $GLOBALS['MASK']));}), '-nth', (function($__L_this, $__L_n, $__L_not_found) { if ((($__L_n >= 0) ? ($__L_n < $__L_this->cnt) : false)) { return aget(array_for($__L_this, $__L_n), ($__L_n & $GLOBALS['MASK']));} else { return $__L_not_found;}}), '-lookup', (function($__L_this, $__L_k) { if ((is_int($__L_k) || is_float($__L_k))) { return _nth($__L_this, $__L_k, null);}}), '-lookup', (function($__L_this, $__L_k, $__L_not_found) { if ((is_int($__L_k) || is_float($__L_k))) { return _nth($__L_this, $__L_k, $__L_not_found);} else { return $__L_not_found;}}), '-contains-key?', (function($__L__, $__L_k) { if ((is_int($__L_k) || is_float($__L_k))) { if (($__L_k >= 0)) { return ($__L_k < $__L__->cnt);} else { return false;}} else { return false;}}), '-assoc', (function($__L_this, $__L_k, $__L_v) { if ((is_int($__L_k) || is_float($__L_k))) { $__L_i = ((int)$__L_k);
if ((($__L_i >= 0) ? ($__L_i < $__L_this->cnt) : false)) { if (($__L_i >= tail_off($__L_this))) { $__L_new_tail = aclone($__L_this->tail);
asetnew_tail(($__L_i & $GLOBALS['MASK']), $__L_v);
return __GT_PersistentVector($__L_this->cnt, $__L_this->shift, $__L_this->root, $__L_new_tail, $__L_this->_meta, null);} else { return __GT_PersistentVector($__L_this->cnt, $__L_this->shift, do_assoc($__L_this->shift, $__L_this->root, $__L_i, $__L_v), $__L_this->tail, $__L_this->_meta, null);}} else { if (\Clojure\Php\equals($__L_i, $__L_this->cnt)) { return _conj($__L_this, $__L_v);} else { if (\Clojure\Php\Kw::create('else')) { throw ex_info('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('index'), $__L_i, \Clojure\Php\Kw::create('count'), $__L_this->cnt));
}}}} else { throw ex_info('Key must be integer', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('key'), $__L_k));
}}), '-peek', (function($__L__) { if (($__L__->cnt > 0)) { return _nth($__L__, ($__L__->cnt - 1));}}), '-pop', (function($__L_this) { if (($__L_this->cnt === 0 || $__L_this->cnt === 0.0)) { throw ex_info('Can\'t pop empty vector', \Clojure\Php\hashMap());
} else { if (\Clojure\Php\equals(1, $__L_this->cnt)) { return $GLOBALS['EMPTY_VECTOR'];} else { if ((($__L_this->cnt - tail_off($__L_this)) > 1)) { $__L_new_tail = make_array((alengthtail() - 1));
System::arraycopy($__L_this->tail, 0, $__L_new_tail, 0, alengthnew_tail());
return __GT_PersistentVector(($__L_this->cnt - 1), $__L_this->shift, $__L_this->root, $__L_new_tail, $__L_this->_meta, null);} else { if (\Clojure\Php\Kw::create('else')) { $__L_new_tail = array_for($__L_this, ($__L_this->cnt - 2));
$__L_new_root = pop_tail($__L_this->shift, $__L_this->root);
if (($__L_new_root === null)) { return __GT_PersistentVector(($__L_this->cnt - 1), $__L_this->shift, $GLOBALS['EMPTY_NODE'], $__L_new_tail, $__L_this->_meta, null);} else { if ((($__L_this->shift > $GLOBALS['BITS']) ? (aget($__L_new_root->arr, 1) === null) : false)) { return __GT_PersistentVector(($__L_this->cnt - 1), ($__L_this->shift - $GLOBALS['BITS']), aget($__L_new_root->arr, 0), $__L_new_tail, $__L_this->_meta, null);} else { if (\Clojure\Php\Kw::create('else')) { return __GT_PersistentVector(($__L_this->cnt - 1), $__L_this->shift, $__L_new_root, $__L_new_tail, $__L_this->_meta, null);}}}}}}}}), '-conj', (function($__L_this, $__L_val) { if ((($__L_this->cnt - tail_off($__L_this)) < $GLOBALS['WIDTH'])) { $__L_new_tail = make_array((alengthtail() + 1));
System::arraycopy($__L_this->tail, 0, $__L_new_tail, 0, alengthtail());
asetnew_tail(alengthtail(), $__L_val);
return __GT_PersistentVector(($__L_this->cnt + 1), $__L_this->shift, $__L_this->root, $__L_new_tail, $__L_this->_meta, null);} else { $__L_tail_node = __GT_VectorNode(null, $__L_this->tail);
$__L_new_shift = $__L_this->shift;
$__L_new_root = ((unsigned_bit_shift_right($__L_this->cnt, $GLOBALS['BITS']) > (1 << $__L_this->shift)) ? (call_user_func(function() { $__L_new_root = __GT_VectorNode(null, object_array($GLOBALS['WIDTH'])); aset($__L_new_root->arr, 0, $__L_this->root);
aset($__L_new_root->arr, 1, new_path(null, $__L_this->shift, $__L_tail_node));
$new_shift = ($__L_this->shift + $GLOBALS['BITS']);
return $__L_new_root; })) : push_tail($__L_this->shift, $__L_this->root, $__L_tail_node));
__GT_PersistentVector(($__L_this->cnt + 1), $__L_new_shift, $__L_new_root, object_array(1), $__L_this->_meta, null);
aset($GLOBALS['_']->tail, 0, $__L_val);
return $GLOBALS['_'];}}), '-empty', (function($__L__) { return $GLOBALS['EMPTY_VECTOR'];}), '-seq', (function($__L_this) { if (($__L_this->cnt > 0)) { return chunked_seq($__L_this, 0, 0);}}), '-equiv', (function($__L_this, $__L_other) {  while(true) { if (($__L_this === $__L_other)) { return true;} else { if (satisfies_QMARK_($GLOBALS['IVector'], $__L_other)) { if (\Clojure\Php\equals($__L_this->cnt, _count($__L_other))) { $__L_i = 0;
 while(true) { if (($__L_i < $__L_this->cnt)) { if (\Clojure\Php\equals(_nth($__L_this, $__L_i), _nth($__L_other, $__L_i))) { $__recur_0 = ($__L_i + 1); $__L_i = $__recur_0; continue;} else { return false;}} else { return true;} break; }
} else { return false;}} else { if (satisfies_QMARK_($GLOBALS['ISequential'], $__L_other)) { $__L_s1 = _seq($__L_this);
$__L_s2 = _seq($__L_other);
 while(true) { if (($__L_s1 === null)) { return ($__L_s2 === null);} else { if (($__L_s2 === null)) { return false;} else { if (\Clojure\Php\equals(_first($__L_s1), _first($__L_s2))) { $__recur_0 = _next($__L_s1); $__recur_1 = _next($__L_s2); $__L_s1 = $__recur_0; $__L_s2 = $__recur_1; continue;} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}} break; }
} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}} break; }}), '-hash', (function($__L_this) {  while(true) { if ($__L_this->_hash) { return $__L_this->_hash;} else { $__L_h = (call_user_func(function() { $__L_s = _seq($__L_this); $__L_h = 1;  while(true) { if ($__L_s) { $__recur_0 = _next($__L_s); $__recur_1 = ((31 * $__L_h) + hash(_first($__L_s))); $__L_s = $__recur_0; $__L_h = $__recur_1; continue;} else { $__L_h;
}
 break; } return $__L_s; }));
$__L_this->_hash = $__L_h;
return $__L_h;} break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_PersistentVector($__L__->cnt, $__L__->shift, $__L__->root, $__L__->tail, $__L_m, $__L__->_hash);}), '-reduce', (function($__L_this, $__L_f) {  while(true) { if (($__L_this->cnt > 0)) { $__L_i = 1;
$__L_acc = _nth($__L_this, 0);
 while(true) { if (($__L_i < $__L_this->cnt)) { $__L_ret = call_user_func($__L_f, $__L_acc, _nth($__L_this, $__L_i));
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = ($__L_i + 1); $__recur_1 = $__L_ret; $__L_i = $__recur_0; $__L_acc = $__recur_1; continue;}} else { return $__L_acc;} break; }
} else { return call_user_func($__L_f);} break; }}), '-reduce-init', (function($__L_this, $__L_f, $__L_init) {  while(true) { $__L_i = 0;
$__L_acc = $__L_init;
 while(true) { if (($__L_i < $__L_this->cnt)) { $__L_ret = call_user_func($__L_f, $__L_acc, _nth($__L_this, $__L_i));
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = ($__L_i + 1); $__recur_1 = $__L_ret; $__L_i = $__recur_0; $__L_acc = $__recur_1; continue;}} else { return $__L_acc;} break; }
 break; }}), '-kv-reduce', (function($__L_this, $__L_f, $__L_init) {  while(true) { $__L_i = 0;
$__L_acc = $__L_init;
 while(true) { if (($__L_i < $__L_this->cnt)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L_i, _nth($__L_this, $__L_i));
if (reduced_QMARK_($__L_ret)) { return deref($__L_ret);} else { $__recur_0 = ($__L_i + 1); $__recur_1 = $__L_ret; $__L_i = $__recur_0; $__L_acc = $__recur_1; continue;}} else { return $__L_acc;} break; }
 break; }}), '-invoke', (function($__L_this, $__L_k) { return _nth($__L_this, $__L_k);}), '-invoke', (function($__L_this, $__L_k, $__L_not_found) { return _nth($__L_this, $__L_k, $__L_not_found);}), '-rseq', (function($__L_this) { if (($__L_this->cnt > 0)) { return rseq($__L_this, ($__L_this->cnt - 1));}})));
function __GT_PersistentVector($__L_cnt, $__L_shift, $__L_root, $__L_tail, $__L__meta, $__L__hash) {
return call_user_func('\Clojure\Php\createType', 'PersistentVector', $__L_cnt, $__L_shift, $__L_root, $__L_tail, $__L__meta, $__L__hash);
}
$GLOBALS['EMPTY_VECTOR'] = __GT_PersistentVector(0, $GLOBALS['BITS'], $GLOBALS['EMPTY_NODE'], object_array(0), null, null);
$EMPTY_VECTOR = &$GLOBALS['EMPTY_VECTOR'];
function vector() {
return $GLOBALS['EMPTY_VECTOR'];
}
function vec($__L_coll) {
if (($__L_coll instanceof \Clojure\Php\Vec)) { return $__L_coll;} else { return reduce($GLOBALS['conj'], $GLOBALS['EMPTY_VECTOR'], $__L_coll);}
}
function vector_QMARK_($__L_x) {
return instance_QMARK_($GLOBALS['PersistentVector'], $__L_x);
}
function subvec($__L_v, $__L_start) {
return subvec($__L_v, $__L_start, \Clojure\Php\count_($__L_v));
}
ns(\Clojure\Php\Sym::create('clojure.lang.PersistentHashMap'), 'Persistent Hash Map - matches JVM clojure.lang.PersistentHashMap API.

   Hash Array Mapped Trie (HAMT) implementation providing:
   - O(log32 n) lookup, insert, and delete
   - Structural sharing for efficiency', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$GLOBALS['BITS'] = 5;
$BITS = &$GLOBALS['BITS'];
$GLOBALS['WIDTH'] = 32;
$WIDTH = &$GLOBALS['WIDTH'];
$GLOBALS['MASK'] = 31;
$MASK = &$GLOBALS['MASK'];
function hash_code($__L_x) {
if (($__L_x === null)) { return 0;} else { return hash($__L_x);}
}
$GLOBALS['EMPTY_MAP'] = null;
$EMPTY_MAP = &$GLOBALS['EMPTY_MAP'];
call_user_func('\Clojure\Php\defType', 'BitmapIndexedNode', array('bitmap', 'arr'), array(), array());
function __GT_BitmapIndexedNode($__L_bitmap, $__L_arr) {
return call_user_func('\Clojure\Php\createType', 'BitmapIndexedNode', $__L_bitmap, $__L_arr);
}
call_user_func('\Clojure\Php\defType', 'ArrayNode', array('cnt', 'arr'), array(), array());
function __GT_ArrayNode($__L_cnt, $__L_arr) {
return call_user_func('\Clojure\Php\createType', 'ArrayNode', $__L_cnt, $__L_arr);
}
call_user_func('\Clojure\Php\defType', 'HashCollisionNode', array('hash', 'cnt', 'arr'), array(), array());
function __GT_HashCollisionNode($__L_hash, $__L_cnt, $__L_arr) {
return call_user_func('\Clojure\Php\createType', 'HashCollisionNode', $__L_hash, $__L_cnt, $__L_arr);
}
$GLOBALS['EMPTY_BITMAP_NODE'] = __GT_BitmapIndexedNode(0, object_array(0));
$EMPTY_BITMAP_NODE = &$GLOBALS['EMPTY_BITMAP_NODE'];
function mask($__L_hash, $__L_shift) {
return (unsigned_bit_shift_right($__L_hash, $__L_shift) & $GLOBALS['MASK']);
}
function bitpos($__L_hash, $__L_shift) {
return (1 << mask($__L_hash, $__L_shift));
}
function index($__L_bitmap, $__L_bit) {
return Integer::bitCount(($__L_bitmap & ($__L_bit - 1)));
}
$GLOBALS['node_assoc'] = null;
$node_assoc = &$GLOBALS['node_assoc'];
$GLOBALS['node_find'] = null;
$node_find = &$GLOBALS['node_find'];
$GLOBALS['node_dissoc'] = null;
$node_dissoc = &$GLOBALS['node_dissoc'];
function clone_and_set($__L_arr, $__L_i, $__L_val) {
$__L_clone = aclone($__L_arr);
aset($__L_clone, $__L_i, $__L_val);
return $__L_clone;
}
function remove_pair($__L_arr, $__L_i) {
$__L_new_arr = object_array((alength($__L_arr) - 2));
System::arraycopy($__L_arr, 0, $__L_new_arr, 0, (2 * $__L_i));
System::arraycopy($__L_arr, (2 * ($__L_i + 1)), $__L_new_arr, (2 * $__L_i), (alength($__L_new_arr) - (2 * $__L_i)));
return $__L_new_arr;
}
function create_node($__L_shift, $__L_key1, $__L_val1, $__L_key2_hash, $__L_key2, $__L_val2) {
$__L_key1_hash = hash_code($__L_key1);
if (\Clojure\Php\equals($__L_key1_hash, $__L_key2_hash)) { return __GT_HashCollisionNode($__L_key1_hash, 2, object_array(\Clojure\Php\vec($__L_key1, $__L_val1, $__L_key2, $__L_val2)));} else { $__L_n1 = node_assoc($GLOBALS['EMPTY_BITMAP_NODE'], $__L_shift, $__L_key1_hash, $__L_key1, $__L_val1);
$__L_n2 = node_assoc($__L_n1, $__L_shift, $__L_key2_hash, $__L_key2, $__L_val2);
return $__L_n2;}
}
function bitmap_node_assoc($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_val) {
$__L_bit = bitpos($__L_hash, $__L_shift);
$__L_idx = index($__L_node->bitmap, $__L_bit);
$__L_arr = $__L_node->arr;
if ((!\Clojure\Php\equals(0, ($__L_node->bitmap & $__L_bit)))) { $__L_key_or_null = aget($__L_arr, (2 * $__L_idx));
$__L_val_or_node = aget($__L_arr, ((2 * $__L_idx) + 1));
if (($__L_key_or_null === null)) { $__L_n = node_assoc($__L_val_or_node, ($__L_shift + $GLOBALS['BITS']), $__L_hash, $__L_key, $__L_val);
if (($__L_n === $__L_val_or_node)) { return $__L_node;} else { return __GT_BitmapIndexedNode($__L_node->bitmap, clone_and_set($__L_arr, ((2 * $__L_idx) + 1), $__L_n));}} else { if (\Clojure\Php\equals($__L_key, $__L_key_or_null)) { if (($__L_val === $__L_val_or_node)) { return $__L_node;} else { return __GT_BitmapIndexedNode($__L_node->bitmap, clone_and_set($__L_arr, ((2 * $__L_idx) + 1), $__L_val));}} else { if (\Clojure\Php\Kw::create('else')) { return __GT_BitmapIndexedNode($__L_node->bitmap, clone_and_set($__L_arr, (2 * $__L_idx), null, ((2 * $__L_idx) + 1), create_node(($__L_shift + $GLOBALS['BITS']), $__L_key_or_null, $__L_val_or_node, $__L_hash, $__L_key, $__L_val)));}}}} else { $__L_n = alength($__L_arr);
$__L_new_arr = object_array(($__L_n + 2));
System::arraycopy($__L_arr, 0, $__L_new_arr, 0, (2 * $__L_idx));
aset($__L_new_arr, (2 * $__L_idx), $__L_key);
aset($__L_new_arr, ((2 * $__L_idx) + 1), $__L_val);
System::arraycopy($__L_arr, (2 * $__L_idx), $__L_new_arr, (2 * ($__L_idx + 1)), ($__L_n - (2 * $__L_idx)));
return __GT_BitmapIndexedNode(($__L_node->bitmap | $__L_bit), $__L_new_arr);}
}
function bitmap_node_find($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_not_found) {
$__L_bit = bitpos($__L_hash, $__L_shift);
if (\Clojure\Php\equals(0, ($__L_node->bitmap & $__L_bit))) { return $__L_not_found;} else { $__L_idx = index($__L_node->bitmap, $__L_bit);
$__L_key_or_null = aget($__L_node->arr, (2 * $__L_idx));
$__L_val_or_node = aget($__L_node->arr, ((2 * $__L_idx) + 1));
if (($__L_key_or_null === null)) { return node_find($__L_val_or_node, ($__L_shift + $GLOBALS['BITS']), $__L_hash, $__L_key, $__L_not_found);} else { if (\Clojure\Php\equals($__L_key, $__L_key_or_null)) { return $__L_val_or_node;} else { if (\Clojure\Php\Kw::create('else')) { return $__L_not_found;}}}}
}
function bitmap_node_dissoc($__L_node, $__L_shift, $__L_hash, $__L_key) {
$__L_bit = bitpos($__L_hash, $__L_shift);
if (\Clojure\Php\equals(0, ($__L_node->bitmap & $__L_bit))) { return $__L_node;} else { $__L_idx = index($__L_node->bitmap, $__L_bit);
$__L_key_or_null = aget($__L_node->arr, (2 * $__L_idx));
$__L_val_or_node = aget($__L_node->arr, ((2 * $__L_idx) + 1));
if (($__L_key_or_null === null)) { $__L_n = node_dissoc($__L_val_or_node, ($__L_shift + $GLOBALS['BITS']), $__L_hash, $__L_key);
if (($__L_n === $__L_val_or_node)) { return $__L_node;} else { if (($__L_n !== null)) { return __GT_BitmapIndexedNode($__L_node->bitmap, clone_and_set($__L_node->arr, ((2 * $__L_idx) + 1), $__L_n));} else { if (\Clojure\Php\equals($__L_node->bitmap, $__L_bit)) { return null;} else { if (\Clojure\Php\Kw::create('else')) { return __GT_BitmapIndexedNode(($__L_node->bitmap ^ $__L_bit), remove_pair($__L_node->arr, $__L_idx));}}}}} else { if (\Clojure\Php\equals($__L_key, $__L_key_or_null)) { if (\Clojure\Php\equals($__L_node->bitmap, $__L_bit)) { return null;} else { return __GT_BitmapIndexedNode(($__L_node->bitmap ^ $__L_bit), remove_pair($__L_node->arr, $__L_idx));}} else { if (\Clojure\Php\Kw::create('else')) { return $__L_node;}}}}
}
function collision_node_find_index($__L_arr, $__L_cnt, $__L_key) {
$__L_i = 0;
 while(true) { if (($__L_i < $__L_cnt)) { if (\Clojure\Php\equals($__L_key, aget($__L_arr, (2 * $__L_i)))) { return $__L_i;} else { $__recur_0 = ($__L_i + 1); $__L_i = $__recur_0; continue;}} else { return -1;} break; }

}
function collision_node_assoc($__L_node, $__L_hash, $__L_key, $__L_val) {
$__L_idx = collision_node_find_index($__L_node->arr, $__L_node->cnt, $__L_key);
if (\Clojure\Php\equals($__L_idx, -1)) { $__L_new_arr = object_array((alength($__L_node->arr) + 2));
System::arraycopy($__L_node->arr, 0, $__L_new_arr, 0, alength($__L_node->arr));
aset($__L_new_arr, alength($__L_node->arr), $__L_key);
aset($__L_new_arr, (alength($__L_node->arr) + 1), $__L_val);
return __GT_HashCollisionNode($__L_hash, ($__L_node->cnt + 1), $__L_new_arr);} else { if (\Clojure\Php\equals($__L_val, aget($__L_node->arr, ((2 * $__L_idx) + 1)))) { return $__L_node;} else { return __GT_HashCollisionNode($__L_hash, $__L_node->cnt, clone_and_set($__L_node->arr, ((2 * $__L_idx) + 1), $__L_val));}}
}
function collision_node_find($__L_node, $__L_key, $__L_not_found) {
$__L_idx = collision_node_find_index($__L_node->arr, $__L_node->cnt, $__L_key);
if (\Clojure\Php\equals($__L_idx, -1)) { return $__L_not_found;} else { return aget($__L_node->arr, ((2 * $__L_idx) + 1));}
}
function node_assoc($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_val) {
if (instance_QMARK_($GLOBALS['BitmapIndexedNode'], $__L_node)) { return bitmap_node_assoc($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_val);} else { if (instance_QMARK_($GLOBALS['HashCollisionNode'], $__L_node)) { return collision_node_assoc($__L_node, $__L_hash, $__L_key, $__L_val);} else { if (\Clojure\Php\Kw::create('else')) { throw ex_info('Unknown node type', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('node'), $__L_node));
}}}
}
function node_find($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_not_found) {
if (($__L_node === null)) { return $__L_not_found;} else { if (instance_QMARK_($GLOBALS['BitmapIndexedNode'], $__L_node)) { return bitmap_node_find($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_not_found);} else { if (instance_QMARK_($GLOBALS['HashCollisionNode'], $__L_node)) { return collision_node_find($__L_node, $__L_key, $__L_not_found);} else { if (\Clojure\Php\Kw::create('else')) { throw ex_info('Unknown node type', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('node'), $__L_node));
}}}}
}
function node_dissoc($__L_node, $__L_shift, $__L_hash, $__L_key) {
if (($__L_node === null)) { return null;} else { if (instance_QMARK_($GLOBALS['BitmapIndexedNode'], $__L_node)) { return bitmap_node_dissoc($__L_node, $__L_shift, $__L_hash, $__L_key);} else { if (\Clojure\Php\Kw::create('else')) { return $__L_node;}}}
}
call_user_func('\Clojure\Php\defType', 'PersistentHashMap', array('cnt', 'root', 'has-nil', 'nil-val', '_meta', '_hash'), array(), array('-count', (function($__L__) { return $__L__->cnt;}), '-lookup', (function($__L__, $__L_k) { if (($__L_k === null)) { if ($__L__->has_nil) { return $__L__->nil_val;}} else { if (($__L__->root === null)) { return null;} else { return node_find($__L__->root, 0, hash_code($__L_k), $__L_k, null);}}}), '-lookup', (function($__L__, $__L_k, $__L_not_found) { if (($__L_k === null)) { if ($__L__->has_nil) { return $__L__->nil_val;} else { return $__L_not_found;}} else { if (($__L__->root === null)) { return $__L_not_found;} else { return node_find($__L__->root, 0, hash_code($__L_k), $__L_k, $__L_not_found);}}}), '-contains-key?', (function($__L_this, $__L_k) { return (!(_lookup($__L_this, $__L_k, \Clojure\Php\Kw::createNs('user', 'not-found')) === \Clojure\Php\Kw::createNs('user', 'not-found')));}), '-assoc', (function($__L_this, $__L_k, $__L_v) { if (($__L_k === null)) { if (($__L_this->has_nil ? ($__L_v === $__L_this->nil_val) : false)) { return $__L_this;} else { return __GT_PersistentHashMap(($__L_this->has_nil ? $__L_this->cnt : ($__L_this->cnt + 1)), $__L_this->root, true, $__L_v, $__L_this->_meta, null);}} else { $__L_new_root = node_assoc((call_user_func(function() { $__L_or__3042 = $__L_this->root; if ($__L_or__3042) { return $__L_or__3042;} else { return $GLOBALS['EMPTY_BITMAP_NODE'];} })), 0, hash_code($__L_k), $__L_k, $__L_v);
if (($__L_new_root === $__L_this->root)) { return $__L_this;} else { return __GT_PersistentHashMap(($__L_this->cnt + 1), $__L_new_root, $__L_this->has_nil, $__L_this->nil_val, $__L_this->_meta, null);}}}), '-dissoc', (function($__L_this, $__L_k) { if (($__L_k === null)) { if ($__L_this->has_nil) { return __GT_PersistentHashMap(($__L_this->cnt - 1), $__L_this->root, false, null, $__L_this->_meta, null);} else { return $__L_this;}} else { if (($__L_this->root === null)) { return $__L_this;} else { $__L_new_root = node_dissoc($__L_this->root, 0, hash_code($__L_k), $__L_k);
if (($__L_new_root === $__L_this->root)) { return $__L_this;} else { return __GT_PersistentHashMap(($__L_this->cnt - 1), $__L_new_root, $__L_this->has_nil, $__L_this->nil_val, $__L_this->_meta, null);}}}}), '-conj', (function($__L_this, $__L_entry) { if (($__L_entry instanceof \Clojure\Php\Vec)) { return _assoc($__L_this, \Clojure\Php\nth($__L_entry, 0), \Clojure\Php\nth($__L_entry, 1));} else { return reduce((function($__L_m, $__L_e) use (&$__L_this, &$__L_entry) { return _assoc($__L_m, key($__L_e), val($__L_e));}), $__L_this, $__L_entry);}}), '-empty', (function($__L__) { return $GLOBALS['EMPTY_MAP'];}), '-seq', (function($__L_this) { if (($__L_this->cnt > 0)) { return null;}}), '-equiv', (function($__L_this, $__L_other) { if (satisfies_QMARK_($GLOBALS['IMap'], $__L_other)) { if (\Clojure\Php\equals($__L_this->cnt, _count($__L_other))) { return every_QMARK_((function($__L___dest_6) use (&$__L_this, &$__L_other) { $__L___dest_7 = $__L___dest_6;
$__L_k = \Clojure\Php\nth($__L___dest_7, 0);
$__L_v = \Clojure\Php\nth($__L___dest_7, 1);
if (_contains_key_QMARK_($__L_other, $__L_k)) { return \Clojure\Php\equals($__L_v, _lookup($__L_other, $__L_k));} else { return false;}}), $__L_this);} else { return false;}} else { return false;}}), '-hash', (function($__L_this) { if ($__L_this->_hash) { return $__L_this->_hash;} else { $__L_h = reduce_kv((function($__L_h, $__L_k, $__L_v) use (&$__L_this) { return ($__L_h + (hash($__L_k) ^ hash($__L_v)));}), 0, $__L_this);
$__L_this->_hash = $__L_h;
return $__L_h;}}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_PersistentHashMap($__L__->cnt, $__L__->root, $__L__->has_nil, $__L__->nil_val, $__L_m, $__L__->_hash);}), '-kv-reduce', (function($__L__, $__L_f, $__L_init) { return $__L_init;}), '-invoke', (function($__L_this, $__L_k) { return _lookup($__L_this, $__L_k);}), '-invoke', (function($__L_this, $__L_k, $__L_not_found) { return _lookup($__L_this, $__L_k, $__L_not_found);})));
function __GT_PersistentHashMap($__L_cnt, $__L_root, $__L_has_nil, $__L_nil_val, $__L__meta, $__L__hash) {
return call_user_func('\Clojure\Php\createType', 'PersistentHashMap', $__L_cnt, $__L_root, $__L_has_nil, $__L_nil_val, $__L__meta, $__L__hash);
}
$GLOBALS['EMPTY_MAP'] = __GT_PersistentHashMap(0, null, false, null, null, null);
$EMPTY_MAP = &$GLOBALS['EMPTY_MAP'];
function hash_map($__L_keyvals) {
$__L_m = $GLOBALS['EMPTY_MAP'];
$__L_kvs = \Clojure\Php\seq($__L_keyvals);
 while(true) { if ($__L_kvs) { if (\Clojure\Php\next_($__L_kvs)) { $__recur_0 = \Clojure\Php\assoc($__L_m, \Clojure\Php\first($__L_kvs), \Clojure\Php\second($__L_kvs)); $__recur_1 = nnext($__L_kvs); $__L_m = $__recur_0; $__L_kvs = $__recur_1; continue;} else { throw ex_info('hash-map requires even number of arguments', \Clojure\Php\hashMap());
}} else { return $__L_m;} break; }

}
function map_QMARK_($__L_x) {
return satisfies_QMARK_($GLOBALS['IMap'], $__L_x);
}
call_user_func('\Clojure\Php\defType', 'MapEntry', array('k', 'v'), array(), array('-key', (function($__L__) { return $__L__->k;}), '-val', (function($__L__) { return $__L__->v;}), '-nth', (function($__L__, $__L_n) { return case($__L_n, 0, $__L__->k, 1, $__L__->v, (throw ex_info('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n))));}), '-nth', (function($__L__, $__L_n, $__L_not_found) { return case($__L_n, 0, $__L__->k, 1, $__L__->v, $__L_not_found);}), '-count', (function($__L__) { return 2;}), '-equiv', (function($__L__, $__L_other) { if (satisfies_QMARK_($GLOBALS['IMapEntry'], $__L_other)) { if (\Clojure\Php\equals($__L__->k, _key($__L_other))) { return \Clojure\Php\equals($__L__->v, _val($__L_other));} else { return false;}} else { return false;}})));
function __GT_MapEntry($__L_k, $__L_v) {
return call_user_func('\Clojure\Php\createType', 'MapEntry', $__L_k, $__L_v);
}
function key($__L_e) {
return _key($__L_e);
}
function val($__L_e) {
return _val($__L_e);
}
ns(\Clojure\Php\Sym::create('clojure.lang.PersistentHashSet'), 'Persistent Hash Set - matches JVM clojure.lang.PersistentHashSet API.

   Hash Array Mapped Trie (HAMT) implementation providing:
   - O(log32 n) lookup, insert, and delete
   - Structural sharing for efficiency', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')], [\Clojure\Php\Sym::create('clojure.lang.PersistentHashMap'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('m')]));
$GLOBALS['EMPTY_SET'] = null;
$EMPTY_SET = &$GLOBALS['EMPTY_SET'];
call_user_func('\Clojure\Php\defType', 'PersistentHashSet', array('impl', '_meta', '_hash'), array(), array('-count', (function($__L__) { return _count($__L__->impl);}), '-lookup', (function($__L__, $__L_k) { return _lookup($__L__->impl, $__L_k);}), '-lookup', (function($__L__, $__L_k, $__L_not_found) { return _lookup($__L__->impl, $__L_k, $__L_not_found);}), '-contains?', (function($__L__, $__L_k) { return _contains_key_QMARK_($__L__->impl, $__L_k);}), '-disjoin', (function($__L_this, $__L_k) { if (_contains_key_QMARK_($__L_this->impl, $__L_k)) { return __GT_PersistentHashSet(_dissoc($__L_this->impl, $__L_k), $__L_this->_meta, null);} else { return $__L_this;}}), '-conj', (function($__L_this, $__L_v) { if (_contains_key_QMARK_($__L_this->impl, $__L_v)) { return $__L_this;} else { return __GT_PersistentHashSet(_assoc($__L_this->impl, $__L_v, $__L_v), $__L_this->_meta, null);}}), '-empty', (function($__L__) { return $GLOBALS['EMPTY_SET'];}), '-seq', (function($__L__) { if ((_count($__L__->impl) > 0)) { return map($GLOBALS['key'], _seq($__L__->impl));}}), '-equiv', (function($__L_this, $__L_other) { if (satisfies_QMARK_($GLOBALS['ISet'], $__L_other)) { if (\Clojure\Php\equals(_count($__L_this), _count($__L_other))) { return every_QMARK_((function($__L_p1__3043_SHARP_) use (&$__L_this, &$__L_other) { return _contains_QMARK_($__L_other, $__L_p1__3043_SHARP_);}), $__L_this);} else { return false;}} else { return false;}}), '-hash', (function($__L_this) { if ($__L_this->_hash) { return $__L_this->_hash;} else { $__L_h = reduce((function($__L_h, $__L_x) use (&$__L_this) { return ($__L_h + hash($__L_x));}), 0, $__L_this);
$__L_this->_hash = $__L_h;
return $__L_h;}}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return __GT_PersistentHashSet($__L__->impl, $__L_m, $__L__->_hash);}), '-reduce', (function($__L_this, $__L_f) { $__L_s = _seq($__L_this);
if ($__L_s) { return reduce($__L_f, \Clojure\Php\first($__L_s), \Clojure\Php\next_($__L_s));} else { return call_user_func($__L_f);}}), '-reduce-init', (function($__L_this, $__L_f, $__L_init) { return reduce($__L_f, $__L_init, _seq($__L_this));}), '-invoke', (function($__L_this, $__L_k) { if (_contains_QMARK_($__L_this, $__L_k)) { return $__L_k;}}), '-invoke', (function($__L_this, $__L_k, $__L_not_found) { if (_contains_QMARK_($__L_this, $__L_k)) { return $__L_k;} else { return $__L_not_found;}})));
function __GT_PersistentHashSet($__L_impl, $__L__meta, $__L__hash) {
return call_user_func('\Clojure\Php\createType', 'PersistentHashSet', $__L_impl, $__L__meta, $__L__hash);
}
$GLOBALS['EMPTY_SET'] = __GT_PersistentHashSet($GLOBALS['EMPTY_MAP'], null, null);
$EMPTY_SET = &$GLOBALS['EMPTY_SET'];
function hash_set($__L_keys) {
return reduce($GLOBALS['conj'], $GLOBALS['EMPTY_SET'], $__L_keys);
}
function set($__L_coll) {
return reduce($GLOBALS['conj'], $GLOBALS['EMPTY_SET'], $__L_coll);
}
function set_QMARK_($__L_x) {
return satisfies_QMARK_($GLOBALS['ISet'], $__L_x);
}
function union() {
return $GLOBALS['EMPTY_SET'];
}
function intersection($__L_s1) {
return $__L_s1;
}
function difference($__L_s1) {
return $__L_s1;
}
function subset_QMARK_($__L_s1, $__L_s2) {
if ((\Clojure\Php\count_($__L_s1) <= \Clojure\Php\count_($__L_s2))) { return every_QMARK_((function($__L_p1__3044_SHARP_) use (&$__L_s1, &$__L_s2) { return \Clojure\Php\contains($__L_s2, $__L_p1__3044_SHARP_);}), $__L_s1);} else { return false;}
}
function superset_QMARK_($__L_s1, $__L_s2) {
return subset_QMARK_($__L_s2, $__L_s1);
}
ns(\Clojure\Php\Sym::create('clojure.lang.transient'), 'Transient collections for efficient batch operations.

   Transients provide O(1) mutability during construction,
   then become persistent with persistent!.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$GLOBALS['BITS'] = 5;
$BITS = &$GLOBALS['BITS'];
$GLOBALS['WIDTH'] = 32;
$WIDTH = &$GLOBALS['WIDTH'];
$GLOBALS['MASK'] = 31;
$MASK = &$GLOBALS['MASK'];
$GLOBALS['EMPTY_TRANSIENT_VECTOR'] = null;
$EMPTY_TRANSIENT_VECTOR = &$GLOBALS['EMPTY_TRANSIENT_VECTOR'];
call_user_func('\Clojure\Php\defType', 'TransientVector', array('cnt', 'shift', 'root', 'tail', 'edit'), array('cnt', 'shift', 'root', 'tail', 'edit'), array('-conj!', (function($__L_this, $__L_val) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
if ((($__L_this->cnt - tail_off_tv($__L_this)) < $GLOBALS['WIDTH'])) { aset($__L_this->tail, ($__L_this->cnt & $GLOBALS['MASK']), $__L_val);
$__L_this->cnt = ($__L_this->cnt + 1);
return $__L_this;} else { $__L_new_tail = object_array($GLOBALS['WIDTH']);
aset($__L_new_tail, 0, $__L_val);
$__L_this->root = push_tail_tv($__L_this, $__L_this->shift, $__L_this->root, $__L_this->tail);
if ((unsigned_bit_shift_right($__L_this->cnt, $GLOBALS['BITS']) > (1 << $__L_this->shift))) { $__L_new_root = object_array($GLOBALS['WIDTH']);
aset($__L_new_root, 0, $__L_this->root);
aset($__L_new_root, 1, new_path_tv($__L_this->edit, $__L_this->shift, $__L_this->tail));
$__L_this->root = $__L_new_root;
$__L_this->shift = ($__L_this->shift + $GLOBALS['BITS']);
}
$__L_this->tail = $__L_new_tail;
$__L_this->cnt = ($__L_this->cnt + 1);
return $__L_this;}}), '-persistent!', (function($__L_this) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_this->edit = null;
$__L_trimmed_tail = object_array(($__L_this->cnt - tail_off_tv($__L_this)));
System::arraycopy($__L_this->tail, 0, $__L_trimmed_tail, 0, alength($__L_trimmed_tail));
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('persistent-vector'), \Clojure\Php\Kw::create('cnt'), $__L_this->cnt, \Clojure\Php\Kw::create('shift'), $__L_this->shift, \Clojure\Php\Kw::create('root'), $__L_this->root, \Clojure\Php\Kw::create('tail'), $__L_trimmed_tail);}), '-assoc!', (function($__L_this, $__L_k, $__L_v) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_i = ((int)$__L_k);
if ((($__L_i >= 0) ? ($__L_i < $__L_this->cnt) : false)) { if (($__L_i >= tail_off_tv($__L_this))) { aset($__L_this->tail, ($__L_i & $GLOBALS['MASK']), $__L_v);
return $__L_this;} else { $__L_this->root = do_assoc_tv($__L_this, $__L_this->shift, $__L_this->root, $__L_i, $__L_v);
return $__L_this;}} else { if (\Clojure\Php\equals($__L_i, $__L_this->cnt)) { return _conj_BANG_($__L_this, $__L_v);} else { if (\Clojure\Php\Kw::create('else')) { throw ex_info('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('index'), $__L_i, \Clojure\Php\Kw::create('count'), $__L_this->cnt));
}}}}), '-assoc-n!', (function($__L_this, $__L_i, $__L_val) { return _assoc_BANG_($__L_this, $__L_i, $__L_val);}), '-pop!', (function($__L_this) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
if (($__L_this->cnt === 0 || $__L_this->cnt === 0.0)) { throw ex_info('Can\'t pop empty vector', \Clojure\Php\hashMap());
} else { if (\Clojure\Php\equals(1, $__L_this->cnt)) { $__L_this->cnt = 0;
return $__L_this;} else { if ((($__L_this->cnt - tail_off_tv($__L_this)) > 1)) { $__L_this->cnt = ($__L_this->cnt - 1);
return $__L_this;} else { if (\Clojure\Php\Kw::create('else')) { $__L_new_tail = editable_array_for_tv($__L_this, ($__L_this->cnt - 2));
$__L_this->root = pop_tail_tv($__L_this, $__L_this->shift, $__L_this->root);
if ((($__L_this->shift > $GLOBALS['BITS']) ? (aget($__L_this->root, 1) === null) : false)) { $__L_this->root = aget($__L_this->root, 0);
$__L_this->shift = ($__L_this->shift - $GLOBALS['BITS']);
}
$__L_this->tail = $__L_new_tail;
$__L_this->cnt = ($__L_this->cnt - 1);
return $__L_this;}}}}}), '-count', (function($__L__) { return $__L__->cnt;}), '-nth', (function($__L_this, $__L_n) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
if ((($__L_n >= 0) ? ($__L_n < $__L_this->cnt) : false)) { return aget(array_for_tv($__L_this, $__L_n), ($__L_n & $GLOBALS['MASK']));} else { throw ex_info('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n, \Clojure\Php\Kw::create('count'), $__L_this->cnt));
}}), '-nth', (function($__L_this, $__L_n, $__L_not_found) { if ((($__L_n >= 0) ? ($__L_n < $__L_this->cnt) : false)) { return aget(array_for_tv($__L_this, $__L_n), ($__L_n & $GLOBALS['MASK']));} else { return $__L_not_found;}}), '-lookup', (function($__L_this, $__L_k) { if ((is_int($__L_k) || is_float($__L_k))) { return _nth($__L_this, $__L_k, null);}}), '-lookup', (function($__L_this, $__L_k, $__L_not_found) { if ((is_int($__L_k) || is_float($__L_k))) { return _nth($__L_this, $__L_k, $__L_not_found);} else { return $__L_not_found;}})));
function __GT_TransientVector($__L_cnt, $__L_shift, $__L_root, $__L_tail, $__L_edit) {
return call_user_func('\Clojure\Php\createType', 'TransientVector', $__L_cnt, $__L_shift, $__L_root, $__L_tail, $__L_edit);
}
function tail_off_tv($__L_tv) {
$__L_cnt = $__L_tv->cnt;
if (($__L_cnt < $GLOBALS['WIDTH'])) { return 0;} else { return (unsigned_bit_shift_right(($__L_cnt - 1), $GLOBALS['BITS']) << $GLOBALS['BITS']);}
}
function new_path_tv($__L_edit, $__L_level, $__L_node) {
if (($__L_level === 0 || $__L_level === 0.0)) { return $__L_node;} else { $__L_ret = object_array($GLOBALS['WIDTH']);
aset($__L_ret, 0, new_path_tv($__L_edit, ($__L_level - $GLOBALS['BITS']), $__L_node));
return $__L_ret;}
}
function push_tail_tv($__L_tv, $__L_level, $__L_parent, $__L_tail_node) {
$__L_subidx = (unsigned_bit_shift_right(($__L_tv->cnt - 1), $__L_level) & $GLOBALS['MASK']);
$__L_ret = aclone($__L_parent);
if (\Clojure\Php\equals($__L_level, $GLOBALS['BITS'])) { aset($__L_ret, $__L_subidx, $__L_tail_node);
} else { $__L_child = aget($__L_parent, $__L_subidx);
aset($__L_ret, $__L_subidx, ($__L_child ? push_tail_tv($__L_tv, ($__L_level - $GLOBALS['BITS']), $__L_child, $__L_tail_node) : new_path_tv($__L_tv->edit, ($__L_level - $GLOBALS['BITS']), $__L_tail_node)));
}
return $__L_ret;
}
function do_assoc_tv($__L_tv, $__L_level, $__L_node, $__L_i, $__L_val) {
$__L_ret = aclone($__L_node);
if (($__L_level === 0 || $__L_level === 0.0)) { aset($__L_ret, ($__L_i & $GLOBALS['MASK']), $__L_val);
return $__L_ret;} else { $__L_subidx = (unsigned_bit_shift_right($__L_i, $__L_level) & $GLOBALS['MASK']);
aset($__L_ret, $__L_subidx, do_assoc_tv($__L_tv, ($__L_level - $GLOBALS['BITS']), aget($__L_node, $__L_subidx), $__L_i, $__L_val));
return $__L_ret;}
}
function array_for_tv($__L_tv, $__L_i) {
if (($__L_i >= tail_off_tv($__L_tv))) { return $__L_tv->tail;} else { $__L_node = $__L_tv->root;
$__L_level = $__L_tv->shift;
 while(true) { if (($__L_level > 0)) { $__recur_0 = aget($__L_node, (unsigned_bit_shift_right($__L_i, $__L_level) & $GLOBALS['MASK'])); $__recur_1 = ($__L_level - $GLOBALS['BITS']); $__L_node = $__recur_0; $__L_level = $__recur_1; continue;} else { return $__L_node;} break; }
}
}
function editable_array_for_tv($__L_tv, $__L_i) {
return array_for_tv($__L_tv, $__L_i);
}
function pop_tail_tv($__L_tv, $__L_level, $__L_node) {
$__L_subidx = (unsigned_bit_shift_right(($__L_tv->cnt - 2), $__L_level) & $GLOBALS['MASK']);
if (($__L_level > $GLOBALS['BITS'])) { $__L_new_child = pop_tail_tv($__L_tv, ($__L_level - $GLOBALS['BITS']), aget($__L_node, $__L_subidx));
if ((($__L_new_child === null) ? ($__L_subidx === 0 || $__L_subidx === 0.0) : false)) { return null;} else { $__L_ret = aclone($__L_node);
aset($__L_ret, $__L_subidx, $__L_new_child);
return $__L_ret;}} else { if (($__L_subidx === 0 || $__L_subidx === 0.0)) { return null;} else { if (\Clojure\Php\Kw::create('else')) { $__L_ret = aclone($__L_node);
aset($__L_ret, $__L_subidx, null);
return $__L_ret;}}}
}
call_user_func('\Clojure\Php\defType', 'TransientHashMap', array('cnt', 'root', 'has-nil', 'nil-val', 'edit'), array('cnt', 'root', 'has-nil', 'nil-val', 'edit'), array('-conj!', (function($__L_this, $__L_entry) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
if (($__L_entry instanceof \Clojure\Php\Vec)) { return _assoc_BANG_($__L_this, \Clojure\Php\nth($__L_entry, 0), \Clojure\Php\nth($__L_entry, 1));} else { return reduce((function($__L_m, $__L_e) use (&$__L_this, &$__L_entry) { return _assoc_BANG_($__L_m, key($__L_e), val($__L_e));}), $__L_this, $__L_entry);}}), '-persistent!', (function($__L_this) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_this->edit = null;
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('persistent-hash-map'), \Clojure\Php\Kw::create('cnt'), $__L_this->cnt, \Clojure\Php\Kw::create('root'), $__L_this->root, \Clojure\Php\Kw::create('has-nil'), $__L_this->has_nil, \Clojure\Php\Kw::create('nil-val'), $__L_this->nil_val);}), '-assoc!', (function($__L_this, $__L_k, $__L_v) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
if (($__L_k === null)) { if ($__L_this->has_nil) { null;
} else { $__L_this->cnt = ($__L_this->cnt + 1);
}
$__L_this->has_nil = true;
$__L_this->nil_val = $__L_v;
return $__L_this;} else { if (contains_key_QMARK_($__L_this, $__L_k)) { null;
} else { $__L_this->cnt = ($__L_this->cnt + 1);
}
$__L_this->root = assoc_node($__L_this->root, $__L_k, $__L_v);
return $__L_this;}}), '-dissoc!', (function($__L_this, $__L_k) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
if (($__L_k === null)) { if ($__L_this->has_nil) { $__L_this->has_nil = false;
$__L_this->nil_val = null;
$__L_this->cnt = ($__L_this->cnt - 1);
return $__L_this;} else { return $__L_this;}} else { if (contains_key_QMARK_($__L_this, $__L_k)) { $__L_this->cnt = ($__L_this->cnt - 1);
return $__L_this->root = dissoc_node($__L_this->root, $__L_k);}}}), '-count', (function($__L__) { return $__L__->cnt;}), '-lookup', (function($__L__, $__L_k) { if (($__L_k === null)) { if ($__L__->has_nil) { return $__L__->nil_val;}} else { return get_node($__L__->root, $__L_k);}}), '-lookup', (function($__L__, $__L_k, $__L_not_found) { if (($__L_k === null)) { if ($__L__->has_nil) { return $__L__->nil_val;} else { return $__L_not_found;}} else { return get_node($__L__->root, $__L_k, $__L_not_found);}})));
function __GT_TransientHashMap($__L_cnt, $__L_root, $__L_has_nil, $__L_nil_val, $__L_edit) {
return call_user_func('\Clojure\Php\createType', 'TransientHashMap', $__L_cnt, $__L_root, $__L_has_nil, $__L_nil_val, $__L_edit);
}
function contains_key_QMARK_($__L_tm, $__L_k) {
return (!(_lookup($__L_tm, $__L_k, \Clojure\Php\Kw::createNs('user', 'not-found')) === \Clojure\Php\Kw::createNs('user', 'not-found')));
}
function assoc_node($__L_root, $__L_k, $__L_v) {
return \Clojure\Php\assoc((call_user_func(function() { $__L_or__3045 = $__L_root; if ($__L_or__3045) { return $__L_or__3045;} else { return \Clojure\Php\hashMap();} })), $__L_k, $__L_v);
}
function dissoc_node($__L_root, $__L_k) {
return \Clojure\Php\dissoc($__L_root, $__L_k);
}
function get_node($__L_root, $__L_k) {
return \Clojure\Php\get_($__L_root, $__L_k);
}
call_user_func('\Clojure\Php\defType', 'TransientHashSet', array('impl', 'edit'), array('impl', 'edit'), array('-conj!', (function($__L_this, $__L_v) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_this->impl = _assoc_BANG_($__L_this->impl, $__L_v, $__L_v);
return $__L_this;}), '-persistent!', (function($__L_this) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_this->edit = null;
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('persistent-hash-set'), \Clojure\Php\Kw::create('impl'), _persistent_BANG_($__L_this->impl));}), '-disjoin!', (function($__L_this, $__L_k) { if ($__L_this->edit) { null;
} else { throw ex_info('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_this->impl = _dissoc_BANG_($__L_this->impl, $__L_k);
return $__L_this;}), '-count', (function($__L__) { return _count($__L__->impl);}), '-lookup', (function($__L__, $__L_k) { return _lookup($__L__->impl, $__L_k);}), '-lookup', (function($__L__, $__L_k, $__L_not_found) { return _lookup($__L__->impl, $__L_k, $__L_not_found);})));
function __GT_TransientHashSet($__L_impl, $__L_edit) {
return call_user_func('\Clojure\Php\createType', 'TransientHashSet', $__L_impl, $__L_edit);
}
function transient_vector($__L_pv) {
return __GT_TransientVector(\Clojure\Php\count_($__L_pv), $GLOBALS['BITS'], object_array($GLOBALS['WIDTH']), object_array($GLOBALS['WIDTH']), object_array(1));
}
function transient_hash_map($__L_pm) {
return __GT_TransientHashMap(\Clojure\Php\count_($__L_pm), into(\Clojure\Php\hashMap(), $__L_pm), false, null, object_array(1));
}
function transient_hash_set($__L_ps) {
$__L_tm = transient_hash_map(into(\Clojure\Php\hashMap(), map((function($__L_x) use (&$__L_ps) { return \Clojure\Php\vec($__L_x, $__L_x);}), $__L_ps)));
return __GT_TransientHashSet($__L_tm, object_array(1));
}
function transient($__L_coll) {
if (($__L_coll instanceof \Clojure\Php\Vec)) { return transient_vector($__L_coll);} else { if (($__L_coll instanceof \Clojure\Php\Map)) { return transient_hash_map($__L_coll);} else { if (($__L_coll instanceof \Clojure\Php\Set)) { return transient_hash_set($__L_coll);} else { if (\Clojure\Php\Kw::create('else')) { throw ex_info('Can\'t create transient from this type', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), type($__L_coll)));
}}}}
}
function persistent_BANG_($__L_tcoll) {
return _persistent_BANG_($__L_tcoll);
}
function conj_BANG_($__L_tcoll) {
return $__L_tcoll;
}
function assoc_BANG_($__L_tcoll, $__L_k, $__L_v) {
return _assoc_BANG_($__L_tcoll, $__L_k, $__L_v);
}
function dissoc_BANG_($__L_tcoll, $__L_k) {
return _dissoc_BANG_($__L_tcoll, $__L_k);
}
function pop_BANG_($__L_tcoll) {
return _pop_BANG_($__L_tcoll);
}
function disj_BANG_($__L_tset, $__L_k) {
return _disjoin_BANG_($__L_tset, $__L_k);
}
ns(\Clojure\Php\Sym::create('clojure.lang.string'), 'String operations using platform primitives.

   These functions work with strings on any platform.
   The compiler emits platform-appropriate string operations.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
function blank_QMARK_($__L_s) {
$__L_or__3047 = ($__L_s === null);
if ($__L_or__3047) { return $__L_or__3047;} else { $__L_or__3048 = \Clojure\Php\equals('', $__L_s);
if ($__L_or__3048) { return $__L_or__3048;} else { return every_QMARK_((function($__L_p1__3046_SHARP_) use (&$__L_or__3048, &$__L_or__3047, &$__L_s) { return Character::isWhitespace($__L_p1__3046_SHARP_);}), $__L_s);}}
}
function join($__L_coll) {
return apply($GLOBALS['str'], $__L_coll);
}
function split($__L_s, $__L_re) {
return \Vec::create($__L_s->split(((string)$__L_re)));
}
function split_lines($__L_s) {
return split($__L_s, '\r?\n');
}
function trim($__L_s) {
return $__L_s->trim();
}
function triml($__L_s) {
return $__L_s->replaceFirst('^\s+', '');
}
function trimr($__L_s) {
return $__L_s->replaceFirst('\s+$', '');
}
function trim_newline($__L_s) {
$__L_i = \Clojure\Php\count_($__L_s);
 while(true) { if (($__L_i === 0 || $__L_i === 0.0)) { return '';} else { $__L_ch = \Clojure\Php\nth($__L_s, ($__L_i - 1));
if ((call_user_func(function() { $__L_or__3049 = \Clojure\Php\equals($__L_ch, \newline); if ($__L_or__3049) { return $__L_or__3049;} else { return \Clojure\Php\equals($__L_ch, \return);} }))) { $__recur_0 = ($__L_i - 1); $__L_i = $__recur_0; continue;} else { return subs($__L_s, 0, $__L_i);}} break; }

}
function upper_case($__L_s) {
return $__L_s->toUpperCase();
}
function lower_case($__L_s) {
return $__L_s->toLowerCase();
}
function capitalize($__L_s) {
if ((\Clojure\Php\count_($__L_s) < 2)) { return upper_case($__L_s);} else { return \Clojure\Php\str_(upper_case(subs($__L_s, 0, 1)), lower_case(subs($__L_s, 1)));}
}
function starts_with_QMARK_($__L_s, $__L_substr) {
return $__L_s->startsWith($__L_substr);
}
function ends_with_QMARK_($__L_s, $__L_substr) {
return $__L_s->endsWith($__L_substr);
}
function includes_QMARK_($__L_s, $__L_substr) {
return $__L_s->contains($__L_substr);
}
function index_of($__L_s, $__L_value) {
$__L_result = $__L_s->indexOf($__L_value);
if (($__L_result >= 0)) { return $__L_result;}
}
function last_index_of($__L_s, $__L_value) {
$__L_result = $__L_s->lastIndexOf($__L_value);
if (($__L_result >= 0)) { return $__L_result;}
}
function replace($__L_s, $__L_match, $__L_replacement) {
if (is_string($__L_match)) { return $__L_s->replace($__L_match, $__L_replacement);} else { return $__L_s->replaceAll(((string)$__L_match), $__L_replacement);}
}
function replace_first($__L_s, $__L_match, $__L_replacement) {
if (is_string($__L_match)) { return $__L_s->replaceFirst(java.util.regex.Pattern::quote($__L_match), $__L_replacement);} else { return $__L_s->replaceFirst(((string)$__L_match), $__L_replacement);}
}
function subs($__L_s, $__L_start) {
return $__L_s->substring($__L_start);
}
function reverse($__L_s) {
return \Clojure\Php\str_((new StringBuilder($__L_s)), $GLOBALS['_DOT_reverse']);
}
function escape($__L_s, $__L_cmap) {
$__L_sb = (new StringBuilder(\Clojure\Php\count_($__L_s)));
$__L_s3050 = \Clojure\Php\seq($__L_s);
 while(true) { if ($__L_s3050) { $__L_ch = \Clojure\Php\first($__L_s3050);
$__L_if_let__3051 = \Clojure\Php\get_($__L_cmap, $__L_ch);
if ($__L_if_let__3051) { $__L_replacement = $__L_if_let__3051;
$__L_sb->append($__L_replacement);
} else { $__L_sb->append($__L_ch);
}
$__recur_0 = \Clojure\Php\next_($__L_s3050); $__L_s3050 = $__recur_0; continue;}
 break; }
return ((string)$__L_sb);
}
function re_quote_replacement($__L_replacement) {
return java.util.regex.Matcher::quoteReplacement($__L_replacement);
}
function re_pattern($__L_s) {
return java.util.regex.Pattern::compile($__L_s);
}
function re_matcher($__L_re, $__L_s) {
return $__L_re->matcher($__L_s);
}
function re_find($__L_m) {
if ($__L_m->find()) { $__L_gc = $__L_m->groupCount();
if (($__L_gc === 0 || $__L_gc === 0.0)) { return $__L_m->group();} else { $__L_ret = \Clojure\Php\vec();
$__L_i = 0;
 while(true) { if (($__L_i <= $__L_gc)) { $__recur_0 = \Clojure\Php\conj($__L_ret, $__L_m->group($__L_i)); $__recur_1 = ($__L_i + 1); $__L_ret = $__recur_0; $__L_i = $__recur_1; continue;} else { return $__L_ret;} break; }
}}
}
function re_matches($__L_re, $__L_s) {
$__L_m = re_matcher($__L_re, $__L_s);
if ($__L_m->matches()) { $__L_gc = $__L_m->groupCount();
if (($__L_gc === 0 || $__L_gc === 0.0)) { return $__L_m->group();} else { $__L_ret = \Clojure\Php\vec();
$__L_i = 0;
 while(true) { if (($__L_i <= $__L_gc)) { $__recur_0 = \Clojure\Php\conj($__L_ret, $__L_m->group($__L_i)); $__recur_1 = ($__L_i + 1); $__L_ret = $__recur_0; $__L_i = $__recur_1; continue;} else { return $__L_ret;} break; }
}}
}
function re_seq($__L_re, $__L_s) {
$__L_m = re_matcher($__L_re, $__L_s);
$__L_step = (function() use (&$__L_step, &$__L_m, &$__L_re, &$__L_s) { return lazy_seq((call_user_func(function() { $__L_when_let__3052 = re_find($__L_m); if ($__L_when_let__3052) { $__L_match = $__L_when_let__3052;
return \Clojure\Php\cons($__L_match, call_user_func($__L_step));} })));});
return call_user_func($__L_step);
}
function re_groups($__L_m) {
$__L_gc = $__L_m->groupCount();
if (($__L_gc === 0 || $__L_gc === 0.0)) { return $__L_m->group();} else { $__L_ret = \Clojure\Php\vec();
$__L_i = 0;
 while(true) { if (($__L_i <= $__L_gc)) { $__recur_0 = \Clojure\Php\conj($__L_ret, $__L_m->group($__L_i)); $__recur_1 = ($__L_i + 1); $__L_ret = $__recur_0; $__L_i = $__recur_1; continue;} else { return $__L_ret;} break; }
}
}
function pad($__L_s, $__L_n, $__L_pad_str, $__L_left_QMARK_) {
$__L_s = ((string)$__L_s);
$__L_len = \Clojure\Php\count_($__L_s);
$__L_pad_len = ($__L_n - $__L_len);
if (($__L_pad_len <= 0)) { return $__L_s;} else { $__L_padding = (call_user_func(function() { $__L_acc = ''; $__L_remaining = $__L_pad_len;  while(true) { if (($__L_remaining === 0 || $__L_remaining === 0.0)) { $__L_acc;
} else { $__L_chunk = subs($__L_pad_str, 0, min(\Clojure\Php\count_($__L_pad_str), $__L_remaining));
$__recur_0 = \Clojure\Php\str_($__L_acc, $__L_chunk); $__recur_1 = ($__L_remaining - \Clojure\Php\count_($__L_chunk)); $__L_acc = $__recur_0; $__L_remaining = $__recur_1; continue;}
 break; } return $__L_acc; }));
if ($__L_left_QMARK_) { return \Clojure\Php\str_($__L_padding, $__L_s);} else { return \Clojure\Php\str_($__L_s, $__L_padding);}}
}
function pad_left($__L_s, $__L_n) {
return pad_left($__L_s, $__L_n, ' ');
}
function pad_right($__L_s, $__L_n) {
return pad_right($__L_s, $__L_n, ' ');
}
function char($__L_x) {
if (char_QMARK_($__L_x)) { return $__L_x;} else { if ((is_int($__L_x) || is_float($__L_x))) { return char($__L_x);} else { if (is_string($__L_x)) { if (\Clojure\Php\equals(1, \Clojure\Php\count_($__L_x))) { return \Clojure\Php\first($__L_x);} else { throw ex_info('String must have exactly one character', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('s'), $__L_x));
}} else { if (\Clojure\Php\Kw::create('else')) { throw ex_info('Cannot coerce to char', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('x'), $__L_x));
}}}}
}
function char_QMARK_($__L_x) {
return instance_QMARK_($GLOBALS['Character'], $__L_x);
}
ns(\Clojure\Php\Sym::create('clojure.lang.var'), 'Var - Clojure\'s mechanism for dynamic binding.

   Vars provide a mechanism to refer to a mutable storage location
   that can be dynamically rebound on a per-thread basis.

   In non-threaded environments (PHP, single-threaded JS), dynamic
   binding uses a simple binding stack.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
call_user_func('\Clojure\Php\defType', 'BindingFrame', array('bindings', 'prev'), array(), array());
function __GT_BindingFrame($__L_bindings, $__L_prev) {
return call_user_func('\Clojure\Php\createType', 'BindingFrame', $__L_bindings, $__L_prev);
}
$GLOBALS['_STAR_binding_frame_STAR_'] = null;
$_STAR_binding_frame_STAR_ = &$GLOBALS['_STAR_binding_frame_STAR_'];
call_user_func('\Clojure\Php\defType', 'Var', array('sym', 'root', 'dynamic?', '_meta', 'validator'), array('root', 'dynamic?', 'validator'), array('-name', (function($__L__) { return _name($__L__->sym);}), '-namespace', (function($__L__) { return _namespace($__L__->sym);}), '-deref', (function($__L_this) {  while(true) { if ($__L_this->dynamic_QMARK_) { $__L_if_let__3053 = $GLOBALS['_STAR_binding_frame_STAR_'];
if ($__L_if_let__3053) { $__L_frame = $__L_if_let__3053;
$__L_f = $__L_frame;
 while(true) { if ($__L_f) { $__L_if_let__3054 = \Clojure\Php\get_($__L_f->bindings, $__L_this);
if ($__L_if_let__3054) { $__L_v = $__L_if_let__3054;
return $__L_v;} else { $__recur_0 = $__L_f->prev; $__L_f = $__recur_0; continue;}} else { return $__L_this->root;} break; }
} else { return $__L_this->root;}} else { return $__L_this->root;} break; }}), '-reset!', (function($__L_this, $__L_v) { if ($__L_this->validator) { if (call_user_func($__L_this->validator, $__L_v)) { null;
} else { throw ex_info('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L_this->sym, \Clojure\Php\Kw::create('val'), $__L_v));
}
}
$__L_this->root = $__L_v;
return $__L_v;}), '-set-validator!', (function($__L__, $__L_vf) { if ($__L_vf) { if (call_user_func($__L_vf, $__L__->root)) { null;
} else { throw ex_info('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L__->sym, \Clojure\Php\Kw::create('val'), $__L__->root));
}
}
return $__L__->validator = $__L_vf;}), '-get-validator', (function($__L__) { return $__L__->validator;}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { throw ex_info('Use alter-meta! for Var metadata', \Clojure\Php\hashMap());
}), '-invoke', (function($__L_this) { $__L_f = _deref($__L_this);
return call_user_func($__L_f);}), '-invoke', (function($__L_this, $__L_a) { $__L_f = _deref($__L_this);
return call_user_func($__L_f, $__L_a);}), '-invoke', (function($__L_this, $__L_a, $__L_b) { $__L_f = _deref($__L_this);
return call_user_func($__L_f, $__L_a, $__L_b);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c) { $__L_f = _deref($__L_this);
return call_user_func($__L_f, $__L_a, $__L_b, $__L_c);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d) { $__L_f = _deref($__L_this);
return call_user_func($__L_f, $__L_a, $__L_b, $__L_c, $__L_d);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e) { $__L_f = _deref($__L_this);
return call_user_func($__L_f, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f) { $__L_fn_PRIME_ = _deref($__L_this);
return call_user_func($__L_fn_PRIME_, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g) { $__L_fn_PRIME_ = _deref($__L_this);
return call_user_func($__L_fn_PRIME_, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h) { $__L_fn_PRIME_ = _deref($__L_this);
return call_user_func($__L_fn_PRIME_, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h, $__L_i) { $__L_fn_PRIME_ = _deref($__L_this);
return call_user_func($__L_fn_PRIME_, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h, $__L_i);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h, $__L_i, $__L_j) { $__L_fn_PRIME_ = _deref($__L_this);
return call_user_func($__L_fn_PRIME_, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h, $__L_i, $__L_j);}), '-apply', (function($__L_this, $__L_args) { return apply(_deref($__L_this), $__L_args);}), '-equiv', (function($__L_this, $__L_other) { return ($__L_this === $__L_other);}), '-hash', (function($__L_this) { return hash($__L_this->sym);})));
function __GT_Var($__L_sym, $__L_root, $__L_dynamic_QMARK_, $__L__meta, $__L_validator) {
return call_user_func('\Clojure\Php\createType', 'Var', $__L_sym, $__L_root, $__L_dynamic_QMARK_, $__L__meta, $__L_validator);
}
call_user_func('\Clojure\Php\defType', 'Unbound', array('var-sym'), array(), array('-invoke', (function($__L__) { throw ex_info('Attempting to call unbound fn', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L__->var_sym));
}), '-invoke', (function($__L__, $__L__a) { throw ex_info('Attempting to call unbound fn', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L__->var_sym));
}), '-invoke', (function($__L__, $__L__a, $__L__b) { throw ex_info('Attempting to call unbound fn', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L__->var_sym));
}), '-apply', (function($__L__, $__L__args) { throw ex_info('Attempting to call unbound fn', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L__->var_sym));
})));
function __GT_Unbound($__L_var_sym) {
return call_user_func('\Clojure\Php\createType', 'Unbound', $__L_var_sym);
}
function unbound_QMARK_($__L_v) {
return instance_QMARK_($GLOBALS['Unbound'], _deref($__L_v));
}
function var_QMARK_($__L_x) {
return instance_QMARK_($GLOBALS['Var'], $__L_x);
}
function bound_QMARK_($__L_v) {
return (!unbound_QMARK_($__L_v));
}
function dynamic_QMARK_($__L_v) {
return $__L_v->dynamic_QMARK_;
}
function set_dynamic_BANG_($__L_v) {
$__L_v->dynamic_QMARK_ = true;
return $__L_v;
}
function alter_var_root($__L_v, $__L_f, $__L_args) {
return _reset_BANG_($__L_v, apply($__L_f, _deref($__L_v), $__L_args));
}
function push_thread_bindings($__L_bindings) {
$__L_s3055 = \Clojure\Php\seq($__L_bindings);
 while(true) { if ($__L_s3055) { $__L___dest_8 = \Clojure\Php\first($__L_s3055);
$__L_v = \Clojure\Php\nth($__L___dest_8, 0);
if (dynamic_QMARK_($__L_v)) { null;
} else { throw ex_info('Can\'t dynamically bind non-dynamic var', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L_v));
}
$__recur_0 = \Clojure\Php\next_($__L_s3055); $__L_s3055 = $__recur_0; continue;}
 break; }
return $GLOBALS['_STAR_binding_frame_STAR_'] = __GT_BindingFrame($__L_bindings, $GLOBALS['_STAR_binding_frame_STAR_']);
}
function pop_thread_bindings() {
if ($GLOBALS['_STAR_binding_frame_STAR_']) { null;
} else { throw ex_info('No bindings to pop', \Clojure\Php\hashMap());
}
return $GLOBALS['_STAR_binding_frame_STAR_'] = $GLOBALS['_STAR_binding_frame_STAR_']->prev;
}
function get_thread_bindings() {
$__L_f = $GLOBALS['_STAR_binding_frame_STAR_'];
$__L_result = \Clojure\Php\hashMap();
 while(true) { if ($__L_f) { $__recur_0 = $__L_f->prev; $__recur_1 = merge($__L_result, $__L_f->bindings); $__L_f = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }

}
function create_var($__L_sym) {
return __GT_Var($__L_sym, __GT_Unbound($__L_sym), false, null, null);
}
function intern($__L_ns, $__L_sym) {
return create_var(symbol((is_string($__L_ns) ? $__L_ns : $__L_ns->name()), (is_string($__L_sym) ? $__L_sym : $__L_sym->name())));
}
ns(\Clojure\Php\Sym::create('clojure.lang.namespace'), 'Namespace - Clojure\'s mechanism for organizing code.

   Namespaces provide:
   - A mapping from symbols to vars
   - Alias support for referring to other namespaces
   - Import/refer functionality', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')], [\Clojure\Php\Sym::create('clojure.lang.var'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('v')]));
$GLOBALS['namespaces'] = atom(\Clojure\Php\hashMap());
$namespaces = &$GLOBALS['namespaces'];
call_user_func('\Clojure\Php\defType', 'Namespace', array('name', 'mappings', 'aliases', '_meta'), array(), array('-name', (function($__L__) { return _name($__L__->name);}), '-namespace', (function($__L__) { return null;}), '-lookup', (function($__L__, $__L_sym) { return \Clojure\Php\get_(deref($__L__->mappings), $__L_sym);}), '-lookup', (function($__L__, $__L_sym, $__L_not_found) { return \Clojure\Php\get_(deref($__L__->mappings), $__L_sym, $__L_not_found);}), '-meta', (function($__L__) { return deref($__L__->_meta);}), '-with-meta', (function($__L__, $__L_m) { reset_BANG_($__L__->_meta, $__L_m);
return $__L__;}), '-equiv', (function($__L__, $__L_other) { if (instance_QMARK_($GLOBALS['Namespace'], $__L_other)) { return \Clojure\Php\equals($__L__->name, $__L_other->name);} else { return false;}}), '-hash', (function($__L__) { return hash($__L__->name);})));
function __GT_Namespace($__L_name, $__L_mappings, $__L_aliases, $__L__meta) {
return call_user_func('\Clojure\Php\createType', 'Namespace', $__L_name, $__L_mappings, $__L_aliases, $__L__meta);
}
function namespace_QMARK_($__L_x) {
return instance_QMARK_($GLOBALS['Namespace'], $__L_x);
}
function find_ns($__L_sym) {
return \Clojure\Php\get_(deref($GLOBALS['namespaces']), (($__L_sym instanceof \Clojure\Php\Sym) ? $__L_sym : symbol((is_string($__L_sym) ? $__L_sym : $__L_sym->name()))));
}
function create_ns($__L_sym) {
$__L_s = (($__L_sym instanceof \Clojure\Php\Sym) ? $__L_sym : symbol((is_string($__L_sym) ? $__L_sym : $__L_sym->name())));
$__L_if_let__3056 = find_ns($__L_s);
if ($__L_if_let__3056) { $__L_ns = $__L_if_let__3056;
return $__L_ns;} else { $__L_ns = __GT_Namespace($__L_s, atom(\Clojure\Php\hashMap()), atom(\Clojure\Php\hashMap()), atom(null));
swap_BANG_($GLOBALS['namespaces'], $GLOBALS['assoc'], $__L_s, $__L_ns);
return $__L_ns;}
}
function remove_ns($__L_sym) {
$__L_s = (($__L_sym instanceof \Clojure\Php\Sym) ? $__L_sym : symbol((is_string($__L_sym) ? $__L_sym : $__L_sym->name())));
if (\Clojure\Php\equals($__L_s, \Clojure\Php\Sym::create('clojure.core'))) { return null;} else { return swap_BANG_($GLOBALS['namespaces'], $GLOBALS['dissoc'], $__L_s);}
}
function all_ns() {
return vals(deref($GLOBALS['namespaces']));
}
function the_ns($__L_x) {
if (namespace_QMARK_($__L_x)) { return $__L_x;} else { $__L_or__3057 = find_ns($__L_x);
if ($__L_or__3057) { return $__L_or__3057;} else { throw ex_info('No namespace found', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('ns'), $__L_x));
}}
}
function ns_name($__L_ns) {
return the_ns($__L_ns)->name;
}
function ns_map($__L_ns) {
return deref(the_ns($__L_ns)->mappings);
}
function ns_aliases($__L_ns) {
return deref(the_ns($__L_ns)->aliases);
}
function ns_intern($__L_ns, $__L_sym) {
$__L_ns = the_ns($__L_ns);
$__L_mappings = $__L_ns->mappings;
$__L_if_let__3058 = \Clojure\Php\get_(deref($__L_mappings), $__L_sym);
if ($__L_if_let__3058) { $__L_v = $__L_if_let__3058;
if ((var_QMARK_($__L_v) ? \Clojure\Php\equals(_namespace($__L_v->sym), _name($__L_ns->name)) : false)) { return $__L_v;} else { $__L_new_var = create_var(symbol(_name($__L_ns->name), _name($__L_sym)));
swap_BANG_($__L_mappings, $GLOBALS['assoc'], $__L_sym, $__L_new_var);
return $__L_new_var;}} else { $__L_new_var = create_var(symbol(_name($__L_ns->name), _name($__L_sym)));
swap_BANG_($__L_mappings, $GLOBALS['assoc'], $__L_sym, $__L_new_var);
return $__L_new_var;}
}
function ns_resolve($__L_ns, $__L_sym) {
return ns_resolve($__L_ns, null, $__L_sym);
}
function ns_refer($__L_ns, $__L_sym, $__L_var) {
$__L_ns = the_ns($__L_ns);
return swap_BANG_($__L_ns->mappings, $GLOBALS['assoc'], $__L_sym, $__L_var);
}
function ns_unmap($__L_ns, $__L_sym) {
$__L_ns = the_ns($__L_ns);
return swap_BANG_($__L_ns->mappings, $GLOBALS['dissoc'], $__L_sym);
}
function ns_alias($__L_ns, $__L_alias, $__L_ns_sym) {
$__L_ns = the_ns($__L_ns);
$__L_target_ns = the_ns($__L_ns_sym);
return swap_BANG_($__L_ns->aliases, $GLOBALS['assoc'], $__L_alias, $__L_target_ns);
}
function ns_unalias($__L_ns, $__L_sym) {
$__L_ns = the_ns($__L_ns);
return swap_BANG_($__L_ns->aliases, $GLOBALS['dissoc'], $__L_sym);
}
function refer($__L_ns_sym, $__L_filters) {
$__L_ns = the_ns($__L_ns_sym);
$__L_filter_map = apply($GLOBALS['hash_map'], $__L_filters);
$__L_only = \Clojure\Php\get_($__L_filter_map, \Clojure\Php\Kw::create('only'));
$__L_exclude = set(\Clojure\Php\get_($__L_filter_map, \Clojure\Php\Kw::create('exclude')));
$__L_rename = \Clojure\Php\get_($__L_filter_map, \Clojure\Php\Kw::create('rename'));
$__L_s3062 = \Clojure\Php\seq(deref($__L_ns->mappings));
 while(true) { if ($__L_s3062) { $__L___dest_9 = \Clojure\Php\first($__L_s3062);
$__L_sym = \Clojure\Php\nth($__L___dest_9, 0);
$__L_var = \Clojure\Php\nth($__L___dest_9, 1);
if (((call_user_func(function() { $__L_or__3063 = ($__L_only === null); if ($__L_or__3063) { return $__L_or__3063;} else { return \Clojure\Php\contains(set($__L_only), $__L_sym);} })) ? (!\Clojure\Php\contains($__L_exclude, $__L_sym)) : false)) { $__L_sym_PRIME_ = \Clojure\Php\get_($__L_rename, $__L_sym, $__L_sym);
ns_refer($GLOBALS['_STAR_ns_STAR_'], $__L_sym_PRIME_, $__L_var);
}
$__recur_0 = \Clojure\Php\next_($__L_s3062); $__L_s3062 = $__recur_0; continue;} break; }

}
function init_core_ns_BANG_() {
return create_ns(\Clojure\Php\Sym::create('clojure.core'));
}
ns(\Clojure\Php\Sym::create('clojure.lang.multifn'), 'MultiFn - Clojure\'s multimethod implementation.

   Multimethods provide runtime polymorphic dispatch based on
   an arbitrary dispatch function.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$GLOBALS['global_hierarchy'] = atom(\Clojure\Php\hashMap(\Clojure\Php\Kw::create('parents'), \Clojure\Php\hashMap(), \Clojure\Php\Kw::create('descendants'), \Clojure\Php\hashMap(), \Clojure\Php\Kw::create('ancestors'), \Clojure\Php\hashMap()));
$global_hierarchy = &$GLOBALS['global_hierarchy'];
function make_hierarchy() {
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('parents'), \Clojure\Php\hashMap(), \Clojure\Php\Kw::create('descendants'), \Clojure\Php\hashMap(), \Clojure\Php\Kw::create('ancestors'), \Clojure\Php\hashMap());
}
function derive_impl($__L_h, $__L_tag, $__L_parent) {
$__L_tp = call_user_func(\Clojure\Php\Kw::create('parents'), $__L_h);
$__L_td = call_user_func(\Clojure\Php\Kw::create('descendants'), $__L_h);
$__L_ta = call_user_func(\Clojure\Php\Kw::create('ancestors'), $__L_h);
if (\Clojure\Php\contains(\Clojure\Php\get_($__L_ta, $__L_tag), $__L_parent)) { $__L_h;
}
if (\Clojure\Php\contains(\Clojure\Php\get_($__L_ta, $__L_parent), $__L_tag)) { throw ex_info('Cyclic derivation', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('tag'), $__L_tag, \Clojure\Php\Kw::create('parent'), $__L_parent));
}
$__L_new_parents = update($__L_tp, $__L_tag, fnil($GLOBALS['conj'], \Clojure\Php\hashSet()), $__L_parent);
$__L_new_ancestors = reduce((function($__L_ta_PRIME_, $__L___dest_10) use (&$__L_tag, &$__L_parent, &$__L_td, &$__L_new_parents, &$__L_tp, &$__L_h, &$__L_ta) { $__L___dest_11 = $__L___dest_10;
$__L_t = \Clojure\Php\nth($__L___dest_11, 0);
$__L_ancestors = \Clojure\Php\nth($__L___dest_11, 1);
if (\Clojure\Php\contains($__L_ancestors, $__L_tag)) { return update($__L_ta_PRIME_, $__L_t, $GLOBALS['into'], \Clojure\Php\conj(\Clojure\Php\get_($__L_ta, $__L_parent, \Clojure\Php\hashSet()), $__L_parent));} else { return $__L_ta_PRIME_;}}), update($__L_ta, $__L_tag, fnil($GLOBALS['into'], \Clojure\Php\hashSet()), \Clojure\Php\conj(\Clojure\Php\get_($__L_ta, $__L_parent, \Clojure\Php\hashSet()), $__L_parent)), $__L_ta);
$__L_new_descendants = reduce((function($__L_td_PRIME_, $__L___dest_12) use (&$__L_tag, &$__L_new_ancestors, &$__L_parent, &$__L_td, &$__L_new_parents, &$__L_tp, &$__L_h, &$__L_ta) { $__L___dest_13 = $__L___dest_12;
$__L_t = \Clojure\Php\nth($__L___dest_13, 0);
$__L_descendants = \Clojure\Php\nth($__L___dest_13, 1);
if (\Clojure\Php\contains($__L_descendants, $__L_parent)) { return update($__L_td_PRIME_, $__L_t, $GLOBALS['into'], \Clojure\Php\conj(\Clojure\Php\get_($__L_td, $__L_tag, \Clojure\Php\hashSet()), $__L_tag));} else { return $__L_td_PRIME_;}}), update($__L_td, $__L_parent, fnil($GLOBALS['into'], \Clojure\Php\hashSet()), \Clojure\Php\conj(\Clojure\Php\get_($__L_td, $__L_tag, \Clojure\Php\hashSet()), $__L_tag)), $__L_td);
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('parents'), $__L_new_parents, \Clojure\Php\Kw::create('descendants'), $__L_new_descendants, \Clojure\Php\Kw::create('ancestors'), $__L_new_ancestors);
}
function derive($__L_tag, $__L_parent) {
swap_BANG_($GLOBALS['global_hierarchy'], $GLOBALS['derive_impl'], $__L_tag, $__L_parent);
return null;
}
function underive($__L_tag, $__L_parent) {
swap_BANG_($GLOBALS['global_hierarchy'], (function($__L_h) use (&$__L_tag, &$__L_parent) { return update_in($__L_h, \Clojure\Php\vec(\Clojure\Php\Kw::create('parents'), $__L_tag), $GLOBALS['disj'], $__L_parent);}));
return null;
}
function parents($__L_tag) {
return parents(deref($GLOBALS['global_hierarchy']), $__L_tag);
}
function ancestors($__L_tag) {
return ancestors(deref($GLOBALS['global_hierarchy']), $__L_tag);
}
function descendants($__L_tag) {
return descendants(deref($GLOBALS['global_hierarchy']), $__L_tag);
}
function isa_QMARK_($__L_child, $__L_parent) {
return isa_QMARK_(deref($GLOBALS['global_hierarchy']), $__L_child, $__L_parent);
}
call_user_func('\Clojure\Php\defType', 'MultiFn', array('name', 'dispatch-fn', 'default-val', 'hierarchy', 'method-table', 'prefer-table', 'cached-hierarchy', 'method-cache'), array('method-table', 'prefer-table', 'cached-hierarchy', 'method-cache'), array('-invoke', (function($__L_this) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn);
return call_user_func(get_method($__L_this, $__L_dispatch_val));}), '-invoke', (function($__L_this, $__L_a) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a);
return call_user_func(get_method($__L_this, $__L_dispatch_val), $__L_a);}), '-invoke', (function($__L_this, $__L_a, $__L_b) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b);
return call_user_func(get_method($__L_this, $__L_dispatch_val), $__L_a, $__L_b);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b, $__L_c);
return call_user_func(get_method($__L_this, $__L_dispatch_val), $__L_a, $__L_b, $__L_c);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b, $__L_c, $__L_d);
return call_user_func(get_method($__L_this, $__L_dispatch_val), $__L_a, $__L_b, $__L_c, $__L_d);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e);
return call_user_func(get_method($__L_this, $__L_dispatch_val), $__L_a, $__L_b, $__L_c, $__L_d, $__L_e);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f);
return call_user_func(get_method($__L_this, $__L_dispatch_val), $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g);
return call_user_func(get_method($__L_this, $__L_dispatch_val), $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g);}), '-apply', (function($__L_this, $__L_args) { $__L_dispatch_val = apply($__L_this->dispatch_fn, $__L_args);
return apply(get_method($__L_this, $__L_dispatch_val), $__L_args);}), '-name', (function($__L__) { return $__L__->name;}), '-namespace', (function($__L__) { return null;})));
function __GT_MultiFn($__L_name, $__L_dispatch_fn, $__L_default_val, $__L_hierarchy, $__L_method_table, $__L_prefer_table, $__L_cached_hierarchy, $__L_method_cache) {
return call_user_func('\Clojure\Php\createType', 'MultiFn', $__L_name, $__L_dispatch_fn, $__L_default_val, $__L_hierarchy, $__L_method_table, $__L_prefer_table, $__L_cached_hierarchy, $__L_method_cache);
}
function reset_cache_BANG_($__L_mf) {
$__L_mf->cached_hierarchy = deref($__L_mf->hierarchy);
return $__L_mf->method_cache = $__L_mf->method_table;
}
function find_and_cache_best_method($__L_mf, $__L_dispatch_val) {
$__L_h = deref($__L_mf->hierarchy);
$__L_method_table = $__L_mf->method_table;
$__L_default_val = $__L_mf->default_val;
$__L_prefer_table = $__L_mf->prefer_table;
$__L_dominates_QMARK_ = (function($__L_x, $__L_y) use (&$__L_method_table, &$__L_dominates_QMARK_, &$__L_find_best, &$__L_dispatch_val, &$__L_prefer_table, &$__L_default_val, &$__L_mf, &$__L_h) { $__L_or__3069 = isa_QMARK_($__L_h, $__L_x, $__L_y);
if ($__L_or__3069) { return $__L_or__3069;} else { return \Clojure\Php\contains(\Clojure\Php\get_($__L_prefer_table, $__L_x), $__L_y);}});
$__L_find_best = (function($__L_entries, $__L_best) use (&$__L_method_table, &$__L_dominates_QMARK_, &$__L_find_best, &$__L_dispatch_val, &$__L_prefer_table, &$__L_default_val, &$__L_mf, &$__L_h) {  while(true) { if (\Clojure\Php\isEmpty($__L_entries)) { return $__L_best;} else { $__L___dest_14 = \Clojure\Php\first($__L_entries);
$__L_k = \Clojure\Php\nth($__L___dest_14, 0);
$__L_m = \Clojure\Php\nth($__L___dest_14, 1);
if (isa_QMARK_($__L_h, $__L_dispatch_val, $__L_k)) { if ((call_user_func(function() { $__L_or__3070 = ($__L_best === null); if ($__L_or__3070) { return $__L_or__3070;} else { return call_user_func($__L_dominates_QMARK_, $__L_k, \Clojure\Php\first($__L_best));} }))) { $__recur_0 = \Clojure\Php\rest($__L_entries); $__recur_1 = \Clojure\Php\vec($__L_k, $__L_m); $__L_entries = $__recur_0; $__L_best = $__recur_1; continue;} else { if (call_user_func($__L_dominates_QMARK_, \Clojure\Php\first($__L_best), $__L_k)) { $__recur_0 = \Clojure\Php\rest($__L_entries); $__recur_1 = $__L_best; $__L_entries = $__recur_0; $__L_best = $__recur_1; continue;} else { throw ex_info('Multiple methods match dispatch value, none preferred', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('dispatch-val'), $__L_dispatch_val, \Clojure\Php\Kw::create('methods'), \Clojure\Php\vec($__L_k, \Clojure\Php\first($__L_best))));
}}} else { $__recur_0 = \Clojure\Php\rest($__L_entries); $__recur_1 = $__L_best; $__L_entries = $__recur_0; $__L_best = $__recur_1; continue;}} break; }});
$__L_best = call_user_func($__L_find_best, \Clojure\Php\seq($__L_method_table), null);
if ($__L_best) { return \Clojure\Php\second($__L_best);} else { return \Clojure\Php\get_($__L_method_table, $__L_default_val);}
}
function get_method($__L_mf, $__L_dispatch_val) {
if ((deref($__L_mf->hierarchy) === $__L_mf->cached_hierarchy)) { null;
} else { reset_cache_BANG_($__L_mf);
}
$__L_if_let__3071 = \Clojure\Php\get_($__L_mf->method_cache, $__L_dispatch_val);
if ($__L_if_let__3071) { $__L_method = $__L_if_let__3071;
return $__L_method;} else { $__L_if_let__3072 = find_and_cache_best_method($__L_mf, $__L_dispatch_val);
if ($__L_if_let__3072) { $__L_method = $__L_if_let__3072;
$__L_mf->method_cache = \Clojure\Php\assoc($__L_mf->method_cache, $__L_dispatch_val, $__L_method);
return $__L_method;} else { throw ex_info('No method in multimethod for dispatch value', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('name'), $__L_mf->name, \Clojure\Php\Kw::create('dispatch-val'), $__L_dispatch_val));
}}
}
function methods($__L_mf) {
return $__L_mf->method_table;
}
function prefers($__L_mf) {
return $__L_mf->prefer_table;
}
function multifn($__L_name, $__L_dispatch_fn) {
return multifn($__L_name, $__L_dispatch_fn, \Clojure\Php\Kw::create('default'), $GLOBALS['global_hierarchy']);
}
function add_method($__L_mf, $__L_dispatch_val, $__L_f) {
$__L_mf->method_table = \Clojure\Php\assoc($__L_mf->method_table, $__L_dispatch_val, $__L_f);
reset_cache_BANG_($__L_mf);
return $__L_mf;
}
function remove_method($__L_mf, $__L_dispatch_val) {
$__L_mf->method_table = \Clojure\Php\dissoc($__L_mf->method_table, $__L_dispatch_val);
reset_cache_BANG_($__L_mf);
return $__L_mf;
}
function remove_all_methods($__L_mf) {
$__L_mf->method_table = \Clojure\Php\hashMap();
reset_cache_BANG_($__L_mf);
return $__L_mf;
}
function prefer_method($__L_mf, $__L_dispatch_val_x, $__L_dispatch_val_y) {
if (dominates_QMARK_($__L_mf, $__L_dispatch_val_y, $__L_dispatch_val_x)) { throw ex_info('Preference conflict', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('x'), $__L_dispatch_val_x, \Clojure\Php\Kw::create('y'), $__L_dispatch_val_y));
}
$__L_mf->prefer_table = update($__L_mf->prefer_table, $__L_dispatch_val_x, fnil($GLOBALS['conj'], \Clojure\Php\hashSet()), $__L_dispatch_val_y);
reset_cache_BANG_($__L_mf);
return $__L_mf;
}
function dominates_QMARK_($__L_mf, $__L_x, $__L_y) {
$__L_h = deref($__L_mf->hierarchy);
$__L_or__3073 = isa_QMARK_($__L_h, $__L_x, $__L_y);
if ($__L_or__3073) { return $__L_or__3073;} else { return \Clojure\Php\contains(\Clojure\Php\get_($__L_mf->prefer_table, $__L_x), $__L_y);}
}
ns(\Clojure\Php\Sym::create('clojure.lang.ref'), 'Ref - Software Transactional Memory reference.

   Refs provide coordinated, synchronous access to shared state.
   Changes to refs must occur within a transaction (dosync).

   In single-threaded environments (PHP, single-threaded JS), STM
   semantics are simplified as there\'s no concurrent access.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$GLOBALS['_STAR_current_transaction_STAR_'] = null;
$_STAR_current_transaction_STAR_ = &$GLOBALS['_STAR_current_transaction_STAR_'];
call_user_func('\Clojure\Php\defType', 'Transaction', array('in-transaction?', 'values', 'sets', 'ensures', 'commutes'), array('in-transaction?', 'values', 'sets', 'ensures', 'commutes'), array());
function __GT_Transaction($__L_in_transaction_QMARK_, $__L_values, $__L_sets, $__L_ensures, $__L_commutes) {
return call_user_func('\Clojure\Php\createType', 'Transaction', $__L_in_transaction_QMARK_, $__L_values, $__L_sets, $__L_ensures, $__L_commutes);
}
function make_transaction() {
return __GT_Transaction(true, atom(\Clojure\Php\hashMap()), atom(\Clojure\Php\hashSet()), atom(\Clojure\Php\hashSet()), atom(\Clojure\Php\hashMap()));
}
call_user_func('\Clojure\Php\defType', 'Ref', array('val', '_meta', 'validator', 'watches', 'min-history', 'max-history'), array('val', 'validator', 'watches', 'min-history', 'max-history'), array('-deref', (function($__L_this) { if ($GLOBALS['_STAR_current_transaction_STAR_']) { $__L_or__3074 = \Clojure\Php\get_(deref($GLOBALS['_STAR_current_transaction_STAR_']->values), $__L_this);
if ($__L_or__3074) { return $__L_or__3074;} else { return $__L_this->val;}} else { return $__L_this->val;}}), '-set-validator!', (function($__L__, $__L_vf) { if (($__L_vf ? (!call_user_func($__L_vf, $__L__->val)) : false)) { throw ex_info('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L__->val));
}
return $__L__->validator = $__L_vf;}), '-get-validator', (function($__L__) { return $__L__->validator;}), '-add-watch', (function($__L_this, $__L_k, $__L_f) { $__L_this->watches = \Clojure\Php\assoc($__L_this->watches, $__L_k, $__L_f);
return $__L_this;}), '-remove-watch', (function($__L_this, $__L_k) { $__L_this->watches = \Clojure\Php\dissoc($__L_this->watches, $__L_k);
return $__L_this;}), '-notify-watches', (function($__L_this, $__L_old_val, $__L_new_val) {  while(true) { $__L_s3075 = \Clojure\Php\seq($__L_this->watches);
 while(true) { if ($__L_s3075) { $__L___dest_15 = \Clojure\Php\first($__L_s3075);
$__L_k = \Clojure\Php\nth($__L___dest_15, 0);
$__L_f = \Clojure\Php\nth($__L___dest_15, 1);
call_user_func($__L_f, $__L_k, $__L_this, $__L_old_val, $__L_new_val);
$__recur_0 = \Clojure\Php\next_($__L_s3075); $__L_s3075 = $__recur_0; continue;} break; }
 break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { throw ex_info('Refs don\'t support with-meta directly', \Clojure\Php\hashMap());
}), '-equiv', (function($__L_this, $__L_other) { return ($__L_this === $__L_other);}), '-hash', (function($__L_this) { return System::identityHashCode($__L_this);})));
function __GT_Ref($__L_val, $__L__meta, $__L_validator, $__L_watches, $__L_min_history, $__L_max_history) {
return call_user_func('\Clojure\Php\createType', 'Ref', $__L_val, $__L__meta, $__L_validator, $__L_watches, $__L_min_history, $__L_max_history);
}
function ref($__L_val) {
return __GT_Ref($__L_val, null, null, \Clojure\Php\hashMap(), 0, 10);
}
function ref_QMARK_($__L_x) {
return instance_QMARK_($GLOBALS['Ref'], $__L_x);
}
function ensure_in_transaction_BANG_() {
if ($GLOBALS['_STAR_current_transaction_STAR_']) { return null;} else { throw ex_info('No transaction running', \Clojure\Php\hashMap());
}
}
function ref_set($__L_ref, $__L_val) {
ensure_in_transaction_BANG_();
$__L_when_let__3076 = $__L_ref->validator;
if ($__L_when_let__3076) { $__L_validator = $__L_when_let__3076;
if (call_user_func($__L_validator, $__L_val)) { null;
} else { throw ex_info('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L_val));
}
}
swap_BANG_($GLOBALS['_STAR_current_transaction_STAR_']->values, $GLOBALS['assoc'], $__L_ref, $__L_val);
swap_BANG_($GLOBALS['_STAR_current_transaction_STAR_']->sets, $GLOBALS['conj'], $__L_ref);
return $__L_val;
}
function alter($__L_ref, $__L_f, $__L_args) {
ensure_in_transaction_BANG_();
$__L_old_val = _deref($__L_ref);
$__L_new_val = apply($__L_f, $__L_old_val, $__L_args);
return ref_set($__L_ref, $__L_new_val);
}
function commute($__L_ref, $__L_f, $__L_args) {
ensure_in_transaction_BANG_();
$__L_old_val = _deref($__L_ref);
$__L_new_val = apply($__L_f, $__L_old_val, $__L_args);
swap_BANG_($GLOBALS['_STAR_current_transaction_STAR_']->commutes, $GLOBALS['update'], $__L_ref, fnil($GLOBALS['conj'], \Clojure\Php\vec()), \Clojure\Php\vec($__L_f, $__L_args));
swap_BANG_($GLOBALS['_STAR_current_transaction_STAR_']->values, $GLOBALS['assoc'], $__L_ref, $__L_new_val);
return $__L_new_val;
}
function ensure($__L_ref) {
ensure_in_transaction_BANG_();
swap_BANG_($GLOBALS['_STAR_current_transaction_STAR_']->ensures, $GLOBALS['conj'], $__L_ref);
return deref($__L_ref);
}
function commit_transaction_BANG_($__L_txn) {
$__L_s3077 = \Clojure\Php\seq(deref($__L_txn->sets));
 while(true) { if ($__L_s3077) { $__L_ref = \Clojure\Php\first($__L_s3077);
$__L_new_val = \Clojure\Php\get_(deref($__L_txn->values), $__L_ref);
$__L_when_let__3078 = $__L_ref->validator;
if ($__L_when_let__3078) { $__L_validator = $__L_when_let__3078;
if (call_user_func($__L_validator, $__L_new_val)) { null;
} else { throw ex_info('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L_new_val));
}
}
$__recur_0 = \Clojure\Php\next_($__L_s3077); $__L_s3077 = $__recur_0; continue;}
 break; }
$__L_s3079 = \Clojure\Php\seq(deref($__L_txn->commutes));
 while(true) { if ($__L_s3079) { $__L___dest_16 = \Clojure\Php\first($__L_s3079);
$__L_ref = \Clojure\Php\nth($__L___dest_16, 0);
$__L_commute_ops = \Clojure\Php\nth($__L___dest_16, 1);
$__L_base_val = (call_user_func(function() { $__L_or__3080 = \Clojure\Php\get_(deref($__L_txn->values), $__L_ref); if ($__L_or__3080) { return $__L_or__3080;} else { return $__L_ref->val;} }));
$__L_final_val = reduce((function($__L_v, $__L___dest_17) use (&$__L_s3079, &$__L_ref, &$__L_txn, &$__L___dest_16, &$__L_commute_ops, &$__L_base_val) { $__L___dest_18 = $__L___dest_17;
$__L_f = \Clojure\Php\nth($__L___dest_18, 0);
$__L_args = \Clojure\Php\nth($__L___dest_18, 1);
return apply($__L_f, $__L_v, $__L_args);}), $__L_base_val, $__L_commute_ops);
swap_BANG_($__L_txn->values, $GLOBALS['assoc'], $__L_ref, $__L_final_val);
$__recur_0 = \Clojure\Php\next_($__L_s3079); $__L_s3079 = $__recur_0; continue;}
 break; }
$__L_s3081 = \Clojure\Php\seq(deref($__L_txn->values));
 while(true) { if ($__L_s3081) { $__L___dest_19 = \Clojure\Php\first($__L_s3081);
$__L_ref = \Clojure\Php\nth($__L___dest_19, 0);
$__L_new_val = \Clojure\Php\nth($__L___dest_19, 1);
$__L_old_val = $__L_ref->val;
$__L_ref->val = $__L_new_val;
if (\Clojure\Php\equals($__L_old_val, $__L_new_val)) { null;
} else { _notify_watches($__L_ref, $__L_old_val, $__L_new_val);
}
$__recur_0 = \Clojure\Php\next_($__L_s3081); $__L_s3081 = $__recur_0; continue;} break; }

}
function run_in_transaction($__L_f) {
if ($GLOBALS['_STAR_current_transaction_STAR_']) { return call_user_func($__L_f);} else { $__L_txn = make_transaction();
try { return binding(\Clojure\Php\vec($GLOBALS['_STAR_current_transaction_STAR_'], $__L_txn), (call_user_func(function() { $__L_result = call_user_func($__L_f); commit_transaction_BANG_($__L_txn);
return $__L_result; }))); } catch (\Throwable $__L_e) { throw $__L_e;
 }
}
}
function ref_history_count($__L_ref) {
return 0;
}
function ref_min_history($__L_ref) {
return $__L_ref->min_history;
}
function ref_max_history($__L_ref) {
return $__L_ref->max_history;
}
ns(\Clojure\Php\Sym::create('clojure.lang.agent'), 'Agent - Asynchronous reference type.

   Agents provide independent, asynchronous updates to shared state.
   Actions sent to agents are queued and executed asynchronously.

   In environments without true concurrency (PHP), agents execute
   actions in order but still provide the same API for portability.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$GLOBALS['agent_queue'] = atom(\Clojure\Php\vec());
$agent_queue = &$GLOBALS['agent_queue'];
$GLOBALS['_STAR_agent_STAR_'] = null;
$_STAR_agent_STAR_ = &$GLOBALS['_STAR_agent_STAR_'];
call_user_func('\Clojure\Php\defType', 'Agent', array('val', 'error', 'error-handler', 'error-mode', 'validator', 'watches', '_meta', 'action-queue'), array('val', 'error', 'error-handler', 'error-mode', 'validator', 'watches', 'action-queue'), array('-deref', (function($__L__) { if ($__L__->error) { throw $__L__->error;
}
return $__L__->val;}), '-set-validator!', (function($__L__, $__L_vf) { if (($__L_vf ? (!call_user_func($__L_vf, $__L__->val)) : false)) { throw ex_info('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L__->val));
}
return $__L__->validator = $__L_vf;}), '-get-validator', (function($__L__) { return $__L__->validator;}), '-add-watch', (function($__L_this, $__L_k, $__L_f) { $__L_this->watches = \Clojure\Php\assoc($__L_this->watches, $__L_k, $__L_f);
return $__L_this;}), '-remove-watch', (function($__L_this, $__L_k) { $__L_this->watches = \Clojure\Php\dissoc($__L_this->watches, $__L_k);
return $__L_this;}), '-notify-watches', (function($__L_this, $__L_old_val, $__L_new_val) {  while(true) { $__L_s3082 = \Clojure\Php\seq($__L_this->watches);
 while(true) { if ($__L_s3082) { $__L___dest_20 = \Clojure\Php\first($__L_s3082);
$__L_k = \Clojure\Php\nth($__L___dest_20, 0);
$__L_f = \Clojure\Php\nth($__L___dest_20, 1);
call_user_func($__L_f, $__L_k, $__L_this, $__L_old_val, $__L_new_val);
$__recur_0 = \Clojure\Php\next_($__L_s3082); $__L_s3082 = $__recur_0; continue;} break; }
 break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { throw ex_info('Agents don\'t support with-meta directly', \Clojure\Php\hashMap());
}), '-equiv', (function($__L_this, $__L_other) { return ($__L_this === $__L_other);}), '-hash', (function($__L_this) { return System::identityHashCode($__L_this);})));
function __GT_Agent($__L_val, $__L_error, $__L_error_handler, $__L_error_mode, $__L_validator, $__L_watches, $__L__meta, $__L_action_queue) {
return call_user_func('\Clojure\Php\createType', 'Agent', $__L_val, $__L_error, $__L_error_handler, $__L_error_mode, $__L_validator, $__L_watches, $__L__meta, $__L_action_queue);
}
function agent($__L_val) {
return __GT_Agent($__L_val, null, null, \Clojure\Php\Kw::create('fail'), null, \Clojure\Php\hashMap(), null, atom(\Clojure\Php\vec()));
}
function agent_QMARK_($__L_x) {
return instance_QMARK_($GLOBALS['Agent'], $__L_x);
}
function agent_error($__L_a) {
return $__L_a->error;
}
function restart_agent($__L_a, $__L_new_state, $__L_options) {
$__L_opts = apply($GLOBALS['hash_map'], $__L_options);
$__L_clear_actions_QMARK_ = \Clojure\Php\get_($__L_opts, \Clojure\Php\Kw::create('clear-actions'), false);
if ($__L_a->error) { null;
} else { throw ex_info('Agent does not have an error', \Clojure\Php\hashMap());
}
$__L_when_let__3083 = $__L_a->validator;
if ($__L_when_let__3083) { $__L_validator = $__L_when_let__3083;
if (call_user_func($__L_validator, $__L_new_state)) { null;
} else { throw ex_info('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L_new_state));
}
}
$__L_a->val = $__L_new_state;
$__L_a->error = null;
if ($__L_clear_actions_QMARK_) { reset_BANG_($__L_a->action_queue, \Clojure\Php\vec());
}
process_agent_queue_BANG_($__L_a);
return $__L_new_state;
}
function set_error_handler_BANG_($__L_a, $__L_f) {
$__L_a->error_handler = $__L_f;
return $__L_a;
}
function error_handler($__L_a) {
return $__L_a->error_handler;
}
function set_error_mode_BANG_($__L_a, $__L_mode) {
if (\Clojure\Php\contains(\Clojure\Php\hashSet(\Clojure\Php\Kw::create('continue'), \Clojure\Php\Kw::create('fail')), $__L_mode)) { null;
} else { throw ex_info('Invalid error mode', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('mode'), $__L_mode));
}
$__L_a->error_mode = $__L_mode;
return $__L_a;
}
function error_mode($__L_a) {
return $__L_a->error_mode;
}
function execute_action_BANG_($__L_a, $__L_f, $__L_args) {
try { $__L_old_val = $__L_a->val;
$__L_new_val = binding(\Clojure\Php\vec($GLOBALS['_STAR_agent_STAR_'], $__L_a), apply($__L_f, $__L_old_val, $__L_args));
$__L_when_let__3084 = $__L_a->validator;
if ($__L_when_let__3084) { $__L_validator = $__L_when_let__3084;
if (call_user_func($__L_validator, $__L_new_val)) { null;
} else { throw ex_info('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L_new_val));
}
}
$__L_a->val = $__L_new_val;
return _notify_watches($__L_a, $__L_old_val, $__L_new_val); } catch (\Throwable $__L_e) { if (\Clojure\Php\equals(\Clojure\Php\Kw::create('continue'), $__L_a->error_mode)) { $__L_when_let__3085 = $__L_a->error_handler;
if ($__L_when_let__3085) { $__L_handler = $__L_when_let__3085;
try { return call_user_func($__L_handler, $__L_a, $__L_e); } catch (\Throwable $__L__) { return null; }
}} else { return $__L_a->error = $__L_e;} }

}
function process_agent_queue_BANG_($__L_a) {
if ($__L_a->error) { return null;} else {  while(true) { $__L_when_let__3086 = \Clojure\Php\seq(deref($__L_a->action_queue));
if ($__L_when_let__3086) { $__L_actions = $__L_when_let__3086;
$__L___dest_21 = \Clojure\Php\first($__L_actions);
$__L_f = \Clojure\Php\nth($__L___dest_21, 0);
$__L_args = \Clojure\Php\nth($__L___dest_21, 1);
swap_BANG_($__L_a->action_queue, comp($GLOBALS['vec'], $GLOBALS['rest']));
execute_action_BANG_($__L_a, $__L_f, $__L_args);
if ($__L_a->error) { return null;} else { continue;}} break; }
}
}
function send($__L_a, $__L_f, $__L_args) {
if ($__L_a->error) { throw ex_info('Agent has errors', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('agent'), $__L_a, \Clojure\Php\Kw::create('error'), $__L_a->error));
}
swap_BANG_($__L_a->action_queue, $GLOBALS['conj'], \Clojure\Php\vec($__L_f, $__L_args));
process_agent_queue_BANG_($__L_a);
return $__L_a;
}
function send_off($__L_a, $__L_f, $__L_args) {
return apply($GLOBALS['send'], $__L_a, $__L_f, $__L_args);
}
function send_via($__L_executor, $__L_a, $__L_f, $__L_args) {
return apply($GLOBALS['send'], $__L_a, $__L_f, $__L_args);
}
function await($__L_agents) {
return null;
}
function await_for($__L_timeout_ms, $__L_agents) {
return true;
}
function release_pending_sends() {
return null;
}
function _STAR_agent_STAR__fn() {
return $GLOBALS['_STAR_agent_STAR_'];
}
function shutdown_agents() {
return null;
}
ns(\Clojure\Php\Sym::create('clojure.lang.ex'), 'Exception types and utilities.

   Clojure uses ex-info/ex-data for rich exception information.
   This provides the portable implementation.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
call_user_func('\Clojure\Php\defType', 'ExceptionInfo', array('message', 'data', 'cause'), array(), array('-data', (function($__L__) { return $__L__->data;}), '-message', (function($__L__) { return $__L__->message;}), '-cause', (function($__L__) { return $__L__->cause;}), '-lookup', (function($__L__, $__L_k) { return \Clojure\Php\get_($__L__->data, $__L_k);}), '-lookup', (function($__L__, $__L_k, $__L_not_found) { return \Clojure\Php\get_($__L__->data, $__L_k, $__L_not_found);})));
function __GT_ExceptionInfo($__L_message, $__L_data, $__L_cause) {
return call_user_func('\Clojure\Php\createType', 'ExceptionInfo', $__L_message, $__L_data, $__L_cause);
}
function ex_info($__L_msg, $__L_data) {
return __GT_ExceptionInfo($__L_msg, $__L_data, null);
}
function ex_data($__L_ex) {
if (instance_QMARK_($GLOBALS['ExceptionInfo'], $__L_ex)) { return $__L_ex->data;}
}
function ex_message($__L_ex) {
if (instance_QMARK_($GLOBALS['ExceptionInfo'], $__L_ex)) { return $__L_ex->message;} else { return $__L_ex->getMessage();}
}
function ex_cause($__L_ex) {
if (instance_QMARK_($GLOBALS['ExceptionInfo'], $__L_ex)) { return $__L_ex->cause;} else { return $__L_ex->getCause();}
}
function ex_info_QMARK_($__L_x) {
return instance_QMARK_($GLOBALS['ExceptionInfo'], $__L_x);
}
$GLOBALS['_STAR_assert_STAR_'] = true;
$_STAR_assert_STAR_ = &$GLOBALS['_STAR_assert_STAR_'];
function print_stack_trace($__L_tr) {
return print_stack_trace($__L_tr, $GLOBALS['_STAR_out_STAR_']);
}
function get_stack_trace($__L_ex) {
return \Clojure\Php\seq($__L_ex->getStackTrace());
}
function stack_element_str($__L_el) {
return ((string)$__L_el);
}
function root_cause($__L_ex) {
$__L_cause = $__L_ex;
 while(true) { $__L_if_let__3087 = ex_cause($__L_cause);
if ($__L_if_let__3087) { $__L_c = $__L_if_let__3087;
$__recur_0 = $__L_c; $__L_cause = $__recur_0; continue;} else { return $__L_cause;} break; }

}
function exception_chain($__L_ex) {
return lazy_seq(($__L_ex ? \Clojure\Php\cons($__L_ex, exception_chain(ex_cause($__L_ex))) : null));
}
function get_suppressed($__L_ex) {
try { return \Clojure\Php\seq($__L_ex->getSuppressed()); } catch (\Throwable $__L__) { return null; }

}
function add_suppressed($__L_ex, $__L_suppressed) {
try { return $__L_ex->addSuppressed($__L_suppressed); } catch (\Throwable $__L__) { return null; }
return $__L_ex;
}
function throw_arity($__L_name, $__L_n) {
throw ex_info(\Clojure\Php\str_('Wrong number of args (', $__L_n, ') passed to: ', $__L_name), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('arity-exception'), \Clojure\Php\Kw::create('name'), $__L_name, \Clojure\Php\Kw::create('arity'), $__L_n));

}
function throw_illegal_arg($__L_msg) {
throw ex_info(apply($GLOBALS['str'], $__L_msg), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('illegal-argument')));

}
function throw_illegal_state($__L_msg) {
throw ex_info(apply($GLOBALS['str'], $__L_msg), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('illegal-state')));

}
function throw_unsupported($__L_msg) {
throw ex_info(apply($GLOBALS['str'], $__L_msg), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('unsupported-operation')));

}
function throw_index_out_of_bounds($__L_n, $__L_count) {
throw ex_info(\Clojure\Php\str_('Index ', $__L_n, ' out of bounds for count ', $__L_count), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('index-out-of-bounds'), \Clojure\Php\Kw::create('index'), $__L_n, \Clojure\Php\Kw::create('count'), $__L_count));

}
function throw_key_not_found($__L_key) {
throw ex_info(\Clojure\Php\str_('Key not found: ', $__L_key), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('key-not-found'), \Clojure\Php\Kw::create('key'), $__L_key));

}
function throw_class_cast($__L_from, $__L_to) {
throw ex_info(\Clojure\Php\str_('Cannot cast ', $__L_from, ' to ', $__L_to), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('class-cast'), \Clojure\Php\Kw::create('from'), $__L_from, \Clojure\Php\Kw::create('to'), $__L_to));

}
function throw_null_pointer($__L_msg) {
throw ex_info((call_user_func(function() { $__L_or__3088 = \Clojure\Php\first($__L_msg); if ($__L_or__3088) { return $__L_or__3088;} else { return 'Null pointer exception';} })), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('null-pointer')));

}
function inc($__L_x) {
return ($__L_x + 1);
}
function dec($__L_x) {
return ($__L_x - 1);
}
function identity($__L_x) {
return $__L_x;
}
function not($__L_x) {
if ($__L_x) { return false;} else { return true;}
}
function nil_QMARK_($__L_x) {
return is_null($__L_x);
}
function apply($__L_f, $__L_args) {
$__L_flat_args = (\Clojure\Php\next_($__L_args) ? array_merge(array_slice($__L_args, 0, -1), last($__L_args)) : \Clojure\Php\first($__L_args));
return call_user_func_array($__L_f, call_user_func('\Clojure\Php\intoArray', $__L_flat_args));
}
function comp($__L_fs) {
$__L_fs = array_reverse($__L_fs);
return (function(...$__L_args) use (&$__L_fs) {  while(true) { $__L_ret = apply(\Clojure\Php\first($__L_fs), $__L_args);
$__L_fs = \Clojure\Php\next_($__L_fs);
 while(true) { if ($__L_fs) { $__recur_0 = call_user_func(\Clojure\Php\first($__L_fs), $__L_ret); $__recur_1 = \Clojure\Php\next_($__L_fs); $__L_ret = $__recur_0; $__L_fs = $__recur_1; continue;} else { return $__L_ret;} break; }
 break; }});
}
function partial($__L_f, $__L_args) {
return (function(...$__L_more) use (&$__L_args, &$__L_f) { return apply($__L_f, array_merge($__L_args, $__L_more));});
}
function count($__L_coll) {
if (($__L_coll === null)) { return 0;} else { return count($__L_coll);}
}
function seq($__L_coll) {
if (($__L_coll === null)) { return null;} else { if (is_array($__L_coll)) { if ((0 === count($__L_coll))) { return null;} else { return $__L_coll;}} else { if (method_exists($__L_coll, 'seq')) { return $__L_coll->seq();} else { return $__L_coll;}}}
}
function first($__L_coll) {
if (($__L_coll === null)) { return null;} else { if (is_array($__L_coll)) { if ((0 === count($__L_coll))) { return null;} else { return reset($__L_coll);}} else { return $__L_coll->first();}}
}
function rest($__L_coll) {
if (($__L_coll === null)) { return \Clojure\Php\vec();} else { if (is_array($__L_coll)) { return array_slice($__L_coll, 1);} else { return $__L_coll->rest();}}
}
function next($__L_coll) {
if (($__L_coll === null)) { return null;} else { $__L_r = \Clojure\Php\rest($__L_coll);
return \Clojure\Php\seq($__L_r);}
}
function cons($__L_x, $__L_coll) {
return call_user_func('\Clojure\Php\cons', $__L_x, $__L_coll);
}
function vector($__L_args) {
return call_user_func_array('\Clojure\Php\vec', $__L_args);
}
function conj($__L_coll, $__L_x) {
return call_user_func('\Clojure\Php\conj', $__L_coll, $__L_x);
}
function reduced($__L_x) {
return call_user_func('\Clojure\Php\reduced', $__L_x);
}
function reduced_QMARK_($__L_x) {
return call_user_func('\Clojure\Php\isReduced', $__L_x);
}
function unreduced($__L_x) {
if (reduced_QMARK_($__L_x)) { return call_user_func('\Clojure\Php\unreduced', $__L_x);} else { return $__L_x;}
}
function reduce($__L_f, $__L_coll) {
if (\Clojure\Php\seq($__L_coll)) { return reduce($__L_f, \Clojure\Php\first($__L_coll), \Clojure\Php\next_($__L_coll));} else { return call_user_func($__L_f);}
}
function reduce_kv($__L_f, $__L_init, $__L_coll) {
if (($__L_coll === null)) { return $__L_init;} else { $__L_acc = $__L_init;
$__L_ks = keys($__L_coll);
 while(true) { if (\Clojure\Php\seq($__L_ks)) { $__L_k = \Clojure\Php\first($__L_ks);
$__L_ret = call_user_func($__L_f, $__L_acc, $__L_k, \Clojure\Php\get_($__L_coll, $__L_k));
if (reduced_QMARK_($__L_ret)) { return unreduced($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = \Clojure\Php\next_($__L_ks); $__L_acc = $__recur_0; $__L_ks = $__recur_1; continue;}} else { return $__L_acc;} break; }
}
}
function last($__L_coll) {
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\next_($__L_coll)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__L_coll = $__recur_0; continue;} else { return \Clojure\Php\first($__L_coll);} break; }

}
function second($__L_coll) {
return \Clojure\Php\first(\Clojure\Php\next_($__L_coll));
}
function map($__L_f, $__L_coll) {
return (new \Clojure\Php\LazySeq((function() use (&$__L_coll, &$__L_f) { if (\Clojure\Php\seq($__L_coll)) { return \Clojure\Php\cons(call_user_func($__L_f, \Clojure\Php\first($__L_coll)), map($__L_f, \Clojure\Php\next_($__L_coll)));}})));
}
function filter($__L_pred, $__L_coll) {
return (new \Clojure\Php\LazySeq((function() use (&$__L_pred, &$__L_coll) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
if (call_user_func($__L_pred, $__L_x)) { return \Clojure\Php\cons($__L_x, filter($__L_pred, \Clojure\Php\next_($__L_coll)));} else { return filter($__L_pred, \Clojure\Php\next_($__L_coll));}}})));
}
function mapv($__L_f, $__L_coll) {
'Eager version of map - returns a vector';
$__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__recur_0 = \Clojure\Php\conj($__L_result, call_user_func($__L_f, \Clojure\Php\first($__L_coll))); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_result;} break; }

}
function filterv($__L_pred, $__L_coll) {
'Eager version of filter - returns a vector';
$__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
$__recur_0 = (call_user_func($__L_pred, $__L_x) ? \Clojure\Php\conj($__L_result, $__L_x) : $__L_result); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_result;} break; }

}
function remove($__L_pred, $__L_coll) {
return filter((function($__L_x) use (&$__L_pred, &$__L_coll) { return (!call_user_func($__L_pred, $__L_x));}), $__L_coll);
}
function mapcat($__L_f, $__L_coll) {
return apply($GLOBALS['concat'], map($__L_f, $__L_coll));
}
function map_indexed($__L_f, $__L_coll) {
$__L_result = \Clojure\Php\vec();
$__L_idx = 0;
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__recur_0 = \Clojure\Php\conj($__L_result, call_user_func($__L_f, $__L_idx, \Clojure\Php\first($__L_coll))); $__recur_1 = ($__L_idx + 1); $__recur_2 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_idx = $__recur_1; $__L_coll = $__recur_2; continue;} else { return $__L_result;} break; }

}
function keep($__L_f, $__L_coll) {
$__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_v = call_user_func($__L_f, \Clojure\Php\first($__L_coll));
$__recur_0 = (($__L_v === null) ? $__L_result : \Clojure\Php\conj($__L_result, $__L_v)); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_result;} break; }

}
function keep_indexed($__L_f, $__L_coll) {
$__L_result = \Clojure\Php\vec();
$__L_idx = 0;
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_v = call_user_func($__L_f, $__L_idx, \Clojure\Php\first($__L_coll));
$__recur_0 = (($__L_v === null) ? $__L_result : \Clojure\Php\conj($__L_result, $__L_v)); $__recur_1 = ($__L_idx + 1); $__recur_2 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_idx = $__recur_1; $__L_coll = $__recur_2; continue;} else { return $__L_result;} break; }

}
function flatten($__L_coll) {
$__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
if ((is_array($__L_x) ? is_array($__L_x) : coll_QMARK_($__L_x))) { $__recur_0 = into($__L_result, flatten($__L_x)); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;} else { $__recur_0 = \Clojure\Php\conj($__L_result, $__L_x); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;}} else { return $__L_result;} break; }

}
function interleave($__L_c1, $__L_c2) {
$__L_result = \Clojure\Php\vec();
$__L_s1 = $__L_c1;
$__L_s2 = $__L_c2;
 while(true) { if ((\Clojure\Php\seq($__L_s1) ? \Clojure\Php\seq($__L_s2) : false)) { $__recur_0 = \Clojure\Php\conj(\Clojure\Php\conj($__L_result, \Clojure\Php\first($__L_s1)), \Clojure\Php\first($__L_s2)); $__recur_1 = \Clojure\Php\next_($__L_s1); $__recur_2 = \Clojure\Php\next_($__L_s2); $__L_result = $__recur_0; $__L_s1 = $__recur_1; $__L_s2 = $__recur_2; continue;} else { return $__L_result;} break; }

}
function interpose($__L_sep, $__L_coll) {
if (\Clojure\Php\seq($__L_coll)) { $__L_result = \Clojure\Php\vec(\Clojure\Php\first($__L_coll));
$__L_coll = \Clojure\Php\next_($__L_coll);
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__recur_0 = \Clojure\Php\conj(\Clojure\Php\conj($__L_result, $__L_sep), \Clojure\Php\first($__L_coll)); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_result;} break; }
} else { return \Clojure\Php\vec();}
}
function _EQ_($__L_args) {
if ((count($__L_args) < 2)) { return true;} else { $__L_first_arg = array_shift($__L_args);
$__L_args = $__L_args;
 while(true) { if (\Clojure\Php\seq($__L_args)) { if (call_user_func('\Clojure\Php\equals', $__L_first_arg, \Clojure\Php\first($__L_args))) { $__recur_0 = \Clojure\Php\next_($__L_args); $__L_args = $__recur_0; continue;} else { return false;}} else { return true;} break; }
}
}
function not_EQ_($__L_args) {
return (!apply($GLOBALS['_EQ_'], $__L_args));
}
function atom($__L_x) {
return (new \Clojure\Php\Atom($__L_x));
}
function deref($__L_x) {
return $__L_x->deref();
}
function reset_BANG_($__L_x, $__L_newval) {
$__L_x->reset($__L_newval);
return $__L_newval;
}
function swap_BANG_($__L_x, $__L_f, $__L_args) {
$__L_old = $__L_x->deref();
$__L_new = apply($__L_f, $__L_old, $__L_args);
$__L_x->reset($__L_new);
return $__L_new;
}
function assoc($__L_map, $__L_key, $__L_val, $__L_kvs) {
$__L_m = call_user_func('\Clojure\Php\assoc', $__L_map, $__L_key, $__L_val);
if ($__L_kvs) { return apply($GLOBALS['assoc'], $__L_m, $__L_kvs);} else { return $__L_m;}
}
function update($__L_m, $__L_k, $__L_f, $__L_args) {
return \Clojure\Php\assoc($__L_m, $__L_k, apply($__L_f, \Clojure\Php\get_($__L_m, $__L_k), $__L_args));
}
function get($__L_m, $__L_k) {
return call_user_func('\Clojure\Php\get', $__L_m, $__L_k);
}
function str($__L_args) {
return implode('', $__L_args);
}
function print($__L_more) {
return print(apply($GLOBALS['str'], $__L_more));
}
function println($__L_more) {
print(apply($GLOBALS['str'], $__L_more));
return print(PHP_EOL);
}
function pr_str($__L_xs) {
'Returns a string containing the printed representation of xs (EDN format)';
return implode(' ', call_user_func('\Clojure\Php\toArray', map((function($__L_x) use (&$__L_xs) { return call_user_func('\Clojure\Php\prStr', $__L_x);}), $__L_xs)));
}
function prn_str($__L_xs) {
'Same as pr-str but with a trailing newline';
return \Clojure\Php\str_(apply($GLOBALS['pr_str'], $__L_xs), PHP_EOL);
}
function pr($__L_xs) {
'Prints the object(s) in a machine-readable form';
return print(apply($GLOBALS['pr_str'], $__L_xs));
}
function prn($__L_xs) {
'Same as pr followed by newline';
print(apply($GLOBALS['pr_str'], $__L_xs));
return print(PHP_EOL);
}
function print_str($__L_xs) {
'Returns a string of the values printed by print';
return apply($GLOBALS['str'], $__L_xs);
}
function _PLUS_($__L_xs) {
return reduce((function($__L_a, $__L_b) use (&$__L_xs) { return ($__L_a + $__L_b);}), 0, $__L_xs);
}
function _($__L_x, $__L_xs) {
if ($__L_xs) { return reduce((function($__L_a, $__L_b) use (&$__L_x, &$__L_xs) { return ($__L_a - $__L_b);}), $__L_x, $__L_xs);} else { return (0 - $__L_x);}
}
function _STAR_($__L_xs) {
return reduce((function($__L_a, $__L_b) use (&$__L_xs) { return ($__L_a * $__L_b);}), 1, $__L_xs);
}
function _SLASH_($__L_x, $__L_xs) {
if ($__L_xs) { return reduce((function($__L_a, $__L_b) use (&$__L_x, &$__L_xs) { return ($__L_a / $__L_b);}), $__L_x, $__L_xs);} else { return (1 / $__L_x);}
}
function mod($__L_num, $__L_div) {
$__L_m = ($__L_num % $__L_div);
if (((0 === $__L_m) ? (0 === $__L_m) : (($__L_num > 0) === ($__L_div > 0)))) { return $__L_m;} else { return ($__L_m + $__L_div);}
}
function rem($__L_num, $__L_div) {
return ($__L_num % $__L_div);
}
function quot($__L_num, $__L_div) {
return intdiv($__L_num, $__L_div);
}
function max($__L_x) {
return $__L_x;
}
function min($__L_x) {
return $__L_x;
}
function abs($__L_a) {
return abs($__L_a);
}
function zero_QMARK_($__L_x) {
return (0 === $__L_x);
}
function pos_QMARK_($__L_x) {
return ($__L_x > 0);
}
function neg_QMARK_($__L_x) {
return ($__L_x < 0);
}
function even_QMARK_($__L_n) {
return (0 === ($__L_n % 2));
}
function odd_QMARK_($__L_n) {
return (1 === abs(($__L_n % 2)));
}
function string_QMARK_($__L_x) {
return is_string($__L_x);
}
function int_QMARK_($__L_x) {
return is_int($__L_x);
}
function integer_QMARK_($__L_x) {
return is_int($__L_x);
}
function float_QMARK_($__L_x) {
return is_float($__L_x);
}
function number_QMARK_($__L_x) {
return is_numeric($__L_x);
}
function boolean_QMARK_($__L_x) {
return is_bool($__L_x);
}
function true_QMARK_($__L_x) {
return ($__L_x === true);
}
function false_QMARK_($__L_x) {
return ($__L_x === false);
}
function array_QMARK_($__L_x) {
return is_array($__L_x);
}
function fn_QMARK_($__L_x) {
return is_callable($__L_x);
}
function symbol_QMARK_($__L_x) {
return is_string($__L_x);
}
function keyword_QMARK_($__L_x) {
return ($__L_x instanceof \Clojure\Php\Kw);
}
function vector_QMARK_($__L_x) {
return ($__L_x instanceof \Clojure\Php\Vec);
}
function map_QMARK_($__L_x) {
return ($__L_x instanceof \Clojure\Php\Map);
}
function set_QMARK_($__L_x) {
return ($__L_x instanceof \Clojure\Php\Set);
}
function list_QMARK_($__L_x) {
return ($__L_x instanceof \Clojure\Php\PList);
}
function coll_QMARK_($__L_x) {
if (($__L_x instanceof \Clojure\Php\Vec)) { return ($__L_x instanceof \Clojure\Php\Vec);} else { if (($__L_x instanceof \Clojure\Php\Map)) { return ($__L_x instanceof \Clojure\Php\Map);} else { if (($__L_x instanceof \Clojure\Php\Set)) { return ($__L_x instanceof \Clojure\Php\Set);} else { if (($__L_x instanceof \Clojure\Php\PList)) { return ($__L_x instanceof \Clojure\Php\PList);} else { return is_array($__L_x);}}}}
}
function seq_QMARK_($__L_x) {
if (($__L_x instanceof \Clojure\Php\Vec)) { return ($__L_x instanceof \Clojure\Php\Vec);} else { return ($__L_x instanceof \Clojure\Php\PList);}
}
function empty_QMARK_($__L_x) {
return (0 === \Clojure\Php\count_($__L_x));
}
function some_QMARK_($__L_x) {
return (!($__L_x === null));
}
function keys($__L_m) {
if (($__L_m === null)) { return null;} else { return call_user_func('\Clojure\Php\keys_', $__L_m);}
}
function vals($__L_m) {
if (($__L_m === null)) { return null;} else { return call_user_func('\Clojure\Php\vals', $__L_m);}
}
function contains_QMARK_($__L_coll, $__L_key) {
if (($__L_coll === null)) { return false;} else { return call_user_func('\Clojure\Php\contains', $__L_coll, $__L_key);}
}
function take($__L_n, $__L_coll) {
$__L_n = $__L_n;
$__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if ((($__L_n > 0) ? \Clojure\Php\seq($__L_coll) : false)) { $__L_x = \Clojure\Php\first($__L_coll);
$__recur_0 = ($__L_n - 1); $__recur_1 = \Clojure\Php\next_($__L_coll); $__recur_2 = \Clojure\Php\conj($__L_result, $__L_x); $__L_n = $__recur_0; $__L_coll = $__recur_1; $__L_result = $__recur_2; continue;} else { return $__L_result;} break; }

}
function drop($__L_n, $__L_coll) {
$__L_n = $__L_n;
$__L_coll = $__L_coll;
 while(true) { if ((($__L_n > 0) ? \Clojure\Php\seq($__L_coll) : false)) { $__recur_0 = ($__L_n - 1); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_n = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_coll;} break; }

}
function take_while($__L_pred, $__L_coll) {
$__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
if (call_user_func($__L_pred, $__L_x)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_x); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;}} else { return $__L_result;} break; }

}
function drop_while($__L_pred, $__L_coll) {
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { if (call_user_func($__L_pred, \Clojure\Php\first($__L_coll))) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__L_coll = $__recur_0; continue;} else { return $__L_coll;}} else { return $__L_coll;} break; }

}
function concat($__L_colls) {
return reduce((function($__L_acc, $__L_c) use (&$__L_colls) {  while(true) { $__L_acc = $__L_acc;
$__L_c = $__L_c;
 while(true) { if (\Clojure\Php\seq($__L_c)) { $__recur_0 = \Clojure\Php\conj($__L_acc, \Clojure\Php\first($__L_c)); $__recur_1 = \Clojure\Php\next_($__L_c); $__L_acc = $__recur_0; $__L_c = $__recur_1; continue;} else { return $__L_acc;} break; }
 break; }}), \Clojure\Php\vec(), $__L_colls);
}
function reverse($__L_coll) {
$__L_arr = call_user_func('\Clojure\Php\intoArray', $__L_coll);
return array_reverse($__L_arr);
}
function nth($__L_coll, $__L_n) {
return \Clojure\Php\nth($__L_coll, $__L_n);
}
function range3($__L_start, $__L_end, $__L_step) {
$__L_n = $__L_start;
$__L_result = \Clojure\Php\vec();
 while(true) { if (($__L_n < $__L_end)) { $__L_current = $__L_n;
$__recur_0 = ($__L_n + $__L_step); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_current); $__L_n = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }

}
function range($__L_end_or_start, $__L_args) {
if ((\Clojure\Php\first($__L_args) === null)) { return range3(0, $__L_end_or_start, 1);} else { $__L_end = \Clojure\Php\first($__L_args);
$__L_step = (\Clojure\Php\second($__L_args) ? \Clojure\Php\second($__L_args) : 1);
return range3($__L_end_or_start, $__L_end, $__L_step);}
}
function repeat($__L_n, $__L_x) {
$__L_i = $__L_n;
$__L_result = \Clojure\Php\vec();
 while(true) { if (($__L_i > 0)) { $__recur_0 = ($__L_i - 1); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_x); $__L_i = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }

}
function into($__L_to, $__L_from) {
return reduce($GLOBALS['conj'], $__L_to, $__L_from);
}
function zipmap($__L_keys, $__L_vals) {
$__L_m = \Clojure\Php\hashMap();
$__L_ks = $__L_keys;
$__L_vs = $__L_vals;
 while(true) { if ((\Clojure\Php\seq($__L_ks) ? \Clojure\Php\seq($__L_vs) : false)) { $__recur_0 = \Clojure\Php\assoc($__L_m, \Clojure\Php\first($__L_ks), \Clojure\Php\first($__L_vs)); $__recur_1 = \Clojure\Php\next_($__L_ks); $__recur_2 = \Clojure\Php\next_($__L_vs); $__L_m = $__recur_0; $__L_ks = $__recur_1; $__L_vs = $__recur_2; continue;} else { return $__L_m;} break; }

}
function partition($__L_n, $__L_coll) {
return partition($__L_n, $__L_n, $__L_coll);
}
function distinct($__L_coll) {
$__L_coll = $__L_coll;
$__L_seen = \Clojure\Php\hashSet();
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
if (\Clojure\Php\contains($__L_seen, $__L_x)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = $__L_seen; $__recur_2 = $__L_result; $__L_coll = $__recur_0; $__L_seen = $__recur_1; $__L_result = $__recur_2; continue;} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_seen, $__L_x); $__recur_2 = \Clojure\Php\conj($__L_result, $__L_x); $__L_coll = $__recur_0; $__L_seen = $__recur_1; $__L_result = $__recur_2; continue;}} else { return $__L_result;} break; }

}
function group_by($__L_f, $__L_coll) {
return reduce((function($__L_m, $__L_x) use (&$__L_coll, &$__L_f) { $__L_k = call_user_func($__L_f, $__L_x);
return update($__L_m, $__L_k, (function($__L_v) use (&$__L_x, &$__L_m, &$__L_coll, &$__L_k, &$__L_f) { return \Clojure\Php\conj(($__L_v ? $__L_v : \Clojure\Php\vec()), $__L_x);}));}), \Clojure\Php\hashMap(), $__L_coll);
}
function frequencies($__L_coll) {
return reduce((function($__L_m, $__L_x) use (&$__L_coll) { return update($__L_m, $__L_x, (function($__L_v) use (&$__L_x, &$__L_m, &$__L_coll) { return (($__L_v ? $__L_v : 0) + 1);}));}), \Clojure\Php\hashMap(), $__L_coll);
}
function sort($__L_coll) {
$__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
sort($__L_arr);
return call_user_func('\Clojure\Php\vector', $__L_arr);
}
function sort_by($__L_keyfn, $__L_coll) {
$__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
usort($__L_arr, (function($__L_a, $__L_b) use (&$__L_arr, &$__L_coll, &$__L_keyfn) { $__L_ka = call_user_func($__L_keyfn, $__L_a);
$__L_kb = call_user_func($__L_keyfn, $__L_b);
if (($__L_ka < $__L_kb)) { return -1;} else { if (($__L_ka > $__L_kb)) { return 1;} else { return 0;}}}));
return call_user_func('\Clojure\Php\vector', $__L_arr);
}
function merge($__L_maps) {
return reduce((function($__L_m1, $__L_m2) use (&$__L_maps) { if ($__L_m2) { return reduce((function($__L_m, $__L_kv) use (&$__L_m1, &$__L_maps, &$__L_m2) { return \Clojure\Php\assoc($__L_m, \Clojure\Php\first($__L_kv), \Clojure\Php\second($__L_kv));}), $__L_m1, \Clojure\Php\seq($__L_m2));} else { return $__L_m1;}}), \Clojure\Php\hashMap(), $__L_maps);
}
function get_in($__L_m, $__L_ks) {
return reduce($GLOBALS['get'], $__L_m, $__L_ks);
}
function assoc_in($__L_m, $__L_ks, $__L_v) {
$__L_k = \Clojure\Php\first($__L_ks);
$__L_ks = \Clojure\Php\next_($__L_ks);
if ($__L_ks) { return \Clojure\Php\assoc($__L_m, $__L_k, assoc_in(\Clojure\Php\get_($__L_m, $__L_k), $__L_ks, $__L_v));} else { return \Clojure\Php\assoc($__L_m, $__L_k, $__L_v);}
}
function update_in($__L_m, $__L_ks, $__L_f, $__L_args) {
$__L_k = \Clojure\Php\first($__L_ks);
$__L_ks = \Clojure\Php\next_($__L_ks);
if ($__L_ks) { return \Clojure\Php\assoc($__L_m, $__L_k, apply($GLOBALS['update_in'], \Clojure\Php\get_($__L_m, $__L_k), $__L_ks, $__L_f, $__L_args));} else { return \Clojure\Php\assoc($__L_m, $__L_k, apply($__L_f, \Clojure\Php\get_($__L_m, $__L_k), $__L_args));}
}
function dissoc($__L_m, $__L_ks) {
return reduce((function($__L_m, $__L_k) use (&$__L_ks) { return call_user_func('\Clojure\Php\dissoc', $__L_m, $__L_k);}), $__L_m, $__L_ks);
}
function select_keys($__L_m, $__L_ks) {
return reduce((function($__L_result, $__L_k) use (&$__L_ks, &$__L_m) { if (\Clojure\Php\contains($__L_m, $__L_k)) { return \Clojure\Php\assoc($__L_result, $__L_k, \Clojure\Php\get_($__L_m, $__L_k));} else { return $__L_result;}}), \Clojure\Php\hashMap(), $__L_ks);
}
function every_QMARK_($__L_pred, $__L_coll) {
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { if (call_user_func($__L_pred, \Clojure\Php\first($__L_coll))) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__L_coll = $__recur_0; continue;} else { return false;}} else { return true;} break; }

}
function some($__L_pred, $__L_coll) {
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_result = call_user_func($__L_pred, \Clojure\Php\first($__L_coll));
if ($__L_result) { return $__L_result;} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__L_coll = $__recur_0; continue;}} break; }

}
function not_every_QMARK_($__L_pred, $__L_coll) {
return (!every_QMARK_($__L_pred, $__L_coll));
}
function not_any_QMARK_($__L_pred, $__L_coll) {
return (!some($__L_pred, $__L_coll));
}
function constantly($__L_x) {
return (function(...$__L_args) use (&$__L_x) { return $__L_x;});
}
function complement($__L_f) {
return (function(...$__L_args) use (&$__L_f) { return (!apply($__L_f, $__L_args));});
}
function juxt($__L_fs) {
return (function(...$__L_args) use (&$__L_fs) { return mapv((function($__L_f) use (&$__L_fs, &$__L_args) { return apply($__L_f, $__L_args);}), $__L_fs);});
}
function fnil($__L_f, $__L_x) {
return (function($__L_arg, ...$__L_args) use (&$__L_x, &$__L_f) { return apply($__L_f, (($__L_arg === null) ? $__L_x : $__L_arg), $__L_args);});
}
function memoize($__L_f) {
$__L_cache = atom(\Clojure\Php\hashMap());
return (function(...$__L_args) use (&$__L_cache, &$__L_f) { $__L_key = apply($GLOBALS['vector'], $__L_args);
$__L_cached = \Clojure\Php\get_(deref($__L_cache), $__L_key);
if (($__L_cached !== null)) { return $__L_cached;} else { $__L_result = apply($__L_f, $__L_args);
swap_BANG_($__L_cache, $GLOBALS['assoc'], $__L_key, $__L_result);
return $__L_result;}});
}
function compare($__L_x, $__L_y) {
if (($__L_x < $__L_y)) { return -1;} else { if (($__L_x > $__L_y)) { return 1;} else { if (\Clojure\Php\Kw::create('else')) { return 0;} else { return null;}}}
}
function identical_QMARK_($__L_x, $__L_y) {
return ($__L_x === $__L_y);
}
function name($__L_x) {
if (($__L_x instanceof \Clojure\Php\Kw)) { return $__L_x->getName();} else { if (($__L_x instanceof \Clojure\Php\Sym)) { return strval($__L_x);} else { if (is_string($__L_x)) { return $__L_x;} else { if (\Clojure\Php\Kw::create('else')) { return strval($__L_x);} else { return null;}}}}
}
function namespace($__L_x) {
if (($__L_x instanceof \Clojure\Php\Kw)) { return $__L_x->getNamespace();}
}
function subs($__L_s, $__L_start) {
return substr($__L_s, $__L_start);
}
function interleave($__L_c1, $__L_c2) {
$__L_c1 = $__L_c1;
$__L_c2 = $__L_c2;
$__L_result = \Clojure\Php\vec();
 while(true) { if ((\Clojure\Php\seq($__L_c1) ? \Clojure\Php\seq($__L_c2) : false)) { $__recur_0 = \Clojure\Php\next_($__L_c1); $__recur_1 = \Clojure\Php\next_($__L_c2); $__recur_2 = \Clojure\Php\conj(\Clojure\Php\conj($__L_result, \Clojure\Php\first($__L_c1)), \Clojure\Php\first($__L_c2)); $__L_c1 = $__recur_0; $__L_c2 = $__recur_1; $__L_result = $__recur_2; continue;} else { return $__L_result;} break; }

}
function interpose($__L_sep, $__L_coll) {
$__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
$__L_first_QMARK_ = true;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { if ($__L_first_QMARK_) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, \Clojure\Php\first($__L_coll)); $__recur_2 = false; $__L_coll = $__recur_0; $__L_result = $__recur_1; $__L_first_QMARK_ = $__recur_2; continue;} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj(\Clojure\Php\conj($__L_result, $__L_sep), \Clojure\Php\first($__L_coll)); $__recur_2 = false; $__L_coll = $__recur_0; $__L_result = $__recur_1; $__L_first_QMARK_ = $__recur_2; continue;}} else { return $__L_result;} break; }

}
function flatten($__L_coll) {
$__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
if (coll_QMARK_($__L_x)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = into($__L_result, flatten($__L_x)); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_x); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;}} else { return $__L_result;} break; }

}
function mapcat($__L_f, $__L_coll) {
return apply($GLOBALS['concat'], map($__L_f, $__L_coll));
}
function keep($__L_f, $__L_coll) {
$__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_v = call_user_func($__L_f, \Clojure\Php\first($__L_coll));
$__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = (($__L_v === null) ? $__L_result : \Clojure\Php\conj($__L_result, $__L_v)); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }

}
function keep_indexed($__L_f, $__L_coll) {
$__L_coll = $__L_coll;
$__L_idx = 0;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_v = call_user_func($__L_f, $__L_idx, \Clojure\Php\first($__L_coll));
$__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = ($__L_idx + 1); $__recur_2 = (($__L_v === null) ? $__L_result : \Clojure\Php\conj($__L_result, $__L_v)); $__L_coll = $__recur_0; $__L_idx = $__recur_1; $__L_result = $__recur_2; continue;} else { return $__L_result;} break; }

}
function map_indexed($__L_f, $__L_coll) {
$__L_coll = $__L_coll;
$__L_idx = 0;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = ($__L_idx + 1); $__recur_2 = \Clojure\Php\conj($__L_result, call_user_func($__L_f, $__L_idx, \Clojure\Php\first($__L_coll))); $__L_coll = $__recur_0; $__L_idx = $__recur_1; $__L_result = $__recur_2; continue;} else { return $__L_result;} break; }

}
function partition_by($__L_f, $__L_coll) {
$__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
$__L_current = \Clojure\Php\vec();
$__L_current_val = null;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
$__L_v = call_user_func($__L_f, $__L_x);
if ((($__L_current_val === null) ? ($__L_current_val === null) : \Clojure\Php\equals($__L_v, $__L_current_val))) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = $__L_result; $__recur_2 = \Clojure\Php\conj($__L_current, $__L_x); $__recur_3 = $__L_v; $__L_coll = $__recur_0; $__L_result = $__recur_1; $__L_current = $__recur_2; $__L_current_val = $__recur_3; continue;} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_current); $__recur_2 = \Clojure\Php\vec($__L_x); $__recur_3 = $__L_v; $__L_coll = $__recur_0; $__L_result = $__recur_1; $__L_current = $__recur_2; $__L_current_val = $__recur_3; continue;}} else { if (\Clojure\Php\seq($__L_current)) { return \Clojure\Php\conj($__L_result, $__L_current);} else { return $__L_result;}} break; }

}
function split_at($__L_n, $__L_coll) {
return \Clojure\Php\vec(take($__L_n, $__L_coll), drop($__L_n, $__L_coll));
}
function split_with($__L_pred, $__L_coll) {
return \Clojure\Php\vec(take_while($__L_pred, $__L_coll), drop_while($__L_pred, $__L_coll));
}
function butlast($__L_coll) {
$__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\next_($__L_coll)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, \Clojure\Php\first($__L_coll)); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }

}
function shuffle($__L_coll) {
$__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
shuffle($__L_arr);
return call_user_func('\Clojure\Php\vector', $__L_arr);
}
function union($__L_s1, $__L_s2) {
return reduce($GLOBALS['conj'], $__L_s1, $__L_s2);
}
function intersection($__L_s1, $__L_s2) {
return reduce((function($__L_result, $__L_x) use (&$__L_s1, &$__L_s2) { if (\Clojure\Php\contains($__L_s2, $__L_x)) { return \Clojure\Php\conj($__L_result, $__L_x);} else { return $__L_result;}}), \Clojure\Php\hashSet(), $__L_s1);
}
function difference($__L_s1, $__L_s2) {
return reduce((function($__L_result, $__L_x) use (&$__L_s1, &$__L_s2) { if (\Clojure\Php\contains($__L_s2, $__L_x)) { return $__L_result;} else { return \Clojure\Php\conj($__L_result, $__L_x);}}), \Clojure\Php\hashSet(), $__L_s1);
}
function subset_QMARK_($__L_s1, $__L_s2) {
return every_QMARK_((function($__L_x) use (&$__L_s1, &$__L_s2) { return \Clojure\Php\contains($__L_s2, $__L_x);}), $__L_s1);
}
function superset_QMARK_($__L_s1, $__L_s2) {
return subset_QMARK_($__L_s2, $__L_s1);
}
function max($__L_xs) {
return reduce((function($__L_a, $__L_b) use (&$__L_xs) { if (($__L_a > $__L_b)) { return $__L_a;} else { return $__L_b;}}), \Clojure\Php\first($__L_xs), \Clojure\Php\next_($__L_xs));
}
function min($__L_xs) {
return reduce((function($__L_a, $__L_b) use (&$__L_xs) { if (($__L_a < $__L_b)) { return $__L_a;} else { return $__L_b;}}), \Clojure\Php\first($__L_xs), \Clojure\Php\next_($__L_xs));
}
function abs($__L_x) {
if (($__L_x < 0)) { return (-$__L_x);} else { return $__L_x;}
}
function mod($__L_n, $__L_d) {
return ($__L_n % $__L_d);
}
function quot($__L_n, $__L_d) {
return intdiv($__L_n, $__L_d);
}
function rem($__L_n, $__L_d) {
return ($__L_n % $__L_d);
}
function even_QMARK_($__L_n) {
return (0 === ($__L_n % 2));
}
function odd_QMARK_($__L_n) {
return (!(($__L_n & 1) === 0));
}
function pos_QMARK_($__L_n) {
return ($__L_n > 0);
}
function neg_QMARK_($__L_n) {
return ($__L_n < 0);
}
function zero_QMARK_($__L_n) {
return ($__L_n === 0);
}
function rand() {
return (rand() / getrandmax());
}
function rand_int($__L_n) {
return rand(0, ($__L_n - 1));
}
function rand_nth($__L_coll) {
return \Clojure\Php\nth($__L_coll, rand_int(\Clojure\Php\count_($__L_coll)));
}
function read_string($__L_s) {
'Parse an EDN string. Currently uses PHP\'s json_decode for JSON-compatible values.';
return json_decode($__L_s, true);
}
function ex_info($__L_msg, $__L_data) {
return call_user_func('\Clojure\Php\exInfo', $__L_msg, $__L_data);
}
function ex_data($__L_ex) {
return call_user_func('\Clojure\Php\exData', $__L_ex);
}
function ex_message($__L_ex) {
return call_user_func('\Clojure\Php\exMessage', $__L_ex);
}
function ex_cause($__L_ex) {
return call_user_func('\Clojure\Php\exCause', $__L_ex);
}
function Throwable__GT_map($__L_ex) {
if (($__L_ex === null)) { return null;} else { $__L_data = ex_data($__L_ex);
$__L_msg = ex_message($__L_ex);
$__L_cause = ex_cause($__L_ex);
$__L_class_name = get_class($__L_ex);
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), $__L_class_name, \Clojure\Php\Kw::create('message'), $__L_msg, \Clojure\Php\Kw::create('data'), $__L_data, \Clojure\Php\Kw::create('cause'), ($__L_cause ? Throwable__GT_map($__L_cause) : null));}
}
function ex_triage($__L_throwable_map) {
$__L___dest_22 = $__L_throwable_map;
$__L_type = \Clojure\Php\get_($__L___dest_22, \Clojure\Php\Kw::create('type'));
$__L_message = \Clojure\Php\get_($__L___dest_22, \Clojure\Php\Kw::create('message'));
$__L_data = \Clojure\Php\get_($__L___dest_22, \Clojure\Php\Kw::create('data'));
$__L_cause = \Clojure\Php\get_($__L___dest_22, \Clojure\Php\Kw::create('cause'));
$__L_phase = (\Clojure\Php\get_($__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'phase')) ? \Clojure\Php\get_($__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'phase')) : \Clojure\Php\Kw::create('execution'));
$__L_err_type = \Clojure\Php\get_($__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'type'));
$__L_file = \Clojure\Php\get_($__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'file'));
$__L_line = \Clojure\Php\get_($__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'line'));
$__L_col = \Clojure\Php\get_($__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'column'));
$__L_sym = \Clojure\Php\get_($__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'symbol'));
$__L_hint = \Clojure\Php\get_($__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'hint'));
return \Clojure\Php\hashMap(\Clojure\Php\Kw::createNs('cljp.error', 'file'), $__L_file, \Clojure\Php\Kw::createNs('cljp.error', 'type'), $__L_err_type, \Clojure\Php\Kw::createNs('cljp.error', 'column'), $__L_col, \Clojure\Php\Kw::createNs('cljp.error', 'message'), $__L_message, \Clojure\Php\Kw::createNs('cljp.error', 'line'), $__L_line, \Clojure\Php\Kw::createNs('cljp.error', 'phase'), $__L_phase, \Clojure\Php\Kw::createNs('cljp.error', 'hint'), $__L_hint, \Clojure\Php\Kw::createNs('cljp.error', 'class'), $__L_type, \Clojure\Php\Kw::createNs('cljp.error', 'cause'), ($__L_cause ? ex_message(\Clojure\Php\hashMap(\Clojure\Php\Kw::create('message'), call_user_func(\Clojure\Php\Kw::create('message'), $__L_cause))) : null), \Clojure\Php\Kw::createNs('cljp.error', 'symbol'), $__L_sym);
}
function ex_str($__L_triage_data) {
$__L_phase = \Clojure\Php\get_($__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'phase'));
$__L_err_type = \Clojure\Php\get_($__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'type'));
$__L_class = \Clojure\Php\get_($__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'class'));
$__L_message = \Clojure\Php\get_($__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'message'));
$__L_file = \Clojure\Php\get_($__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'file'));
$__L_line = \Clojure\Php\get_($__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'line'));
$__L_column = \Clojure\Php\get_($__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'column'));
$__L_symbol = \Clojure\Php\get_($__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'symbol'));
$__L_hint = \Clojure\Php\get_($__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'hint'));
$__L_loc = \Clojure\Php\str_(($__L_file ? $__L_file : 'REPL'), ':', ($__L_line ? $__L_line : 1), ($__L_column ? \Clojure\Php\str_(':', $__L_column) : ''));
$__L_class_str = ($__L_class ? ((string)$__L_class) : '');
$__L_class_simple = basename(str_replace('\\', '/', $__L_class_str));
$__L_type_str = ($__L_err_type ? ltrim(((string)$__L_err_type), ':') : '');
$__L_type_label = ($__L_err_type ? \Clojure\Php\str_(ucfirst($__L_type_str), 'Error') : ($__L_class ? $__L_class_simple : 'Error'));
return \Clojure\Php\str_($__L_type_label, ': ', $__L_message, ' at (', $__L_loc, ')', ($__L_hint ? \Clojure\Php\str_('
Hint: ', $__L_hint) : ''));
}
function err__GT_msg($__L_ex) {
return ex_str(ex_triage(Throwable__GT_map($__L_ex)));
}
$GLOBALS['_STAR_1'] = null;
$_STAR_1 = &$GLOBALS['_STAR_1'];
$GLOBALS['_STAR_2'] = null;
$_STAR_2 = &$GLOBALS['_STAR_2'];
$GLOBALS['_STAR_3'] = null;
$_STAR_3 = &$GLOBALS['_STAR_3'];
$GLOBALS['_STAR_e'] = null;
$_STAR_e = &$GLOBALS['_STAR_e'];
$GLOBALS['_STAR_ns_STAR_'] = 'user';
$_STAR_ns_STAR_ = &$GLOBALS['_STAR_ns_STAR_'];
$GLOBALS['_STAR_print_length_STAR_'] = null;
$_STAR_print_length_STAR_ = &$GLOBALS['_STAR_print_length_STAR_'];
$GLOBALS['_STAR_print_level_STAR_'] = null;
$_STAR_print_level_STAR_ = &$GLOBALS['_STAR_print_level_STAR_'];
$GLOBALS['_STAR_print_meta_STAR_'] = null;
$_STAR_print_meta_STAR_ = &$GLOBALS['_STAR_print_meta_STAR_'];
function _push_repl_result($__L_value) {
'Internal: Update REPL result history.';
return $__L_value;
}
function symbol($__L_name) {
return call_user_func('\Clojure\Php\sym', $__L_name);
}
function format($__L_fmt, $__L_args) {
return call_user_func_array('sprintf', \Clojure\Php\cons($__L_fmt, $__L_args));
}
function realized_QMARK_($__L_x) {
if (method_exists($__L_x, 'isRealized')) { return $__L_x->isRealized();} else { return true;}
}
function doall($__L_coll) {
return doall($__L_coll, null);
}
function dorun($__L_coll) {
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ($__L_s) { $__recur_0 = \Clojure\Php\next_($__L_s); $__L_s = $__recur_0; continue;}
 break; }
return null;
}
function re_pattern($__L_s) {
if (is_string($__L_s)) { return $__L_s;} else { throw (new \InvalidArgumentException('re-pattern requires a string'));
}
}
function re_matches($__L_re, $__L_s) {
$__L_result = array();
$__L_matched = preg_match(\Clojure\Php\str_('^', $__L_re, '$'), $__L_s, $__L_result);
if (($__L_matched > 0)) { if ((1 === count($__L_result))) { return \Clojure\Php\first($__L_result);} else { return call_user_func('\Clojure\Php\vec', $__L_result);}}
}
function re_find($__L_re, $__L_s) {
$__L_result = array();
$__L_matched = preg_match($__L_re, $__L_s, $__L_result);
if (($__L_matched > 0)) { if ((1 === count($__L_result))) { return \Clojure\Php\first($__L_result);} else { return call_user_func('\Clojure\Php\vec', $__L_result);}}
}
function re_seq($__L_re, $__L_s) {
$__L_result = array();
$__L__ = preg_match_all($__L_re, $__L_s, $__L_result);
if ((count(\Clojure\Php\first($__L_result)) > 0)) { return call_user_func('\Clojure\Php\vec', \Clojure\Php\first($__L_result));}
}
function re_groups($__L_m) {
if (is_string($__L_m)) { return $__L_m;} else { if ((\Clojure\Php\count_($__L_m) > 1)) { return $__L_m;} else { return \Clojure\Php\first($__L_m);}}
}
function line_seq($__L_rdr) {
return (new \Clojure\Php\LazySeq((function() use (&$__L_rdr) { $__L_line = fgets($__L_rdr);
if (($__L_line !== false)) { return \Clojure\Php\cons(rtrim($__L_line, PHP_EOL), line_seq($__L_rdr));}})));
}
function tree_seq($__L_branch_QMARK_, $__L_children, $__L_root) {
$__L_walk = (function($__L_node) use (&$__L_branch_QMARK_, &$__L_children, &$__L_root) { return (new \Clojure\Php\LazySeq((function() use (&$__L_branch_QMARK_, &$__L_children, &$__L_node, &$__L_root) { return \Clojure\Php\cons($__L_node, (call_user_func($__L_branch_QMARK_, $__L_node) ? mapcat($GLOBALS['walk'], call_user_func($__L_children, $__L_node)) : null));})));});
return call_user_func($__L_walk, $__L_root);
}
function file_seq($__L_dir) {
return tree_seq((function($__L_f) use (&$__L_dir) { return is_dir($__L_f);}), (function($__L_d) use (&$__L_dir) { $__L_files = scandir($__L_d);
return filter((function($__L_f) use (&$__L_dir, &$__L_files, &$__L_d) { return (!(\Clojure\Php\equals($__L_f, '.') ? \Clojure\Php\equals($__L_f, '.') : \Clojure\Php\equals($__L_f, '..')));}), map((function($__L_f) use (&$__L_dir, &$__L_files, &$__L_d) { return \Clojure\Php\str_($__L_d, '/', $__L_f);}), $__L_files));}), $__L_dir);
}
function xml_seq($__L_root) {
return tree_seq((function($__L_node) use (&$__L_root) { if (($__L_node instanceof \Clojure\Php\Map)) { return \Clojure\Php\contains($__L_node, \Clojure\Php\Kw::create('content'));} else { return false;}}), (function($__L_node) use (&$__L_root) { return \Clojure\Php\get_($__L_node, \Clojure\Php\Kw::create('content'));}), $__L_root);
}
function bit_not($__L_x) {
return $__L_x;
}
function bit_test($__L_x, $__L_n) {
return (0 !== ($__L_x & (1 << $__L_n)));
}
function bit_set($__L_x, $__L_n) {
return ($__L_x | (1 << $__L_n));
}
function bit_clear($__L_x, $__L_n) {
return ($__L_x & (1 << $__L_n));
}
function bit_flip($__L_x, $__L_n) {
return ($__L_x ^ (1 << $__L_n));
}
function unsigned_bit_shift_right($__L_x, $__L_n) {
if (($__L_x >= 0)) { return ($__L_x >> $__L_n);} else { return (($__L_x + (1 << ((8 * PHP_INT_SIZE) - 1))) >> ($__L_n - 1));}
}
function add_watch($__L_ref, $__L_key, $__L_fn) {
$__L_ref->addWatch($__L_key, $__L_fn);
return $__L_ref;
}
function remove_watch($__L_ref, $__L_key) {
$__L_ref->removeWatch($__L_key);
return $__L_ref;
}
function set_validator_BANG_($__L_ref, $__L_validator_fn) {
$__L_ref->setValidator($__L_validator_fn);
return null;
}
function get_validator($__L_ref) {
return $__L_ref->getValidator();
}
function compare_and_set_BANG_($__L_atom, $__L_oldval, $__L_newval) {
return $__L_atom->compareAndSet($__L_oldval, $__L_newval);
}
function run_BANG_($__L_proc, $__L_coll) {
reduce((function($__L__, $__L_x) use (&$__L_proc, &$__L_coll) { call_user_func($__L_proc, $__L_x);
return null;}), null, $__L_coll);
return null;
}
function transient($__L_coll) {
if (method_exists($__L_coll, 'asTransient')) { return $__L_coll->asTransient();} else { throw (new \InvalidArgumentException('transient not supported'));
}
}
function persistent_BANG_($__L_tcoll) {
if (method_exists($__L_tcoll, 'persistent')) { return $__L_tcoll->persistent();} else { throw (new \InvalidArgumentException('persistent! not supported'));
}
}
function conj_BANG_($__L_tcoll, $__L_val) {
if (method_exists($__L_tcoll, 'conj')) { return $__L_tcoll->conj($__L_val);} else { throw (new \InvalidArgumentException('conj! not supported'));
}
}
function assoc_BANG_($__L_tcoll, $__L_key, $__L_val) {
if (method_exists($__L_tcoll, 'assoc')) { return $__L_tcoll->assoc($__L_key, $__L_val);} else { throw (new \InvalidArgumentException('assoc! not supported'));
}
}
function dissoc_BANG_($__L_tcoll, $__L_key) {
if (method_exists($__L_tcoll, 'dissoc')) { return $__L_tcoll->dissoc($__L_key);} else { throw (new \InvalidArgumentException('dissoc! not supported'));
}
}
function pop_BANG_($__L_tcoll) {
if (method_exists($__L_tcoll, 'pop')) { return $__L_tcoll->pop();} else { throw (new \InvalidArgumentException('pop! not supported'));
}
}
function disj_BANG_($__L_tset, $__L_val) {
if (method_exists($__L_tset, 'disj')) { return $__L_tset->disj($__L_val);} else { throw (new \InvalidArgumentException('disj! not supported'));
}
}
function select($__L_pred, $__L_xset) {
return reduce((function($__L_s, $__L_k) use (&$__L_pred, &$__L_xset) { if (call_user_func($__L_pred, $__L_k)) { return $__L_s;} else { return disj($__L_s, $__L_k);}}), $__L_xset, $__L_xset);
}
function project($__L_xrel, $__L_ks) {
return into(\Clojure\Php\hashSet(), map((function($__L_m) use (&$__L_ks, &$__L_xrel) { return select_keys($__L_m, $__L_ks);}), $__L_xrel));
}
function rename_keys($__L_map, $__L_kmap) {
return reduce((function($__L_m, $__L___dest_23) use (&$__L_map, &$__L_kmap) { $__L___dest_24 = $__L___dest_23;
$__L_old = \Clojure\Php\nth($__L___dest_24, 0);
$__L_new = \Clojure\Php\nth($__L___dest_24, 1);
if (\Clojure\Php\contains($__L_m, $__L_old)) { return \Clojure\Php\assoc(\Clojure\Php\dissoc($__L_m, $__L_old), $__L_new, \Clojure\Php\get_($__L_m, $__L_old));} else { return $__L_m;}}), $__L_map, $__L_kmap);
}
function rename($__L_xrel, $__L_kmap) {
return into(\Clojure\Php\hashSet(), map((function($__L_m) use (&$__L_xrel, &$__L_kmap) { return rename_keys($__L_m, $__L_kmap);}), $__L_xrel));
}
function index($__L_xrel, $__L_ks) {
return reduce((function($__L_m, $__L_x) use (&$__L_ks, &$__L_xrel) { $__L_ik = select_keys($__L_x, $__L_ks);
return update($__L_m, $__L_ik, (function($__L_is) use (&$__L_x, &$__L_ks, &$__L_xrel, &$__L_m, &$__L_ik) { return \Clojure\Php\conj(($__L_is ? $__L_is : \Clojure\Php\hashSet()), $__L_x);}));}), \Clojure\Php\hashMap(), $__L_xrel);
}
function map_invert($__L_m) {
return reduce((function($__L_result, $__L___dest_25) use (&$__L_m) { $__L___dest_26 = $__L___dest_25;
$__L_k = \Clojure\Php\nth($__L___dest_26, 0);
$__L_v = \Clojure\Php\nth($__L___dest_26, 1);
return \Clojure\Php\assoc($__L_result, $__L_v, $__L_k);}), \Clojure\Php\hashMap(), $__L_m);
}
function join($__L_xrel, $__L_yrel) {
if ((\Clojure\Php\seq($__L_xrel) ? \Clojure\Php\seq($__L_yrel) : false)) { $__L_ks = intersection(into(\Clojure\Php\hashSet(), keys(\Clojure\Php\first($__L_xrel))), into(\Clojure\Php\hashSet(), keys(\Clojure\Php\first($__L_yrel))));
$__L___dest_27 = ((\Clojure\Php\count_($__L_xrel) <= \Clojure\Php\count_($__L_yrel)) ? \Clojure\Php\vec($__L_xrel, $__L_yrel) : \Clojure\Php\vec($__L_yrel, $__L_xrel));
$__L_r = \Clojure\Php\nth($__L___dest_27, 0);
$__L_s = \Clojure\Php\nth($__L___dest_27, 1);
$__L_idx = index($__L_r, $__L_ks);
return reduce((function($__L_ret, $__L_x) use (&$__L_idx, &$__L_r, &$__L_ks, &$__L_xrel, &$__L_s, &$__L___dest_27, &$__L_yrel) { $__L_found = \Clojure\Php\get_($__L_idx, select_keys($__L_x, $__L_ks));
if ($__L_found) { return reduce((function($__L_ret, $__L_y) use (&$__L_idx, &$__L_x, &$__L_r, &$__L_ks, &$__L_xrel, &$__L_s, &$__L___dest_27, &$__L_yrel, &$__L_found) { return \Clojure\Php\conj($__L_ret, merge($__L_y, $__L_x));}), $__L_ret, $__L_found);} else { return $__L_ret;}}), \Clojure\Php\hashSet(), $__L_s);} else { return \Clojure\Php\hashSet();}
}
function gensym() {
return gensym('G__');
}
function with_meta($__L_obj, $__L_m) {
if (method_exists($__L_obj, 'withMeta')) { return $__L_obj->withMeta($__L_m);} else { return $__L_obj;}
}
function meta($__L_obj) {
if (method_exists($__L_obj, 'meta')) { return $__L_obj->meta();}
}
function vary_meta($__L_obj, $__L_f, $__L_args) {
return with_meta($__L_obj, apply($__L_f, meta($__L_obj), $__L_args));
}
function alter_meta_BANG_($__L_ref, $__L_f, $__L_args) {
if (method_exists($__L_ref, 'alterMeta')) { return $__L_ref->alterMeta($__L_f, $__L_args);} else { throw (new \InvalidArgumentException('alter-meta! not supported'));
}
}
function reset_meta_BANG_($__L_ref, $__L_m) {
if (method_exists($__L_ref, 'resetMeta')) { return $__L_ref->resetMeta($__L_m);} else { throw (new \InvalidArgumentException('reset-meta! not supported'));
}
}
function bounded_count($__L_n, $__L_coll) {
if (($__L_coll instanceof \Countable)) { return min($__L_n, \Clojure\Php\count_($__L_coll));} else { $__L_i = 0;
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if (($__L_s ? ($__L_i < $__L_n) : false)) { $__recur_0 = ($__L_i + 1); $__recur_1 = \Clojure\Php\next_($__L_s); $__L_i = $__recur_0; $__L_s = $__recur_1; continue;} else { return $__L_i;} break; }
}
}
function not_empty($__L_coll) {
if (\Clojure\Php\seq($__L_coll)) { return $__L_coll;}
}
function int($__L_x) {
return intval($__L_x);
}
function long($__L_x) {
return intval($__L_x);
}
function float($__L_x) {
return floatval($__L_x);
}
function double($__L_x) {
return floatval($__L_x);
}
function char($__L_x) {
if (is_int($__L_x)) { return chr($__L_x);} else { if (is_string($__L_x)) { return \Clojure\Php\first($__L_x);} else { throw (new \InvalidArgumentException('Cannot coerce to char'));
}}
}
function boolean($__L_x) {
return boolval($__L_x);
}
function byte($__L_x) {
return (intval($__L_x) & 255);
}
function short($__L_x) {
return intval($__L_x);
}
function object_array($__L_n) {
return array_fill(0, $__L_n, null);
}
function num($__L_x) {
if ((is_int($__L_x) || is_float($__L_x))) { return $__L_x;} else { throw (new \InvalidArgumentException('Cannot coerce to num'));
}
}
function bigint($__L_x) {
return intval($__L_x);
}
function bigdec($__L_x) {
return floatval($__L_x);
}
function rationalize($__L_x) {
return floatval($__L_x);
}
println('Test 1: let in expr context');
println((1 + (call_user_func(function() { $__L_x = 2; return $__L_x; }))));
println('Test 2: do in expr context');
println((10 + (call_user_func(function() { $GLOBALS['temp'] = 5;
$temp = &$GLOBALS['temp'];
return $GLOBALS['temp']; }))));
println('Test 3: nested let');
println(((call_user_func(function() { $__L_a = 1; return $__L_a; })) + (call_user_func(function() { $__L_b = 2; return $__L_b; }))));
println('Test 4: let with multiple bindings');
println((2 * (call_user_func(function() { $__L_x = 3; $__L_y = 4; return ($__L_x + $__L_y); }))));
println('Test 5: letfn in expr context');
println((100 + (call_user_func(function() { $__L_double = (function($__L_n) use (&$__L_double) { return (2 * $__L_n);}); return 5.0; }))));
println('Test 6: do with side effects');
$GLOBALS['counter'] = atom(0);
$counter = &$GLOBALS['counter'];
println((1 + (call_user_func(function() { swap_BANG_($GLOBALS['counter'], $GLOBALS['inc']);
return deref($GLOBALS['counter']); }))));
println('Test 7: let with fn call');
println(\Clojure\Php\str_('Result: ', (call_user_func(function() { $__L_nums = \Clojure\Php\vec(1, 2, 3); return reduce($GLOBALS['_PLUS_'], $__L_nums); }))));
println('All IIFE tests passed!');
