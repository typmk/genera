<?php

require __DIR__ . '/src/php/clojure/bootstrap.php';

($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.protocols'), 'Core protocols - the foundation of Clojure\'s abstractions.
   These replace clojure.lang.I* Java interfaces.');
call_user_func('\Clojure\Php\defProtocol', 'ISeqable', array(\Clojure\Php\vec('-seq')));
$NS->_seq = (function($__L_o, ...$__L_args2918) { return call_user_func('\Clojure\Php\protocolDispatch', 'ISeqable', '-seq', $__L_o, $__L_args2918);});
call_user_func('\Clojure\Php\defProtocol', 'ISeq', array(\Clojure\Php\vec('-first', '-rest')));
$NS->_first = (function($__L_coll, ...$__L_args2919) { return call_user_func('\Clojure\Php\protocolDispatch', 'ISeq', '-first', $__L_coll, $__L_args2919);});
$NS->_rest = (function($__L_coll, ...$__L_args2920) { return call_user_func('\Clojure\Php\protocolDispatch', 'ISeq', '-rest', $__L_coll, $__L_args2920);});
call_user_func('\Clojure\Php\defProtocol', 'INext', array(\Clojure\Php\vec('-next')));
$NS->_next = (function($__L_coll, ...$__L_args2921) { return call_user_func('\Clojure\Php\protocolDispatch', 'INext', '-next', $__L_coll, $__L_args2921);});
call_user_func('\Clojure\Php\defProtocol', 'ICounted', array(\Clojure\Php\vec('-count')));
$NS->_count = (function($__L_coll, ...$__L_args2922) { return call_user_func('\Clojure\Php\protocolDispatch', 'ICounted', '-count', $__L_coll, $__L_args2922);});
call_user_func('\Clojure\Php\defProtocol', 'IIndexed', array(\Clojure\Php\vec('-nth')));
$NS->_nth = (function($__L_coll, ...$__L_args2923) { return call_user_func('\Clojure\Php\protocolDispatch', 'IIndexed', '-nth', $__L_coll, $__L_args2923);});
call_user_func('\Clojure\Php\defProtocol', 'ILookup', array(\Clojure\Php\vec('-lookup')));
$NS->_lookup = (function($__L_o, ...$__L_args2924) { return call_user_func('\Clojure\Php\protocolDispatch', 'ILookup', '-lookup', $__L_o, $__L_args2924);});
call_user_func('\Clojure\Php\defProtocol', 'IAssociative', array(\Clojure\Php\vec('-contains-key?', '-assoc')));
$NS->_contains_key_QMARK_ = (function($__L_coll, ...$__L_args2925) { return call_user_func('\Clojure\Php\protocolDispatch', 'IAssociative', '-contains-key?', $__L_coll, $__L_args2925);});
$NS->_assoc = (function($__L_coll, ...$__L_args2926) { return call_user_func('\Clojure\Php\protocolDispatch', 'IAssociative', '-assoc', $__L_coll, $__L_args2926);});
call_user_func('\Clojure\Php\defProtocol', 'IMap', array(\Clojure\Php\vec('-dissoc')));
$NS->_dissoc = (function($__L_coll, ...$__L_args2927) { return call_user_func('\Clojure\Php\protocolDispatch', 'IMap', '-dissoc', $__L_coll, $__L_args2927);});
call_user_func('\Clojure\Php\defProtocol', 'ISet', array(\Clojure\Php\vec('-disjoin')));
$NS->_disjoin = (function($__L_coll, ...$__L_args2928) { return call_user_func('\Clojure\Php\protocolDispatch', 'ISet', '-disjoin', $__L_coll, $__L_args2928);});
call_user_func('\Clojure\Php\defProtocol', 'IStack', array(\Clojure\Php\vec('-peek', '-pop')));
$NS->_peek = (function($__L_coll, ...$__L_args2929) { return call_user_func('\Clojure\Php\protocolDispatch', 'IStack', '-peek', $__L_coll, $__L_args2929);});
$NS->_pop = (function($__L_coll, ...$__L_args2930) { return call_user_func('\Clojure\Php\protocolDispatch', 'IStack', '-pop', $__L_coll, $__L_args2930);});
call_user_func('\Clojure\Php\defProtocol', 'ICollection', array(\Clojure\Php\vec('-conj')));
$NS->_conj = (function($__L_coll, ...$__L_args2931) { return call_user_func('\Clojure\Php\protocolDispatch', 'ICollection', '-conj', $__L_coll, $__L_args2931);});
call_user_func('\Clojure\Php\defProtocol', 'IEmptyableCollection', array(\Clojure\Php\vec('-empty')));
$NS->_empty = (function($__L_coll, ...$__L_args2932) { return call_user_func('\Clojure\Php\protocolDispatch', 'IEmptyableCollection', '-empty', $__L_coll, $__L_args2932);});
call_user_func('\Clojure\Php\defProtocol', 'IEquiv', array(\Clojure\Php\vec('-equiv')));
$NS->_equiv = (function($__L_o, ...$__L_args2933) { return call_user_func('\Clojure\Php\protocolDispatch', 'IEquiv', '-equiv', $__L_o, $__L_args2933);});
call_user_func('\Clojure\Php\defProtocol', 'IHash', array(\Clojure\Php\vec('-hash')));
$NS->_hash = (function($__L_o, ...$__L_args2934) { return call_user_func('\Clojure\Php\protocolDispatch', 'IHash', '-hash', $__L_o, $__L_args2934);});
call_user_func('\Clojure\Php\defProtocol', 'IMeta', array(\Clojure\Php\vec('-meta')));
$NS->_meta = (function($__L_o, ...$__L_args2935) { return call_user_func('\Clojure\Php\protocolDispatch', 'IMeta', '-meta', $__L_o, $__L_args2935);});
call_user_func('\Clojure\Php\defProtocol', 'IWithMeta', array(\Clojure\Php\vec('-with-meta')));
$NS->_with_meta = (function($__L_o, ...$__L_args2936) { return call_user_func('\Clojure\Php\protocolDispatch', 'IWithMeta', '-with-meta', $__L_o, $__L_args2936);});
call_user_func('\Clojure\Php\defProtocol', 'IFn', array(\Clojure\Php\vec('-invoke')));
$NS->_invoke = (function($__L_this, ...$__L_args2937) { return call_user_func('\Clojure\Php\protocolDispatch', 'IFn', '-invoke', $__L_this, $__L_args2937);});
call_user_func('\Clojure\Php\defProtocol', 'IReduce', array(\Clojure\Php\vec('-reduce')));
$NS->_reduce = (function($__L_coll, ...$__L_args2938) { return call_user_func('\Clojure\Php\protocolDispatch', 'IReduce', '-reduce', $__L_coll, $__L_args2938);});
call_user_func('\Clojure\Php\defProtocol', 'IReduceInit', array(\Clojure\Php\vec('-reduce-init')));
$NS->_reduce_init = (function($__L_coll, ...$__L_args2939) { return call_user_func('\Clojure\Php\protocolDispatch', 'IReduceInit', '-reduce-init', $__L_coll, $__L_args2939);});
call_user_func('\Clojure\Php\defProtocol', 'IKVReduce', array(\Clojure\Php\vec('-kv-reduce')));
$NS->_kv_reduce = (function($__L_coll, ...$__L_args2940) { return call_user_func('\Clojure\Php\protocolDispatch', 'IKVReduce', '-kv-reduce', $__L_coll, $__L_args2940);});
call_user_func('\Clojure\Php\defProtocol', 'IEditableCollection', array(\Clojure\Php\vec('-as-transient')));
$NS->_as_transient = (function($__L_coll, ...$__L_args2941) { return call_user_func('\Clojure\Php\protocolDispatch', 'IEditableCollection', '-as-transient', $__L_coll, $__L_args2941);});
call_user_func('\Clojure\Php\defProtocol', 'ITransientCollection', array(\Clojure\Php\vec('-conj!', '-persistent!')));
$NS->_conj_BANG_ = (function($__L_tcoll, ...$__L_args2942) { return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientCollection', '-conj!', $__L_tcoll, $__L_args2942);});
$NS->_persistent_BANG_ = (function($__L_tcoll, ...$__L_args2943) { return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientCollection', '-persistent!', $__L_tcoll, $__L_args2943);});
call_user_func('\Clojure\Php\defProtocol', 'ITransientAssociative', array(\Clojure\Php\vec('-assoc!')));
$NS->_assoc_BANG_ = (function($__L_tcoll, ...$__L_args2944) { return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientAssociative', '-assoc!', $__L_tcoll, $__L_args2944);});
call_user_func('\Clojure\Php\defProtocol', 'ITransientMap', array(\Clojure\Php\vec('-dissoc!')));
$NS->_dissoc_BANG_ = (function($__L_tcoll, ...$__L_args2945) { return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientMap', '-dissoc!', $__L_tcoll, $__L_args2945);});
call_user_func('\Clojure\Php\defProtocol', 'ITransientVector', array(\Clojure\Php\vec('-assoc-n!', '-pop!')));
$NS->_assoc_n_BANG_ = (function($__L_tcoll, ...$__L_args2946) { return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientVector', '-assoc-n!', $__L_tcoll, $__L_args2946);});
$NS->_pop_BANG_ = (function($__L_tcoll, ...$__L_args2947) { return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientVector', '-pop!', $__L_tcoll, $__L_args2947);});
call_user_func('\Clojure\Php\defProtocol', 'ITransientSet', array(\Clojure\Php\vec('-disjoin!')));
$NS->_disjoin_BANG_ = (function($__L_tcoll, ...$__L_args2948) { return call_user_func('\Clojure\Php\protocolDispatch', 'ITransientSet', '-disjoin!', $__L_tcoll, $__L_args2948);});
call_user_func('\Clojure\Php\defProtocol', 'IDeref', array(\Clojure\Php\vec('-deref')));
$NS->_deref = (function($__L_o, ...$__L_args2949) { return call_user_func('\Clojure\Php\protocolDispatch', 'IDeref', '-deref', $__L_o, $__L_args2949);});
call_user_func('\Clojure\Php\defProtocol', 'IDerefWithTimeout', array(\Clojure\Php\vec('-deref-with-timeout')));
$NS->_deref_with_timeout = (function($__L_o, ...$__L_args2950) { return call_user_func('\Clojure\Php\protocolDispatch', 'IDerefWithTimeout', '-deref-with-timeout', $__L_o, $__L_args2950);});
call_user_func('\Clojure\Php\defProtocol', 'IReset', array(\Clojure\Php\vec('-reset!')));
$NS->_reset_BANG_ = (function($__L_o, ...$__L_args2951) { return call_user_func('\Clojure\Php\protocolDispatch', 'IReset', '-reset!', $__L_o, $__L_args2951);});
call_user_func('\Clojure\Php\defProtocol', 'ISwap', array(\Clojure\Php\vec('-swap!')));
$NS->_swap_BANG_ = (function($__L_o, ...$__L_args2952) { return call_user_func('\Clojure\Php\protocolDispatch', 'ISwap', '-swap!', $__L_o, $__L_args2952);});
call_user_func('\Clojure\Php\defProtocol', 'IAtom', array(\Clojure\Php\vec('-compare-and-set!')));
$NS->_compare_and_set_BANG_ = (function($__L_o, ...$__L_args2953) { return call_user_func('\Clojure\Php\protocolDispatch', 'IAtom', '-compare-and-set!', $__L_o, $__L_args2953);});
call_user_func('\Clojure\Php\defProtocol', 'IRef', array(\Clojure\Php\vec('-set!')));
$NS->_set_BANG_ = (function($__L_o, ...$__L_args2954) { return call_user_func('\Clojure\Php\protocolDispatch', 'IRef', '-set!', $__L_o, $__L_args2954);});
call_user_func('\Clojure\Php\defProtocol', 'IWatchable', array(\Clojure\Php\vec('-add-watch', '-remove-watch')));
$NS->_add_watch = (function($__L_o, ...$__L_args2955) { return call_user_func('\Clojure\Php\protocolDispatch', 'IWatchable', '-add-watch', $__L_o, $__L_args2955);});
$NS->_remove_watch = (function($__L_o, ...$__L_args2956) { return call_user_func('\Clojure\Php\protocolDispatch', 'IWatchable', '-remove-watch', $__L_o, $__L_args2956);});
call_user_func('\Clojure\Php\defProtocol', 'IValidatable', array(\Clojure\Php\vec('-set-validator!', '-get-validator')));
$NS->_set_validator_BANG_ = (function($__L_o, ...$__L_args2957) { return call_user_func('\Clojure\Php\protocolDispatch', 'IValidatable', '-set-validator!', $__L_o, $__L_args2957);});
$NS->_get_validator = (function($__L_o, ...$__L_args2958) { return call_user_func('\Clojure\Php\protocolDispatch', 'IValidatable', '-get-validator', $__L_o, $__L_args2958);});
call_user_func('\Clojure\Php\defProtocol', 'IPending', array(\Clojure\Php\vec('-realized?')));
$NS->_realized_QMARK_ = (function($__L_o, ...$__L_args2959) { return call_user_func('\Clojure\Php\protocolDispatch', 'IPending', '-realized?', $__L_o, $__L_args2959);});
call_user_func('\Clojure\Php\defProtocol', 'INamed', array(\Clojure\Php\vec('-name', '-namespace')));
$NS->_name = (function($__L_o, ...$__L_args2960) { return call_user_func('\Clojure\Php\protocolDispatch', 'INamed', '-name', $__L_o, $__L_args2960);});
$NS->_namespace = (function($__L_o, ...$__L_args2961) { return call_user_func('\Clojure\Php\protocolDispatch', 'INamed', '-namespace', $__L_o, $__L_args2961);});
call_user_func('\Clojure\Php\defProtocol', 'IPrintWithWriter', array(\Clojure\Php\vec('-pr-writer')));
$NS->_pr_writer = (function($__L_o, ...$__L_args2962) { return call_user_func('\Clojure\Php\protocolDispatch', 'IPrintWithWriter', '-pr-writer', $__L_o, $__L_args2962);});
call_user_func('\Clojure\Php\defProtocol', 'IComparable', array(\Clojure\Php\vec('-compare')));
$NS->_compare = (function($__L_x, ...$__L_args2963) { return call_user_func('\Clojure\Php\protocolDispatch', 'IComparable', '-compare', $__L_x, $__L_args2963);});
call_user_func('\Clojure\Php\defProtocol', 'ISorted', array(\Clojure\Php\vec('-sorted-seq', '-sorted-seq-from', '-entry-key', '-comparator')));
$NS->_sorted_seq = (function($__L_coll, ...$__L_args2964) { return call_user_func('\Clojure\Php\protocolDispatch', 'ISorted', '-sorted-seq', $__L_coll, $__L_args2964);});
$NS->_sorted_seq_from = (function($__L_coll, ...$__L_args2965) { return call_user_func('\Clojure\Php\protocolDispatch', 'ISorted', '-sorted-seq-from', $__L_coll, $__L_args2965);});
$NS->_entry_key = (function($__L_coll, ...$__L_args2966) { return call_user_func('\Clojure\Php\protocolDispatch', 'ISorted', '-entry-key', $__L_coll, $__L_args2966);});
$NS->_comparator = (function($__L_coll, ...$__L_args2967) { return call_user_func('\Clojure\Php\protocolDispatch', 'ISorted', '-comparator', $__L_coll, $__L_args2967);});
call_user_func('\Clojure\Php\defProtocol', 'IReversible', array(\Clojure\Php\vec('-rseq')));
$NS->_rseq = (function($__L_coll, ...$__L_args2968) { return call_user_func('\Clojure\Php\protocolDispatch', 'IReversible', '-rseq', $__L_coll, $__L_args2968);});
call_user_func('\Clojure\Php\defProtocol', 'IChunk', array(\Clojure\Php\vec('-drop-first')));
$NS->_drop_first = (function($__L_coll, ...$__L_args2969) { return call_user_func('\Clojure\Php\protocolDispatch', 'IChunk', '-drop-first', $__L_coll, $__L_args2969);});
call_user_func('\Clojure\Php\defProtocol', 'IChunkedSeq', array(\Clojure\Php\vec('-chunked-first', '-chunked-rest')));
$NS->_chunked_first = (function($__L_coll, ...$__L_args2970) { return call_user_func('\Clojure\Php\protocolDispatch', 'IChunkedSeq', '-chunked-first', $__L_coll, $__L_args2970);});
$NS->_chunked_rest = (function($__L_coll, ...$__L_args2971) { return call_user_func('\Clojure\Php\protocolDispatch', 'IChunkedSeq', '-chunked-rest', $__L_coll, $__L_args2971);});
call_user_func('\Clojure\Php\defProtocol', 'IChunkedNext', array(\Clojure\Php\vec('-chunked-next')));
$NS->_chunked_next = (function($__L_coll, ...$__L_args2972) { return call_user_func('\Clojure\Php\protocolDispatch', 'IChunkedNext', '-chunked-next', $__L_coll, $__L_args2972);});
call_user_func('\Clojure\Php\defProtocol', 'IIterable', array(\Clojure\Php\vec('-iterator')));
$NS->_iterator = (function($__L_coll, ...$__L_args2973) { return call_user_func('\Clojure\Php\protocolDispatch', 'IIterable', '-iterator', $__L_coll, $__L_args2973);});
call_user_func('\Clojure\Php\defProtocol', 'ISequential', array(\Clojure\Php\vec()));
call_user_func('\Clojure\Php\defProtocol', 'IRecord', array(\Clojure\Php\vec()));
call_user_func('\Clojure\Php\defProtocol', 'IMapEntry', array(\Clojure\Php\vec('-key', '-val')));
$NS->_key = (function($__L_coll, ...$__L_args2974) { return call_user_func('\Clojure\Php\protocolDispatch', 'IMapEntry', '-key', $__L_coll, $__L_args2974);});
$NS->_val = (function($__L_coll, ...$__L_args2975) { return call_user_func('\Clojure\Php\protocolDispatch', 'IMapEntry', '-val', $__L_coll, $__L_args2975);});
call_user_func('\Clojure\Php\defProtocol', 'IVolatile', array(\Clojure\Php\vec('-vreset!')));
$NS->_vreset_BANG_ = (function($__L_o, ...$__L_args2976) { return call_user_func('\Clojure\Php\protocolDispatch', 'IVolatile', '-vreset!', $__L_o, $__L_args2976);});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.array'), 'Portable array operations.

   This namespace provides platform-independent array operations.
   Each platform (JVM, PHP, JS, etc.) implements the protocols
   defined here via clojure.{platform}.array namespaces.

   Maps to:
   - JVM: java.lang.reflect.Array, native arrays
   - PHP: array functions
   - JS: Array, TypedArrays');
call_user_func('\Clojure\Php\defProtocol', 'IArray', array(\Clojure\Php\vec('-aget', '-aset', '-alength', '-array-type')));
$NS->_aget = (function($__L_arr, ...$__L_args2977) { return call_user_func('\Clojure\Php\protocolDispatch', 'IArray', '-aget', $__L_arr, $__L_args2977);});
$NS->_aset = (function($__L_arr, ...$__L_args2978) { return call_user_func('\Clojure\Php\protocolDispatch', 'IArray', '-aset', $__L_arr, $__L_args2978);});
$NS->_alength = (function($__L_arr, ...$__L_args2979) { return call_user_func('\Clojure\Php\protocolDispatch', 'IArray', '-alength', $__L_arr, $__L_args2979);});
$NS->_array_type = (function($__L_arr, ...$__L_args2980) { return call_user_func('\Clojure\Php\protocolDispatch', 'IArray', '-array-type', $__L_arr, $__L_args2980);});
call_user_func('\Clojure\Php\defProtocol', 'IMutableBuffer', array(\Clojure\Php\vec('-buf-add', '-buf-empty?', '-buf-size', '-buf-to-array', '-buf-clear')));
$NS->_buf_add = (function($__L_buf, ...$__L_args2981) { return call_user_func('\Clojure\Php\protocolDispatch', 'IMutableBuffer', '-buf-add', $__L_buf, $__L_args2981);});
$NS->_buf_empty_QMARK_ = (function($__L_buf, ...$__L_args2982) { return call_user_func('\Clojure\Php\protocolDispatch', 'IMutableBuffer', '-buf-empty?', $__L_buf, $__L_args2982);});
$NS->_buf_size = (function($__L_buf, ...$__L_args2983) { return call_user_func('\Clojure\Php\protocolDispatch', 'IMutableBuffer', '-buf-size', $__L_buf, $__L_args2983);});
$NS->_buf_to_array = (function($__L_buf, ...$__L_args2984) { return call_user_func('\Clojure\Php\protocolDispatch', 'IMutableBuffer', '-buf-to-array', $__L_buf, $__L_args2984);});
$NS->_buf_clear = (function($__L_buf, ...$__L_args2985) { return call_user_func('\Clojure\Php\protocolDispatch', 'IMutableBuffer', '-buf-clear', $__L_buf, $__L_args2985);});
call_user_func('\Clojure\Php\defProtocol', 'ArrayEngine', array(\Clojure\Php\vec('-make-array', '-make-array-2d', '-make-array-nd', '-int-array', '-long-array', '-float-array', '-double-array', '-byte-array', '-char-array', '-boolean-array', '-short-array', '-object-array', '-to-array', '-to-array-2d', '-array-seq', '-array-seq-offset', '-aclone', '-acopy', '-afill', '-afill-range', '-array?', '-array-class', '-component-type', '-asort', '-asort-comparator', '-shuffle', '-make-buffer', '-make-buffer-sized')));
$NS->_make_array = (function($__L_e, ...$__L_args2986) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-make-array', $__L_e, $__L_args2986);});
$NS->_make_array_2d = (function($__L_e, ...$__L_args2987) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-make-array-2d', $__L_e, $__L_args2987);});
$NS->_make_array_nd = (function($__L_e, ...$__L_args2988) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-make-array-nd', $__L_e, $__L_args2988);});
$NS->_int_array = (function($__L_e, ...$__L_args2989) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-int-array', $__L_e, $__L_args2989);});
$NS->_long_array = (function($__L_e, ...$__L_args2990) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-long-array', $__L_e, $__L_args2990);});
$NS->_float_array = (function($__L_e, ...$__L_args2991) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-float-array', $__L_e, $__L_args2991);});
$NS->_double_array = (function($__L_e, ...$__L_args2992) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-double-array', $__L_e, $__L_args2992);});
$NS->_byte_array = (function($__L_e, ...$__L_args2993) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-byte-array', $__L_e, $__L_args2993);});
$NS->_char_array = (function($__L_e, ...$__L_args2994) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-char-array', $__L_e, $__L_args2994);});
$NS->_boolean_array = (function($__L_e, ...$__L_args2995) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-boolean-array', $__L_e, $__L_args2995);});
$NS->_short_array = (function($__L_e, ...$__L_args2996) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-short-array', $__L_e, $__L_args2996);});
$NS->_object_array = (function($__L_e, ...$__L_args2997) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-object-array', $__L_e, $__L_args2997);});
$NS->_to_array = (function($__L_e, ...$__L_args2998) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-to-array', $__L_e, $__L_args2998);});
$NS->_to_array_2d = (function($__L_e, ...$__L_args2999) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-to-array-2d', $__L_e, $__L_args2999);});
$NS->_array_seq = (function($__L_e, ...$__L_args3000) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-array-seq', $__L_e, $__L_args3000);});
$NS->_array_seq_offset = (function($__L_e, ...$__L_args3001) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-array-seq-offset', $__L_e, $__L_args3001);});
$NS->_aclone = (function($__L_e, ...$__L_args3002) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-aclone', $__L_e, $__L_args3002);});
$NS->_acopy = (function($__L_e, ...$__L_args3003) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-acopy', $__L_e, $__L_args3003);});
$NS->_afill = (function($__L_e, ...$__L_args3004) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-afill', $__L_e, $__L_args3004);});
$NS->_afill_range = (function($__L_e, ...$__L_args3005) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-afill-range', $__L_e, $__L_args3005);});
$NS->_array_QMARK_ = (function($__L_e, ...$__L_args3006) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-array?', $__L_e, $__L_args3006);});
$NS->_array_class = (function($__L_e, ...$__L_args3007) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-array-class', $__L_e, $__L_args3007);});
$NS->_component_type = (function($__L_e, ...$__L_args3008) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-component-type', $__L_e, $__L_args3008);});
$NS->_asort = (function($__L_e, ...$__L_args3009) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-asort', $__L_e, $__L_args3009);});
$NS->_asort_comparator = (function($__L_e, ...$__L_args3010) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-asort-comparator', $__L_e, $__L_args3010);});
$NS->_shuffle = (function($__L_e, ...$__L_args3011) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-shuffle', $__L_e, $__L_args3011);});
$NS->_make_buffer = (function($__L_e, ...$__L_args3012) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-make-buffer', $__L_e, $__L_args3012);});
$NS->_make_buffer_sized = (function($__L_e, ...$__L_args3013) { return call_user_func('\Clojure\Php\protocolDispatch', 'ArrayEngine', '-make-buffer-sized', $__L_e, $__L_args3013);});
$NS->_STAR_array_engine_STAR_ = null;
$NS->make_array = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_type = $__args[0]; $__L_dim1 = $__args[1]; $__L_dim2 = $__args[2]; return ($NS->_make_array_2d)($NS->_STAR_array_engine_STAR_, $__L_type, $__L_dim1, $__L_dim2); } else if ($__n == 2) { $__L_type = $__args[0]; $__L_length = $__args[1]; return ($NS->_make_array)($NS->_STAR_array_engine_STAR_, $__L_type, $__L_length); } else if ($__n >= 3) { $__L_type = $__args[0]; $__L_dim1 = $__args[1]; $__L_dim2 = $__args[2]; $__L_more = array_slice($__args, 3); return ($NS->_make_array_nd)($NS->_STAR_array_engine_STAR_, $__L_type, ($NS->into)(\Clojure\Php\vec($__L_dim1, $__L_dim2), $__L_more)); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->int_array = (function($__L_size_or_seq) { return ($NS->_int_array)($NS->_STAR_array_engine_STAR_, $__L_size_or_seq);});
$NS->long_array = (function($__L_size_or_seq) { return ($NS->_long_array)($NS->_STAR_array_engine_STAR_, $__L_size_or_seq);});
$NS->float_array = (function($__L_size_or_seq) { return ($NS->_float_array)($NS->_STAR_array_engine_STAR_, $__L_size_or_seq);});
$NS->double_array = (function($__L_size_or_seq) { return ($NS->_double_array)($NS->_STAR_array_engine_STAR_, $__L_size_or_seq);});
$NS->byte_array = (function($__L_size_or_seq) { return ($NS->_byte_array)($NS->_STAR_array_engine_STAR_, $__L_size_or_seq);});
$NS->char_array = (function($__L_size_or_seq) { return ($NS->_char_array)($NS->_STAR_array_engine_STAR_, $__L_size_or_seq);});
$NS->boolean_array = (function($__L_size_or_seq) { return ($NS->_boolean_array)($NS->_STAR_array_engine_STAR_, $__L_size_or_seq);});
$NS->short_array = (function($__L_size_or_seq) { return ($NS->_short_array)($NS->_STAR_array_engine_STAR_, $__L_size_or_seq);});
$NS->object_array = (function($__L_size_or_seq) { return ($NS->_object_array)($NS->_STAR_array_engine_STAR_, $__L_size_or_seq);});
$NS->aget = (function($__L_arr, $__L_idx) { return ($NS->_aget)($__L_arr, $__L_idx);});
$NS->aset = (function($__L_arr, $__L_idx, $__L_val) { ($NS->_aset)($__L_arr, $__L_idx, $__L_val);
return $__L_val;});
$NS->alength = (function($__L_arr) { return ($NS->_alength)($__L_arr);});
$NS->to_array = (function($__L_coll) { return ($NS->_to_array)($NS->_STAR_array_engine_STAR_, $__L_coll);});
$NS->to_array_2d = (function($__L_coll) { return ($NS->_to_array_2d)($NS->_STAR_array_engine_STAR_, $__L_coll);});
$NS->into_array = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_type = $__args[0]; $__L_coll = $__args[1];  while(true) { $__L_arr = ($__L_type ? ($NS->make_array)($__L_type, \Clojure\Php\count_($__L_coll)) : ($NS->object_array)(\Clojure\Php\count_($__L_coll)));
$__L_i = 0;
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ($__L_s) { ($NS->aset)($__L_arr, $__L_i, \Clojure\Php\first($__L_s));
$__recur_0 = ($__L_i + 1); $__recur_1 = \Clojure\Php\next_($__L_s); $__L_i = $__recur_0; $__L_s = $__recur_1; continue;} else { return $__L_arr;} break; }
 break; } } else if ($__n == 1) { $__L_coll = $__args[0]; return ($NS->into_array)(null, $__L_coll); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->array_seq = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_arr = $__args[0]; $__L_offset = $__args[1]; return ($NS->_array_seq_offset)($NS->_STAR_array_engine_STAR_, $__L_arr, $__L_offset); } else if ($__n == 1) { $__L_arr = $__args[0]; return ($NS->_array_seq)($NS->_STAR_array_engine_STAR_, $__L_arr); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->aclone = (function($__L_arr) { return ($NS->_aclone)($NS->_STAR_array_engine_STAR_, $__L_arr);});
$NS->acopy = (function($__L_src, $__L_src_pos, $__L_dest, $__L_dest_pos, $__L_length) { return ($NS->_acopy)($NS->_STAR_array_engine_STAR_, $__L_src, $__L_src_pos, $__L_dest, $__L_dest_pos, $__L_length);});
$NS->afill = (function(...$__args) { $__n = count($__args); if ($__n == 4) { $__L_arr = $__args[0]; $__L_start = $__args[1]; $__L_end = $__args[2]; $__L_val = $__args[3]; return ($NS->_afill_range)($NS->_STAR_array_engine_STAR_, $__L_arr, $__L_start, $__L_end, $__L_val); } else if ($__n == 2) { $__L_arr = $__args[0]; $__L_val = $__args[1]; return ($NS->_afill)($NS->_STAR_array_engine_STAR_, $__L_arr, $__L_val); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->array_QMARK_ = (function($__L_x) { return ($NS->_array_QMARK_)($NS->_STAR_array_engine_STAR_, $__L_x);});
$NS->amap = (function($__L_f, $__L_arr) {  while(true) { $__L_len = ($NS->alength)($__L_arr);
$__L_ret = ($NS->aclone)($__L_arr);
$__L_i = 0;
 while(true) { if (($__L_i < $__L_len)) { ($NS->aset)($__L_ret, $__L_i, call_user_func($__L_f, ($NS->aget)($__L_arr, $__L_i)));
$__recur_0 = ($__L_i + 1); $__L_i = $__recur_0; continue;}
 break; }
return $__L_ret; break; }});
$NS->areduce = (function($__L_f, $__L_init, $__L_arr) {  while(true) { $__L_len = ($NS->alength)($__L_arr);
$__L_i = 0;
$__L_acc = $__L_init;
 while(true) { if (($__L_i < $__L_len)) { $__recur_0 = ($__L_i + 1); $__recur_1 = call_user_func($__L_f, $__L_acc, ($NS->aget)($__L_arr, $__L_i)); $__L_i = $__recur_0; $__L_acc = $__recur_1; continue;} else { return $__L_acc;} break; }
 break; }});
$NS->afilter = (function($__L_pred, $__L_arr) { return ($NS->into_array)(($NS->filter)($__L_pred, ($NS->array_seq)($__L_arr)));});
$NS->asort = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_arr = $__args[0]; $__L_comparator = $__args[1]; return ($NS->_asort_comparator)($NS->_STAR_array_engine_STAR_, $__L_arr, $__L_comparator); } else if ($__n == 1) { $__L_arr = $__args[0]; return ($NS->_asort)($NS->_STAR_array_engine_STAR_, $__L_arr); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->shuffle = (function($__L_coll) { return ($NS->_shuffle)($NS->_STAR_array_engine_STAR_, $__L_coll);});
$NS->make_buffer = (function(...$__args) { $__n = count($__args); if ($__n == 1) { $__L_capacity = $__args[0]; return ($NS->_make_buffer_sized)($NS->_STAR_array_engine_STAR_, $__L_capacity); } else if ($__n == 0) { return ($NS->_make_buffer)($NS->_STAR_array_engine_STAR_); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->buf_add = (function($__L_buf, $__L_val) { return ($NS->_buf_add)($__L_buf, $__L_val);});
$NS->buf_empty_QMARK_ = (function($__L_buf) { return ($NS->_buf_empty_QMARK_)($__L_buf);});
$NS->buf_size = (function($__L_buf) { return ($NS->_buf_size)($__L_buf);});
$NS->buf_to_array = (function($__L_buf) { return ($NS->_buf_to_array)($__L_buf);});
$NS->buf_clear = (function($__L_buf) { return ($NS->_buf_clear)($__L_buf);});
($NS->ns)(\Clojure\Php\Sym::create('clojure.php.array'), 'PHP implementation of array protocols.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.array'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('arr')]));
call_user_func('\Clojure\Php\defType', 'MutableBuffer', array('arr'), array('arr'), array('-buf-add', (function($__L_this, $__L_val) { aget($__L_this->arr, count($__L_this->arr), $__L_val);
return $__L_this;}), '-buf-empty?', (function($__L__) { return (0 === count($__L__->arr));}), '-buf-size', (function($__L__) { return count($__L__->arr);}), '-buf-to-array', (function($__L__) { return $__L__->arr;}), '-buf-clear', (function($__L_this) { $__L_this->arr = array();
return $__L_this;})));
$NS->__GT_MutableBuffer = (function($__L_arr) { return call_user_func('\Clojure\Php\createType', 'MutableBuffer', $__L_arr);});
$NS->engine = call_user_func('\Clojure\Php\reify', array('-make-array', (function($__L__, $__L_type, $__L_length) { return array_fill(0, $__L_length, null);}), '-make-array-2d', (function($__L__, $__L_type, $__L_dim1, $__L_dim2) { $__L_outer = array_fill(0, $__L_dim1, null);
($NS->dotimes)(\Clojure\Php\vec($NS->i, $__L_dim1), aset($__L_outer, $NS->i, array_fill(0, $__L_dim2, null)));
return $__L_outer;}), '-make-array-nd', (function($__L__, $__L_type, $__L_dims) { if (\Clojure\Php\equals(1, \Clojure\Php\count_($__L_dims))) { return array_fill(0, \Clojure\Php\first($__L_dims), null);} else { $__L_outer = array_fill(0, \Clojure\Php\first($__L_dims), null);
($NS->dotimes)(\Clojure\Php\vec($NS->i, \Clojure\Php\first($__L_dims)), aset($__L_outer, $NS->i, ($Arr->_make_array_nd)($__L__, $__L_type, \Clojure\Php\rest($__L_dims))));
return $__L_outer;}}), '-int-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0);} else { return array_values(($NS->to_array)($__L_size_or_seq));}}), '-long-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0);} else { return array_values(($NS->to_array)($__L_size_or_seq));}}), '-float-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0.0);} else { return array_values(($NS->to_array)($__L_size_or_seq));}}), '-double-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0.0);} else { return array_values(($NS->to_array)($__L_size_or_seq));}}), '-byte-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0);} else { return array_values(($NS->to_array)($__L_size_or_seq));}}), '-char-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, '');} else { return array_values(($NS->to_array)($__L_size_or_seq));}}), '-boolean-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, false);} else { return array_values(($NS->to_array)($__L_size_or_seq));}}), '-short-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, 0);} else { return array_values(($NS->to_array)($__L_size_or_seq));}}), '-object-array', (function($__L__, $__L_size_or_seq) { if ((is_int($__L_size_or_seq) || is_float($__L_size_or_seq))) { return array_fill(0, $__L_size_or_seq, null);} else { return array_values(($NS->to_array)($__L_size_or_seq));}}), '-to-array', (function($__L__, $__L_coll) {  while(true) { if (is_array($__L_coll)) { return array_values($__L_coll);} else { $__L_arr = array();
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ($__L_s) { aget($__L_arr, count($__L_arr), \Clojure\Php\first($__L_s));
$__recur_0 = \Clojure\Php\next_($__L_s); $__L_s = $__recur_0; continue;}
 break; }
return $__L_arr;} break; }}), '-to-array-2d', (function($__L__, $__L_coll) {  while(true) { $__L_arr = array();
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ($__L_s) { aget($__L_arr, count($__L_arr), ($Arr->_to_array)($__L__, \Clojure\Php\first($__L_s)));
$__recur_0 = \Clojure\Php\next_($__L_s); $__L_s = $__recur_0; continue;}
 break; }
return $__L_arr; break; }}), '-array-seq', (function($__L__, $__L_arr) { if (($__L_arr ? (count($__L_arr) > 0) : false)) { return \Clojure\Php\seq(array_values($__L_arr));}}), '-array-seq-offset', (function($__L__, $__L_arr, $__L_offset) { if (($__L_arr ? ($__L_offset < count($__L_arr)) : false)) { return \Clojure\Php\seq(array_slice($__L_arr, $__L_offset));}}), '-aclone', (function($__L__, $__L_arr) { return array_values($__L_arr);}), '-acopy', (function($__L__, $__L_src, $__L_src_pos, $__L_dest, $__L_dest_pos, $__L_length) { return ($NS->dotimes)(\Clojure\Php\vec($NS->i, $__L_length), aset($__L_dest, ($__L_dest_pos + $NS->i), aget($__L_src, ($__L_src_pos + $NS->i))));}), '-afill', (function($__L__, $__L_arr, $__L_val) { $__L_len = count($__L_arr);
return ($NS->dotimes)(\Clojure\Php\vec($NS->i, $__L_len), aset($__L_arr, $NS->i, $__L_val));}), '-afill-range', (function($__L__, $__L_arr, $__L_start, $__L_end, $__L_val) {  while(true) { $__L_i = $__L_start;
 while(true) { if (($__L_i < $__L_end)) { aset($__L_arr, $__L_i, $__L_val);
$__recur_0 = ($__L_i + 1); $__L_i = $__recur_0; continue;} break; }
 break; }}), '-array?', (function($__L__, $__L_x) { return is_array($__L_x);}), '-array-class', (function($__L__, $__L_type) { return null;}), '-component-type', (function($__L__, $__L_arr) { return null;}), '-asort', (function($__L__, $__L_arr) { sort($__L_arr);
return $__L_arr;}), '-asort-comparator', (function($__L__, $__L_arr, $__L_comparator) { usort($__L_arr, $__L_comparator);
return $__L_arr;}), '-shuffle', (function($__L__, $__L_coll) { $__L_arr = ($Arr->_to_array)($__L__, $__L_coll);
shuffle($__L_arr);
return \Vec::create($__L_arr);}), '-make-buffer', (function($__L__) { return ($NS->__GT_MutableBuffer)(array());}), '-make-buffer-sized', (function($__L__, $__L_capacity) { return ($NS->__GT_MutableBuffer)(array());})));
$NS->init_BANG_ = (function() { return $GLOBALS['_STAR_array_engine_STAR_'] = $NS->engine;});
($NS->init_BANG_)();
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.kernel'), 'Kernel types - the minimal platform-provided types.

   These are the only types that need platform-specific implementations.
   Everything else is built in pure Clojure on top of these.

   Platform implementors provide:
   - Cons: a pair (head, tail)
   - Sym: a symbol (ns, name)
   - Kw: a keyword (ns, name), interned
   - Atom: a mutable reference (val)

   The compiler emits appropriate constructors/field access for each platform.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')]));
call_user_func('\Clojure\Php\defType', 'Cons', array('head', 'tail', '_meta'), array(), array('-first', (function($__L__) { return $__L__->head;}), '-rest', (function($__L__) { if (($__L__->tail === null)) { return \Clojure\Php\emptyList();} else { return $__L__->tail;}}), '-next', (function($__L__) { if (($__L__->tail === null)) { return null;} else { return ($P->_seq)($__L__->tail);}}), '-seq', (function($__L_this) { return $__L_this;}), '-count', (function($__L_this) {  while(true) { $__L_s = $__L_this;
$__L_c = 0;
 while(true) { if (($__L_s === null)) { return $__L_c;} else { $__recur_0 = ($P->_next)($__L_s); $__recur_1 = ($__L_c + 1); $__L_s = $__recur_0; $__L_c = $__recur_1; continue;} break; }
 break; }}), '-conj', (function($__L_this, $__L_o) { return ($NS->__GT_Cons)($__L_o, $__L_this, $__L_this->_meta);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-equiv', (function($__L_this, $__L_other) {  while(true) { if (($NS->satisfies_QMARK_)($P->ISequential, $__L_other)) { $__L_s1 = $__L_this;
$__L_s2 = ($P->_seq)($__L_other);
 while(true) { if (($__L_s1 === null)) { return ($__L_s2 === null);} else { if (($__L_s2 === null)) { return false;} else { if (\Clojure\Php\equals(($P->_first)($__L_s1), ($P->_first)($__L_s2))) { $__recur_0 = ($P->_next)($__L_s1); $__recur_1 = ($P->_next)($__L_s2); $__L_s1 = $__recur_0; $__L_s2 = $__recur_1; continue;} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}} break; }
} else { return false;} break; }}), '-hash', (function($__L_this) {  while(true) { $__L_s = $__L_this;
$__L_h = 1;
 while(true) { if (($__L_s === null)) { return $__L_h;} else { $__recur_0 = ($P->_next)($__L_s); $__recur_1 = ((31 * $__L_h) + ($NS->hash)(($P->_first)($__L_s))); $__L_s = $__recur_0; $__L_h = $__recur_1; continue;} break; }
 break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_Cons)($__L__->head, $__L__->tail, $__L_m);})));
$NS->__GT_Cons = (function($__L_head, $__L_tail, $__L__meta) { return call_user_func('\Clojure\Php\createType', 'Cons', $__L_head, $__L_tail, $__L__meta);});
call_user_func('\Clojure\Php\defType', 'Sym', array('ns', 'name', '_meta', '_hash'), array(), array('-namespace', (function($__L__) { return $__L__->ns;}), '-name', (function($__L__) { return $__L__->name;}), '-equiv', (function($__L__, $__L_other) { if (($NS->instance_QMARK_)($NS->Sym, $__L_other)) { if (\Clojure\Php\equals($__L__->ns, $__L_other->ns)) { return \Clojure\Php\equals($__L__->name, $__L_other->name);} else { return false;}} else { return false;}}), '-hash', (function($__L__) { if (($__L__->_hash === null)) { $__L_h = (($NS->hash)($__L__->name) + (31 * ($NS->hash)($__L__->ns)));
$__L__->_hash = $__L_h;
return $__L_h;} else { return $__L__->_hash;}}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_Sym)($__L__->ns, $__L__->name, $__L_m, $__L__->_hash);}), '-invoke', (function($__L_this, $__L_coll) { return ($P->_lookup)($__L_coll, $__L_this);}), '-invoke', (function($__L_this, $__L_coll, $__L_not_found) { return ($P->_lookup)($__L_coll, $__L_this, $__L_not_found);})));
$NS->__GT_Sym = (function($__L_ns, $__L_name, $__L__meta, $__L__hash) { return call_user_func('\Clojure\Php\createType', 'Sym', $__L_ns, $__L_name, $__L__meta, $__L__hash);});
$NS->symbol = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_ns = $__args[0]; $__L_name = $__args[1]; return ($NS->__GT_Sym)($__L_ns, $__L_name, null, null); } else if ($__n == 1) { $__L_name = $__args[0]; if (($NS->instance_QMARK_)($NS->Sym, $__L_name)) { return $__L_name;} else { $__L_idx = (is_string($__L_name) ? $__L_name->indexOf('/') : false);
if (($__L_idx ? ($__L_idx > 0) : false)) { return ($NS->__GT_Sym)(($NS->subs)($__L_name, 0, $__L_idx), ($NS->subs)($__L_name, ($__L_idx + 1)), null, null);} else { return ($NS->__GT_Sym)(null, $__L_name, null, null);}} } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->keyword_cache = ($NS->atom)(\Clojure\Php\hashMap());
call_user_func('\Clojure\Php\defType', 'Kw', array('ns', 'name', '_hash'), array(), array('-namespace', (function($__L__) { return $__L__->ns;}), '-name', (function($__L__) { return $__L__->name;}), '-equiv', (function($__L_this, $__L_other) { return ($__L_this === $__L_other);}), '-hash', (function($__L__) { if (($__L__->_hash === null)) { $__L_h = (($NS->hash)($__L__->name) + (31 * ($NS->hash)($__L__->ns)) + 2654435769);
$__L__->_hash = $__L_h;
return $__L_h;} else { return $__L__->_hash;}}), '-invoke', (function($__L_this, $__L_coll) { return ($P->_lookup)($__L_coll, $__L_this);}), '-invoke', (function($__L_this, $__L_coll, $__L_not_found) { return ($P->_lookup)($__L_coll, $__L_this, $__L_not_found);})));
$NS->__GT_Kw = (function($__L_ns, $__L_name, $__L__hash) { return call_user_func('\Clojure\Php\createType', 'Kw', $__L_ns, $__L_name, $__L__hash);});
$NS->keyword = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_ns = $__args[0]; $__L_name = $__args[1]; $__L_k = \Clojure\Php\str_($__L_ns, '/', $__L_name);
$__L_or__3014 = \Clojure\Php\get_(($Clojure_Core->deref)($NS->keyword_cache), $__L_k);
if ($__L_or__3014) { return $__L_or__3014;} else { $__L_kw = ($NS->__GT_Kw)($__L_ns, $__L_name, null);
($NS->swap_BANG_)($NS->keyword_cache, $NS->assoc, $__L_k, $__L_kw);
return $__L_kw;} } else if ($__n == 1) { $__L_name = $__args[0]; if (($NS->instance_QMARK_)($NS->Kw, $__L_name)) { return $__L_name;} else { $__L_idx = (is_string($__L_name) ? $__L_name->indexOf('/') : false);
if (($__L_idx ? ($__L_idx > 0) : false)) { return ($NS->keyword)(($NS->subs)($__L_name, 0, $__L_idx), ($NS->subs)($__L_name, ($__L_idx + 1)));} else { return ($NS->keyword)(null, $__L_name);}} } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
call_user_func('\Clojure\Php\defType', 'Atom', array('val', '_meta', 'validator', 'watches'), array('val', 'validator', 'watches'), array('-deref', (function($__L__) { return $__L__->val;}), '-reset!', (function($__L_this, $__L_new_value) {  while(true) { if ($__L_this->validator) { if (call_user_func($__L_this->validator, $__L_new_value)) { null;
} else { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('value'), $__L_new_value));
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
return $__L_new_value; break; }}), '-swap!', (function($__L_this, $__L_f) { return ($P->_reset_BANG_)($__L_this, call_user_func($__L_f, $__L_this->val));}), '-swap!', (function($__L_this, $__L_f, $__L_a) { return ($P->_reset_BANG_)($__L_this, call_user_func($__L_f, $__L_this->val, $__L_a));}), '-swap!', (function($__L_this, $__L_f, $__L_a, $__L_b) { return ($P->_reset_BANG_)($__L_this, call_user_func($__L_f, $__L_this->val, $__L_a, $__L_b));}), '-swap!', (function($__L_this, $__L_f, $__L_a, $__L_b, $__L_xs) { return ($P->_reset_BANG_)($__L_this, ($NS->apply)($__L_f, $__L_this->val, $__L_a, $__L_b, $__L_xs));}), '-compare-and-set!', (function($__L_this, $__L_oldval, $__L_newval) { if (\Clojure\Php\equals($__L_this->val, $__L_oldval)) { ($P->_reset_BANG_)($__L_this, $__L_newval);
return true;} else { return false;}}), '-add-watch', (function($__L__, $__L_key, $__L_f) { return $__L__->watches = \Clojure\Php\assoc(((function() { $__L_or__3016 = $__L__->watches; if ($__L_or__3016) { return $__L_or__3016;} else { return \Clojure\Php\hashMap();} })()), $__L_key, $__L_f);}), '-remove-watch', (function($__L__, $__L_key) { return $__L__->watches = \Clojure\Php\dissoc($__L__->watches, $__L_key);}), '-set-validator!', (function($__L__, $__L_f) { if ($__L_f) { if (call_user_func($__L_f, $__L__->val)) { null;
} else { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('value'), $__L__->val));
}
}
return $__L__->validator = $__L_f;}), '-get-validator', (function($__L__) { return $__L__->validator;}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_Atom)($__L__->val, $__L_m, $__L__->validator, $__L__->watches);})));
$NS->__GT_Atom = (function($__L_val, $__L__meta, $__L_validator, $__L_watches) { return call_user_func('\Clojure\Php\createType', 'Atom', $__L_val, $__L__meta, $__L_validator, $__L_watches);});
$NS->atom = (function(...$__args) { $__n = count($__args); if ($__n == 1) { $__L_val = $__args[0]; return ($NS->__GT_Atom)($__L_val, null, null, null); } else if ($__n >= 1) { $__L_val = $__args[0]; $__L___dest_2 = array_slice($__args, 1); $__L___dest_3 = $__L___dest_2;
$__L_meta = \Clojure\Php\get_($__L___dest_3, \Clojure\Php\Kw::create('meta'));
$__L_validator = \Clojure\Php\get_($__L___dest_3, \Clojure\Php\Kw::create('validator'));
$__L_a = ($NS->__GT_Atom)($__L_val, $__L_meta, null, null);
if ($__L_validator) { ($P->_set_validator_BANG_)($__L_a, $__L_validator);
}
return $__L_a; } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
call_user_func('\Clojure\Php\defType', 'EmptyList', array('_meta'), array(), array('-first', (function($__L__) { return null;}), '-rest', (function($__L__) { return \Clojure\Php\emptyList();}), '-next', (function($__L__) { return null;}), '-seq', (function($__L__) { return null;}), '-count', (function($__L__) { return 0;}), '-conj', (function($__L__, $__L_o) { return ($NS->__GT_Cons)($__L_o, null, $__L__->_meta);}), '-empty', (function($__L_this) { return $__L_this;}), '-equiv', (function($__L__, $__L_other) { if (($NS->satisfies_QMARK_)($P->ISequential, $__L_other)) { return (($P->_seq)($__L_other) === null);} else { return false;}}), '-hash', (function($__L__) { return 1;}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_EmptyList)($__L_m);})));
$NS->__GT_EmptyList = (function($__L__meta) { return call_user_func('\Clojure\Php\createType', 'EmptyList', $__L__meta);});
$NS->EMPTY_LIST = ($NS->__GT_EmptyList)(null);
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.RT'), 'Runtime functions - portable implementation matching JVM clojure.lang.RT.

   Provides the same API as the JVM RT class so core.cljc works unchanged.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$NS->seq = (function($__L_coll) { if ($__L_coll) { return ($P->_seq)($__L_coll);}});
$NS->first = (function($__L_coll) { if (($__L_coll === null)) { return null;} else { if (($NS->satisfies_QMARK_)($P->ISeq, $__L_coll)) { return ($P->_first)($__L_coll);} else { $__L_s = \Clojure\Php\seq($__L_coll);
if ($__L_s) { return ($P->_first)($__L_s);}}}});
$NS->rest = (function($__L_coll) { if (($__L_coll === null)) { return \Clojure\Php\emptyList();} else { if (($NS->satisfies_QMARK_)($P->ISeq, $__L_coll)) { return ($P->_rest)($__L_coll);} else { $__L_s = \Clojure\Php\seq($__L_coll);
if ($__L_s) { return ($P->_rest)($__L_s);} else { return \Clojure\Php\emptyList();}}}});
$NS->next = (function($__L_coll) { if (($__L_coll === null)) { return null;} else { if (($NS->satisfies_QMARK_)($P->INext, $__L_coll)) { return ($P->_next)($__L_coll);} else { return \Clojure\Php\seq(\Clojure\Php\rest($__L_coll));}}});
$NS->cons = (function($__L_x, $__L_coll) { return ($NS->__GT_Cons)($__L_x, \Clojure\Php\seq($__L_coll), null);});
$NS->conj = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_coll = $__args[0]; $__L_x = $__args[1]; if (($__L_coll === null)) { return ($NS->list)($__L_x);} else { return ($P->_conj)($__L_coll, $__L_x);} } else if ($__n == 1) { $__L_coll = $__args[0]; return $__L_coll; } else if ($__n == 0) { return \Clojure\Php\vec(); } else if ($__n >= 2) { $__L_coll = $__args[0]; $__L_x = $__args[1]; $__L_xs = array_slice($__args, 2);  while(true) { if ($__L_xs) { $__recur_0 = \Clojure\Php\conj($__L_coll, $__L_x); $__recur_1 = \Clojure\Php\first($__L_xs); $__recur_2 = \Clojure\Php\next_($__L_xs); $__L_coll = $__recur_0; $__L_x = $__recur_1; $__L_xs = $__recur_2; continue;} else { return \Clojure\Php\conj($__L_coll, $__L_x);} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->list = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_a = $__args[0]; $__L_b = $__args[1]; $__L_c = $__args[2]; return \Clojure\Php\cons($__L_a, \Clojure\Php\cons($__L_b, \Clojure\Php\cons($__L_c, null))); } else if ($__n == 2) { $__L_a = $__args[0]; $__L_b = $__args[1]; return \Clojure\Php\cons($__L_a, \Clojure\Php\cons($__L_b, null)); } else if ($__n == 1) { $__L_a = $__args[0]; return \Clojure\Php\cons($__L_a, null); } else if ($__n == 0) { return $K->EMPTY_LIST; } else if ($__n >= 3) { $__L_a = $__args[0]; $__L_b = $__args[1]; $__L_c = $__args[2]; $__L_more = array_slice($__args, 3); return \Clojure\Php\cons($__L_a, \Clojure\Php\cons($__L_b, \Clojure\Php\cons($__L_c, ($NS->reduce)($NS->cons, \Clojure\Php\emptyList(), ($NS->reverse)($__L_more))))); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->count = (function($__L_coll) {  while(true) { if (($__L_coll === null)) { return 0;} else { if (($NS->satisfies_QMARK_)($P->ICounted, $__L_coll)) { return ($P->_count)($__L_coll);} else { $__L_s = \Clojure\Php\seq($__L_coll);
$__L_n = 0;
 while(true) { if ($__L_s) { $__recur_0 = \Clojure\Php\next_($__L_s); $__recur_1 = ($__L_n + 1); $__L_s = $__recur_0; $__L_n = $__recur_1; continue;} else { return $__L_n;} break; }
}} break; }});
$NS->empty_QMARK_ = (function($__L_coll) { return (!\Clojure\Php\seq($__L_coll));});
$NS->nth = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_coll = $__args[0]; $__L_n = $__args[1]; $__L_not_found = $__args[2];  while(true) { if (($__L_coll === null)) { return $__L_not_found;} else { if (($NS->satisfies_QMARK_)($P->IIndexed, $__L_coll)) { return ($P->_nth)($__L_coll, $__L_n, $__L_not_found);} else { $__L_s = \Clojure\Php\seq($__L_coll);
$__L_i = $__L_n;
 while(true) { if ($__L_s) { if (($__L_i === 0 || $__L_i === 0.0)) { return \Clojure\Php\first($__L_s);} else { $__recur_0 = \Clojure\Php\next_($__L_s); $__recur_1 = ($__L_i - 1); $__L_s = $__recur_0; $__L_i = $__recur_1; continue;}} else { return $__L_not_found;} break; }
}} break; } } else if ($__n == 2) { $__L_coll = $__args[0]; $__L_n = $__args[1];  while(true) { if (($__L_coll === null)) { throw ($NS->ex_info)('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n));
} else { if (($NS->satisfies_QMARK_)($P->IIndexed, $__L_coll)) { return ($P->_nth)($__L_coll, $__L_n);} else { $__L_s = \Clojure\Php\seq($__L_coll);
$__L_i = $__L_n;
 while(true) { if ($__L_s) { if (($__L_i === 0 || $__L_i === 0.0)) { return \Clojure\Php\first($__L_s);} else { $__recur_0 = \Clojure\Php\next_($__L_s); $__recur_1 = ($__L_i - 1); $__L_s = $__recur_0; $__L_i = $__recur_1; continue;}} else { throw ($NS->ex_info)('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n));
} break; }
}} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->get = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_m = $__args[0]; $__L_k = $__args[1]; $__L_not_found = $__args[2]; if ($__L_m) { if (($NS->satisfies_QMARK_)($P->ILookup, $__L_m)) { return ($P->_lookup)($__L_m, $__L_k, $__L_not_found);} else { return $__L_not_found;}} else { return $__L_not_found;} } else if ($__n == 2) { $__L_m = $__args[0]; $__L_k = $__args[1]; if ($__L_m) { if (($NS->satisfies_QMARK_)($P->ILookup, $__L_m)) { return ($P->_lookup)($__L_m, $__L_k);}} } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->contains_QMARK_ = (function($__L_coll, $__L_k) { if (($__L_coll === null)) { return false;} else { if (($NS->satisfies_QMARK_)($P->IAssociative, $__L_coll)) { return ($P->_contains_key_QMARK_)($__L_coll, $__L_k);} else { return (!(\Clojure\Php\get_($__L_coll, $__L_k, \Clojure\Php\Kw::createNs('user', 'not-found')) === \Clojure\Php\Kw::createNs('user', 'not-found')));}}});
$NS->assoc = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_m = $__args[0]; $__L_k = $__args[1]; $__L_v = $__args[2]; if (($__L_m === null)) { return \Clojure\Php\hashMap($__L_k, $__L_v);} else { return ($P->_assoc)($__L_m, $__L_k, $__L_v);} } else if ($__n >= 3) { $__L_m = $__args[0]; $__L_k = $__args[1]; $__L_v = $__args[2]; $__L_kvs = array_slice($__args, 3);  while(true) { $__L_ret = \Clojure\Php\assoc($__L_m, $__L_k, $__L_v);
if ($__L_kvs) { if (\Clojure\Php\next_($__L_kvs)) { $__recur_0 = $__L_ret; $__recur_1 = \Clojure\Php\first($__L_kvs); $__recur_2 = \Clojure\Php\second($__L_kvs); $__recur_3 = ($NS->nnext)($__L_kvs); $__L_m = $__recur_0; $__L_k = $__recur_1; $__L_v = $__recur_2; $__L_kvs = $__recur_3; continue;} else { throw ($NS->ex_info)('assoc expects even number of arguments', \Clojure\Php\hashMap());
}} else { return $__L_ret;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->dissoc = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_m = $__args[0]; $__L_k = $__args[1]; if ($__L_m) { return ($P->_dissoc)($__L_m, $__L_k);} } else if ($__n == 1) { $__L_m = $__args[0]; return $__L_m; } else if ($__n >= 2) { $__L_m = $__args[0]; $__L_k = $__args[1]; $__L_ks = array_slice($__args, 2);  while(true) { $__L_ret = \Clojure\Php\dissoc($__L_m, $__L_k);
if ($__L_ks) { $__recur_0 = $__L_ret; $__recur_1 = \Clojure\Php\first($__L_ks); $__recur_2 = \Clojure\Php\next_($__L_ks); $__L_m = $__recur_0; $__L_k = $__recur_1; $__L_ks = $__recur_2; continue;} else { return $__L_ret;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->peek = (function($__L_coll) { if ($__L_coll) { return ($P->_peek)($__L_coll);}});
$NS->pop = (function($__L_coll) { if ($__L_coll) { return ($P->_pop)($__L_coll);}});
$NS->deref = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_ref = $__args[0]; $__L_timeout_ms = $__args[1]; $__L_timeout_val = $__args[2]; if (($NS->satisfies_QMARK_)($P->IDerefWithTimeout, $__L_ref)) { return ($P->_deref_with_timeout)($__L_ref, $__L_timeout_ms, $__L_timeout_val);} else { return ($P->_deref)($__L_ref);} } else if ($__n == 1) { $__L_ref = $__args[0]; return ($P->_deref)($__L_ref); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->reset_BANG_ = (function($__L_atom, $__L_new_value) { return ($P->_reset_BANG_)($__L_atom, $__L_new_value);});
$NS->swap_BANG_ = (function(...$__args) { $__n = count($__args); if ($__n == 4) { $__L_atom = $__args[0]; $__L_f = $__args[1]; $__L_x = $__args[2]; $__L_y = $__args[3]; return ($P->_swap_BANG_)($__L_atom, $__L_f, $__L_x, $__L_y); } else if ($__n == 3) { $__L_atom = $__args[0]; $__L_f = $__args[1]; $__L_x = $__args[2]; return ($P->_swap_BANG_)($__L_atom, $__L_f, $__L_x); } else if ($__n == 2) { $__L_atom = $__args[0]; $__L_f = $__args[1]; return ($P->_swap_BANG_)($__L_atom, $__L_f); } else if ($__n >= 4) { $__L_atom = $__args[0]; $__L_f = $__args[1]; $__L_x = $__args[2]; $__L_y = $__args[3]; $__L_args = array_slice($__args, 4); return ($P->_swap_BANG_)($__L_atom, $__L_f, $__L_x, $__L_y, $__L_args); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->compare_and_set_BANG_ = (function($__L_atom, $__L_old, $__L_new) { return ($P->_compare_and_set_BANG_)($__L_atom, $__L_old, $__L_new);});
$NS->meta = (function($__L_obj) { if (($NS->satisfies_QMARK_)($P->IMeta, $__L_obj)) { return ($P->_meta)($__L_obj);}});
$NS->with_meta = (function($__L_obj, $__L_m) { return ($P->_with_meta)($__L_obj, $__L_m);});
$NS->vary_meta = (function($__L_obj, $__L_f, ...$__L_args) { return ($NS->with_meta)($__L_obj, ($NS->apply)($__L_f, ($NS->meta)($__L_obj), $__L_args));});
$NS->name = (function($__L_x) { if (is_string($__L_x)) { return $__L_x;} else { return ($P->_name)($__L_x);}});
$NS->namespace = (function($__L_x) { return ($P->_namespace)($__L_x);});
$NS->_EQ_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; if (($__L_x === $__L_y)) { return true;} else { if (($NS->satisfies_QMARK_)($P->IEquiv, $__L_x)) { return ($P->_equiv)($__L_x, $__L_y);} else { return \Clojure\Php\equals($__L_y, $__L_x);}} } else if ($__n == 1) { $__L_x = $__args[0]; return true; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2);  while(true) { if (\Clojure\Php\equals($__L_x, $__L_y)) { if (\Clojure\Php\next_($__L_more)) { $__recur_0 = $__L_y; $__recur_1 = \Clojure\Php\first($__L_more); $__recur_2 = \Clojure\Php\next_($__L_more); $__L_x = $__recur_0; $__L_y = $__recur_1; $__L_more = $__recur_2; continue;} else { return \Clojure\Php\equals($__L_y, \Clojure\Php\first($__L_more));}} else { return false;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->hash = (function($__L_x) { if (($__L_x === null)) { return 0;} else { if (($NS->satisfies_QMARK_)($P->IHash, $__L_x)) { return ($P->_hash)($__L_x);} else { throw ($NS->ex_info)('No hash implementation', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('value'), $__L_x));
}}});
$NS->reduce = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_f = $__args[0]; $__L_init = $__args[1]; $__L_coll = $__args[2];  while(true) { if (($NS->satisfies_QMARK_)($P->IReduceInit, $__L_coll)) { return ($P->_reduce_init)($__L_coll, $__L_f, $__L_init);} else { $__L_acc = $__L_init;
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ($__L_s) { $__L_ret = call_user_func($__L_f, $__L_acc, \Clojure\Php\first($__L_s));
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = \Clojure\Php\next_($__L_s); $__L_acc = $__recur_0; $__L_s = $__recur_1; continue;}} else { return $__L_acc;} break; }
} break; } } else if ($__n == 2) { $__L_f = $__args[0]; $__L_coll = $__args[1]; if (($NS->satisfies_QMARK_)($P->IReduce, $__L_coll)) { return ($P->_reduce)($__L_coll, $__L_f);} else { $__L_s = \Clojure\Php\seq($__L_coll);
if ($__L_s) { return ($NS->reduce)($__L_f, \Clojure\Php\first($__L_s), \Clojure\Php\next_($__L_s));} else { return call_user_func($__L_f);}} } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->reduce_kv = (function($__L_f, $__L_init, $__L_coll) { if (($NS->satisfies_QMARK_)($P->IKVReduce, $__L_coll)) { return ($P->_kv_reduce)($__L_coll, $__L_f, $__L_init);} else { return ($NS->reduce)((function($__L_acc, $__L___dest_4) use (&$__L_init, &$__L_coll, &$__L_f) { $__L___dest_5 = $__L___dest_4;
$__L_k = \Clojure\Php\nth($__L___dest_5, 0);
$__L_v = \Clojure\Php\nth($__L___dest_5, 1);
return call_user_func($__L_f, $__L_acc, $__L_k, $__L_v);}), $__L_init, $__L_coll);}});
call_user_func('\Clojure\Php\defType', 'Reduced', array('val'), array(), array('-deref', (function($__L__) { return $__L__->val;})));
$NS->__GT_Reduced = (function($__L_val) { return call_user_func('\Clojure\Php\createType', 'Reduced', $__L_val);});
$NS->reduced = (function($__L_x) { return ($NS->__GT_Reduced)($__L_x);});
$NS->reduced_QMARK_ = (function($__L_x) { return ($NS->instance_QMARK_)($NS->Reduced, $__L_x);});
$NS->unreduced = (function($__L_x) { if (($NS->reduced_QMARK_)($__L_x)) { return ($Clojure_Core->deref)($__L_x);} else { return $__L_x;}});
$NS->ensure_reduced = (function($__L_x) { if (($NS->reduced_QMARK_)($__L_x)) { return $__L_x;} else { return ($NS->reduced)($__L_x);}});
$NS->second = (function($__L_coll) { return \Clojure\Php\first(\Clojure\Php\next_($__L_coll));});
$NS->ffirst = (function($__L_coll) { return \Clojure\Php\first(\Clojure\Php\first($__L_coll));});
$NS->nfirst = (function($__L_coll) { return \Clojure\Php\next_(\Clojure\Php\first($__L_coll));});
$NS->fnext = (function($__L_coll) { return \Clojure\Php\first(\Clojure\Php\next_($__L_coll));});
$NS->nnext = (function($__L_coll) { return \Clojure\Php\next_(\Clojure\Php\next_($__L_coll));});
$NS->last = (function($__L_coll) {  while(true) { $__L_s = \Clojure\Php\next_($__L_coll);
if ($__L_s) { $__recur_0 = $__L_s; $__L_coll = $__recur_0; continue;} else { return \Clojure\Php\first($__L_coll);} break; }});
$NS->butlast = (function($__L_coll) {  while(true) { $__L_ret = \Clojure\Php\vec();
$__L_s = $__L_coll;
 while(true) { if (\Clojure\Php\next_($__L_s)) { $__recur_0 = \Clojure\Php\conj($__L_ret, \Clojure\Php\first($__L_s)); $__recur_1 = \Clojure\Php\next_($__L_s); $__L_ret = $__recur_0; $__L_s = $__recur_1; continue;} else { return \Clojure\Php\seq($__L_ret);} break; }
 break; }});
$NS->seq_QMARK_ = (function($__L_x) { return ($NS->satisfies_QMARK_)($P->ISeq, $__L_x);});
$NS->seqable_QMARK_ = (function($__L_x) { return ($NS->satisfies_QMARK_)($P->ISeqable, $__L_x);});
$NS->sequential_QMARK_ = (function($__L_x) { return ($NS->satisfies_QMARK_)($P->ISequential, $__L_x);});
$NS->counted_QMARK_ = (function($__L_coll) { return ($NS->satisfies_QMARK_)($P->ICounted, $__L_coll);});
$NS->indexed_QMARK_ = (function($__L_coll) { return ($NS->satisfies_QMARK_)($P->IIndexed, $__L_coll);});
$NS->reversible_QMARK_ = (function($__L_coll) { return ($NS->satisfies_QMARK_)($P->IReversible, $__L_coll);});
$NS->sorted_QMARK_ = (function($__L_coll) { return ($NS->satisfies_QMARK_)($P->ISorted, $__L_coll);});
$NS->associative_QMARK_ = (function($__L_coll) { return ($NS->satisfies_QMARK_)($P->IAssociative, $__L_coll);});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.numbers'), 'Arithmetic operations using platform primitives.

   The compiler emits appropriate operations for each platform:
   - JVM: use primitives or clojure.lang.Numbers
   - PHP: use native PHP operators
   - JS: use native JS operators
   - Rust: use native Rust operators

   These functions define the semantics. The emitter handles optimization.');
$NS->_PLUS_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x + $__L_y); } else if ($__n == 1) { $__L_x = $__args[0]; return $__L_x; } else if ($__n == 0) { return 0; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->_PLUS_, ($__L_x + $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x - $__L_y); } else if ($__n == 1) { $__L_x = $__args[0]; return (-$__L_x); } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->_, ($__L_x - $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->_STAR_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x * $__L_y); } else if ($__n == 1) { $__L_x = $__args[0]; return $__L_x; } else if ($__n == 0) { return 1; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->_STAR_, ($__L_x * $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->_SLASH_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x / $__L_y); } else if ($__n == 1) { $__L_x = $__args[0]; return (1 / $__L_x); } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->_SLASH_, ($__L_x / $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->inc = (function($__L_x) { return ($__L_x + 1);});
$NS->dec = (function($__L_x) { return ($__L_x - 1);});
$NS->_LT_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x < $__L_y); } else if ($__n == 1) { $__L__ = $__args[0]; return true; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2);  while(true) { if (($__L_x < $__L_y)) { if (\Clojure\Php\next_($__L_more)) { $__recur_0 = $__L_y; $__recur_1 = \Clojure\Php\first($__L_more); $__recur_2 = \Clojure\Php\next_($__L_more); $__L_x = $__recur_0; $__L_y = $__recur_1; $__L_more = $__recur_2; continue;} else { return ($__L_y < \Clojure\Php\first($__L_more));}} else { return false;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->_GT_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x > $__L_y); } else if ($__n == 1) { $__L__ = $__args[0]; return true; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2);  while(true) { if (($__L_x > $__L_y)) { if (\Clojure\Php\next_($__L_more)) { $__recur_0 = $__L_y; $__recur_1 = \Clojure\Php\first($__L_more); $__recur_2 = \Clojure\Php\next_($__L_more); $__L_x = $__recur_0; $__L_y = $__recur_1; $__L_more = $__recur_2; continue;} else { return ($__L_y > \Clojure\Php\first($__L_more));}} else { return false;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->_LT__EQ_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x <= $__L_y); } else if ($__n == 1) { $__L__ = $__args[0]; return true; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2);  while(true) { if (($__L_x <= $__L_y)) { if (\Clojure\Php\next_($__L_more)) { $__recur_0 = $__L_y; $__recur_1 = \Clojure\Php\first($__L_more); $__recur_2 = \Clojure\Php\next_($__L_more); $__L_x = $__recur_0; $__L_y = $__recur_1; $__L_more = $__recur_2; continue;} else { return ($__L_y <= \Clojure\Php\first($__L_more));}} else { return false;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->_GT__EQ_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x >= $__L_y); } else if ($__n == 1) { $__L__ = $__args[0]; return true; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2);  while(true) { if (($__L_x >= $__L_y)) { if (\Clojure\Php\next_($__L_more)) { $__recur_0 = $__L_y; $__recur_1 = \Clojure\Php\first($__L_more); $__recur_2 = \Clojure\Php\next_($__L_more); $__L_x = $__recur_0; $__L_y = $__recur_1; $__L_more = $__recur_2; continue;} else { return ($__L_y >= \Clojure\Php\first($__L_more));}} else { return false;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->_EQ__EQ_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($Clojure_Core->_EQ__EQ_)($__L_x, $__L_y); } else if ($__n == 1) { $__L__ = $__args[0]; return true; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2);  while(true) { if (($NS->_EQ__EQ_)($__L_x, $__L_y)) { if (\Clojure\Php\next_($__L_more)) { $__recur_0 = $__L_y; $__recur_1 = \Clojure\Php\first($__L_more); $__recur_2 = \Clojure\Php\next_($__L_more); $__L_x = $__recur_0; $__L_y = $__recur_1; $__L_more = $__recur_2; continue;} else { return ($NS->_EQ__EQ_)($__L_y, \Clojure\Php\first($__L_more));}} else { return false;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->zero_QMARK_ = (function($__L_x) { return ($NS->_EQ__EQ_)($__L_x, 0);});
$NS->pos_QMARK_ = (function($__L_x) { return ($__L_x > 0);});
$NS->neg_QMARK_ = (function($__L_x) { return ($__L_x < 0);});
$NS->even_QMARK_ = (function($__L_n) { return (($__L_n % 2) === 0 || ($__L_n % 2) === 0.0);});
$NS->odd_QMARK_ = (function($__L_n) { return (!(($__L_n & 1) === 0));});
$NS->max = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; if (($__L_x > $__L_y)) { return $__L_x;} else { return $__L_y;} } else if ($__n == 1) { $__L_x = $__args[0]; return $__L_x; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->max, ($NS->max)($__L_x, $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->min = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; if (($__L_x < $__L_y)) { return $__L_x;} else { return $__L_y;} } else if ($__n == 1) { $__L_x = $__args[0]; return $__L_x; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->min, ($NS->min)($__L_x, $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->quot = (function($__L_num, $__L_div) { return ($Clojure_Core->quot)($__L_num, $__L_div);});
$NS->rem = (function($__L_num, $__L_div) { return ($__L_num % $__L_div);});
$NS->mod = (function($__L_num, $__L_div) { return ($__L_num % $__L_div);});
$NS->bit_and = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x & $__L_y); } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->bit_and, ($__L_x & $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->bit_or = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x | $__L_y); } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->bit_or, ($__L_x | $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->bit_xor = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x ^ $__L_y); } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->bit_xor, ($__L_x ^ $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->bit_not = (function($__L_x) { return ($Clojure_Core->bit_not)($__L_x);});
$NS->bit_shift_left = (function($__L_x, $__L_n) { return ($__L_x << $__L_n);});
$NS->bit_shift_right = (function($__L_x, $__L_n) { return ($__L_x >> $__L_n);});
$NS->unsigned_bit_shift_right = (function($__L_x, $__L_n) { return ($Clojure_Core->unsigned_bit_shift_right)($__L_x, $__L_n);});
$NS->bit_and_not = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return ($__L_x & ($NS->bit_not)($__L_y)); } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->bit_and_not, ($NS->bit_and_not)($__L_x, $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->bit_clear = (function($__L_x, $__L_n) { return ($__L_x & ($NS->bit_not)((1 << $__L_n)));});
$NS->bit_set = (function($__L_x, $__L_n) { return ($__L_x | (1 << $__L_n));});
$NS->bit_flip = (function($__L_x, $__L_n) { return ($__L_x ^ (1 << $__L_n));});
$NS->bit_test = (function($__L_x, $__L_n) { return (!(($__L_x & (1 << $__L_n)) === 0 || ($__L_x & (1 << $__L_n)) === 0.0));});
$NS->abs = (function($__L_n) { if (($__L_n < 0)) { return (-$__L_n);} else { return $__L_n;}});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.seqs'), 'Sequence operations - the heart of Clojure.

   These functions operate lazily over any seqable collection,
   providing the functional programming core of Clojure.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')], [\Clojure\Php\Sym::create('clojure.lang.lazy'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('lazy')]));
$NS->map = (function(...$__args) { $__n = count($__args); if ($__n == 4) { $__L_f = $__args[0]; $__L_c1 = $__args[1]; $__L_c2 = $__args[2]; $__L_c3 = $__args[3]; return lazy::lazy-seq*((function() use (&$__L_c3, &$__L_c2, &$__L_c1, &$__L_f) { $__L_s1 = \Clojure\Php\seq($__L_c1);
$__L_s2 = \Clojure\Php\seq($__L_c2);
$__L_s3 = \Clojure\Php\seq($__L_c3);
if (($__L_s1 ? ($__L_s2 ? $__L_s3 : false) : false)) { return ($NS->__GT_Cons)(call_user_func($__L_f, \Clojure\Php\first($__L_s1), \Clojure\Php\first($__L_s2), \Clojure\Php\first($__L_s3)), ($NS->map)($__L_f, \Clojure\Php\rest($__L_s1), \Clojure\Php\rest($__L_s2), \Clojure\Php\rest($__L_s3)), null);}})); } else if ($__n == 3) { $__L_f = $__args[0]; $__L_c1 = $__args[1]; $__L_c2 = $__args[2]; return lazy::lazy-seq*((function() use (&$__L_c2, &$__L_c1, &$__L_f) { $__L_s1 = \Clojure\Php\seq($__L_c1);
$__L_s2 = \Clojure\Php\seq($__L_c2);
if (($__L_s1 ? $__L_s2 : false)) { return ($NS->__GT_Cons)(call_user_func($__L_f, \Clojure\Php\first($__L_s1), \Clojure\Php\first($__L_s2)), ($NS->map)($__L_f, \Clojure\Php\rest($__L_s1), \Clojure\Php\rest($__L_s2)), null);}})); } else if ($__n == 2) { $__L_f = $__args[0]; $__L_coll = $__args[1]; return lazy::lazy-seq*((function() use (&$__L_coll, &$__L_f) { $__L_when_let__3020 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3020) { $__L_s = $__L_when_let__3020;
return ($NS->__GT_Cons)(call_user_func($__L_f, \Clojure\Php\first($__L_s)), ($NS->map)($__L_f, \Clojure\Php\rest($__L_s)), null);}})); } else if ($__n == 1) { $__L_f = $__args[0]; return (function($__L_rf) use (&$__L_f) { return (function(...$__args) use (&$__L_rf, &$__L_f) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; return call_user_func($__L_rf, $__L_result, call_user_func($__L_f, $__L_input)); } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else if ($__n >= 4) { $__L_f = $__args[0]; $__L_c1 = $__args[1]; $__L_c2 = $__args[2]; $__L_c3 = $__args[3]; $__L_colls = array_slice($__args, 4); $__L_step = (function($__L_cs) use (&$__L_colls, &$__L_c3, &$__L_c2, &$__L_c1, &$__L_f) { return lazy::lazy-seq*((function() use (&$__L_colls, &$__L_c3, &$__L_cs, &$__L_c2, &$__L_c1, &$__L_f) { $__L_ss = ($NS->map)($NS->seq, $__L_cs);
if (($NS->every_QMARK_)($NS->identity, $__L_ss)) { return ($NS->__GT_Cons)(($NS->map)($NS->first, $__L_ss), ($NS->step)(($NS->map)($NS->rest, $__L_ss)), null);}}));});
return ($NS->map)((function($__L_p1__3017_SHARP_) use (&$__L_step, &$__L_colls, &$__L_c3, &$__L_c2, &$__L_c1, &$__L_f) { return ($NS->apply)($__L_f, $__L_p1__3017_SHARP_);}), call_user_func($__L_step, ($NS->list_STAR_)($__L_c1, $__L_c2, $__L_c3, $__L_colls))); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->mapcat = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_f = $__args[0]; $__L_coll = $__args[1]; return lazy::lazy-seq*((function() use (&$__L_coll, &$__L_f) { $__L_when_let__3021 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3021) { $__L_s = $__L_when_let__3021;
return ($NS->concat)(call_user_func($__L_f, \Clojure\Php\first($__L_s)), ($NS->mapcat)($__L_f, \Clojure\Php\rest($__L_s)));}})); } else if ($__n == 1) { $__L_f = $__args[0]; return ($NS->comp)(($NS->map)($__L_f), $NS->cat); } else if ($__n >= 2) { $__L_f = $__args[0]; $__L_coll = $__args[1]; $__L_colls = array_slice($__args, 2); return ($NS->apply)($NS->concat, ($NS->apply)($NS->map, $__L_f, $__L_coll, $__L_colls)); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->filter = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_pred = $__args[0]; $__L_coll = $__args[1]; return lazy::lazy-seq*((function() use (&$__L_pred, &$__L_coll) { $__L_when_let__3022 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3022) { $__L_s = $__L_when_let__3022;
$__L_f = \Clojure\Php\first($__L_s);
$__L_r = \Clojure\Php\rest($__L_s);
if (call_user_func($__L_pred, $__L_f)) { return ($NS->__GT_Cons)($__L_f, ($NS->filter)($__L_pred, $__L_r), null);} else { return ($NS->filter)($__L_pred, $__L_r);}}})); } else if ($__n == 1) { $__L_pred = $__args[0]; return (function($__L_rf) use (&$__L_pred) { return (function(...$__args) use (&$__L_rf, &$__L_pred) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; if (call_user_func($__L_pred, $__L_input)) { return call_user_func($__L_rf, $__L_result, $__L_input);} else { return $__L_result;} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->remove = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_pred = $__args[0]; $__L_coll = $__args[1]; return ($NS->filter)(($NS->complement)($__L_pred), $__L_coll); } else if ($__n == 1) { $__L_pred = $__args[0]; return ($NS->filter)(($NS->complement)($__L_pred)); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->keep = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_f = $__args[0]; $__L_coll = $__args[1]; return lazy::lazy-seq*((function() use (&$__L_coll, &$__L_f) { $__L_when_let__3023 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3023) { $__L_s = $__L_when_let__3023;
$__L_x = call_user_func($__L_f, \Clojure\Php\first($__L_s));
if (($__L_x === null)) { return ($NS->keep)($__L_f, \Clojure\Php\rest($__L_s));} else { return ($NS->__GT_Cons)($__L_x, ($NS->keep)($__L_f, \Clojure\Php\rest($__L_s)), null);}}})); } else if ($__n == 1) { $__L_f = $__args[0]; return (function($__L_rf) use (&$__L_f) { return (function(...$__args) use (&$__L_rf, &$__L_f) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_v = call_user_func($__L_f, $__L_input);
if (($__L_v === null)) { return $__L_result;} else { return call_user_func($__L_rf, $__L_result, $__L_v);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->keep_indexed = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_f = $__args[0]; $__L_coll = $__args[1]; $__L_keepi = (function($__L_idx, $__L_s) use (&$__L_keepi, &$__L_coll, &$__L_f) { return lazy::lazy-seq*((function() use (&$__L_idx, &$__L_keepi, &$__L_coll, &$__L_s, &$__L_f) { $__L_when_let__3024 = \Clojure\Php\seq($__L_s);
if ($__L_when_let__3024) { $__L_s = $__L_when_let__3024;
$__L_x = call_user_func($__L_f, $__L_idx, \Clojure\Php\first($__L_s));
if (($__L_x === null)) { return call_user_func($__L_keepi, ($__L_idx + 1), \Clojure\Php\rest($__L_s));} else { return ($NS->__GT_Cons)($__L_x, call_user_func($__L_keepi, ($__L_idx + 1), \Clojure\Php\rest($__L_s)), null);}}}));});
return call_user_func($__L_keepi, 0, $__L_coll); } else if ($__n == 1) { $__L_f = $__args[0]; return (function($__L_rf) use (&$__L_f) { $__L_i = ($NS->volatile_BANG_)(-1);
return (function(...$__args) use (&$__L_i, &$__L_rf, &$__L_f) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_v = call_user_func($__L_f, ($NS->vswap_BANG_)($__L_i, $NS->inc), $__L_input);
if (($__L_v === null)) { return $__L_result;} else { return call_user_func($__L_rf, $__L_result, $__L_v);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->take = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; return lazy::lazy-seq*((function() use (&$__L_n, &$__L_coll) { if (($__L_n > 0)) { $__L_when_let__3025 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3025) { $__L_s = $__L_when_let__3025;
return ($NS->__GT_Cons)(\Clojure\Php\first($__L_s), ($NS->take)(($__L_n - 1), \Clojure\Php\rest($__L_s)), null);}}})); } else if ($__n == 1) { $__L_n = $__args[0]; return (function($__L_rf) use (&$__L_n) { $__L_nv = ($NS->volatile_BANG_)($__L_n);
return (function(...$__args) use (&$__L_rf, &$__L_n, &$__L_nv) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_n = ($Clojure_Core->deref)($__L_nv);
$__L_nn = ($NS->vswap_BANG_)($__L_nv, $NS->dec);
$__L_result = (($__L_n > 0) ? call_user_func($__L_rf, $__L_result, $__L_input) : $__L_result);
if ((!($__L_nn > 0))) { return ($NS->ensure_reduced)($__L_result);} else { return $__L_result;} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->take_while = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_pred = $__args[0]; $__L_coll = $__args[1]; return lazy::lazy-seq*((function() use (&$__L_pred, &$__L_coll) { $__L_when_let__3026 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3026) { $__L_s = $__L_when_let__3026;
if (call_user_func($__L_pred, \Clojure\Php\first($__L_s))) { return ($NS->__GT_Cons)(\Clojure\Php\first($__L_s), ($NS->take_while)($__L_pred, \Clojure\Php\rest($__L_s)), null);}}})); } else if ($__n == 1) { $__L_pred = $__args[0]; return (function($__L_rf) use (&$__L_pred) { return (function(...$__args) use (&$__L_rf, &$__L_pred) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; if (call_user_func($__L_pred, $__L_input)) { return call_user_func($__L_rf, $__L_result, $__L_input);} else { return ($NS->reduced)($__L_result);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->take_nth = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; return lazy::lazy-seq*((function() use (&$__L_n, &$__L_coll) { $__L_when_let__3027 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3027) { $__L_s = $__L_when_let__3027;
return ($NS->__GT_Cons)(\Clojure\Php\first($__L_s), ($NS->take_nth)($__L_n, ($NS->drop)($__L_n, $__L_s)), null);}})); } else if ($__n == 1) { $__L_n = $__args[0]; return (function($__L_rf) use (&$__L_n) { $__L_i = ($NS->volatile_BANG_)(-1);
return (function(...$__args) use (&$__L_i, &$__L_rf, &$__L_n) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_idx = ($NS->vswap_BANG_)($__L_i, $NS->inc);
if ((($__L_idx % $__L_n) === 0 || ($__L_idx % $__L_n) === 0.0)) { return call_user_func($__L_rf, $__L_result, $__L_input);} else { return $__L_result;} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->drop = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; $__L_step = (function($__L_n, $__L_coll) {  while(true) { $__L_s = \Clojure\Php\seq($__L_coll);
if ((($__L_n > 0) ? $__L_s : false)) { $__recur_0 = ($__L_n - 1); $__recur_1 = \Clojure\Php\rest($__L_s); $__L_n = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_s;} break; }});
return lazy::lazy-seq*((function() use (&$__L_step, &$__L_n, &$__L_coll) { return call_user_func($__L_step, $__L_n, $__L_coll);})); } else if ($__n == 1) { $__L_n = $__args[0]; return (function($__L_rf) use (&$__L_n) { $__L_nv = ($NS->volatile_BANG_)($__L_n);
return (function(...$__args) use (&$__L_rf, &$__L_n, &$__L_nv) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_n = ($Clojure_Core->deref)($__L_nv);
($NS->vswap_BANG_)($__L_nv, $NS->dec);
if (($__L_n > 0)) { return $__L_result;} else { return call_user_func($__L_rf, $__L_result, $__L_input);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->drop_while = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_pred = $__args[0]; $__L_coll = $__args[1]; $__L_step = (function($__L_pred, $__L_coll) {  while(true) { $__L_s = \Clojure\Php\seq($__L_coll);
if (($__L_s ? call_user_func($__L_pred, \Clojure\Php\first($__L_s)) : false)) { $__recur_0 = $__L_pred; $__recur_1 = \Clojure\Php\rest($__L_s); $__L_pred = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_s;} break; }});
return lazy::lazy-seq*((function() use (&$__L_step, &$__L_pred, &$__L_coll) { return call_user_func($__L_step, $__L_pred, $__L_coll);})); } else if ($__n == 1) { $__L_pred = $__args[0]; return (function($__L_rf) use (&$__L_pred) { $__L_dv = ($NS->volatile_BANG_)(true);
return (function(...$__args) use (&$__L_rf, &$__L_pred, &$__L_dv) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_drop_QMARK_ = ($Clojure_Core->deref)($__L_dv);
if (($__L_drop_QMARK_ ? call_user_func($__L_pred, $__L_input) : false)) { return $__L_result;} else { ($NS->vreset_BANG_)($__L_dv, null);
return call_user_func($__L_rf, $__L_result, $__L_input);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->drop_last = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; return ($NS->map)((function($__L_x, $__L__) use (&$__L_n, &$__L_coll) { return $__L_x;}), $__L_coll, ($NS->drop)($__L_n, $__L_coll)); } else if ($__n == 1) { $__L_coll = $__args[0]; return ($NS->drop_last)(1, $__L_coll); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->take_last = (function($__L_n, $__L_coll) {  while(true) { $__L_s = \Clojure\Php\seq($__L_coll);
$__L_lead = \Clojure\Php\seq(($NS->drop)($__L_n, $__L_coll));
 while(true) { if ($__L_lead) { $__recur_0 = \Clojure\Php\next_($__L_s); $__recur_1 = \Clojure\Php\next_($__L_lead); $__L_s = $__recur_0; $__L_lead = $__recur_1; continue;} else { return $__L_s;} break; }
 break; }});
$NS->split_at = (function($__L_n, $__L_coll) { return \Clojure\Php\vec(($NS->take)($__L_n, $__L_coll), ($NS->drop)($__L_n, $__L_coll));});
$NS->split_with = (function($__L_pred, $__L_coll) { return \Clojure\Php\vec(($NS->take_while)($__L_pred, $__L_coll), ($NS->drop_while)($__L_pred, $__L_coll));});
$NS->partition = (function(...$__args) { $__n = count($__args); if ($__n == 4) { $__L_n = $__args[0]; $__L_step = $__args[1]; $__L_pad = $__args[2]; $__L_coll = $__args[3]; return lazy::lazy-seq*((function() use (&$__L_step, &$__L_n, &$__L_coll, &$__L_pad) { $__L_when_let__3029 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3029) { $__L_s = $__L_when_let__3029;
$__L_p = ($NS->take)($__L_n, $__L_s);
if (\Clojure\Php\equals($__L_n, \Clojure\Php\count_($__L_p))) { return ($NS->__GT_Cons)(($NS->apply)($NS->list, $__L_p), ($NS->partition)($__L_n, $__L_step, $__L_pad, ($NS->drop)($__L_step, $__L_s)), null);} else { return ($NS->list)(($NS->take)($__L_n, ($NS->concat)($__L_p, $__L_pad)));}}})); } else if ($__n == 3) { $__L_n = $__args[0]; $__L_step = $__args[1]; $__L_coll = $__args[2]; return lazy::lazy-seq*((function() use (&$__L_step, &$__L_n, &$__L_coll) { $__L_when_let__3028 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3028) { $__L_s = $__L_when_let__3028;
$__L_p = ($NS->take)($__L_n, $__L_s);
if (\Clojure\Php\equals($__L_n, \Clojure\Php\count_($__L_p))) { return ($NS->__GT_Cons)(($NS->apply)($NS->list, $__L_p), ($NS->partition)($__L_n, $__L_step, ($NS->drop)($__L_step, $__L_s)), null);}}})); } else if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; return ($NS->partition)($__L_n, $__L_n, $__L_coll); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->partition_all = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_n = $__args[0]; $__L_step = $__args[1]; $__L_coll = $__args[2]; return lazy::lazy-seq*((function() use (&$__L_step, &$__L_n, &$__L_coll) { $__L_when_let__3030 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3030) { $__L_s = $__L_when_let__3030;
$__L_p = ($NS->take)($__L_n, $__L_s);
return ($NS->__GT_Cons)(($NS->apply)($NS->list, $__L_p), ($NS->partition_all)($__L_n, $__L_step, ($NS->drop)($__L_step, $__L_s)), null);}})); } else if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; return ($NS->partition_all)($__L_n, $__L_n, $__L_coll); } else if ($__n == 1) { $__L_n = $__args[0]; return (function($__L_rf) use (&$__L_n) { $__L_a = (new java.util.ArrayList($__L_n));
return (function(...$__args) use (&$__L_a, &$__L_rf, &$__L_n) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_a->add($__L_input);
if (\Clojure\Php\equals($__L_n, $__L_a->size())) { $__L_v = \Vec::create($__L_a->toArray());
$__L_a->clear();
return call_user_func($__L_rf, $__L_result, $__L_v);} else { return $__L_result;} } else if ($__n == 1) { $__L_result = $__args[0]; $__L_result = ($__L_a->isEmpty() ? $__L_result : ((function() { $__L_v = \Vec::create($__L_a->toArray()); $__L_a->clear();
return ($NS->unreduced)(call_user_func($__L_rf, $__L_result, $__L_v)); })()));
return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->partition_by = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_f = $__args[0]; $__L_coll = $__args[1]; return lazy::lazy-seq*((function() use (&$__L_coll, &$__L_f) { $__L_when_let__3032 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3032) { $__L_s = $__L_when_let__3032;
$__L_fst = \Clojure\Php\first($__L_s);
$__L_fv = call_user_func($__L_f, $__L_fst);
$__L_run = ($NS->__GT_Cons)($__L_fst, ($NS->take_while)((function($__L_p1__3018_SHARP_) use (&$__L_fst, &$__L_coll, &$__L_when_let__3032, &$__L_s, &$__L_fv, &$__L_f) { return \Clojure\Php\equals($__L_fv, call_user_func($__L_f, $__L_p1__3018_SHARP_));}), \Clojure\Php\rest($__L_s)), null);
return ($NS->__GT_Cons)($__L_run, ($NS->partition_by)($__L_f, ($NS->drop)(\Clojure\Php\count_($__L_run), $__L_s)), null);}})); } else if ($__n == 1) { $__L_f = $__args[0]; return (function($__L_rf) use (&$__L_f) { $__L_a = (new java.util.ArrayList());
$__L_pv = ($NS->volatile_BANG_)(\Clojure\Php\Kw::createNs('user', 'none'));
return (function(...$__args) use (&$__L_a, &$__L_rf, &$__L_pv, &$__L_f) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_pval = ($Clojure_Core->deref)($__L_pv);
$__L_val = call_user_func($__L_f, $__L_input);
($NS->vreset_BANG_)($__L_pv, $__L_val);
if (((function() { $__L_or__3031 = ($__L_pval === \Clojure\Php\Kw::createNs('user', 'none')); if ($__L_or__3031) { return $__L_or__3031;} else { return \Clojure\Php\equals($__L_val, $__L_pval);} })())) { $__L_a->add($__L_input);
return $__L_result;} else { $__L_v = \Vec::create($__L_a->toArray());
$__L_a->clear();
$__L_ret = call_user_func($__L_rf, $__L_result, $__L_v);
if (($NS->reduced_QMARK_)($__L_ret)) { null;
} else { $__L_a->add($__L_input);
}
return $__L_ret;} } else if ($__n == 1) { $__L_result = $__args[0]; $__L_result = ($__L_a->isEmpty() ? $__L_result : ((function() { $__L_v = \Vec::create($__L_a->toArray()); $__L_a->clear();
return ($NS->unreduced)(call_user_func($__L_rf, $__L_result, $__L_v)); })()));
return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->concat = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; return lazy::lazy-seq*((function() use (&$__L_x, &$__L_y) { $__L_s = \Clojure\Php\seq($__L_x);
if ($__L_s) { return ($NS->__GT_Cons)(\Clojure\Php\first($__L_s), ($NS->concat)(\Clojure\Php\rest($__L_s), $__L_y), null);} else { return \Clojure\Php\seq($__L_y);}})); } else if ($__n == 1) { $__L_x = $__args[0]; return lazy::lazy-seq*((function() use (&$__L_x) { return \Clojure\Php\seq($__L_x);})); } else if ($__n == 0) { return null; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_zs = array_slice($__args, 2); $__L_cat = (function($__L_xys, $__L_zs) use (&$__L_x, &$__L_y) { return lazy::lazy-seq*((function() use (&$__L_x, &$__L_y, &$__L_xys, &$__L_zs) { $__L_xys = \Clojure\Php\seq($__L_xys);
if ($__L_xys) { $__L_s = \Clojure\Php\seq(\Clojure\Php\first($__L_xys));
if ($__L_s) { return ($NS->__GT_Cons)(\Clojure\Php\first($__L_s), ($NS->cat)(($NS->__GT_Cons)(\Clojure\Php\rest($__L_s), \Clojure\Php\rest($__L_xys), null), $__L_zs), null);} else { return ($NS->cat)(\Clojure\Php\rest($__L_xys), $__L_zs);}} else { if ($__L_zs) { return ($NS->cat)(\Clojure\Php\first($__L_zs), \Clojure\Php\next_($__L_zs));}}}));});
return call_user_func($__L_cat, ($NS->list)($__L_x, $__L_y), $__L_zs); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->cat = 'A transducer that concatenates the contents of each input.';
$NS->interleave = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_c1 = $__args[0]; $__L_c2 = $__args[1]; return lazy::lazy-seq*((function() use (&$__L_c2, &$__L_c1) { $__L_s1 = \Clojure\Php\seq($__L_c1);
$__L_s2 = \Clojure\Php\seq($__L_c2);
if (($__L_s1 ? $__L_s2 : false)) { return ($NS->__GT_Cons)(\Clojure\Php\first($__L_s1), ($NS->__GT_Cons)(\Clojure\Php\first($__L_s2), ($NS->interleave)(\Clojure\Php\rest($__L_s1), \Clojure\Php\rest($__L_s2)), null), null);}})); } else if ($__n == 1) { $__L_c1 = $__args[0]; return lazy::lazy-seq*((function() use (&$__L_c1) { return \Clojure\Php\seq($__L_c1);})); } else if ($__n == 0) { return \Clojure\Php\emptyList(); } else if ($__n >= 2) { $__L_c1 = $__args[0]; $__L_c2 = $__args[1]; $__L_colls = array_slice($__args, 2); return lazy::lazy-seq*((function() use (&$__L_colls, &$__L_c2, &$__L_c1) { $__L_ss = ($NS->map)($NS->seq, ($NS->list_STAR_)($__L_c1, $__L_c2, $__L_colls));
if (($NS->every_QMARK_)($NS->identity, $__L_ss)) { return ($NS->concat)(($NS->map)($NS->first, $__L_ss), ($NS->apply)($NS->interleave, ($NS->map)($NS->rest, $__L_ss)));}})); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->interpose = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_sep = $__args[0]; $__L_coll = $__args[1]; return ($NS->drop)(1, ($NS->interleave)(lazy::repeat($__L_sep), $__L_coll)); } else if ($__n == 1) { $__L_sep = $__args[0]; return (function($__L_rf) use (&$__L_sep) { $__L_started = ($NS->volatile_BANG_)(false);
return (function(...$__args) use (&$__L_started, &$__L_sep, &$__L_rf) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; if (($Clojure_Core->deref)($__L_started)) { $__L_sepr = call_user_func($__L_rf, $__L_result, $__L_sep);
if (($NS->reduced_QMARK_)($__L_sepr)) { return $__L_sepr;} else { return call_user_func($__L_rf, $__L_sepr, $__L_input);}} else { ($NS->vreset_BANG_)($__L_started, true);
return call_user_func($__L_rf, $__L_result, $__L_input);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->map_indexed = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_f = $__args[0]; $__L_coll = $__args[1]; $__L_mapi = (function($__L_idx, $__L_coll) use (&$__L_mapi, &$__L_f) { return lazy::lazy-seq*((function() use (&$__L_idx, &$__L_mapi, &$__L_coll, &$__L_f) { $__L_when_let__3033 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3033) { $__L_s = $__L_when_let__3033;
return ($NS->__GT_Cons)(call_user_func($__L_f, $__L_idx, \Clojure\Php\first($__L_s)), call_user_func($__L_mapi, ($__L_idx + 1), \Clojure\Php\rest($__L_s)), null);}}));});
return call_user_func($__L_mapi, 0, $__L_coll); } else if ($__n == 1) { $__L_f = $__args[0]; return (function($__L_rf) use (&$__L_f) { $__L_i = ($NS->volatile_BANG_)(-1);
return (function(...$__args) use (&$__L_i, &$__L_rf, &$__L_f) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; return call_user_func($__L_rf, $__L_result, call_user_func($__L_f, ($NS->vswap_BANG_)($__L_i, $NS->inc), $__L_input)); } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->zipmap = (function($__L_keys, $__L_vals) {  while(true) { $__L_map = \Clojure\Php\hashMap();
$__L_ks = \Clojure\Php\seq($__L_keys);
$__L_vs = \Clojure\Php\seq($__L_vals);
 while(true) { if (($__L_ks ? $__L_vs : false)) { $__recur_0 = \Clojure\Php\assoc($__L_map, \Clojure\Php\first($__L_ks), \Clojure\Php\first($__L_vs)); $__recur_1 = \Clojure\Php\next_($__L_ks); $__recur_2 = \Clojure\Php\next_($__L_vs); $__L_map = $__recur_0; $__L_ks = $__recur_1; $__L_vs = $__recur_2; continue;} else { return $__L_map;} break; }
 break; }});
$NS->flatten = (function($__L_x) { return ($NS->filter)(($NS->complement)($NS->sequential_QMARK_), \Clojure\Php\rest(($NS->tree_seq)($NS->sequential_QMARK_, $NS->seq, $__L_x)));});
$NS->tree_seq = (function($__L_branch_QMARK_, $__L_children, $__L_root) { $__L_walk = (function($__L_node) use (&$__L_branch_QMARK_, &$__L_children, &$__L_root) { return lazy::lazy-seq*((function() use (&$__L_branch_QMARK_, &$__L_children, &$__L_node, &$__L_root) { return ($NS->__GT_Cons)($__L_node, (call_user_func($__L_branch_QMARK_, $__L_node) ? ($NS->mapcat)($NS->walk, call_user_func($__L_children, $__L_node)) : null), null);}));});
return call_user_func($__L_walk, $__L_root);});
$NS->distinct = (function(...$__args) { $__n = count($__args); if ($__n == 1) { $__L_coll = $__args[0]; $__L_step = (function($__L_xs, $__L_seen) use (&$__L_coll) { return lazy::lazy-seq*((function() use (&$__L_coll, &$__L_xs, &$__L_seen) {  while(true) { $__L_xs = \Clojure\Php\seq($__L_xs);
$__L_seen = $__L_seen;
 while(true) { $__L_when_let__3034 = $__L_xs;
if ($__L_when_let__3034) { $__L_s = $__L_when_let__3034;
$__L_f = \Clojure\Php\first($__L_s);
if (\Clojure\Php\contains($__L_seen, $__L_f)) { $__recur_0 = \Clojure\Php\rest($__L_s); $__recur_1 = $__L_seen; $__L_xs = $__recur_0; $__L_seen = $__recur_1; continue;} else { return ($NS->__GT_Cons)($__L_f, ($NS->step)(\Clojure\Php\rest($__L_s), \Clojure\Php\conj($__L_seen, $__L_f)), null);}} break; }
 break; }}));});
return call_user_func($__L_step, $__L_coll, \Clojure\Php\hashSet()); } else if ($__n == 0) { return (function($__L_rf) { $__L_seen = ($NS->volatile_BANG_)(\Clojure\Php\hashSet());
return (function(...$__args) use (&$__L_rf, &$__L_seen) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; if (\Clojure\Php\contains(($Clojure_Core->deref)($__L_seen), $__L_input)) { return $__L_result;} else { ($NS->vswap_BANG_)($__L_seen, $NS->conj, $__L_input);
return call_user_func($__L_rf, $__L_result, $__L_input);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->dedupe = (function(...$__args) { $__n = count($__args); if ($__n == 1) { $__L_coll = $__args[0]; return lazy::lazy-seq*((function() use (&$__L_coll) { $__L_when_let__3035 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3035) { $__L_s = $__L_when_let__3035;
$__L_f = \Clojure\Php\first($__L_s);
return ($NS->__GT_Cons)($__L_f, ($NS->dedupe)(($NS->drop_while)((function($__L_p1__3019_SHARP_) use (&$__L_when_let__3035, &$__L_coll, &$__L_s, &$__L_f) { return \Clojure\Php\equals($__L_f, $__L_p1__3019_SHARP_);}), \Clojure\Php\rest($__L_s))), null);}})); } else if ($__n == 0) { return (function($__L_rf) { $__L_pv = ($NS->volatile_BANG_)(\Clojure\Php\Kw::createNs('user', 'none'));
return (function(...$__args) use (&$__L_rf, &$__L_pv) { $__n = count($__args); if ($__n == 2) { $__L_result = $__args[0]; $__L_input = $__args[1]; $__L_prior = ($Clojure_Core->deref)($__L_pv);
($NS->vreset_BANG_)($__L_pv, $__L_input);
if (\Clojure\Php\equals($__L_prior, $__L_input)) { return $__L_result;} else { return call_user_func($__L_rf, $__L_result, $__L_input);} } else if ($__n == 1) { $__L_result = $__args[0]; return call_user_func($__L_rf, $__L_result); } else if ($__n == 0) { return call_user_func($__L_rf); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });}); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->group_by = (function($__L_f, $__L_coll) { return ($NS->persistent_BANG_)(($NS->reduce)((function($__L_ret, $__L_x) use (&$__L_coll, &$__L_f) { $__L_k = call_user_func($__L_f, $__L_x);
return ($NS->assoc_BANG_)($__L_ret, $__L_k, \Clojure\Php\conj(\Clojure\Php\get_($__L_ret, $__L_k, \Clojure\Php\vec()), $__L_x));}), ($NS->transient)(\Clojure\Php\hashMap()), $__L_coll));});
$NS->frequencies = (function($__L_coll) { return ($NS->persistent_BANG_)(($NS->reduce)((function($__L_counts, $__L_x) use (&$__L_coll) { return ($NS->assoc_BANG_)($__L_counts, $__L_x, (\Clojure\Php\get_($__L_counts, $__L_x, 0) + 1));}), ($NS->transient)(\Clojure\Php\hashMap()), $__L_coll));});
$NS->some = (function($__L_pred, $__L_coll) {  while(true) { $__L_when_let__3036 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3036) { $__L_s = $__L_when_let__3036;
$__L_or__3037 = call_user_func($__L_pred, \Clojure\Php\first($__L_s));
if ($__L_or__3037) { return $__L_or__3037;} else { $__recur_0 = $__L_pred; $__recur_1 = \Clojure\Php\next_($__L_s); $__L_pred = $__recur_0; $__L_coll = $__recur_1; continue;}} break; }});
$NS->every_QMARK_ = (function($__L_pred, $__L_coll) {  while(true) { if ((\Clojure\Php\seq($__L_coll) === null)) { return true;} else { if (call_user_func($__L_pred, \Clojure\Php\first($__L_coll))) { $__recur_0 = $__L_pred; $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_pred = $__recur_0; $__L_coll = $__recur_1; continue;} else { if (\Clojure\Php\Kw::create('else')) { return false;}}} break; }});
$NS->not_every_QMARK_ = (function($__L_pred, $__L_coll) { return (!($NS->every_QMARK_)($__L_pred, $__L_coll));});
$NS->not_any_QMARK_ = (function($__L_pred, $__L_coll) { return (!($NS->some)($__L_pred, $__L_coll));});
$NS->sort = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_comp = $__args[0]; $__L_coll = $__args[1]; $__L_when_let__3038 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3038) { $__L_s = $__L_when_let__3038;
$__L_a = ($NS->to_array)($__L_s);
java.util.Arrays::sort($__L_a, $__L_comp);
return \Clojure\Php\seq($__L_a);} } else if ($__n == 1) { $__L_coll = $__args[0]; return ($NS->sort)($NS->compare, $__L_coll); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->sort_by = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_keyfn = $__args[0]; $__L_comp = $__args[1]; $__L_coll = $__args[2]; return ($NS->sort)((function($__L_x, $__L_y) use (&$__L_comp, &$__L_coll, &$__L_keyfn) { return call_user_func($__L_comp, call_user_func($__L_keyfn, $__L_x), call_user_func($__L_keyfn, $__L_y));}), $__L_coll); } else if ($__n == 2) { $__L_keyfn = $__args[0]; $__L_coll = $__args[1]; return ($NS->sort_by)($__L_keyfn, $NS->compare, $__L_coll); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->reverse = (function($__L_coll) { return ($NS->reduce)($NS->conj, \Clojure\Php\emptyList(), $__L_coll);});
$NS->shuffle = (function($__L_coll) { $__L_al = (new java.util.ArrayList(($NS->to_array)($__L_coll)));
java.util.Collections::shuffle($__L_al);
return \Vec::create($__L_al->toArray());});
$NS->rand_nth = (function($__L_coll) { return \Clojure\Php\nth($__L_coll, ($NS->rand_int)(\Clojure\Php\count_($__L_coll)));});
$NS->dorun = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1];  while(true) { if ((($__L_n > 0) ? \Clojure\Php\seq($__L_coll) : false)) { $__recur_0 = ($__L_n - 1); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_n = $__recur_0; $__L_coll = $__recur_1; continue;} break; } } else if ($__n == 1) { $__L_coll = $__args[0];  while(true) { $__L_when_let__3039 = \Clojure\Php\seq($__L_coll);
if ($__L_when_let__3039) { $__L_s = $__L_when_let__3039;
$__recur_0 = \Clojure\Php\next_($__L_s); $__L_coll = $__recur_0; continue;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->doall = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; ($NS->dorun)($__L_n, $__L_coll);
return $__L_coll; } else if ($__n == 1) { $__L_coll = $__args[0]; ($NS->dorun)($__L_coll);
return $__L_coll; } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->into = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_to = $__args[0]; $__L_xform = $__args[1]; $__L_from = $__args[2]; if (($NS->satisfies_QMARK_)($P->IEditableCollection, $__L_to)) { $__L_f = call_user_func($__L_xform, $NS->conj_BANG_);
return ($NS->persistent_BANG_)(call_user_func($__L_f, ($NS->reduce)($__L_f, ($NS->transient)($__L_to), $__L_from)));} else { $__L_f = call_user_func($__L_xform, $NS->conj);
return call_user_func($__L_f, ($NS->reduce)($__L_f, $__L_to, $__L_from));} } else if ($__n == 2) { $__L_to = $__args[0]; $__L_from = $__args[1]; if (($NS->satisfies_QMARK_)($P->IEditableCollection, $__L_to)) { return ($NS->persistent_BANG_)(($NS->reduce)($NS->conj_BANG_, ($NS->transient)($__L_to), $__L_from));} else { return ($NS->reduce)($NS->conj, $__L_to, $__L_from);} } else if ($__n == 1) { $__L_to = $__args[0]; return $__L_to; } else if ($__n == 0) { return \Clojure\Php\vec(); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->empty = (function($__L_coll) { if (($NS->satisfies_QMARK_)($P->IEmptyableCollection, $__L_coll)) { return ($P->_empty)($__L_coll);}});
$NS->not_empty = (function($__L_coll) { if (\Clojure\Php\seq($__L_coll)) { return $__L_coll;}});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.lazy'), 'Lazy sequence implementations.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
call_user_func('\Clojure\Php\defType', 'LazySeq', array('fn', 'sv', 's', '_meta'), array('fn', 'sv', 's'), array('-seq', (function($__L_this) {  while(true) { if ($__L_this->fn) { $__L_this->sv = ($Clojure_Core->apply)($NS->fn);
$__L_this->fn = null;
}
if ($__L_this->sv) { $__L_ls = $__L_this->sv;
$__L_this->sv = null;
$__L_ls = $__L_ls;
 while(true) { if (($NS->instance_QMARK_)($NS->LazySeq, $__L_ls)) { $__recur_0 = $__L_ls->sv; $__L_ls = $__recur_0; continue;} else { $__L_this->s = $__L_ls;
$__L_this->s;
}
 break; }
}
return $__L_this->s; break; }}), '-first', (function($__L_this) { ($P->_seq)($__L_this);
if ($__L_this->s) { return ($P->_first)($__L_this->s);}}), '-rest', (function($__L_this) { ($P->_seq)($__L_this);
if ($__L_this->s) { return ($P->_rest)($__L_this->s);} else { return \Clojure\Php\emptyList();}}), '-next', (function($__L_this) { ($P->_seq)($__L_this);
if ($__L_this->s) { return ($P->_next)($__L_this->s);}}), '-conj', (function($__L_this, $__L_o) { return ($NS->__GT_Cons)($__L_o, $__L_this, null);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-count', (function($__L_this) {  while(true) { $__L_s = ($P->_seq)($__L_this);
$__L_c = 0;
 while(true) { if ($__L_s) { $__recur_0 = ($P->_next)($__L_s); $__recur_1 = ($__L_c + 1); $__L_s = $__recur_0; $__L_c = $__recur_1; continue;} else { return $__L_c;} break; }
 break; }}), '-equiv', (function($__L_this, $__L_other) {  while(true) { if (($NS->satisfies_QMARK_)($P->ISequential, $__L_other)) { $__L_s1 = ($P->_seq)($__L_this);
$__L_s2 = ($P->_seq)($__L_other);
 while(true) { if (($__L_s1 === null)) { return ($__L_s2 === null);} else { if (($__L_s2 === null)) { return false;} else { if (\Clojure\Php\equals(($P->_first)($__L_s1), ($P->_first)($__L_s2))) { $__recur_0 = ($P->_next)($__L_s1); $__recur_1 = ($P->_next)($__L_s2); $__L_s1 = $__recur_0; $__L_s2 = $__recur_1; continue;} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}} break; }
} else { return false;} break; }}), '-hash', (function($__L_this) {  while(true) { $__L_s = ($P->_seq)($__L_this);
$__L_h = 1;
 while(true) { if ($__L_s) { $__recur_0 = ($P->_next)($__L_s); $__recur_1 = ((31 * $__L_h) + ($NS->hash)(($P->_first)($__L_s))); $__L_s = $__recur_0; $__L_h = $__recur_1; continue;} else { return $__L_h;} break; }
 break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_LazySeq)($__L__->fn, $__L__->sv, $__L__->s, $__L_m);}), '-realized?', (function($__L__) { return ($__L__->fn === null);})));
$NS->__GT_LazySeq = (function($__L_fn, $__L_sv, $__L_s, $__L__meta) { return call_user_func('\Clojure\Php\createType', 'LazySeq', $__L_fn, $__L_sv, $__L_s, $__L__meta);});
$NS->lazy_seq_STAR_ = (function($__L_f) { return ($NS->__GT_LazySeq)($__L_f, null, null, null);});
call_user_func('\Clojure\Php\defType', 'Delay', array('fn', 'val', 'exception'), array('fn', 'val', 'exception'), array('-deref', (function($__L_this) { if ($__L_this->fn) { try { return $__L_this->val = ($Clojure_Core->apply)($NS->fn); } catch (\Throwable $__L_e) { return $__L_this->exception = $__L_e; }
$__L_this->fn = null;
}
if ($__L_this->exception) { throw $__L_this->exception;
} else { return $__L_this->val;}}), '-realized?', (function($__L__) { return ($__L__->fn === null);})));
$NS->__GT_Delay = (function($__L_fn, $__L_val, $__L_exception) { return call_user_func('\Clojure\Php\createType', 'Delay', $__L_fn, $__L_val, $__L_exception);});
$NS->delay_STAR_ = (function($__L_f) { return ($NS->__GT_Delay)($__L_f, null, null);});
$NS->delay_QMARK_ = (function($__L_x) { return ($NS->instance_QMARK_)($NS->Delay, $__L_x);});
$NS->force = (function($__L_x) { if (($NS->delay_QMARK_)($__L_x)) { return ($P->_deref)($__L_x);} else { return $__L_x;}});
call_user_func('\Clojure\Php\defType', 'Volatile', array('val'), array('val'), array('-deref', (function($__L__) { return $__L__->val;}), '-vreset!', (function($__L__, $__L_new_val) { $__L__->val = $__L_new_val;
return $__L_new_val;})));
$NS->__GT_Volatile = (function($__L_val) { return call_user_func('\Clojure\Php\createType', 'Volatile', $__L_val);});
$NS->volatile_BANG_ = (function($__L_val) { return ($NS->__GT_Volatile)($__L_val);});
$NS->volatile_QMARK_ = (function($__L_x) { return ($NS->instance_QMARK_)($NS->Volatile, $__L_x);});
$NS->vreset_BANG_ = (function($__L_vol, $__L_new_val) { return ($P->_vreset_BANG_)($__L_vol, $__L_new_val);});
$NS->vswap_BANG_ = (function($__L_vol, $__L_f, ...$__L_args) { return ($NS->vreset_BANG_)($__L_vol, ($NS->apply)($__L_f, ($Clojure_Core->deref)($__L_vol), $__L_args));});
call_user_func('\Clojure\Php\defType', 'Range', array('start', 'end', 'step', '_meta'), array(), array('-seq', (function($__L_this) { if (($__L_this->step > 0)) { if (($__L_this->start < $__L_this->end)) { return $__L_this;}} else { if (($__L_this->start > $__L_this->end)) { return $__L_this;}}}), '-first', (function($__L__) { return $__L__->start;}), '-rest', (function($__L__) { $__L_next_val = ($__L__->start + $__L__->step);
if (($__L__->step > 0)) { if (($__L_next_val < $__L__->end)) { return ($NS->__GT_Range)($__L_next_val, $__L__->end, $__L__->step, $__L__->_meta);} else { return \Clojure\Php\emptyList();}} else { if (($__L_next_val > $__L__->end)) { return ($NS->__GT_Range)($__L_next_val, $__L__->end, $__L__->step, $__L__->_meta);} else { return \Clojure\Php\emptyList();}}}), '-next', (function($__L_this) { $__L_r = ($P->_rest)($__L_this);
if (($P->_seq)($__L_r)) { return $__L_r;}}), '-count', (function($__L__) { if (((function() { $__L_or__3040 = (($__L__->step > 0) ? ($__L__->start < $__L__->end) : false); if ($__L_or__3040) { return $__L_or__3040;} else { if (($__L__->step < 0)) { return ($__L__->start > $__L__->end);} else { return false;}} })())) { $__L_diff = ($__L__->end - $__L__->start);
return ((int)Math::ceil(($__L_diff / $__L__->step)));} else { return 0;}}), '-nth', (function($__L_this, $__L_n) { $__L_val = ($__L_this->start + ($__L_n * $__L_this->step));
if (($__L_this->step > 0)) { if ((($__L_val >= $__L_this->start) ? ($__L_val < $__L_this->end) : false)) { return $__L_val;} else { throw ($NS->ex_info)('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n));
}} else { if ((($__L_val <= $__L_this->start) ? ($__L_val > $__L_this->end) : false)) { return $__L_val;} else { throw ($NS->ex_info)('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n));
}}}), '-nth', (function($__L_this, $__L_n, $__L_not_found) { $__L_val = ($__L_this->start + ($__L_n * $__L_this->step));
if (($__L_this->step > 0)) { if ((($__L_val >= $__L_this->start) ? ($__L_val < $__L_this->end) : false)) { return $__L_val;} else { return $__L_not_found;}} else { if ((($__L_val <= $__L_this->start) ? ($__L_val > $__L_this->end) : false)) { return $__L_val;} else { return $__L_not_found;}}}), '-conj', (function($__L_this, $__L_o) { return ($NS->__GT_Cons)($__L_o, $__L_this, null);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-equiv', (function($__L_this, $__L_other) {  while(true) { if (($NS->satisfies_QMARK_)($P->ISequential, $__L_other)) { $__L_s1 = ($P->_seq)($__L_this);
$__L_s2 = ($P->_seq)($__L_other);
 while(true) { if (($__L_s1 === null)) { return ($__L_s2 === null);} else { if (($__L_s2 === null)) { return false;} else { if (\Clojure\Php\equals(($P->_first)($__L_s1), ($P->_first)($__L_s2))) { $__recur_0 = ($P->_next)($__L_s1); $__recur_1 = ($P->_next)($__L_s2); $__L_s1 = $__recur_0; $__L_s2 = $__recur_1; continue;} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}} break; }
} else { return false;} break; }}), '-hash', (function($__L_this) {  while(true) { $__L_s = ($P->_seq)($__L_this);
$__L_h = 1;
 while(true) { if ($__L_s) { $__recur_0 = ($P->_next)($__L_s); $__recur_1 = ((31 * $__L_h) + ($NS->hash)(($P->_first)($__L_s))); $__L_s = $__recur_0; $__L_h = $__recur_1; continue;} else { return $__L_h;} break; }
 break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_Range)($__L__->start, $__L__->end, $__L__->step, $__L_m);}), '-reduce', (function($__L_this, $__L_f) {  while(true) { $__L_acc = $__L_this->start;
$__L_val = ($__L_this->start + $__L_this->step);
 while(true) { if (($__L_this->step > 0)) { if (($__L_val < $__L_this->end)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L_val);
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_val + $__L_this->step); $__L_acc = $__recur_0; $__L_val = $__recur_1; continue;}} else { return $__L_acc;}} else { if (($__L_val > $__L_this->end)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L_val);
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_val + $__L_this->step); $__L_acc = $__recur_0; $__L_val = $__recur_1; continue;}} else { return $__L_acc;}} break; }
 break; }}), '-reduce-init', (function($__L__, $__L_f, $__L_init) {  while(true) { $__L_acc = $__L_init;
$__L_val = $__L__->start;
 while(true) { if (($__L__->step > 0)) { if (($__L_val < $__L__->end)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L_val);
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_val + $__L__->step); $__L_acc = $__recur_0; $__L_val = $__recur_1; continue;}} else { return $__L_acc;}} else { if (($__L_val > $__L__->end)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L_val);
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_val + $__L__->step); $__L_acc = $__recur_0; $__L_val = $__recur_1; continue;}} else { return $__L_acc;}} break; }
 break; }})));
$NS->__GT_Range = (function($__L_start, $__L_end, $__L_step, $__L__meta) { return call_user_func('\Clojure\Php\createType', 'Range', $__L_start, $__L_end, $__L_step, $__L__meta);});
$NS->range = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_start = $__args[0]; $__L_end = $__args[1]; $__L_step = $__args[2]; return ($NS->__GT_Range)($__L_start, $__L_end, $__L_step, null); } else if ($__n == 2) { $__L_start = $__args[0]; $__L_end = $__args[1]; return ($NS->__GT_Range)($__L_start, $__L_end, 1, null); } else if ($__n == 1) { $__L_end = $__args[0]; return ($NS->__GT_Range)(0, $__L_end, 1, null); } else if ($__n == 0) { return ($NS->__GT_Range)(0, Infinity, 1, null); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
call_user_func('\Clojure\Php\defType', 'Repeat', array('val', 'count', '_meta'), array(), array('-seq', (function($__L_this) { if (((function() { $__L_or__3041 = ($__L_this->count === null); if ($__L_or__3041) { return $__L_or__3041;} else { return ($__L_this->count > 0);} })())) { return $__L_this;}}), '-first', (function($__L__) { return $__L__->val;}), '-rest', (function($__L__) { if (($__L__->count === null)) { return ($NS->__GT_Repeat)($__L__->val, null, $__L__->_meta);} else { if (($__L__->count > 1)) { return ($NS->__GT_Repeat)($__L__->val, ($__L__->count - 1), $__L__->_meta);} else { return \Clojure\Php\emptyList();}}}), '-next', (function($__L_this) { $__L_r = ($P->_rest)($__L_this);
if (($P->_seq)($__L_r)) { return $__L_r;}}), '-count', (function($__L__) { if (($__L__->count === null)) { throw ($NS->ex_info)('Cannot count infinite repeat', \Clojure\Php\hashMap());
} else { return $__L__->count;}}), '-conj', (function($__L_this, $__L_o) { return ($NS->__GT_Cons)($__L_o, $__L_this, null);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_Repeat)($__L__->val, $__L__->count, $__L_m);}), '-reduce', (function($__L__, $__L_f) {  while(true) { if (($__L__->count === null)) { $__L_acc = $__L__->val;
 while(true) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L__->val);
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = $__L_ret; $__L_acc = $__recur_0; continue;} break; }
} else { $__L_acc = $__L__->val;
$__L_n = ($__L__->count - 1);
 while(true) { if (($__L_n > 0)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L__->val);
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_n - 1); $__L_acc = $__recur_0; $__L_n = $__recur_1; continue;}} else { return $__L_acc;} break; }
} break; }}), '-reduce-init', (function($__L__, $__L_f, $__L_init) {  while(true) { if (($__L__->count === null)) { $__L_acc = $__L_init;
 while(true) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L__->val);
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = $__L_ret; $__L_acc = $__recur_0; continue;} break; }
} else { $__L_acc = $__L_init;
$__L_n = $__L__->count;
 while(true) { if (($__L_n > 0)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L__->val);
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = ($__L_n - 1); $__L_acc = $__recur_0; $__L_n = $__recur_1; continue;}} else { return $__L_acc;} break; }
} break; }})));
$NS->__GT_Repeat = (function($__L_val, $__L_count, $__L__meta) { return call_user_func('\Clojure\Php\createType', 'Repeat', $__L_val, $__L_count, $__L__meta);});
$NS->repeat = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_x = $__args[1]; return ($NS->__GT_Repeat)($__L_x, $__L_n, null); } else if ($__n == 1) { $__L_x = $__args[0]; return ($NS->__GT_Repeat)($__L_x, null, null); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
call_user_func('\Clojure\Php\defType', 'Iterate', array('f', 'val', '_meta'), array(), array('-seq', (function($__L_this) { return $__L_this;}), '-first', (function($__L__) { return $__L__->val;}), '-rest', (function($__L__) { return ($NS->__GT_Iterate)($__L__->f, call_user_func($__L__->f, $__L__->val), $__L__->_meta);}), '-next', (function($__L_this) { return ($P->_rest)($__L_this);}), '-conj', (function($__L_this, $__L_o) { return ($NS->__GT_Cons)($__L_o, $__L_this, null);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_Iterate)($__L__->f, $__L__->val, $__L_m);}), '-reduce-init', (function($__L__, $__L_rf, $__L_init) {  while(true) { $__L_acc = $__L_init;
$__L_v = $__L__->val;
 while(true) { $__L_ret = call_user_func($__L_rf, $__L_acc, $__L_v);
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = call_user_func($__L__->f, $__L_v); $__L_acc = $__recur_0; $__L_v = $__recur_1; continue;} break; }
 break; }})));
$NS->__GT_Iterate = (function($__L_f, $__L_val, $__L__meta) { return call_user_func('\Clojure\Php\createType', 'Iterate', $__L_f, $__L_val, $__L__meta);});
$NS->iterate = (function($__L_f, $__L_x) { return ($NS->__GT_Iterate)($__L_f, $__L_x, null);});
call_user_func('\Clojure\Php\defType', 'Cycle', array('all', 'current', '_meta'), array(), array('-seq', (function($__L_this) { if (($P->_seq)($__L_this->all)) { return $__L_this;}}), '-first', (function($__L__) { return ($P->_first)($__L__->current);}), '-rest', (function($__L__) { $__L_nxt = ($P->_next)($__L__->current);
if ($__L_nxt) { return ($NS->__GT_Cycle)($__L__->all, $__L_nxt, $__L__->_meta);} else { return ($NS->__GT_Cycle)($__L__->all, ($P->_seq)($__L__->all), $__L__->_meta);}}), '-next', (function($__L_this) { return ($P->_rest)($__L_this);}), '-conj', (function($__L_this, $__L_o) { return ($NS->__GT_Cons)($__L_o, $__L_this, null);}), '-empty', (function($__L__) { return \Clojure\Php\emptyList();}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_Cycle)($__L__->all, $__L__->current, $__L_m);})));
$NS->__GT_Cycle = (function($__L_all, $__L_current, $__L__meta) { return call_user_func('\Clojure\Php\createType', 'Cycle', $__L_all, $__L_current, $__L__meta);});
$NS->cycle = (function($__L_coll) { $__L_s = \Clojure\Php\seq($__L_coll);
if ($__L_s) { return ($NS->__GT_Cycle)($__L_s, $__L_s, null);}});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.PersistentVector'), 'Persistent Vector - matches JVM clojure.lang.PersistentVector API.

   32-way branching trie (HAMT) implementation providing:
   - O(log32 n) access, update, and append
   - Structural sharing for efficiency
   - Transient support for batch operations', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')], [\Clojure\Php\Sym::create('clojure.lang.array'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('arr')]));
$NS->BITS = 5;
$NS->WIDTH = 32;
$NS->MASK = 31;
call_user_func('\Clojure\Php\defType', 'VectorNode', array('edit', 'arr'), array(), array());
$NS->__GT_VectorNode = (function($__L_edit, $__L_arr) { return call_user_func('\Clojure\Php\createType', 'VectorNode', $__L_edit, $__L_arr);});
$NS->EMPTY_NODE = ($NS->__GT_VectorNode)(null, ($Arr->object_array)($NS->WIDTH));
$NS->new_path = (function($__L_edit, $__L_level, $__L_node) { if (($__L_level === 0 || $__L_level === 0.0)) { return $__L_node;} else { $__L_ret = ($NS->__GT_VectorNode)($__L_edit, ($Arr->object_array)($NS->WIDTH));
($Arr->aset)($__L_ret->arr, 0, ($NS->new_path)($__L_edit, ($__L_level - $NS->BITS), $__L_node));
return $__L_ret;}});
$NS->push_tail = (function($__L_level, $__L_parent, $__L_tail_node) { $__L_subidx = (($NS->unsigned_bit_shift_right)(($NS->cnt - 1), $__L_level) & $NS->MASK);
$__L_ret = ($NS->__GT_VectorNode)($__L_parent->edit, ($Arr->aclone)($__L_parent->arr));
$__L_node_to_insert = (\Clojure\Php\equals($__L_level, $NS->BITS) ? $__L_tail_node : ((function() { $__L_child = ($Arr->aget)($__L_parent->arr, $__L_subidx); if ($__L_child) { return ($NS->push_tail)(($__L_level - $NS->BITS), $__L_child, $__L_tail_node);} else { return ($NS->new_path)($__L_parent->edit, ($__L_level - $NS->BITS), $__L_tail_node);} })()));
($Arr->aset)($__L_ret->arr, $__L_subidx, $__L_node_to_insert);
return $__L_ret;});
$NS->do_assoc = (function($__L_level, $__L_node, $__L_i, $__L_val) { $__L_ret = ($NS->__GT_VectorNode)($__L_node->edit, ($Arr->aclone)($__L_node->arr));
if (($__L_level === 0 || $__L_level === 0.0)) { ($Arr->aset)($__L_ret->arr, ($__L_i & $NS->MASK), $__L_val);
return $__L_ret;} else { $__L_subidx = (($NS->unsigned_bit_shift_right)($__L_i, $__L_level) & $NS->MASK);
($Arr->aset)($__L_ret->arr, $__L_subidx, ($NS->do_assoc)(($__L_level - $NS->BITS), ($Arr->aget)($__L_node->arr, $__L_subidx), $__L_i, $__L_val));
return $__L_ret;}});
$NS->array_for = (function($__L_vec, $__L_i) {  while(true) { if ((($__L_i >= 0) ? ($__L_i < $__L_vec->cnt) : false)) { if (($__L_i >= ($NS->tail_off)($__L_vec))) { return $__L_vec->tail;} else { $__L_node = $__L_vec->root;
$__L_level = $__L_vec->shift;
 while(true) { if (($__L_level > 0)) { $__recur_0 = ($Arr->aget)($__L_node->arr, (($NS->unsigned_bit_shift_right)($__L_i, $__L_level) & $NS->MASK)); $__recur_1 = ($__L_level - $NS->BITS); $__L_node = $__recur_0; $__L_level = $__recur_1; continue;} else { return $__L_node->arr;} break; }
}} else { throw ($NS->ex_info)('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('index'), $__L_i, \Clojure\Php\Kw::create('count'), $__L_vec->cnt));
} break; }});
$NS->tail_off = (function($__L_vec) { $__L_cnt = $__L_vec->cnt;
if (($__L_cnt < $NS->WIDTH)) { return 0;} else { return (($NS->unsigned_bit_shift_right)(($__L_cnt - 1), $NS->BITS) << $NS->BITS);}});
$NS->EMPTY_VECTOR = null;
call_user_func('\Clojure\Php\defType', 'PersistentVector', array('cnt', 'shift', 'root', 'tail', '_meta', '_hash'), array(), array('-count', (function($__L__) { return $__L__->cnt;}), '-nth', (function($__L_this, $__L_n) { return ($Arr->aget)(($NS->array_for)($__L_this, $__L_n), ($__L_n & $NS->MASK));}), '-nth', (function($__L_this, $__L_n, $__L_not_found) { if ((($__L_n >= 0) ? ($__L_n < $__L_this->cnt) : false)) { return ($Arr->aget)(($NS->array_for)($__L_this, $__L_n), ($__L_n & $NS->MASK));} else { return $__L_not_found;}}), '-lookup', (function($__L_this, $__L_k) { if ((is_int($__L_k) || is_float($__L_k))) { return ($P->_nth)($__L_this, $__L_k, null);}}), '-lookup', (function($__L_this, $__L_k, $__L_not_found) { if ((is_int($__L_k) || is_float($__L_k))) { return ($P->_nth)($__L_this, $__L_k, $__L_not_found);} else { return $__L_not_found;}}), '-contains-key?', (function($__L__, $__L_k) { if ((is_int($__L_k) || is_float($__L_k))) { if (($__L_k >= 0)) { return ($__L_k < $__L__->cnt);} else { return false;}} else { return false;}}), '-assoc', (function($__L_this, $__L_k, $__L_v) { if ((is_int($__L_k) || is_float($__L_k))) { $__L_i = ((int)$__L_k);
if ((($__L_i >= 0) ? ($__L_i < $__L_this->cnt) : false)) { if (($__L_i >= ($NS->tail_off)($__L_this))) { $__L_new_tail = ($Arr->aclone)($__L_this->tail);
($Arr->asetnew_tail)(($__L_i & $NS->MASK), $__L_v);
return ($NS->__GT_PersistentVector)($__L_this->cnt, $__L_this->shift, $__L_this->root, $__L_new_tail, $__L_this->_meta, null);} else { return ($NS->__GT_PersistentVector)($__L_this->cnt, $__L_this->shift, ($NS->do_assoc)($__L_this->shift, $__L_this->root, $__L_i, $__L_v), $__L_this->tail, $__L_this->_meta, null);}} else { if (\Clojure\Php\equals($__L_i, $__L_this->cnt)) { return ($P->_conj)($__L_this, $__L_v);} else { if (\Clojure\Php\Kw::create('else')) { throw ($NS->ex_info)('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('index'), $__L_i, \Clojure\Php\Kw::create('count'), $__L_this->cnt));
}}}} else { throw ($NS->ex_info)('Key must be integer', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('key'), $__L_k));
}}), '-peek', (function($__L__) { if (($__L__->cnt > 0)) { return ($P->_nth)($__L__, ($__L__->cnt - 1));}}), '-pop', (function($__L_this) { if (($__L_this->cnt === 0 || $__L_this->cnt === 0.0)) { throw ($NS->ex_info)('Can\'t pop empty vector', \Clojure\Php\hashMap());
} else { if (\Clojure\Php\equals(1, $__L_this->cnt)) { return $NS->EMPTY_VECTOR;} else { if ((($__L_this->cnt - ($NS->tail_off)($__L_this)) > 1)) { $__L_new_tail = ($NS->make_array)((($Arr->alengthtail)() - 1));
System::arraycopy($__L_this->tail, 0, $__L_new_tail, 0, ($Arr->alengthnew_tail)());
return ($NS->__GT_PersistentVector)(($__L_this->cnt - 1), $__L_this->shift, $__L_this->root, $__L_new_tail, $__L_this->_meta, null);} else { if (\Clojure\Php\Kw::create('else')) { $__L_new_tail = ($NS->array_for)($__L_this, ($__L_this->cnt - 2));
$__L_new_root = ($NS->pop_tail)($__L_this->shift, $__L_this->root);
if (($__L_new_root === null)) { return ($NS->__GT_PersistentVector)(($__L_this->cnt - 1), $__L_this->shift, $NS->EMPTY_NODE, $__L_new_tail, $__L_this->_meta, null);} else { if ((($__L_this->shift > $NS->BITS) ? (($Arr->aget)($__L_new_root->arr, 1) === null) : false)) { return ($NS->__GT_PersistentVector)(($__L_this->cnt - 1), ($__L_this->shift - $NS->BITS), ($Arr->aget)($__L_new_root->arr, 0), $__L_new_tail, $__L_this->_meta, null);} else { if (\Clojure\Php\Kw::create('else')) { return ($NS->__GT_PersistentVector)(($__L_this->cnt - 1), $__L_this->shift, $__L_new_root, $__L_new_tail, $__L_this->_meta, null);}}}}}}}}), '-conj', (function($__L_this, $__L_val) { if ((($__L_this->cnt - ($NS->tail_off)($__L_this)) < $NS->WIDTH)) { $__L_new_tail = ($NS->make_array)((($Arr->alengthtail)() + 1));
System::arraycopy($__L_this->tail, 0, $__L_new_tail, 0, ($Arr->alengthtail)());
($Arr->asetnew_tail)(($Arr->alengthtail)(), $__L_val);
return ($NS->__GT_PersistentVector)(($__L_this->cnt + 1), $__L_this->shift, $__L_this->root, $__L_new_tail, $__L_this->_meta, null);} else { $__L_tail_node = ($NS->__GT_VectorNode)(null, $__L_this->tail);
$__L_new_shift = $__L_this->shift;
$__L_new_root = ((($NS->unsigned_bit_shift_right)($__L_this->cnt, $NS->BITS) > (1 << $__L_this->shift)) ? ((function() { $__L_new_root = ($NS->__GT_VectorNode)(null, ($Arr->object_array)($NS->WIDTH)); ($Arr->aset)($__L_new_root->arr, 0, $__L_this->root);
($Arr->aset)($__L_new_root->arr, 1, ($NS->new_path)(null, $__L_this->shift, $__L_tail_node));
$new_shift = ($__L_this->shift + $NS->BITS);
return $__L_new_root; })()) : ($NS->push_tail)($__L_this->shift, $__L_this->root, $__L_tail_node));
($NS->__GT_PersistentVector)(($__L_this->cnt + 1), $__L_new_shift, $__L_new_root, ($Arr->object_array)(1), $__L_this->_meta, null);
($Arr->aset)($NS->_->tail, 0, $__L_val);
return $NS->_;}}), '-empty', (function($__L__) { return $NS->EMPTY_VECTOR;}), '-seq', (function($__L_this) { if (($__L_this->cnt > 0)) { return ($NS->chunked_seq)($__L_this, 0, 0);}}), '-equiv', (function($__L_this, $__L_other) {  while(true) { if (($__L_this === $__L_other)) { return true;} else { if (($NS->satisfies_QMARK_)($P->IVector, $__L_other)) { if (\Clojure\Php\equals($__L_this->cnt, ($P->_count)($__L_other))) { $__L_i = 0;
 while(true) { if (($__L_i < $__L_this->cnt)) { if (\Clojure\Php\equals(($P->_nth)($__L_this, $__L_i), ($P->_nth)($__L_other, $__L_i))) { $__recur_0 = ($__L_i + 1); $__L_i = $__recur_0; continue;} else { return false;}} else { return true;} break; }
} else { return false;}} else { if (($NS->satisfies_QMARK_)($P->ISequential, $__L_other)) { $__L_s1 = ($P->_seq)($__L_this);
$__L_s2 = ($P->_seq)($__L_other);
 while(true) { if (($__L_s1 === null)) { return ($__L_s2 === null);} else { if (($__L_s2 === null)) { return false;} else { if (\Clojure\Php\equals(($P->_first)($__L_s1), ($P->_first)($__L_s2))) { $__recur_0 = ($P->_next)($__L_s1); $__recur_1 = ($P->_next)($__L_s2); $__L_s1 = $__recur_0; $__L_s2 = $__recur_1; continue;} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}} break; }
} else { if (\Clojure\Php\Kw::create('else')) { return false;}}}} break; }}), '-hash', (function($__L_this) {  while(true) { if ($__L_this->_hash) { return $__L_this->_hash;} else { $__L_h = ((function() { $__L_s = ($P->_seq)($__L_this); $__L_h = 1;  while(true) { if ($__L_s) { $__recur_0 = ($P->_next)($__L_s); $__recur_1 = ((31 * $__L_h) + ($NS->hash)(($P->_first)($__L_s))); $__L_s = $__recur_0; $__L_h = $__recur_1; continue;} else { $__L_h;
}
 break; } return $__L_s; })());
$__L_this->_hash = $__L_h;
return $__L_h;} break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_PersistentVector)($__L__->cnt, $__L__->shift, $__L__->root, $__L__->tail, $__L_m, $__L__->_hash);}), '-reduce', (function($__L_this, $__L_f) {  while(true) { if (($__L_this->cnt > 0)) { $__L_i = 1;
$__L_acc = ($P->_nth)($__L_this, 0);
 while(true) { if (($__L_i < $__L_this->cnt)) { $__L_ret = call_user_func($__L_f, $__L_acc, ($P->_nth)($__L_this, $__L_i));
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = ($__L_i + 1); $__recur_1 = $__L_ret; $__L_i = $__recur_0; $__L_acc = $__recur_1; continue;}} else { return $__L_acc;} break; }
} else { return call_user_func($__L_f);} break; }}), '-reduce-init', (function($__L_this, $__L_f, $__L_init) {  while(true) { $__L_i = 0;
$__L_acc = $__L_init;
 while(true) { if (($__L_i < $__L_this->cnt)) { $__L_ret = call_user_func($__L_f, $__L_acc, ($P->_nth)($__L_this, $__L_i));
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = ($__L_i + 1); $__recur_1 = $__L_ret; $__L_i = $__recur_0; $__L_acc = $__recur_1; continue;}} else { return $__L_acc;} break; }
 break; }}), '-kv-reduce', (function($__L_this, $__L_f, $__L_init) {  while(true) { $__L_i = 0;
$__L_acc = $__L_init;
 while(true) { if (($__L_i < $__L_this->cnt)) { $__L_ret = call_user_func($__L_f, $__L_acc, $__L_i, ($P->_nth)($__L_this, $__L_i));
if (($NS->reduced_QMARK_)($__L_ret)) { return ($Clojure_Core->deref)($__L_ret);} else { $__recur_0 = ($__L_i + 1); $__recur_1 = $__L_ret; $__L_i = $__recur_0; $__L_acc = $__recur_1; continue;}} else { return $__L_acc;} break; }
 break; }}), '-invoke', (function($__L_this, $__L_k) { return ($P->_nth)($__L_this, $__L_k);}), '-invoke', (function($__L_this, $__L_k, $__L_not_found) { return ($P->_nth)($__L_this, $__L_k, $__L_not_found);}), '-rseq', (function($__L_this) { if (($__L_this->cnt > 0)) { return ($NS->rseq)($__L_this, ($__L_this->cnt - 1));}})));
$NS->__GT_PersistentVector = (function($__L_cnt, $__L_shift, $__L_root, $__L_tail, $__L__meta, $__L__hash) { return call_user_func('\Clojure\Php\createType', 'PersistentVector', $__L_cnt, $__L_shift, $__L_root, $__L_tail, $__L__meta, $__L__hash);});
$NS->EMPTY_VECTOR = ($NS->__GT_PersistentVector)(0, $NS->BITS, $NS->EMPTY_NODE, ($Arr->object_array)(0), null, null);
$NS->vector = (function(...$__args) { $__n = count($__args); if ($__n == 4) { $__L_a = $__args[0]; $__L_b = $__args[1]; $__L_c = $__args[2]; $__L_d = $__args[3]; return ($NS->__GT_)($NS->EMPTY_VECTOR, ($P->_conj)($__L_a), ($P->_conj)($__L_b), ($P->_conj)($__L_c), ($P->_conj)($__L_d)); } else if ($__n == 3) { $__L_a = $__args[0]; $__L_b = $__args[1]; $__L_c = $__args[2]; return ($NS->__GT_)($NS->EMPTY_VECTOR, ($P->_conj)($__L_a), ($P->_conj)($__L_b), ($P->_conj)($__L_c)); } else if ($__n == 2) { $__L_a = $__args[0]; $__L_b = $__args[1]; return ($NS->__GT_)($NS->EMPTY_VECTOR, ($P->_conj)($__L_a), ($P->_conj)($__L_b)); } else if ($__n == 1) { $__L_a = $__args[0]; return ($P->_conj)($NS->EMPTY_VECTOR, $__L_a); } else if ($__n == 0) { return $NS->EMPTY_VECTOR; } else if ($__n >= 4) { $__L_a = $__args[0]; $__L_b = $__args[1]; $__L_c = $__args[2]; $__L_d = $__args[3]; $__L_more = array_slice($__args, 4);  while(true) { $__L_v = ($NS->vector)($__L_a, $__L_b, $__L_c, $__L_d);
$__L_s = \Clojure\Php\seq($__L_more);
 while(true) { if ($__L_s) { $__recur_0 = ($P->_conj)($__L_v, \Clojure\Php\first($__L_s)); $__recur_1 = \Clojure\Php\next_($__L_s); $__L_v = $__recur_0; $__L_s = $__recur_1; continue;} else { return $__L_v;} break; }
 break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->vec = (function($__L_coll) { if (($__L_coll instanceof \Clojure\Php\Vec)) { return $__L_coll;} else { return ($NS->reduce)($NS->conj, $NS->EMPTY_VECTOR, $__L_coll);}});
$NS->vector_QMARK_ = (function($__L_x) { return ($NS->instance_QMARK_)($NS->PersistentVector, $__L_x);});
$NS->subvec = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_v = $__args[0]; $__L_start = $__args[1]; $__L_end = $__args[2];  while(true) { $__L_ret = $NS->EMPTY_VECTOR;
$__L_i = $__L_start;
 while(true) { if (($__L_i < $__L_end)) { $__recur_0 = \Clojure\Php\conj($__L_ret, \Clojure\Php\nth($__L_v, $__L_i)); $__recur_1 = ($__L_i + 1); $__L_ret = $__recur_0; $__L_i = $__recur_1; continue;} else { return $__L_ret;} break; }
 break; } } else if ($__n == 2) { $__L_v = $__args[0]; $__L_start = $__args[1]; return ($NS->subvec)($__L_v, $__L_start, \Clojure\Php\count_($__L_v)); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.PersistentHashMap'), 'Persistent Hash Map - matches JVM clojure.lang.PersistentHashMap API.

   Hash Array Mapped Trie (HAMT) implementation providing:
   - O(log32 n) lookup, insert, and delete
   - Structural sharing for efficiency', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$NS->BITS = 5;
$NS->WIDTH = 32;
$NS->MASK = 31;
$NS->hash_code = (function($__L_x) { if (($__L_x === null)) { return 0;} else { return ($NS->hash)($__L_x);}});
$NS->EMPTY_MAP = null;
call_user_func('\Clojure\Php\defType', 'BitmapIndexedNode', array('bitmap', 'arr'), array(), array());
$NS->__GT_BitmapIndexedNode = (function($__L_bitmap, $__L_arr) { return call_user_func('\Clojure\Php\createType', 'BitmapIndexedNode', $__L_bitmap, $__L_arr);});
call_user_func('\Clojure\Php\defType', 'ArrayNode', array('cnt', 'arr'), array(), array());
$NS->__GT_ArrayNode = (function($__L_cnt, $__L_arr) { return call_user_func('\Clojure\Php\createType', 'ArrayNode', $__L_cnt, $__L_arr);});
call_user_func('\Clojure\Php\defType', 'HashCollisionNode', array('hash', 'cnt', 'arr'), array(), array());
$NS->__GT_HashCollisionNode = (function($__L_hash, $__L_cnt, $__L_arr) { return call_user_func('\Clojure\Php\createType', 'HashCollisionNode', $__L_hash, $__L_cnt, $__L_arr);});
$NS->EMPTY_BITMAP_NODE = ($NS->__GT_BitmapIndexedNode)(0, ($NS->object_array)(0));
$NS->mask = (function($__L_hash, $__L_shift) { return (($NS->unsigned_bit_shift_right)($__L_hash, $__L_shift) & $NS->MASK);});
$NS->bitpos = (function($__L_hash, $__L_shift) { return (1 << ($NS->mask)($__L_hash, $__L_shift));});
$NS->index = (function($__L_bitmap, $__L_bit) { return Integer::bitCount(($__L_bitmap & ($__L_bit - 1)));});
$NS->node_assoc = null;
$NS->node_find = null;
$NS->node_dissoc = null;
$NS->clone_and_set = (function(...$__args) { $__n = count($__args); if ($__n == 5) { $__L_arr = $__args[0]; $__L_i = $__args[1]; $__L_val = $__args[2]; $__L_j = $__args[3]; $__L_val2 = $__args[4]; $__L_clone = ($NS->aclone)($__L_arr);
($NS->aset)($__L_clone, $__L_i, $__L_val);
($NS->aset)($__L_clone, $__L_j, $__L_val2);
return $__L_clone; } else if ($__n == 3) { $__L_arr = $__args[0]; $__L_i = $__args[1]; $__L_val = $__args[2]; $__L_clone = ($NS->aclone)($__L_arr);
($NS->aset)($__L_clone, $__L_i, $__L_val);
return $__L_clone; } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->remove_pair = (function($__L_arr, $__L_i) { $__L_new_arr = ($NS->object_array)((($NS->alength)($__L_arr) - 2));
System::arraycopy($__L_arr, 0, $__L_new_arr, 0, (2 * $__L_i));
System::arraycopy($__L_arr, (2 * ($__L_i + 1)), $__L_new_arr, (2 * $__L_i), (($NS->alength)($__L_new_arr) - (2 * $__L_i)));
return $__L_new_arr;});
$NS->create_node = (function($__L_shift, $__L_key1, $__L_val1, $__L_key2_hash, $__L_key2, $__L_val2) { $__L_key1_hash = ($NS->hash_code)($__L_key1);
if (\Clojure\Php\equals($__L_key1_hash, $__L_key2_hash)) { return ($NS->__GT_HashCollisionNode)($__L_key1_hash, 2, ($NS->object_array)(\Clojure\Php\vec($__L_key1, $__L_val1, $__L_key2, $__L_val2)));} else { $__L_n1 = ($NS->node_assoc)($NS->EMPTY_BITMAP_NODE, $__L_shift, $__L_key1_hash, $__L_key1, $__L_val1);
$__L_n2 = ($NS->node_assoc)($__L_n1, $__L_shift, $__L_key2_hash, $__L_key2, $__L_val2);
return $__L_n2;}});
$NS->bitmap_node_assoc = (function($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_val) { $__L_bit = ($NS->bitpos)($__L_hash, $__L_shift);
$__L_idx = ($NS->index)($__L_node->bitmap, $__L_bit);
$__L_arr = $__L_node->arr;
if ((!\Clojure\Php\equals(0, ($__L_node->bitmap & $__L_bit)))) { $__L_key_or_null = ($NS->aget)($__L_arr, (2 * $__L_idx));
$__L_val_or_node = ($NS->aget)($__L_arr, ((2 * $__L_idx) + 1));
if (($__L_key_or_null === null)) { $__L_n = ($NS->node_assoc)($__L_val_or_node, ($__L_shift + $NS->BITS), $__L_hash, $__L_key, $__L_val);
if (($__L_n === $__L_val_or_node)) { return $__L_node;} else { return ($NS->__GT_BitmapIndexedNode)($__L_node->bitmap, ($NS->clone_and_set)($__L_arr, ((2 * $__L_idx) + 1), $__L_n));}} else { if (\Clojure\Php\equals($__L_key, $__L_key_or_null)) { if (($__L_val === $__L_val_or_node)) { return $__L_node;} else { return ($NS->__GT_BitmapIndexedNode)($__L_node->bitmap, ($NS->clone_and_set)($__L_arr, ((2 * $__L_idx) + 1), $__L_val));}} else { if (\Clojure\Php\Kw::create('else')) { return ($NS->__GT_BitmapIndexedNode)($__L_node->bitmap, ($NS->clone_and_set)($__L_arr, (2 * $__L_idx), null, ((2 * $__L_idx) + 1), ($NS->create_node)(($__L_shift + $NS->BITS), $__L_key_or_null, $__L_val_or_node, $__L_hash, $__L_key, $__L_val)));}}}} else { $__L_n = ($NS->alength)($__L_arr);
$__L_new_arr = ($NS->object_array)(($__L_n + 2));
System::arraycopy($__L_arr, 0, $__L_new_arr, 0, (2 * $__L_idx));
($NS->aset)($__L_new_arr, (2 * $__L_idx), $__L_key);
($NS->aset)($__L_new_arr, ((2 * $__L_idx) + 1), $__L_val);
System::arraycopy($__L_arr, (2 * $__L_idx), $__L_new_arr, (2 * ($__L_idx + 1)), ($__L_n - (2 * $__L_idx)));
return ($NS->__GT_BitmapIndexedNode)(($__L_node->bitmap | $__L_bit), $__L_new_arr);}});
$NS->bitmap_node_find = (function($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_not_found) { $__L_bit = ($NS->bitpos)($__L_hash, $__L_shift);
if (\Clojure\Php\equals(0, ($__L_node->bitmap & $__L_bit))) { return $__L_not_found;} else { $__L_idx = ($NS->index)($__L_node->bitmap, $__L_bit);
$__L_key_or_null = ($NS->aget)($__L_node->arr, (2 * $__L_idx));
$__L_val_or_node = ($NS->aget)($__L_node->arr, ((2 * $__L_idx) + 1));
if (($__L_key_or_null === null)) { return ($NS->node_find)($__L_val_or_node, ($__L_shift + $NS->BITS), $__L_hash, $__L_key, $__L_not_found);} else { if (\Clojure\Php\equals($__L_key, $__L_key_or_null)) { return $__L_val_or_node;} else { if (\Clojure\Php\Kw::create('else')) { return $__L_not_found;}}}}});
$NS->bitmap_node_dissoc = (function($__L_node, $__L_shift, $__L_hash, $__L_key) { $__L_bit = ($NS->bitpos)($__L_hash, $__L_shift);
if (\Clojure\Php\equals(0, ($__L_node->bitmap & $__L_bit))) { return $__L_node;} else { $__L_idx = ($NS->index)($__L_node->bitmap, $__L_bit);
$__L_key_or_null = ($NS->aget)($__L_node->arr, (2 * $__L_idx));
$__L_val_or_node = ($NS->aget)($__L_node->arr, ((2 * $__L_idx) + 1));
if (($__L_key_or_null === null)) { $__L_n = ($NS->node_dissoc)($__L_val_or_node, ($__L_shift + $NS->BITS), $__L_hash, $__L_key);
if (($__L_n === $__L_val_or_node)) { return $__L_node;} else { if (($__L_n !== null)) { return ($NS->__GT_BitmapIndexedNode)($__L_node->bitmap, ($NS->clone_and_set)($__L_node->arr, ((2 * $__L_idx) + 1), $__L_n));} else { if (\Clojure\Php\equals($__L_node->bitmap, $__L_bit)) { return null;} else { if (\Clojure\Php\Kw::create('else')) { return ($NS->__GT_BitmapIndexedNode)(($__L_node->bitmap ^ $__L_bit), ($NS->remove_pair)($__L_node->arr, $__L_idx));}}}}} else { if (\Clojure\Php\equals($__L_key, $__L_key_or_null)) { if (\Clojure\Php\equals($__L_node->bitmap, $__L_bit)) { return null;} else { return ($NS->__GT_BitmapIndexedNode)(($__L_node->bitmap ^ $__L_bit), ($NS->remove_pair)($__L_node->arr, $__L_idx));}} else { if (\Clojure\Php\Kw::create('else')) { return $__L_node;}}}}});
$NS->collision_node_find_index = (function($__L_arr, $__L_cnt, $__L_key) {  while(true) { $__L_i = 0;
 while(true) { if (($__L_i < $__L_cnt)) { if (\Clojure\Php\equals($__L_key, ($NS->aget)($__L_arr, (2 * $__L_i)))) { return $__L_i;} else { $__recur_0 = ($__L_i + 1); $__L_i = $__recur_0; continue;}} else { return -1;} break; }
 break; }});
$NS->collision_node_assoc = (function($__L_node, $__L_hash, $__L_key, $__L_val) { $__L_idx = ($NS->collision_node_find_index)($__L_node->arr, $__L_node->cnt, $__L_key);
if (\Clojure\Php\equals($__L_idx, -1)) { $__L_new_arr = ($NS->object_array)((($NS->alength)($__L_node->arr) + 2));
System::arraycopy($__L_node->arr, 0, $__L_new_arr, 0, ($NS->alength)($__L_node->arr));
($NS->aset)($__L_new_arr, ($NS->alength)($__L_node->arr), $__L_key);
($NS->aset)($__L_new_arr, (($NS->alength)($__L_node->arr) + 1), $__L_val);
return ($NS->__GT_HashCollisionNode)($__L_hash, ($__L_node->cnt + 1), $__L_new_arr);} else { if (\Clojure\Php\equals($__L_val, ($NS->aget)($__L_node->arr, ((2 * $__L_idx) + 1)))) { return $__L_node;} else { return ($NS->__GT_HashCollisionNode)($__L_hash, $__L_node->cnt, ($NS->clone_and_set)($__L_node->arr, ((2 * $__L_idx) + 1), $__L_val));}}});
$NS->collision_node_find = (function($__L_node, $__L_key, $__L_not_found) { $__L_idx = ($NS->collision_node_find_index)($__L_node->arr, $__L_node->cnt, $__L_key);
if (\Clojure\Php\equals($__L_idx, -1)) { return $__L_not_found;} else { return ($NS->aget)($__L_node->arr, ((2 * $__L_idx) + 1));}});
$NS->node_assoc = (function($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_val) { if (($NS->instance_QMARK_)($NS->BitmapIndexedNode, $__L_node)) { return ($NS->bitmap_node_assoc)($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_val);} else { if (($NS->instance_QMARK_)($NS->HashCollisionNode, $__L_node)) { return ($NS->collision_node_assoc)($__L_node, $__L_hash, $__L_key, $__L_val);} else { if (\Clojure\Php\Kw::create('else')) { throw ($NS->ex_info)('Unknown node type', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('node'), $__L_node));
}}}});
$NS->node_find = (function($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_not_found) { if (($__L_node === null)) { return $__L_not_found;} else { if (($NS->instance_QMARK_)($NS->BitmapIndexedNode, $__L_node)) { return ($NS->bitmap_node_find)($__L_node, $__L_shift, $__L_hash, $__L_key, $__L_not_found);} else { if (($NS->instance_QMARK_)($NS->HashCollisionNode, $__L_node)) { return ($NS->collision_node_find)($__L_node, $__L_key, $__L_not_found);} else { if (\Clojure\Php\Kw::create('else')) { throw ($NS->ex_info)('Unknown node type', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('node'), $__L_node));
}}}}});
$NS->node_dissoc = (function($__L_node, $__L_shift, $__L_hash, $__L_key) { if (($__L_node === null)) { return null;} else { if (($NS->instance_QMARK_)($NS->BitmapIndexedNode, $__L_node)) { return ($NS->bitmap_node_dissoc)($__L_node, $__L_shift, $__L_hash, $__L_key);} else { if (\Clojure\Php\Kw::create('else')) { return $__L_node;}}}});
call_user_func('\Clojure\Php\defType', 'PersistentHashMap', array('cnt', 'root', 'has-nil', 'nil-val', '_meta', '_hash'), array(), array('-count', (function($__L__) { return $__L__->cnt;}), '-lookup', (function($__L__, $__L_k) { if (($__L_k === null)) { if ($__L__->has_nil) { return $__L__->nil_val;}} else { if (($__L__->root === null)) { return null;} else { return ($NS->node_find)($__L__->root, 0, ($NS->hash_code)($__L_k), $__L_k, null);}}}), '-lookup', (function($__L__, $__L_k, $__L_not_found) { if (($__L_k === null)) { if ($__L__->has_nil) { return $__L__->nil_val;} else { return $__L_not_found;}} else { if (($__L__->root === null)) { return $__L_not_found;} else { return ($NS->node_find)($__L__->root, 0, ($NS->hash_code)($__L_k), $__L_k, $__L_not_found);}}}), '-contains-key?', (function($__L_this, $__L_k) { return (!(($P->_lookup)($__L_this, $__L_k, \Clojure\Php\Kw::createNs('user', 'not-found')) === \Clojure\Php\Kw::createNs('user', 'not-found')));}), '-assoc', (function($__L_this, $__L_k, $__L_v) { if (($__L_k === null)) { if (($__L_this->has_nil ? ($__L_v === $__L_this->nil_val) : false)) { return $__L_this;} else { return ($NS->__GT_PersistentHashMap)(($__L_this->has_nil ? $__L_this->cnt : ($__L_this->cnt + 1)), $__L_this->root, true, $__L_v, $__L_this->_meta, null);}} else { $__L_new_root = ($NS->node_assoc)(((function() { $__L_or__3042 = $__L_this->root; if ($__L_or__3042) { return $__L_or__3042;} else { return $NS->EMPTY_BITMAP_NODE;} })()), 0, ($NS->hash_code)($__L_k), $__L_k, $__L_v);
if (($__L_new_root === $__L_this->root)) { return $__L_this;} else { return ($NS->__GT_PersistentHashMap)(($__L_this->cnt + 1), $__L_new_root, $__L_this->has_nil, $__L_this->nil_val, $__L_this->_meta, null);}}}), '-dissoc', (function($__L_this, $__L_k) { if (($__L_k === null)) { if ($__L_this->has_nil) { return ($NS->__GT_PersistentHashMap)(($__L_this->cnt - 1), $__L_this->root, false, null, $__L_this->_meta, null);} else { return $__L_this;}} else { if (($__L_this->root === null)) { return $__L_this;} else { $__L_new_root = ($NS->node_dissoc)($__L_this->root, 0, ($NS->hash_code)($__L_k), $__L_k);
if (($__L_new_root === $__L_this->root)) { return $__L_this;} else { return ($NS->__GT_PersistentHashMap)(($__L_this->cnt - 1), $__L_new_root, $__L_this->has_nil, $__L_this->nil_val, $__L_this->_meta, null);}}}}), '-conj', (function($__L_this, $__L_entry) { if (($__L_entry instanceof \Clojure\Php\Vec)) { return ($P->_assoc)($__L_this, \Clojure\Php\nth($__L_entry, 0), \Clojure\Php\nth($__L_entry, 1));} else { return ($NS->reduce)((function($__L_m, $__L_e) use (&$__L_this, &$__L_entry) { return ($P->_assoc)($__L_m, ($NS->key)($__L_e), ($NS->val)($__L_e));}), $__L_this, $__L_entry);}}), '-empty', (function($__L__) { return $NS->EMPTY_MAP;}), '-seq', (function($__L_this) { if (($__L_this->cnt > 0)) { return null;}}), '-equiv', (function($__L_this, $__L_other) { if (($NS->satisfies_QMARK_)($P->IMap, $__L_other)) { if (\Clojure\Php\equals($__L_this->cnt, ($P->_count)($__L_other))) { return ($NS->every_QMARK_)((function($__L___dest_6) use (&$__L_this, &$__L_other) { $__L___dest_7 = $__L___dest_6;
$__L_k = \Clojure\Php\nth($__L___dest_7, 0);
$__L_v = \Clojure\Php\nth($__L___dest_7, 1);
if (($P->_contains_key_QMARK_)($__L_other, $__L_k)) { return \Clojure\Php\equals($__L_v, ($P->_lookup)($__L_other, $__L_k));} else { return false;}}), $__L_this);} else { return false;}} else { return false;}}), '-hash', (function($__L_this) { if ($__L_this->_hash) { return $__L_this->_hash;} else { $__L_h = ($NS->reduce_kv)((function($__L_h, $__L_k, $__L_v) use (&$__L_this) { return ($__L_h + (($NS->hash)($__L_k) ^ ($NS->hash)($__L_v)));}), 0, $__L_this);
$__L_this->_hash = $__L_h;
return $__L_h;}}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_PersistentHashMap)($__L__->cnt, $__L__->root, $__L__->has_nil, $__L__->nil_val, $__L_m, $__L__->_hash);}), '-kv-reduce', (function($__L__, $__L_f, $__L_init) { return $__L_init;}), '-invoke', (function($__L_this, $__L_k) { return ($P->_lookup)($__L_this, $__L_k);}), '-invoke', (function($__L_this, $__L_k, $__L_not_found) { return ($P->_lookup)($__L_this, $__L_k, $__L_not_found);})));
$NS->__GT_PersistentHashMap = (function($__L_cnt, $__L_root, $__L_has_nil, $__L_nil_val, $__L__meta, $__L__hash) { return call_user_func('\Clojure\Php\createType', 'PersistentHashMap', $__L_cnt, $__L_root, $__L_has_nil, $__L_nil_val, $__L__meta, $__L__hash);});
$NS->EMPTY_MAP = ($NS->__GT_PersistentHashMap)(0, null, false, null, null, null);
$NS->hash_map = (function(...$__L_keyvals) {  while(true) { $__L_m = $NS->EMPTY_MAP;
$__L_kvs = \Clojure\Php\seq($__L_keyvals);
 while(true) { if ($__L_kvs) { if (\Clojure\Php\next_($__L_kvs)) { $__recur_0 = \Clojure\Php\assoc($__L_m, \Clojure\Php\first($__L_kvs), \Clojure\Php\second($__L_kvs)); $__recur_1 = ($NS->nnext)($__L_kvs); $__L_m = $__recur_0; $__L_kvs = $__recur_1; continue;} else { throw ($NS->ex_info)('hash-map requires even number of arguments', \Clojure\Php\hashMap());
}} else { return $__L_m;} break; }
 break; }});
$NS->map_QMARK_ = (function($__L_x) { return ($NS->satisfies_QMARK_)($P->IMap, $__L_x);});
call_user_func('\Clojure\Php\defType', 'MapEntry', array('k', 'v'), array(), array('-key', (function($__L__) { return $__L__->k;}), '-val', (function($__L__) { return $__L__->v;}), '-nth', (function($__L__, $__L_n) { return ($NS->case)($__L_n, 0, $__L__->k, 1, $__L__->v, (throw ($NS->ex_info)('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n))));}), '-nth', (function($__L__, $__L_n, $__L_not_found) { return ($NS->case)($__L_n, 0, $__L__->k, 1, $__L__->v, $__L_not_found);}), '-count', (function($__L__) { return 2;}), '-equiv', (function($__L__, $__L_other) { if (($NS->satisfies_QMARK_)($P->IMapEntry, $__L_other)) { if (\Clojure\Php\equals($__L__->k, ($P->_key)($__L_other))) { return \Clojure\Php\equals($__L__->v, ($P->_val)($__L_other));} else { return false;}} else { return false;}})));
$NS->__GT_MapEntry = (function($__L_k, $__L_v) { return call_user_func('\Clojure\Php\createType', 'MapEntry', $__L_k, $__L_v);});
$NS->key = (function($__L_e) { return ($P->_key)($__L_e);});
$NS->val = (function($__L_e) { return ($P->_val)($__L_e);});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.PersistentHashSet'), 'Persistent Hash Set - matches JVM clojure.lang.PersistentHashSet API.

   Hash Array Mapped Trie (HAMT) implementation providing:
   - O(log32 n) lookup, insert, and delete
   - Structural sharing for efficiency', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')], [\Clojure\Php\Sym::create('clojure.lang.PersistentHashMap'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('m')]));
$NS->EMPTY_SET = null;
call_user_func('\Clojure\Php\defType', 'PersistentHashSet', array('impl', '_meta', '_hash'), array(), array('-count', (function($__L__) { return ($P->_count)($__L__->impl);}), '-lookup', (function($__L__, $__L_k) { return ($P->_lookup)($__L__->impl, $__L_k);}), '-lookup', (function($__L__, $__L_k, $__L_not_found) { return ($P->_lookup)($__L__->impl, $__L_k, $__L_not_found);}), '-contains?', (function($__L__, $__L_k) { return ($P->_contains_key_QMARK_)($__L__->impl, $__L_k);}), '-disjoin', (function($__L_this, $__L_k) { if (($P->_contains_key_QMARK_)($__L_this->impl, $__L_k)) { return ($NS->__GT_PersistentHashSet)(($P->_dissoc)($__L_this->impl, $__L_k), $__L_this->_meta, null);} else { return $__L_this;}}), '-conj', (function($__L_this, $__L_v) { if (($P->_contains_key_QMARK_)($__L_this->impl, $__L_v)) { return $__L_this;} else { return ($NS->__GT_PersistentHashSet)(($P->_assoc)($__L_this->impl, $__L_v, $__L_v), $__L_this->_meta, null);}}), '-empty', (function($__L__) { return $NS->EMPTY_SET;}), '-seq', (function($__L__) { if ((($P->_count)($__L__->impl) > 0)) { return ($NS->map)($NS->key, ($P->_seq)($__L__->impl));}}), '-equiv', (function($__L_this, $__L_other) { if (($NS->satisfies_QMARK_)($P->ISet, $__L_other)) { if (\Clojure\Php\equals(($P->_count)($__L_this), ($P->_count)($__L_other))) { return ($NS->every_QMARK_)((function($__L_p1__3043_SHARP_) use (&$__L_this, &$__L_other) { return ($P->_contains_QMARK_)($__L_other, $__L_p1__3043_SHARP_);}), $__L_this);} else { return false;}} else { return false;}}), '-hash', (function($__L_this) { if ($__L_this->_hash) { return $__L_this->_hash;} else { $__L_h = ($NS->reduce)((function($__L_h, $__L_x) use (&$__L_this) { return ($__L_h + ($NS->hash)($__L_x));}), 0, $__L_this);
$__L_this->_hash = $__L_h;
return $__L_h;}}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { return ($NS->__GT_PersistentHashSet)($__L__->impl, $__L_m, $__L__->_hash);}), '-reduce', (function($__L_this, $__L_f) { $__L_s = ($P->_seq)($__L_this);
if ($__L_s) { return ($NS->reduce)($__L_f, \Clojure\Php\first($__L_s), \Clojure\Php\next_($__L_s));} else { return call_user_func($__L_f);}}), '-reduce-init', (function($__L_this, $__L_f, $__L_init) { return ($NS->reduce)($__L_f, $__L_init, ($P->_seq)($__L_this));}), '-invoke', (function($__L_this, $__L_k) { if (($P->_contains_QMARK_)($__L_this, $__L_k)) { return $__L_k;}}), '-invoke', (function($__L_this, $__L_k, $__L_not_found) { if (($P->_contains_QMARK_)($__L_this, $__L_k)) { return $__L_k;} else { return $__L_not_found;}})));
$NS->__GT_PersistentHashSet = (function($__L_impl, $__L__meta, $__L__hash) { return call_user_func('\Clojure\Php\createType', 'PersistentHashSet', $__L_impl, $__L__meta, $__L__hash);});
$NS->EMPTY_SET = ($NS->__GT_PersistentHashSet)($M->EMPTY_MAP, null, null);
$NS->hash_set = (function(...$__L_keys) { return ($NS->reduce)($NS->conj, $NS->EMPTY_SET, $__L_keys);});
$NS->set = (function($__L_coll) { return ($NS->reduce)($NS->conj, $NS->EMPTY_SET, $__L_coll);});
$NS->set_QMARK_ = (function($__L_x) { return ($NS->satisfies_QMARK_)($P->ISet, $__L_x);});
$NS->union = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_s1 = $__args[0]; $__L_s2 = $__args[1]; if ((\Clojure\Php\count_($__L_s1) < \Clojure\Php\count_($__L_s2))) { return ($NS->reduce)($NS->conj, $__L_s2, $__L_s1);} else { return ($NS->reduce)($NS->conj, $__L_s1, $__L_s2);} } else if ($__n == 1) { $__L_s1 = $__args[0]; return $__L_s1; } else if ($__n == 0) { return $NS->EMPTY_SET; } else if ($__n >= 2) { $__L_s1 = $__args[0]; $__L_s2 = $__args[1]; $__L_sets = array_slice($__args, 2); $__L_bubbled_up = ($NS->reduce)((function($__L_ss, $__L_s) use (&$__L_s1, &$__L_sets, &$__L_s2) { return \Clojure\Php\conj(($NS->pop)($__L_ss), ($NS->union)(($NS->peek)($__L_ss), $__L_s));}), ($NS->list)($__L_s1, $__L_s2), $__L_sets);
return ($NS->peek)($__L_bubbled_up); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->intersection = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_s1 = $__args[0]; $__L_s2 = $__args[1];  while(true) { if ((\Clojure\Php\count_($__L_s2) < \Clojure\Php\count_($__L_s1))) { $__recur_0 = $__L_s2; $__recur_1 = $__L_s1; $__L_s1 = $__recur_0; $__L_s2 = $__recur_1; continue;} else { return ($NS->reduce)((function($__L_result, $__L_item) use (&$__L_s1, &$__L_s2) { if (\Clojure\Php\contains($__L_s2, $__L_item)) { return $__L_result;} else { return ($NS->disj)($__L_result, $__L_item);}}), $__L_s1, $__L_s1);} break; } } else if ($__n == 1) { $__L_s1 = $__args[0]; return $__L_s1; } else if ($__n >= 2) { $__L_s1 = $__args[0]; $__L_s2 = $__args[1]; $__L_sets = array_slice($__args, 2); $__L_bubbled_up = ($NS->reduce)((function($__L_ss, $__L_s) use (&$__L_s1, &$__L_sets, &$__L_s2) { return \Clojure\Php\conj(($NS->pop)($__L_ss), ($NS->intersection)(($NS->peek)($__L_ss), $__L_s));}), ($NS->list)($__L_s1, $__L_s2), $__L_sets);
return ($NS->peek)($__L_bubbled_up); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->difference = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_s1 = $__args[0]; $__L_s2 = $__args[1]; if ((\Clojure\Php\count_($__L_s1) < \Clojure\Php\count_($__L_s2))) { return ($NS->reduce)((function($__L_result, $__L_item) use (&$__L_s1, &$__L_s2) { if (\Clojure\Php\contains($__L_s2, $__L_item)) { return ($NS->disj)($__L_result, $__L_item);} else { return $__L_result;}}), $__L_s1, $__L_s1);} else { return ($NS->reduce)($NS->disj, $__L_s1, $__L_s2);} } else if ($__n == 1) { $__L_s1 = $__args[0]; return $__L_s1; } else if ($__n >= 2) { $__L_s1 = $__args[0]; $__L_s2 = $__args[1]; $__L_sets = array_slice($__args, 2); return ($NS->reduce)($NS->difference, $__L_s1, \Clojure\Php\conj($__L_sets, $__L_s2)); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->subset_QMARK_ = (function($__L_s1, $__L_s2) { if ((\Clojure\Php\count_($__L_s1) <= \Clojure\Php\count_($__L_s2))) { return ($NS->every_QMARK_)((function($__L_p1__3044_SHARP_) use (&$__L_s1, &$__L_s2) { return \Clojure\Php\contains($__L_s2, $__L_p1__3044_SHARP_);}), $__L_s1);} else { return false;}});
$NS->superset_QMARK_ = (function($__L_s1, $__L_s2) { return ($NS->subset_QMARK_)($__L_s2, $__L_s1);});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.transient'), 'Transient collections for efficient batch operations.

   Transients provide O(1) mutability during construction,
   then become persistent with persistent!.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$NS->BITS = 5;
$NS->WIDTH = 32;
$NS->MASK = 31;
$NS->EMPTY_TRANSIENT_VECTOR = null;
call_user_func('\Clojure\Php\defType', 'TransientVector', array('cnt', 'shift', 'root', 'tail', 'edit'), array('cnt', 'shift', 'root', 'tail', 'edit'), array('-conj!', (function($__L_this, $__L_val) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
if ((($__L_this->cnt - ($NS->tail_off_tv)($__L_this)) < $NS->WIDTH)) { ($NS->aset)($__L_this->tail, ($__L_this->cnt & $NS->MASK), $__L_val);
$__L_this->cnt = ($__L_this->cnt + 1);
return $__L_this;} else { $__L_new_tail = ($NS->object_array)($NS->WIDTH);
($NS->aset)($__L_new_tail, 0, $__L_val);
$__L_this->root = ($NS->push_tail_tv)($__L_this, $__L_this->shift, $__L_this->root, $__L_this->tail);
if ((($NS->unsigned_bit_shift_right)($__L_this->cnt, $NS->BITS) > (1 << $__L_this->shift))) { $__L_new_root = ($NS->object_array)($NS->WIDTH);
($NS->aset)($__L_new_root, 0, $__L_this->root);
($NS->aset)($__L_new_root, 1, ($NS->new_path_tv)($__L_this->edit, $__L_this->shift, $__L_this->tail));
$__L_this->root = $__L_new_root;
$__L_this->shift = ($__L_this->shift + $NS->BITS);
}
$__L_this->tail = $__L_new_tail;
$__L_this->cnt = ($__L_this->cnt + 1);
return $__L_this;}}), '-persistent!', (function($__L_this) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_this->edit = null;
$__L_trimmed_tail = ($NS->object_array)(($__L_this->cnt - ($NS->tail_off_tv)($__L_this)));
System::arraycopy($__L_this->tail, 0, $__L_trimmed_tail, 0, ($NS->alength)($__L_trimmed_tail));
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('persistent-vector'), \Clojure\Php\Kw::create('cnt'), $__L_this->cnt, \Clojure\Php\Kw::create('shift'), $__L_this->shift, \Clojure\Php\Kw::create('root'), $__L_this->root, \Clojure\Php\Kw::create('tail'), $__L_trimmed_tail);}), '-assoc!', (function($__L_this, $__L_k, $__L_v) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_i = ((int)$__L_k);
if ((($__L_i >= 0) ? ($__L_i < $__L_this->cnt) : false)) { if (($__L_i >= ($NS->tail_off_tv)($__L_this))) { ($NS->aset)($__L_this->tail, ($__L_i & $NS->MASK), $__L_v);
return $__L_this;} else { $__L_this->root = ($NS->do_assoc_tv)($__L_this, $__L_this->shift, $__L_this->root, $__L_i, $__L_v);
return $__L_this;}} else { if (\Clojure\Php\equals($__L_i, $__L_this->cnt)) { return ($P->_conj_BANG_)($__L_this, $__L_v);} else { if (\Clojure\Php\Kw::create('else')) { throw ($NS->ex_info)('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('index'), $__L_i, \Clojure\Php\Kw::create('count'), $__L_this->cnt));
}}}}), '-assoc-n!', (function($__L_this, $__L_i, $__L_val) { return ($P->_assoc_BANG_)($__L_this, $__L_i, $__L_val);}), '-pop!', (function($__L_this) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
if (($__L_this->cnt === 0 || $__L_this->cnt === 0.0)) { throw ($NS->ex_info)('Can\'t pop empty vector', \Clojure\Php\hashMap());
} else { if (\Clojure\Php\equals(1, $__L_this->cnt)) { $__L_this->cnt = 0;
return $__L_this;} else { if ((($__L_this->cnt - ($NS->tail_off_tv)($__L_this)) > 1)) { $__L_this->cnt = ($__L_this->cnt - 1);
return $__L_this;} else { if (\Clojure\Php\Kw::create('else')) { $__L_new_tail = ($NS->editable_array_for_tv)($__L_this, ($__L_this->cnt - 2));
$__L_this->root = ($NS->pop_tail_tv)($__L_this, $__L_this->shift, $__L_this->root);
if ((($__L_this->shift > $NS->BITS) ? (($NS->aget)($__L_this->root, 1) === null) : false)) { $__L_this->root = ($NS->aget)($__L_this->root, 0);
$__L_this->shift = ($__L_this->shift - $NS->BITS);
}
$__L_this->tail = $__L_new_tail;
$__L_this->cnt = ($__L_this->cnt - 1);
return $__L_this;}}}}}), '-count', (function($__L__) { return $__L__->cnt;}), '-nth', (function($__L_this, $__L_n) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
if ((($__L_n >= 0) ? ($__L_n < $__L_this->cnt) : false)) { return ($NS->aget)(($NS->array_for_tv)($__L_this, $__L_n), ($__L_n & $NS->MASK));} else { throw ($NS->ex_info)('Index out of bounds', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('n'), $__L_n, \Clojure\Php\Kw::create('count'), $__L_this->cnt));
}}), '-nth', (function($__L_this, $__L_n, $__L_not_found) { if ((($__L_n >= 0) ? ($__L_n < $__L_this->cnt) : false)) { return ($NS->aget)(($NS->array_for_tv)($__L_this, $__L_n), ($__L_n & $NS->MASK));} else { return $__L_not_found;}}), '-lookup', (function($__L_this, $__L_k) { if ((is_int($__L_k) || is_float($__L_k))) { return ($P->_nth)($__L_this, $__L_k, null);}}), '-lookup', (function($__L_this, $__L_k, $__L_not_found) { if ((is_int($__L_k) || is_float($__L_k))) { return ($P->_nth)($__L_this, $__L_k, $__L_not_found);} else { return $__L_not_found;}})));
$NS->__GT_TransientVector = (function($__L_cnt, $__L_shift, $__L_root, $__L_tail, $__L_edit) { return call_user_func('\Clojure\Php\createType', 'TransientVector', $__L_cnt, $__L_shift, $__L_root, $__L_tail, $__L_edit);});
$NS->tail_off_tv = (function($__L_tv) { $__L_cnt = $__L_tv->cnt;
if (($__L_cnt < $NS->WIDTH)) { return 0;} else { return (($NS->unsigned_bit_shift_right)(($__L_cnt - 1), $NS->BITS) << $NS->BITS);}});
$NS->new_path_tv = (function($__L_edit, $__L_level, $__L_node) { if (($__L_level === 0 || $__L_level === 0.0)) { return $__L_node;} else { $__L_ret = ($NS->object_array)($NS->WIDTH);
($NS->aset)($__L_ret, 0, ($NS->new_path_tv)($__L_edit, ($__L_level - $NS->BITS), $__L_node));
return $__L_ret;}});
$NS->push_tail_tv = (function($__L_tv, $__L_level, $__L_parent, $__L_tail_node) { $__L_subidx = (($NS->unsigned_bit_shift_right)(($__L_tv->cnt - 1), $__L_level) & $NS->MASK);
$__L_ret = ($NS->aclone)($__L_parent);
if (\Clojure\Php\equals($__L_level, $NS->BITS)) { ($NS->aset)($__L_ret, $__L_subidx, $__L_tail_node);
} else { $__L_child = ($NS->aget)($__L_parent, $__L_subidx);
($NS->aset)($__L_ret, $__L_subidx, ($__L_child ? ($NS->push_tail_tv)($__L_tv, ($__L_level - $NS->BITS), $__L_child, $__L_tail_node) : ($NS->new_path_tv)($__L_tv->edit, ($__L_level - $NS->BITS), $__L_tail_node)));
}
return $__L_ret;});
$NS->do_assoc_tv = (function($__L_tv, $__L_level, $__L_node, $__L_i, $__L_val) { $__L_ret = ($NS->aclone)($__L_node);
if (($__L_level === 0 || $__L_level === 0.0)) { ($NS->aset)($__L_ret, ($__L_i & $NS->MASK), $__L_val);
return $__L_ret;} else { $__L_subidx = (($NS->unsigned_bit_shift_right)($__L_i, $__L_level) & $NS->MASK);
($NS->aset)($__L_ret, $__L_subidx, ($NS->do_assoc_tv)($__L_tv, ($__L_level - $NS->BITS), ($NS->aget)($__L_node, $__L_subidx), $__L_i, $__L_val));
return $__L_ret;}});
$NS->array_for_tv = (function($__L_tv, $__L_i) {  while(true) { if (($__L_i >= ($NS->tail_off_tv)($__L_tv))) { return $__L_tv->tail;} else { $__L_node = $__L_tv->root;
$__L_level = $__L_tv->shift;
 while(true) { if (($__L_level > 0)) { $__recur_0 = ($NS->aget)($__L_node, (($NS->unsigned_bit_shift_right)($__L_i, $__L_level) & $NS->MASK)); $__recur_1 = ($__L_level - $NS->BITS); $__L_node = $__recur_0; $__L_level = $__recur_1; continue;} else { return $__L_node;} break; }
} break; }});
$NS->editable_array_for_tv = (function($__L_tv, $__L_i) { return ($NS->array_for_tv)($__L_tv, $__L_i);});
$NS->pop_tail_tv = (function($__L_tv, $__L_level, $__L_node) { $__L_subidx = (($NS->unsigned_bit_shift_right)(($__L_tv->cnt - 2), $__L_level) & $NS->MASK);
if (($__L_level > $NS->BITS)) { $__L_new_child = ($NS->pop_tail_tv)($__L_tv, ($__L_level - $NS->BITS), ($NS->aget)($__L_node, $__L_subidx));
if ((($__L_new_child === null) ? ($__L_subidx === 0 || $__L_subidx === 0.0) : false)) { return null;} else { $__L_ret = ($NS->aclone)($__L_node);
($NS->aset)($__L_ret, $__L_subidx, $__L_new_child);
return $__L_ret;}} else { if (($__L_subidx === 0 || $__L_subidx === 0.0)) { return null;} else { if (\Clojure\Php\Kw::create('else')) { $__L_ret = ($NS->aclone)($__L_node);
($NS->aset)($__L_ret, $__L_subidx, null);
return $__L_ret;}}}});
call_user_func('\Clojure\Php\defType', 'TransientHashMap', array('cnt', 'root', 'has-nil', 'nil-val', 'edit'), array('cnt', 'root', 'has-nil', 'nil-val', 'edit'), array('-conj!', (function($__L_this, $__L_entry) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
if (($__L_entry instanceof \Clojure\Php\Vec)) { return ($P->_assoc_BANG_)($__L_this, \Clojure\Php\nth($__L_entry, 0), \Clojure\Php\nth($__L_entry, 1));} else { return ($NS->reduce)((function($__L_m, $__L_e) use (&$__L_this, &$__L_entry) { return ($P->_assoc_BANG_)($__L_m, ($NS->key)($__L_e), ($NS->val)($__L_e));}), $__L_this, $__L_entry);}}), '-persistent!', (function($__L_this) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_this->edit = null;
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('persistent-hash-map'), \Clojure\Php\Kw::create('cnt'), $__L_this->cnt, \Clojure\Php\Kw::create('root'), $__L_this->root, \Clojure\Php\Kw::create('has-nil'), $__L_this->has_nil, \Clojure\Php\Kw::create('nil-val'), $__L_this->nil_val);}), '-assoc!', (function($__L_this, $__L_k, $__L_v) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
if (($__L_k === null)) { if ($__L_this->has_nil) { null;
} else { $__L_this->cnt = ($__L_this->cnt + 1);
}
$__L_this->has_nil = true;
$__L_this->nil_val = $__L_v;
return $__L_this;} else { if (($NS->contains_key_QMARK_)($__L_this, $__L_k)) { null;
} else { $__L_this->cnt = ($__L_this->cnt + 1);
}
$__L_this->root = ($NS->assoc_node)($__L_this->root, $__L_k, $__L_v);
return $__L_this;}}), '-dissoc!', (function($__L_this, $__L_k) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
if (($__L_k === null)) { if ($__L_this->has_nil) { $__L_this->has_nil = false;
$__L_this->nil_val = null;
$__L_this->cnt = ($__L_this->cnt - 1);
return $__L_this;} else { return $__L_this;}} else { if (($NS->contains_key_QMARK_)($__L_this, $__L_k)) { $__L_this->cnt = ($__L_this->cnt - 1);
return $__L_this->root = ($NS->dissoc_node)($__L_this->root, $__L_k);}}}), '-count', (function($__L__) { return $__L__->cnt;}), '-lookup', (function($__L__, $__L_k) { if (($__L_k === null)) { if ($__L__->has_nil) { return $__L__->nil_val;}} else { return ($NS->get_node)($__L__->root, $__L_k);}}), '-lookup', (function($__L__, $__L_k, $__L_not_found) { if (($__L_k === null)) { if ($__L__->has_nil) { return $__L__->nil_val;} else { return $__L_not_found;}} else { return ($NS->get_node)($__L__->root, $__L_k, $__L_not_found);}})));
$NS->__GT_TransientHashMap = (function($__L_cnt, $__L_root, $__L_has_nil, $__L_nil_val, $__L_edit) { return call_user_func('\Clojure\Php\createType', 'TransientHashMap', $__L_cnt, $__L_root, $__L_has_nil, $__L_nil_val, $__L_edit);});
$NS->contains_key_QMARK_ = (function($__L_tm, $__L_k) { return (!(($P->_lookup)($__L_tm, $__L_k, \Clojure\Php\Kw::createNs('user', 'not-found')) === \Clojure\Php\Kw::createNs('user', 'not-found')));});
$NS->assoc_node = (function($__L_root, $__L_k, $__L_v) { return \Clojure\Php\assoc(((function() { $__L_or__3045 = $__L_root; if ($__L_or__3045) { return $__L_or__3045;} else { return \Clojure\Php\hashMap();} })()), $__L_k, $__L_v);});
$NS->dissoc_node = (function($__L_root, $__L_k) { return \Clojure\Php\dissoc($__L_root, $__L_k);});
$NS->get_node = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_root = $__args[0]; $__L_k = $__args[1]; $__L_not_found = $__args[2]; return \Clojure\Php\get_($__L_root, $__L_k, $__L_not_found); } else if ($__n == 2) { $__L_root = $__args[0]; $__L_k = $__args[1]; return \Clojure\Php\get_($__L_root, $__L_k); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
call_user_func('\Clojure\Php\defType', 'TransientHashSet', array('impl', 'edit'), array('impl', 'edit'), array('-conj!', (function($__L_this, $__L_v) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_this->impl = ($P->_assoc_BANG_)($__L_this->impl, $__L_v, $__L_v);
return $__L_this;}), '-persistent!', (function($__L_this) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_this->edit = null;
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('persistent-hash-set'), \Clojure\Php\Kw::create('impl'), ($P->_persistent_BANG_)($__L_this->impl));}), '-disjoin!', (function($__L_this, $__L_k) { if ($__L_this->edit) { null;
} else { throw ($NS->ex_info)('Transient used after persistent!', \Clojure\Php\hashMap());
}
$__L_this->impl = ($P->_dissoc_BANG_)($__L_this->impl, $__L_k);
return $__L_this;}), '-count', (function($__L__) { return ($P->_count)($__L__->impl);}), '-lookup', (function($__L__, $__L_k) { return ($P->_lookup)($__L__->impl, $__L_k);}), '-lookup', (function($__L__, $__L_k, $__L_not_found) { return ($P->_lookup)($__L__->impl, $__L_k, $__L_not_found);})));
$NS->__GT_TransientHashSet = (function($__L_impl, $__L_edit) { return call_user_func('\Clojure\Php\createType', 'TransientHashSet', $__L_impl, $__L_edit);});
$NS->transient_vector = (function($__L_pv) { return ($NS->__GT_TransientVector)(\Clojure\Php\count_($__L_pv), $NS->BITS, ($NS->object_array)($NS->WIDTH), ($NS->object_array)($NS->WIDTH), ($NS->object_array)(1));});
$NS->transient_hash_map = (function($__L_pm) { return ($NS->__GT_TransientHashMap)(\Clojure\Php\count_($__L_pm), ($NS->into)(\Clojure\Php\hashMap(), $__L_pm), false, null, ($NS->object_array)(1));});
$NS->transient_hash_set = (function($__L_ps) { $__L_tm = ($NS->transient_hash_map)(($NS->into)(\Clojure\Php\hashMap(), ($NS->map)((function($__L_x) use (&$__L_ps) { return \Clojure\Php\vec($__L_x, $__L_x);}), $__L_ps)));
return ($NS->__GT_TransientHashSet)($__L_tm, ($NS->object_array)(1));});
$NS->transient = (function($__L_coll) { if (($__L_coll instanceof \Clojure\Php\Vec)) { return ($NS->transient_vector)($__L_coll);} else { if (($__L_coll instanceof \Clojure\Php\Map)) { return ($NS->transient_hash_map)($__L_coll);} else { if (($__L_coll instanceof \Clojure\Php\Set)) { return ($NS->transient_hash_set)($__L_coll);} else { if (\Clojure\Php\Kw::create('else')) { throw ($NS->ex_info)('Can\'t create transient from this type', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), ($NS->type)($__L_coll)));
}}}}});
$NS->persistent_BANG_ = (function($__L_tcoll) { return ($P->_persistent_BANG_)($__L_tcoll);});
$NS->conj_BANG_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_tcoll = $__args[0]; $__L_x = $__args[1]; return ($P->_conj_BANG_)($__L_tcoll, $__L_x); } else if ($__n == 1) { $__L_tcoll = $__args[0]; return $__L_tcoll; } else if ($__n >= 2) { $__L_tcoll = $__args[0]; $__L_x = $__args[1]; $__L_xs = array_slice($__args, 2);  while(true) { $__L_ret = ($NS->conj_BANG_)($__L_tcoll, $__L_x);
if ($__L_xs) { $__recur_0 = $__L_ret; $__recur_1 = \Clojure\Php\first($__L_xs); $__recur_2 = \Clojure\Php\next_($__L_xs); $__L_tcoll = $__recur_0; $__L_x = $__recur_1; $__L_xs = $__recur_2; continue;} else { return $__L_ret;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->assoc_BANG_ = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_tcoll = $__args[0]; $__L_k = $__args[1]; $__L_v = $__args[2]; return ($P->_assoc_BANG_)($__L_tcoll, $__L_k, $__L_v); } else if ($__n >= 3) { $__L_tcoll = $__args[0]; $__L_k = $__args[1]; $__L_v = $__args[2]; $__L_kvs = array_slice($__args, 3);  while(true) { $__L_ret = ($NS->assoc_BANG_)($__L_tcoll, $__L_k, $__L_v);
if ($__L_kvs) { if (\Clojure\Php\next_($__L_kvs)) { $__recur_0 = $__L_ret; $__recur_1 = \Clojure\Php\first($__L_kvs); $__recur_2 = \Clojure\Php\second($__L_kvs); $__recur_3 = ($NS->nnext)($__L_kvs); $__L_tcoll = $__recur_0; $__L_k = $__recur_1; $__L_v = $__recur_2; $__L_kvs = $__recur_3; continue;} else { throw ($NS->ex_info)('assoc! expects even number of arguments', \Clojure\Php\hashMap());
}} else { return $__L_ret;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->dissoc_BANG_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_tcoll = $__args[0]; $__L_k = $__args[1]; return ($P->_dissoc_BANG_)($__L_tcoll, $__L_k); } else if ($__n >= 2) { $__L_tcoll = $__args[0]; $__L_k = $__args[1]; $__L_ks = array_slice($__args, 2);  while(true) { $__L_ret = ($NS->dissoc_BANG_)($__L_tcoll, $__L_k);
if ($__L_ks) { $__recur_0 = $__L_ret; $__recur_1 = \Clojure\Php\first($__L_ks); $__recur_2 = \Clojure\Php\next_($__L_ks); $__L_tcoll = $__recur_0; $__L_k = $__recur_1; $__L_ks = $__recur_2; continue;} else { return $__L_ret;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->pop_BANG_ = (function($__L_tcoll) { return ($P->_pop_BANG_)($__L_tcoll);});
$NS->disj_BANG_ = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_tset = $__args[0]; $__L_k = $__args[1]; return ($P->_disjoin_BANG_)($__L_tset, $__L_k); } else if ($__n >= 2) { $__L_tset = $__args[0]; $__L_k = $__args[1]; $__L_ks = array_slice($__args, 2);  while(true) { $__L_ret = ($NS->disj_BANG_)($__L_tset, $__L_k);
if ($__L_ks) { $__recur_0 = $__L_ret; $__recur_1 = \Clojure\Php\first($__L_ks); $__recur_2 = \Clojure\Php\next_($__L_ks); $__L_tset = $__recur_0; $__L_k = $__recur_1; $__L_ks = $__recur_2; continue;} else { return $__L_ret;} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.string'), 'String operations using platform primitives.

   These functions work with strings on any platform.
   The compiler emits platform-appropriate string operations.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$NS->blank_QMARK_ = (function($__L_s) { $__L_or__3047 = ($__L_s === null);
if ($__L_or__3047) { return $__L_or__3047;} else { $__L_or__3048 = \Clojure\Php\equals('', $__L_s);
if ($__L_or__3048) { return $__L_or__3048;} else { return ($NS->every_QMARK_)((function($__L_p1__3046_SHARP_) use (&$__L_or__3048, &$__L_or__3047, &$__L_s) { return Character::isWhitespace($__L_p1__3046_SHARP_);}), $__L_s);}}});
$NS->join = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_separator = $__args[0]; $__L_coll = $__args[1];  while(true) { $__L_sb = (new StringBuilder());
$__L_first_QMARK_ = true;
$__L_coll = \Clojure\Php\seq($__L_coll);
 while(true) { if ($__L_coll) { if ($__L_first_QMARK_) { null;
} else { $__L_sb->append($__L_separator);
}
$__L_sb->append(((string)\Clojure\Php\first($__L_coll)));
$__recur_0 = $__L_sb; $__recur_1 = false; $__recur_2 = \Clojure\Php\next_($__L_coll); $__L_sb = $__recur_0; $__L_first_QMARK_ = $__recur_1; $__L_coll = $__recur_2; continue;} else { return ((string)$__L_sb);} break; }
 break; } } else if ($__n == 1) { $__L_coll = $__args[0]; return ($NS->apply)($NS->str, $__L_coll); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->split = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_s = $__args[0]; $__L_re = $__args[1]; $__L_limit = $__args[2]; return \Vec::create($__L_s->split(((string)$__L_re), $__L_limit)); } else if ($__n == 2) { $__L_s = $__args[0]; $__L_re = $__args[1]; return \Vec::create($__L_s->split(((string)$__L_re))); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->split_lines = (function($__L_s) { return ($NS->split)($__L_s, '\r?\n');});
$NS->trim = (function($__L_s) { return $__L_s->trim();});
$NS->triml = (function($__L_s) { return $__L_s->replaceFirst('^\s+', '');});
$NS->trimr = (function($__L_s) { return $__L_s->replaceFirst('\s+$', '');});
$NS->trim_newline = (function($__L_s) {  while(true) { $__L_i = \Clojure\Php\count_($__L_s);
 while(true) { if (($__L_i === 0 || $__L_i === 0.0)) { return '';} else { $__L_ch = \Clojure\Php\nth($__L_s, ($__L_i - 1));
if (((function() { $__L_or__3049 = \Clojure\Php\equals($__L_ch, \newline); if ($__L_or__3049) { return $__L_or__3049;} else { return \Clojure\Php\equals($__L_ch, \return);} })())) { $__recur_0 = ($__L_i - 1); $__L_i = $__recur_0; continue;} else { return ($NS->subs)($__L_s, 0, $__L_i);}} break; }
 break; }});
$NS->upper_case = (function($__L_s) { return $__L_s->toUpperCase();});
$NS->lower_case = (function($__L_s) { return $__L_s->toLowerCase();});
$NS->capitalize = (function($__L_s) { if ((\Clojure\Php\count_($__L_s) < 2)) { return ($NS->upper_case)($__L_s);} else { return \Clojure\Php\str_(($NS->upper_case)(($NS->subs)($__L_s, 0, 1)), ($NS->lower_case)(($NS->subs)($__L_s, 1)));}});
$NS->starts_with_QMARK_ = (function($__L_s, $__L_substr) { return $__L_s->startsWith($__L_substr);});
$NS->ends_with_QMARK_ = (function($__L_s, $__L_substr) { return $__L_s->endsWith($__L_substr);});
$NS->includes_QMARK_ = (function($__L_s, $__L_substr) { return $__L_s->contains($__L_substr);});
$NS->index_of = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_s = $__args[0]; $__L_value = $__args[1]; $__L_from_index = $__args[2]; $__L_result = $__L_s->indexOf($__L_value, $__L_from_index);
if (($__L_result >= 0)) { return $__L_result;} } else if ($__n == 2) { $__L_s = $__args[0]; $__L_value = $__args[1]; $__L_result = $__L_s->indexOf($__L_value);
if (($__L_result >= 0)) { return $__L_result;} } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->last_index_of = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_s = $__args[0]; $__L_value = $__args[1]; $__L_from_index = $__args[2]; $__L_result = $__L_s->lastIndexOf($__L_value, $__L_from_index);
if (($__L_result >= 0)) { return $__L_result;} } else if ($__n == 2) { $__L_s = $__args[0]; $__L_value = $__args[1]; $__L_result = $__L_s->lastIndexOf($__L_value);
if (($__L_result >= 0)) { return $__L_result;} } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->replace = (function($__L_s, $__L_match, $__L_replacement) { if (is_string($__L_match)) { return $__L_s->replace($__L_match, $__L_replacement);} else { return $__L_s->replaceAll(((string)$__L_match), $__L_replacement);}});
$NS->replace_first = (function($__L_s, $__L_match, $__L_replacement) { if (is_string($__L_match)) { return $__L_s->replaceFirst(java.util.regex.Pattern::quote($__L_match), $__L_replacement);} else { return $__L_s->replaceFirst(((string)$__L_match), $__L_replacement);}});
$NS->subs = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_s = $__args[0]; $__L_start = $__args[1]; $__L_end = $__args[2]; return $__L_s->substring($__L_start, $__L_end); } else if ($__n == 2) { $__L_s = $__args[0]; $__L_start = $__args[1]; return $__L_s->substring($__L_start); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->reverse = (function($__L_s) { return \Clojure\Php\str_((new StringBuilder($__L_s)), $NS->_DOT_reverse);});
$NS->escape = (function($__L_s, $__L_cmap) {  while(true) { $__L_sb = (new StringBuilder(\Clojure\Php\count_($__L_s)));
$__L_s3050 = \Clojure\Php\seq($__L_s);
 while(true) { if ($__L_s3050) { $__L_ch = \Clojure\Php\first($__L_s3050);
$__L_if_let__3051 = \Clojure\Php\get_($__L_cmap, $__L_ch);
if ($__L_if_let__3051) { $__L_replacement = $__L_if_let__3051;
$__L_sb->append($__L_replacement);
} else { $__L_sb->append($__L_ch);
}
$__recur_0 = \Clojure\Php\next_($__L_s3050); $__L_s3050 = $__recur_0; continue;}
 break; }
return ((string)$__L_sb); break; }});
$NS->re_quote_replacement = (function($__L_replacement) { return java.util.regex.Matcher::quoteReplacement($__L_replacement);});
$NS->re_pattern = (function($__L_s) { return java.util.regex.Pattern::compile($__L_s);});
$NS->re_matcher = (function($__L_re, $__L_s) { return $__L_re->matcher($__L_s);});
$NS->re_find = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_re = $__args[0]; $__L_s = $__args[1]; $__L_m = ($NS->re_matcher)($__L_re, $__L_s);
return ($NS->re_find)($__L_m); } else if ($__n == 1) { $__L_m = $__args[0];  while(true) { if ($__L_m->find()) { $__L_gc = $__L_m->groupCount();
if (($__L_gc === 0 || $__L_gc === 0.0)) { return $__L_m->group();} else { $__L_ret = \Clojure\Php\vec();
$__L_i = 0;
 while(true) { if (($__L_i <= $__L_gc)) { $__recur_0 = \Clojure\Php\conj($__L_ret, $__L_m->group($__L_i)); $__recur_1 = ($__L_i + 1); $__L_ret = $__recur_0; $__L_i = $__recur_1; continue;} else { return $__L_ret;} break; }
}} break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->re_matches = (function($__L_re, $__L_s) {  while(true) { $__L_m = ($NS->re_matcher)($__L_re, $__L_s);
if ($__L_m->matches()) { $__L_gc = $__L_m->groupCount();
if (($__L_gc === 0 || $__L_gc === 0.0)) { return $__L_m->group();} else { $__L_ret = \Clojure\Php\vec();
$__L_i = 0;
 while(true) { if (($__L_i <= $__L_gc)) { $__recur_0 = \Clojure\Php\conj($__L_ret, $__L_m->group($__L_i)); $__recur_1 = ($__L_i + 1); $__L_ret = $__recur_0; $__L_i = $__recur_1; continue;} else { return $__L_ret;} break; }
}} break; }});
$NS->re_seq = (function($__L_re, $__L_s) { $__L_m = ($NS->re_matcher)($__L_re, $__L_s);
$__L_step = (function() use (&$__L_step, &$__L_m, &$__L_re, &$__L_s) { return ($NS->lazy_seq)(((function() { $__L_when_let__3052 = ($NS->re_find)($__L_m); if ($__L_when_let__3052) { $__L_match = $__L_when_let__3052;
return \Clojure\Php\cons($__L_match, call_user_func($__L_step));} })()));});
return call_user_func($__L_step);});
$NS->re_groups = (function($__L_m) {  while(true) { $__L_gc = $__L_m->groupCount();
if (($__L_gc === 0 || $__L_gc === 0.0)) { return $__L_m->group();} else { $__L_ret = \Clojure\Php\vec();
$__L_i = 0;
 while(true) { if (($__L_i <= $__L_gc)) { $__recur_0 = \Clojure\Php\conj($__L_ret, $__L_m->group($__L_i)); $__recur_1 = ($__L_i + 1); $__L_ret = $__recur_0; $__L_i = $__recur_1; continue;} else { return $__L_ret;} break; }
} break; }});
$NS->pad = (function($__L_s, $__L_n, $__L_pad_str, $__L_left_QMARK_) {  while(true) { $__L_s = ((string)$__L_s);
$__L_len = \Clojure\Php\count_($__L_s);
$__L_pad_len = ($__L_n - $__L_len);
if (($__L_pad_len <= 0)) { return $__L_s;} else { $__L_padding = ((function() { $__L_acc = ''; $__L_remaining = $__L_pad_len;  while(true) { if (($__L_remaining === 0 || $__L_remaining === 0.0)) { $__L_acc;
} else { $__L_chunk = ($NS->subs)($__L_pad_str, 0, ($NS->min)(\Clojure\Php\count_($__L_pad_str), $__L_remaining));
$__recur_0 = \Clojure\Php\str_($__L_acc, $__L_chunk); $__recur_1 = ($__L_remaining - \Clojure\Php\count_($__L_chunk)); $__L_acc = $__recur_0; $__L_remaining = $__recur_1; continue;}
 break; } return $__L_acc; })());
if ($__L_left_QMARK_) { return \Clojure\Php\str_($__L_padding, $__L_s);} else { return \Clojure\Php\str_($__L_s, $__L_padding);}} break; }});
$NS->pad_left = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_s = $__args[0]; $__L_n = $__args[1]; $__L_pad_str = $__args[2]; return ($NS->pad)($__L_s, $__L_n, $__L_pad_str, true); } else if ($__n == 2) { $__L_s = $__args[0]; $__L_n = $__args[1]; return ($NS->pad_left)($__L_s, $__L_n, ' '); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->pad_right = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_s = $__args[0]; $__L_n = $__args[1]; $__L_pad_str = $__args[2]; return ($NS->pad)($__L_s, $__L_n, $__L_pad_str, false); } else if ($__n == 2) { $__L_s = $__args[0]; $__L_n = $__args[1]; return ($NS->pad_right)($__L_s, $__L_n, ' '); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->char = (function($__L_x) { if (($NS->char_QMARK_)($__L_x)) { return $__L_x;} else { if ((is_int($__L_x) || is_float($__L_x))) { return ($Clojure_Core->char)($__L_x);} else { if (is_string($__L_x)) { if (\Clojure\Php\equals(1, \Clojure\Php\count_($__L_x))) { return \Clojure\Php\first($__L_x);} else { throw ($NS->ex_info)('String must have exactly one character', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('s'), $__L_x));
}} else { if (\Clojure\Php\Kw::create('else')) { throw ($NS->ex_info)('Cannot coerce to char', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('x'), $__L_x));
}}}}});
$NS->char_QMARK_ = (function($__L_x) { return ($NS->instance_QMARK_)($NS->Character, $__L_x);});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.var'), 'Var - Clojure\'s mechanism for dynamic binding.

   Vars provide a mechanism to refer to a mutable storage location
   that can be dynamically rebound on a per-thread basis.

   In non-threaded environments (PHP, single-threaded JS), dynamic
   binding uses a simple binding stack.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
call_user_func('\Clojure\Php\defType', 'BindingFrame', array('bindings', 'prev'), array(), array());
$NS->__GT_BindingFrame = (function($__L_bindings, $__L_prev) { return call_user_func('\Clojure\Php\createType', 'BindingFrame', $__L_bindings, $__L_prev);});
$NS->_STAR_binding_frame_STAR_ = null;
call_user_func('\Clojure\Php\defType', 'Var', array('sym', 'root', 'dynamic?', '_meta', 'validator'), array('root', 'dynamic?', 'validator'), array('-name', (function($__L__) { return ($P->_name)($__L__->sym);}), '-namespace', (function($__L__) { return ($P->_namespace)($__L__->sym);}), '-deref', (function($__L_this) {  while(true) { if ($__L_this->dynamic_QMARK_) { $__L_if_let__3053 = $NS->_STAR_binding_frame_STAR_;
if ($__L_if_let__3053) { $__L_frame = $__L_if_let__3053;
$__L_f = $__L_frame;
 while(true) { if ($__L_f) { $__L_if_let__3054 = \Clojure\Php\get_($__L_f->bindings, $__L_this);
if ($__L_if_let__3054) { $__L_v = $__L_if_let__3054;
return $__L_v;} else { $__recur_0 = $__L_f->prev; $__L_f = $__recur_0; continue;}} else { return $__L_this->root;} break; }
} else { return $__L_this->root;}} else { return $__L_this->root;} break; }}), '-reset!', (function($__L_this, $__L_v) { if ($__L_this->validator) { if (call_user_func($__L_this->validator, $__L_v)) { null;
} else { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L_this->sym, \Clojure\Php\Kw::create('val'), $__L_v));
}
}
$__L_this->root = $__L_v;
return $__L_v;}), '-set-validator!', (function($__L__, $__L_vf) { if ($__L_vf) { if (call_user_func($__L_vf, $__L__->root)) { null;
} else { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L__->sym, \Clojure\Php\Kw::create('val'), $__L__->root));
}
}
return $__L__->validator = $__L_vf;}), '-get-validator', (function($__L__) { return $__L__->validator;}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { throw ($NS->ex_info)('Use alter-meta! for Var metadata', \Clojure\Php\hashMap());
}), '-invoke', (function($__L_this) { $__L_f = ($P->_deref)($__L_this);
return call_user_func($__L_f);}), '-invoke', (function($__L_this, $__L_a) { $__L_f = ($P->_deref)($__L_this);
return call_user_func($__L_f, $__L_a);}), '-invoke', (function($__L_this, $__L_a, $__L_b) { $__L_f = ($P->_deref)($__L_this);
return call_user_func($__L_f, $__L_a, $__L_b);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c) { $__L_f = ($P->_deref)($__L_this);
return call_user_func($__L_f, $__L_a, $__L_b, $__L_c);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d) { $__L_f = ($P->_deref)($__L_this);
return call_user_func($__L_f, $__L_a, $__L_b, $__L_c, $__L_d);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e) { $__L_f = ($P->_deref)($__L_this);
return call_user_func($__L_f, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f) { $__L_fn_PRIME_ = ($P->_deref)($__L_this);
return call_user_func($__L_fn_PRIME_, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g) { $__L_fn_PRIME_ = ($P->_deref)($__L_this);
return call_user_func($__L_fn_PRIME_, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h) { $__L_fn_PRIME_ = ($P->_deref)($__L_this);
return call_user_func($__L_fn_PRIME_, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h, $__L_i) { $__L_fn_PRIME_ = ($P->_deref)($__L_this);
return call_user_func($__L_fn_PRIME_, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h, $__L_i);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h, $__L_i, $__L_j) { $__L_fn_PRIME_ = ($P->_deref)($__L_this);
return call_user_func($__L_fn_PRIME_, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g, $__L_h, $__L_i, $__L_j);}), '-apply', (function($__L_this, $__L_args) { return ($NS->apply)(($P->_deref)($__L_this), $__L_args);}), '-equiv', (function($__L_this, $__L_other) { return ($__L_this === $__L_other);}), '-hash', (function($__L_this) { return ($NS->hash)($__L_this->sym);})));
$NS->__GT_Var = (function($__L_sym, $__L_root, $__L_dynamic_QMARK_, $__L__meta, $__L_validator) { return call_user_func('\Clojure\Php\createType', 'Var', $__L_sym, $__L_root, $__L_dynamic_QMARK_, $__L__meta, $__L_validator);});
call_user_func('\Clojure\Php\defType', 'Unbound', array('var-sym'), array(), array('-invoke', (function($__L__) { throw ($NS->ex_info)('Attempting to call unbound fn', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L__->var_sym));
}), '-invoke', (function($__L__, $__L__a) { throw ($NS->ex_info)('Attempting to call unbound fn', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L__->var_sym));
}), '-invoke', (function($__L__, $__L__a, $__L__b) { throw ($NS->ex_info)('Attempting to call unbound fn', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L__->var_sym));
}), '-apply', (function($__L__, $__L__args) { throw ($NS->ex_info)('Attempting to call unbound fn', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L__->var_sym));
})));
$NS->__GT_Unbound = (function($__L_var_sym) { return call_user_func('\Clojure\Php\createType', 'Unbound', $__L_var_sym);});
$NS->unbound_QMARK_ = (function($__L_v) { return ($NS->instance_QMARK_)($NS->Unbound, ($P->_deref)($__L_v));});
$NS->var_QMARK_ = (function($__L_x) { return ($NS->instance_QMARK_)($NS->Var, $__L_x);});
$NS->bound_QMARK_ = (function($__L_v) { return (!($NS->unbound_QMARK_)($__L_v));});
$NS->dynamic_QMARK_ = (function($__L_v) { return $__L_v->dynamic_QMARK_;});
$NS->set_dynamic_BANG_ = (function($__L_v) { $__L_v->dynamic_QMARK_ = true;
return $__L_v;});
$NS->alter_var_root = (function($__L_v, $__L_f, ...$__L_args) { return ($P->_reset_BANG_)($__L_v, ($NS->apply)($__L_f, ($P->_deref)($__L_v), $__L_args));});
$NS->push_thread_bindings = (function($__L_bindings) {  while(true) { $__L_s3055 = \Clojure\Php\seq($__L_bindings);
 while(true) { if ($__L_s3055) { $__L___dest_8 = \Clojure\Php\first($__L_s3055);
$__L_v = \Clojure\Php\nth($__L___dest_8, 0);
if (($NS->dynamic_QMARK_)($__L_v)) { null;
} else { throw ($NS->ex_info)('Can\'t dynamically bind non-dynamic var', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('var'), $__L_v));
}
$__recur_0 = \Clojure\Php\next_($__L_s3055); $__L_s3055 = $__recur_0; continue;}
 break; }
return $GLOBALS['_STAR_binding_frame_STAR_'] = ($NS->__GT_BindingFrame)($__L_bindings, $NS->_STAR_binding_frame_STAR_); break; }});
$NS->pop_thread_bindings = (function() { if ($NS->_STAR_binding_frame_STAR_) { null;
} else { throw ($NS->ex_info)('No bindings to pop', \Clojure\Php\hashMap());
}
return $GLOBALS['_STAR_binding_frame_STAR_'] = $NS->_STAR_binding_frame_STAR_->prev;});
$NS->get_thread_bindings = (function() {  while(true) { $__L_f = $NS->_STAR_binding_frame_STAR_;
$__L_result = \Clojure\Php\hashMap();
 while(true) { if ($__L_f) { $__recur_0 = $__L_f->prev; $__recur_1 = ($NS->merge)($__L_result, $__L_f->bindings); $__L_f = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }
 break; }});
$NS->create_var = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_sym = $__args[0]; $__L_root = $__args[1]; $__L_meta = $__args[2]; return ($NS->__GT_Var)($__L_sym, $__L_root, false, $__L_meta, null); } else if ($__n == 2) { $__L_sym = $__args[0]; $__L_root = $__args[1]; return ($NS->__GT_Var)($__L_sym, $__L_root, false, null, null); } else if ($__n == 1) { $__L_sym = $__args[0]; return ($NS->__GT_Var)($__L_sym, ($NS->__GT_Unbound)($__L_sym), false, null, null); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->intern = (function($__L_ns, $__L_sym) { return ($NS->create_var)(($K->symbol)((is_string($__L_ns) ? $__L_ns : $__L_ns->name()), (is_string($__L_sym) ? $__L_sym : $__L_sym->name())));});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.namespace'), 'Namespace - Clojure\'s mechanism for organizing code.

   Namespaces provide:
   - A mapping from symbols to vars
   - Alias support for referring to other namespaces
   - Import/refer functionality', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')], [\Clojure\Php\Sym::create('clojure.lang.var'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('v')]));
$NS->namespaces = ($NS->atom)(\Clojure\Php\hashMap());
call_user_func('\Clojure\Php\defType', 'Namespace', array('name', 'mappings', 'aliases', '_meta'), array(), array('-name', (function($__L__) { return ($P->_name)($__L__->name);}), '-namespace', (function($__L__) { return null;}), '-lookup', (function($__L__, $__L_sym) { return \Clojure\Php\get_(($Clojure_Core->deref)($__L__->mappings), $__L_sym);}), '-lookup', (function($__L__, $__L_sym, $__L_not_found) { return \Clojure\Php\get_(($Clojure_Core->deref)($__L__->mappings), $__L_sym, $__L_not_found);}), '-meta', (function($__L__) { return ($Clojure_Core->deref)($__L__->_meta);}), '-with-meta', (function($__L__, $__L_m) { ($NS->reset_BANG_)($__L__->_meta, $__L_m);
return $__L__;}), '-equiv', (function($__L__, $__L_other) { if (($NS->instance_QMARK_)($NS->Namespace, $__L_other)) { return \Clojure\Php\equals($__L__->name, $__L_other->name);} else { return false;}}), '-hash', (function($__L__) { return ($NS->hash)($__L__->name);})));
$NS->__GT_Namespace = (function($__L_name, $__L_mappings, $__L_aliases, $__L__meta) { return call_user_func('\Clojure\Php\createType', 'Namespace', $__L_name, $__L_mappings, $__L_aliases, $__L__meta);});
$NS->namespace_QMARK_ = (function($__L_x) { return ($NS->instance_QMARK_)($NS->Namespace, $__L_x);});
$NS->find_ns = (function($__L_sym) { return \Clojure\Php\get_(($Clojure_Core->deref)($NS->namespaces), (($__L_sym instanceof \Clojure\Php\Sym) ? $__L_sym : ($K->symbol)((is_string($__L_sym) ? $__L_sym : $__L_sym->name()))));});
$NS->create_ns = (function($__L_sym) { $__L_s = (($__L_sym instanceof \Clojure\Php\Sym) ? $__L_sym : ($K->symbol)((is_string($__L_sym) ? $__L_sym : $__L_sym->name())));
$__L_if_let__3056 = ($NS->find_ns)($__L_s);
if ($__L_if_let__3056) { $__L_ns = $__L_if_let__3056;
return $__L_ns;} else { $__L_ns = ($NS->__GT_Namespace)($__L_s, ($NS->atom)(\Clojure\Php\hashMap()), ($NS->atom)(\Clojure\Php\hashMap()), ($NS->atom)(null));
($NS->swap_BANG_)($NS->namespaces, $NS->assoc, $__L_s, $__L_ns);
return $__L_ns;}});
$NS->remove_ns = (function($__L_sym) { $__L_s = (($__L_sym instanceof \Clojure\Php\Sym) ? $__L_sym : ($K->symbol)((is_string($__L_sym) ? $__L_sym : $__L_sym->name())));
if (\Clojure\Php\equals($__L_s, \Clojure\Php\Sym::create('clojure.core'))) { return null;} else { return ($NS->swap_BANG_)($NS->namespaces, $NS->dissoc, $__L_s);}});
$NS->all_ns = (function() { return ($NS->vals)(($Clojure_Core->deref)($NS->namespaces));});
$NS->the_ns = (function($__L_x) { if (($NS->namespace_QMARK_)($__L_x)) { return $__L_x;} else { $__L_or__3057 = ($NS->find_ns)($__L_x);
if ($__L_or__3057) { return $__L_or__3057;} else { throw ($NS->ex_info)('No namespace found', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('ns'), $__L_x));
}}});
$NS->ns_name = (function($__L_ns) { return ($NS->the_ns)($__L_ns)->name;});
$NS->ns_map = (function($__L_ns) { return ($Clojure_Core->deref)(($NS->the_ns)($__L_ns)->mappings);});
$NS->ns_aliases = (function($__L_ns) { return ($Clojure_Core->deref)(($NS->the_ns)($__L_ns)->aliases);});
$NS->ns_intern = (function($__L_ns, $__L_sym) { $__L_ns = ($NS->the_ns)($__L_ns);
$__L_mappings = $__L_ns->mappings;
$__L_if_let__3058 = \Clojure\Php\get_(($Clojure_Core->deref)($__L_mappings), $__L_sym);
if ($__L_if_let__3058) { $__L_v = $__L_if_let__3058;
if ((($V->var_QMARK_)($__L_v) ? \Clojure\Php\equals(($P->_namespace)($__L_v->sym), ($P->_name)($__L_ns->name)) : false)) { return $__L_v;} else { $__L_new_var = ($V->create_var)(($K->symbol)(($P->_name)($__L_ns->name), ($P->_name)($__L_sym)));
($NS->swap_BANG_)($__L_mappings, $NS->assoc, $__L_sym, $__L_new_var);
return $__L_new_var;}} else { $__L_new_var = ($V->create_var)(($K->symbol)(($P->_name)($__L_ns->name), ($P->_name)($__L_sym)));
($NS->swap_BANG_)($__L_mappings, $NS->assoc, $__L_sym, $__L_new_var);
return $__L_new_var;}});
$NS->ns_resolve = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_ns = $__args[0]; $__L_env = $__args[1]; $__L_sym = $__args[2]; if (\Clojure\Php\contains($__L_env, $__L_sym)) { return null;} else { $__L_ns = ($NS->the_ns)($__L_ns);
$__L_or__3059 = ((function() { $__L_when_let__3060 = ($P->_namespace)($__L_sym); if ($__L_when_let__3060) { $__L_ns_part = $__L_when_let__3060;
$__L_when_let__3061 = \Clojure\Php\get_(($Clojure_Core->deref)($__L_ns->aliases), ($K->symbol)($__L_ns_part));
if ($__L_when_let__3061) { $__L_alias_ns = $__L_when_let__3061;
return \Clojure\Php\get_(($Clojure_Core->deref)($__L_alias_ns->mappings), ($K->symbol)(($P->_name)($__L_sym)));}} })());
if ($__L_or__3059) { return $__L_or__3059;} else { return \Clojure\Php\get_(($Clojure_Core->deref)($__L_ns->mappings), $__L_sym);}} } else if ($__n == 2) { $__L_ns = $__args[0]; $__L_sym = $__args[1]; return ($NS->ns_resolve)($__L_ns, null, $__L_sym); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->ns_refer = (function($__L_ns, $__L_sym, $__L_var) { $__L_ns = ($NS->the_ns)($__L_ns);
return ($NS->swap_BANG_)($__L_ns->mappings, $NS->assoc, $__L_sym, $__L_var);});
$NS->ns_unmap = (function($__L_ns, $__L_sym) { $__L_ns = ($NS->the_ns)($__L_ns);
return ($NS->swap_BANG_)($__L_ns->mappings, $NS->dissoc, $__L_sym);});
$NS->ns_alias = (function($__L_ns, $__L_alias, $__L_ns_sym) { $__L_ns = ($NS->the_ns)($__L_ns);
$__L_target_ns = ($NS->the_ns)($__L_ns_sym);
return ($NS->swap_BANG_)($__L_ns->aliases, $NS->assoc, $__L_alias, $__L_target_ns);});
$NS->ns_unalias = (function($__L_ns, $__L_sym) { $__L_ns = ($NS->the_ns)($__L_ns);
return ($NS->swap_BANG_)($__L_ns->aliases, $NS->dissoc, $__L_sym);});
$NS->refer = (function($__L_ns_sym, ...$__L_filters) {  while(true) { $__L_ns = ($NS->the_ns)($__L_ns_sym);
$__L_filter_map = ($NS->apply)($NS->hash_map, $__L_filters);
$__L_only = \Clojure\Php\get_($__L_filter_map, \Clojure\Php\Kw::create('only'));
$__L_exclude = ($NS->set)(\Clojure\Php\get_($__L_filter_map, \Clojure\Php\Kw::create('exclude')));
$__L_rename = \Clojure\Php\get_($__L_filter_map, \Clojure\Php\Kw::create('rename'));
$__L_s3062 = \Clojure\Php\seq(($Clojure_Core->deref)($__L_ns->mappings));
 while(true) { if ($__L_s3062) { $__L___dest_9 = \Clojure\Php\first($__L_s3062);
$__L_sym = \Clojure\Php\nth($__L___dest_9, 0);
$__L_var = \Clojure\Php\nth($__L___dest_9, 1);
if ((((function() { $__L_or__3063 = ($__L_only === null); if ($__L_or__3063) { return $__L_or__3063;} else { return \Clojure\Php\contains(($NS->set)($__L_only), $__L_sym);} })()) ? (!\Clojure\Php\contains($__L_exclude, $__L_sym)) : false)) { $__L_sym_PRIME_ = \Clojure\Php\get_($__L_rename, $__L_sym, $__L_sym);
($NS->ns_refer)($NS->_STAR_ns_STAR_, $__L_sym_PRIME_, $__L_var);
}
$__recur_0 = \Clojure\Php\next_($__L_s3062); $__L_s3062 = $__recur_0; continue;} break; }
 break; }});
$NS->init_core_ns_BANG_ = (function() { return ($NS->create_ns)(\Clojure\Php\Sym::create('clojure.core'));});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.multifn'), 'MultiFn - Clojure\'s multimethod implementation.

   Multimethods provide runtime polymorphic dispatch based on
   an arbitrary dispatch function.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$NS->global_hierarchy = ($NS->atom)(\Clojure\Php\hashMap(\Clojure\Php\Kw::create('parents'), \Clojure\Php\hashMap(), \Clojure\Php\Kw::create('descendants'), \Clojure\Php\hashMap(), \Clojure\Php\Kw::create('ancestors'), \Clojure\Php\hashMap()));
$NS->make_hierarchy = (function() { return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('parents'), \Clojure\Php\hashMap(), \Clojure\Php\Kw::create('descendants'), \Clojure\Php\hashMap(), \Clojure\Php\Kw::create('ancestors'), \Clojure\Php\hashMap());});
$NS->derive_impl = (function($__L_h, $__L_tag, $__L_parent) { $__L_tp = call_user_func(\Clojure\Php\Kw::create('parents'), $__L_h);
$__L_td = call_user_func(\Clojure\Php\Kw::create('descendants'), $__L_h);
$__L_ta = call_user_func(\Clojure\Php\Kw::create('ancestors'), $__L_h);
if (\Clojure\Php\contains(\Clojure\Php\get_($__L_ta, $__L_tag), $__L_parent)) { $__L_h;
}
if (\Clojure\Php\contains(\Clojure\Php\get_($__L_ta, $__L_parent), $__L_tag)) { throw ($NS->ex_info)('Cyclic derivation', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('tag'), $__L_tag, \Clojure\Php\Kw::create('parent'), $__L_parent));
}
$__L_new_parents = ($NS->update)($__L_tp, $__L_tag, ($NS->fnil)($NS->conj, \Clojure\Php\hashSet()), $__L_parent);
$__L_new_ancestors = ($NS->reduce)((function($__L_ta_PRIME_, $__L___dest_10) use (&$__L_tag, &$__L_parent, &$__L_td, &$__L_new_parents, &$__L_tp, &$__L_h, &$__L_ta) { $__L___dest_11 = $__L___dest_10;
$__L_t = \Clojure\Php\nth($__L___dest_11, 0);
$__L_ancestors = \Clojure\Php\nth($__L___dest_11, 1);
if (\Clojure\Php\contains($__L_ancestors, $__L_tag)) { return ($NS->update)($__L_ta_PRIME_, $__L_t, $NS->into, \Clojure\Php\conj(\Clojure\Php\get_($__L_ta, $__L_parent, \Clojure\Php\hashSet()), $__L_parent));} else { return $__L_ta_PRIME_;}}), ($NS->update)($__L_ta, $__L_tag, ($NS->fnil)($NS->into, \Clojure\Php\hashSet()), \Clojure\Php\conj(\Clojure\Php\get_($__L_ta, $__L_parent, \Clojure\Php\hashSet()), $__L_parent)), $__L_ta);
$__L_new_descendants = ($NS->reduce)((function($__L_td_PRIME_, $__L___dest_12) use (&$__L_tag, &$__L_new_ancestors, &$__L_parent, &$__L_td, &$__L_new_parents, &$__L_tp, &$__L_h, &$__L_ta) { $__L___dest_13 = $__L___dest_12;
$__L_t = \Clojure\Php\nth($__L___dest_13, 0);
$__L_descendants = \Clojure\Php\nth($__L___dest_13, 1);
if (\Clojure\Php\contains($__L_descendants, $__L_parent)) { return ($NS->update)($__L_td_PRIME_, $__L_t, $NS->into, \Clojure\Php\conj(\Clojure\Php\get_($__L_td, $__L_tag, \Clojure\Php\hashSet()), $__L_tag));} else { return $__L_td_PRIME_;}}), ($NS->update)($__L_td, $__L_parent, ($NS->fnil)($NS->into, \Clojure\Php\hashSet()), \Clojure\Php\conj(\Clojure\Php\get_($__L_td, $__L_tag, \Clojure\Php\hashSet()), $__L_tag)), $__L_td);
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('parents'), $__L_new_parents, \Clojure\Php\Kw::create('descendants'), $__L_new_descendants, \Clojure\Php\Kw::create('ancestors'), $__L_new_ancestors);});
$NS->derive = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_h = $__args[0]; $__L_tag = $__args[1]; $__L_parent = $__args[2]; return ($NS->derive_impl)($__L_h, $__L_tag, $__L_parent); } else if ($__n == 2) { $__L_tag = $__args[0]; $__L_parent = $__args[1]; ($NS->swap_BANG_)($NS->global_hierarchy, $NS->derive_impl, $__L_tag, $__L_parent);
return null; } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->underive = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_h = $__args[0]; $__L_tag = $__args[1]; $__L_parent = $__args[2]; return ($NS->update_in)($__L_h, \Clojure\Php\vec(\Clojure\Php\Kw::create('parents'), $__L_tag), $NS->disj, $__L_parent); } else if ($__n == 2) { $__L_tag = $__args[0]; $__L_parent = $__args[1]; ($NS->swap_BANG_)($NS->global_hierarchy, (function($__L_h) use (&$__L_tag, &$__L_parent) { return ($NS->update_in)($__L_h, \Clojure\Php\vec(\Clojure\Php\Kw::create('parents'), $__L_tag), $NS->disj, $__L_parent);}));
return null; } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->parents = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_h = $__args[0]; $__L_tag = $__args[1]; return \Clojure\Php\get_(call_user_func(\Clojure\Php\Kw::create('parents'), $__L_h), $__L_tag); } else if ($__n == 1) { $__L_tag = $__args[0]; return ($NS->parents)(($Clojure_Core->deref)($NS->global_hierarchy), $__L_tag); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->ancestors = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_h = $__args[0]; $__L_tag = $__args[1]; return \Clojure\Php\get_(call_user_func(\Clojure\Php\Kw::create('ancestors'), $__L_h), $__L_tag); } else if ($__n == 1) { $__L_tag = $__args[0]; return ($NS->ancestors)(($Clojure_Core->deref)($NS->global_hierarchy), $__L_tag); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->descendants = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_h = $__args[0]; $__L_tag = $__args[1]; return \Clojure\Php\get_(call_user_func(\Clojure\Php\Kw::create('descendants'), $__L_h), $__L_tag); } else if ($__n == 1) { $__L_tag = $__args[0]; return ($NS->descendants)(($Clojure_Core->deref)($NS->global_hierarchy), $__L_tag); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->isa_QMARK_ = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_h = $__args[0]; $__L_child = $__args[1]; $__L_parent = $__args[2]; $__L_or__3066 = \Clojure\Php\equals($__L_child, $__L_parent);
if ($__L_or__3066) { return $__L_or__3066;} else { $__L_or__3067 = \Clojure\Php\contains(\Clojure\Php\get_(call_user_func(\Clojure\Php\Kw::create('ancestors'), $__L_h), $__L_child), $__L_parent);
if ($__L_or__3067) { return $__L_or__3067;} else { $__L_or__3068 = (($NS->class_QMARK_)($__L_parent) ? (($NS->class_QMARK_)($__L_child) ? false : false) : false);
if ($__L_or__3068) { return $__L_or__3068;} else { if (($__L_parent instanceof \Clojure\Php\Vec)) { if (($__L_child instanceof \Clojure\Php\Vec)) { if (\Clojure\Php\equals(\Clojure\Php\count_($__L_parent), \Clojure\Php\count_($__L_child))) { return ($NS->every_QMARK_)($NS->identity, ($NS->map)((function($__L_p1__3064_SHARP_, $__L_p2__3065_SHARP_) use (&$__L_child, &$__L_parent, &$__L_or__3066, &$__L_or__3067, &$__L_h, &$__L_or__3068) { return ($NS->isa_QMARK_)($__L_h, $__L_p1__3064_SHARP_, $__L_p2__3065_SHARP_);}), $__L_child, $__L_parent));} else { return false;}} else { return false;}} else { return false;}}}} } else if ($__n == 2) { $__L_child = $__args[0]; $__L_parent = $__args[1]; return ($NS->isa_QMARK_)(($Clojure_Core->deref)($NS->global_hierarchy), $__L_child, $__L_parent); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
call_user_func('\Clojure\Php\defType', 'MultiFn', array('name', 'dispatch-fn', 'default-val', 'hierarchy', 'method-table', 'prefer-table', 'cached-hierarchy', 'method-cache'), array('method-table', 'prefer-table', 'cached-hierarchy', 'method-cache'), array('-invoke', (function($__L_this) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn);
return call_user_func(($NS->get_method)($__L_this, $__L_dispatch_val));}), '-invoke', (function($__L_this, $__L_a) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a);
return call_user_func(($NS->get_method)($__L_this, $__L_dispatch_val), $__L_a);}), '-invoke', (function($__L_this, $__L_a, $__L_b) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b);
return call_user_func(($NS->get_method)($__L_this, $__L_dispatch_val), $__L_a, $__L_b);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b, $__L_c);
return call_user_func(($NS->get_method)($__L_this, $__L_dispatch_val), $__L_a, $__L_b, $__L_c);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b, $__L_c, $__L_d);
return call_user_func(($NS->get_method)($__L_this, $__L_dispatch_val), $__L_a, $__L_b, $__L_c, $__L_d);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e);
return call_user_func(($NS->get_method)($__L_this, $__L_dispatch_val), $__L_a, $__L_b, $__L_c, $__L_d, $__L_e);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f);
return call_user_func(($NS->get_method)($__L_this, $__L_dispatch_val), $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f);}), '-invoke', (function($__L_this, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g) { $__L_dispatch_val = call_user_func($__L_this->dispatch_fn, $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g);
return call_user_func(($NS->get_method)($__L_this, $__L_dispatch_val), $__L_a, $__L_b, $__L_c, $__L_d, $__L_e, $__L_f, $__L_g);}), '-apply', (function($__L_this, $__L_args) { $__L_dispatch_val = ($NS->apply)($__L_this->dispatch_fn, $__L_args);
return ($NS->apply)(($NS->get_method)($__L_this, $__L_dispatch_val), $__L_args);}), '-name', (function($__L__) { return $__L__->name;}), '-namespace', (function($__L__) { return null;})));
$NS->__GT_MultiFn = (function($__L_name, $__L_dispatch_fn, $__L_default_val, $__L_hierarchy, $__L_method_table, $__L_prefer_table, $__L_cached_hierarchy, $__L_method_cache) { return call_user_func('\Clojure\Php\createType', 'MultiFn', $__L_name, $__L_dispatch_fn, $__L_default_val, $__L_hierarchy, $__L_method_table, $__L_prefer_table, $__L_cached_hierarchy, $__L_method_cache);});
$NS->reset_cache_BANG_ = (function($__L_mf) { $__L_mf->cached_hierarchy = ($Clojure_Core->deref)($__L_mf->hierarchy);
return $__L_mf->method_cache = $__L_mf->method_table;});
$NS->find_and_cache_best_method = (function($__L_mf, $__L_dispatch_val) { $__L_h = ($Clojure_Core->deref)($__L_mf->hierarchy);
$__L_method_table = $__L_mf->method_table;
$__L_default_val = $__L_mf->default_val;
$__L_prefer_table = $__L_mf->prefer_table;
$__L_dominates_QMARK_ = (function($__L_x, $__L_y) use (&$__L_method_table, &$__L_dominates_QMARK_, &$__L_find_best, &$__L_dispatch_val, &$__L_prefer_table, &$__L_default_val, &$__L_mf, &$__L_h) { $__L_or__3069 = ($NS->isa_QMARK_)($__L_h, $__L_x, $__L_y);
if ($__L_or__3069) { return $__L_or__3069;} else { return \Clojure\Php\contains(\Clojure\Php\get_($__L_prefer_table, $__L_x), $__L_y);}});
$__L_find_best = (function($__L_entries, $__L_best) use (&$__L_method_table, &$__L_dominates_QMARK_, &$__L_find_best, &$__L_dispatch_val, &$__L_prefer_table, &$__L_default_val, &$__L_mf, &$__L_h) {  while(true) { if (\Clojure\Php\isEmpty($__L_entries)) { return $__L_best;} else { $__L___dest_14 = \Clojure\Php\first($__L_entries);
$__L_k = \Clojure\Php\nth($__L___dest_14, 0);
$__L_m = \Clojure\Php\nth($__L___dest_14, 1);
if (($NS->isa_QMARK_)($__L_h, $__L_dispatch_val, $__L_k)) { if (((function() { $__L_or__3070 = ($__L_best === null); if ($__L_or__3070) { return $__L_or__3070;} else { return call_user_func($__L_dominates_QMARK_, $__L_k, \Clojure\Php\first($__L_best));} })())) { $__recur_0 = \Clojure\Php\rest($__L_entries); $__recur_1 = \Clojure\Php\vec($__L_k, $__L_m); $__L_entries = $__recur_0; $__L_best = $__recur_1; continue;} else { if (call_user_func($__L_dominates_QMARK_, \Clojure\Php\first($__L_best), $__L_k)) { $__recur_0 = \Clojure\Php\rest($__L_entries); $__recur_1 = $__L_best; $__L_entries = $__recur_0; $__L_best = $__recur_1; continue;} else { throw ($NS->ex_info)('Multiple methods match dispatch value, none preferred', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('dispatch-val'), $__L_dispatch_val, \Clojure\Php\Kw::create('methods'), \Clojure\Php\vec($__L_k, \Clojure\Php\first($__L_best))));
}}} else { $__recur_0 = \Clojure\Php\rest($__L_entries); $__recur_1 = $__L_best; $__L_entries = $__recur_0; $__L_best = $__recur_1; continue;}} break; }});
$__L_best = call_user_func($__L_find_best, \Clojure\Php\seq($__L_method_table), null);
if ($__L_best) { return \Clojure\Php\second($__L_best);} else { return \Clojure\Php\get_($__L_method_table, $__L_default_val);}});
$NS->get_method = (function($__L_mf, $__L_dispatch_val) { if ((($Clojure_Core->deref)($__L_mf->hierarchy) === $__L_mf->cached_hierarchy)) { null;
} else { ($NS->reset_cache_BANG_)($__L_mf);
}
$__L_if_let__3071 = \Clojure\Php\get_($__L_mf->method_cache, $__L_dispatch_val);
if ($__L_if_let__3071) { $__L_method = $__L_if_let__3071;
return $__L_method;} else { $__L_if_let__3072 = ($NS->find_and_cache_best_method)($__L_mf, $__L_dispatch_val);
if ($__L_if_let__3072) { $__L_method = $__L_if_let__3072;
$__L_mf->method_cache = \Clojure\Php\assoc($__L_mf->method_cache, $__L_dispatch_val, $__L_method);
return $__L_method;} else { throw ($NS->ex_info)('No method in multimethod for dispatch value', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('name'), $__L_mf->name, \Clojure\Php\Kw::create('dispatch-val'), $__L_dispatch_val));
}}});
$NS->methods = (function($__L_mf) { return $__L_mf->method_table;});
$NS->prefers = (function($__L_mf) { return $__L_mf->prefer_table;});
$NS->multifn = (function(...$__args) { $__n = count($__args); if ($__n == 4) { $__L_name = $__args[0]; $__L_dispatch_fn = $__args[1]; $__L_default_val = $__args[2]; $__L_hierarchy = $__args[3]; return ($NS->__GT_MultiFn)($__L_name, $__L_dispatch_fn, $__L_default_val, $__L_hierarchy, \Clojure\Php\hashMap(), \Clojure\Php\hashMap(), ($Clojure_Core->deref)($__L_hierarchy), \Clojure\Php\hashMap()); } else if ($__n == 3) { $__L_name = $__args[0]; $__L_dispatch_fn = $__args[1]; $__L_default_val = $__args[2]; return ($NS->multifn)($__L_name, $__L_dispatch_fn, $__L_default_val, $NS->global_hierarchy); } else if ($__n == 2) { $__L_name = $__args[0]; $__L_dispatch_fn = $__args[1]; return ($NS->multifn)($__L_name, $__L_dispatch_fn, \Clojure\Php\Kw::create('default'), $NS->global_hierarchy); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->add_method = (function($__L_mf, $__L_dispatch_val, $__L_f) { $__L_mf->method_table = \Clojure\Php\assoc($__L_mf->method_table, $__L_dispatch_val, $__L_f);
($NS->reset_cache_BANG_)($__L_mf);
return $__L_mf;});
$NS->remove_method = (function($__L_mf, $__L_dispatch_val) { $__L_mf->method_table = \Clojure\Php\dissoc($__L_mf->method_table, $__L_dispatch_val);
($NS->reset_cache_BANG_)($__L_mf);
return $__L_mf;});
$NS->remove_all_methods = (function($__L_mf) { $__L_mf->method_table = \Clojure\Php\hashMap();
($NS->reset_cache_BANG_)($__L_mf);
return $__L_mf;});
$NS->prefer_method = (function($__L_mf, $__L_dispatch_val_x, $__L_dispatch_val_y) { if (($NS->dominates_QMARK_)($__L_mf, $__L_dispatch_val_y, $__L_dispatch_val_x)) { throw ($NS->ex_info)('Preference conflict', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('x'), $__L_dispatch_val_x, \Clojure\Php\Kw::create('y'), $__L_dispatch_val_y));
}
$__L_mf->prefer_table = ($NS->update)($__L_mf->prefer_table, $__L_dispatch_val_x, ($NS->fnil)($NS->conj, \Clojure\Php\hashSet()), $__L_dispatch_val_y);
($NS->reset_cache_BANG_)($__L_mf);
return $__L_mf;});
$NS->dominates_QMARK_ = (function($__L_mf, $__L_x, $__L_y) { $__L_h = ($Clojure_Core->deref)($__L_mf->hierarchy);
$__L_or__3073 = ($NS->isa_QMARK_)($__L_h, $__L_x, $__L_y);
if ($__L_or__3073) { return $__L_or__3073;} else { return \Clojure\Php\contains(\Clojure\Php\get_($__L_mf->prefer_table, $__L_x), $__L_y);}});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.ref'), 'Ref - Software Transactional Memory reference.

   Refs provide coordinated, synchronous access to shared state.
   Changes to refs must occur within a transaction (dosync).

   In single-threaded environments (PHP, single-threaded JS), STM
   semantics are simplified as there\'s no concurrent access.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$NS->_STAR_current_transaction_STAR_ = null;
call_user_func('\Clojure\Php\defType', 'Transaction', array('in-transaction?', 'values', 'sets', 'ensures', 'commutes'), array('in-transaction?', 'values', 'sets', 'ensures', 'commutes'), array());
$NS->__GT_Transaction = (function($__L_in_transaction_QMARK_, $__L_values, $__L_sets, $__L_ensures, $__L_commutes) { return call_user_func('\Clojure\Php\createType', 'Transaction', $__L_in_transaction_QMARK_, $__L_values, $__L_sets, $__L_ensures, $__L_commutes);});
$NS->make_transaction = (function() { return ($NS->__GT_Transaction)(true, ($NS->atom)(\Clojure\Php\hashMap()), ($NS->atom)(\Clojure\Php\hashSet()), ($NS->atom)(\Clojure\Php\hashSet()), ($NS->atom)(\Clojure\Php\hashMap()));});
call_user_func('\Clojure\Php\defType', 'Ref', array('val', '_meta', 'validator', 'watches', 'min-history', 'max-history'), array('val', 'validator', 'watches', 'min-history', 'max-history'), array('-deref', (function($__L_this) { if ($NS->_STAR_current_transaction_STAR_) { $__L_or__3074 = \Clojure\Php\get_(($Clojure_Core->deref)($NS->_STAR_current_transaction_STAR_->values), $__L_this);
if ($__L_or__3074) { return $__L_or__3074;} else { return $__L_this->val;}} else { return $__L_this->val;}}), '-set-validator!', (function($__L__, $__L_vf) { if (($__L_vf ? (!call_user_func($__L_vf, $__L__->val)) : false)) { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L__->val));
}
return $__L__->validator = $__L_vf;}), '-get-validator', (function($__L__) { return $__L__->validator;}), '-add-watch', (function($__L_this, $__L_k, $__L_f) { $__L_this->watches = \Clojure\Php\assoc($__L_this->watches, $__L_k, $__L_f);
return $__L_this;}), '-remove-watch', (function($__L_this, $__L_k) { $__L_this->watches = \Clojure\Php\dissoc($__L_this->watches, $__L_k);
return $__L_this;}), '-notify-watches', (function($__L_this, $__L_old_val, $__L_new_val) {  while(true) { $__L_s3075 = \Clojure\Php\seq($__L_this->watches);
 while(true) { if ($__L_s3075) { $__L___dest_15 = \Clojure\Php\first($__L_s3075);
$__L_k = \Clojure\Php\nth($__L___dest_15, 0);
$__L_f = \Clojure\Php\nth($__L___dest_15, 1);
call_user_func($__L_f, $__L_k, $__L_this, $__L_old_val, $__L_new_val);
$__recur_0 = \Clojure\Php\next_($__L_s3075); $__L_s3075 = $__recur_0; continue;} break; }
 break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { throw ($NS->ex_info)('Refs don\'t support with-meta directly', \Clojure\Php\hashMap());
}), '-equiv', (function($__L_this, $__L_other) { return ($__L_this === $__L_other);}), '-hash', (function($__L_this) { return System::identityHashCode($__L_this);})));
$NS->__GT_Ref = (function($__L_val, $__L__meta, $__L_validator, $__L_watches, $__L_min_history, $__L_max_history) { return call_user_func('\Clojure\Php\createType', 'Ref', $__L_val, $__L__meta, $__L_validator, $__L_watches, $__L_min_history, $__L_max_history);});
$NS->ref = (function(...$__args) { $__n = count($__args); if ($__n == 1) { $__L_val = $__args[0]; return ($NS->__GT_Ref)($__L_val, null, null, \Clojure\Php\hashMap(), 0, 10); } else if ($__n >= 1) { $__L_val = $__args[0]; $__L_options = array_slice($__args, 1); $__L_opts = ($NS->apply)($NS->hash_map, $__L_options);
$__L_meta = \Clojure\Php\get_($__L_opts, \Clojure\Php\Kw::create('meta'));
$__L_validator = \Clojure\Php\get_($__L_opts, \Clojure\Php\Kw::create('validator'));
$__L_min_history = \Clojure\Php\get_($__L_opts, \Clojure\Php\Kw::create('min-history'), 0);
$__L_max_history = \Clojure\Php\get_($__L_opts, \Clojure\Php\Kw::create('max-history'), 10);
if (($__L_validator ? (!call_user_func($__L_validator, $__L_val)) : false)) { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L_val));
}
return ($NS->__GT_Ref)($__L_val, $__L_meta, $__L_validator, \Clojure\Php\hashMap(), $__L_min_history, $__L_max_history); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->ref_QMARK_ = (function($__L_x) { return ($NS->instance_QMARK_)($NS->Ref, $__L_x);});
$NS->ensure_in_transaction_BANG_ = (function() { if ($NS->_STAR_current_transaction_STAR_) { return null;} else { throw ($NS->ex_info)('No transaction running', \Clojure\Php\hashMap());
}});
$NS->ref_set = (function($__L_ref, $__L_val) { ($NS->ensure_in_transaction_BANG_)();
$__L_when_let__3076 = $__L_ref->validator;
if ($__L_when_let__3076) { $__L_validator = $__L_when_let__3076;
if (call_user_func($__L_validator, $__L_val)) { null;
} else { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L_val));
}
}
($NS->swap_BANG_)($NS->_STAR_current_transaction_STAR_->values, $NS->assoc, $__L_ref, $__L_val);
($NS->swap_BANG_)($NS->_STAR_current_transaction_STAR_->sets, $NS->conj, $__L_ref);
return $__L_val;});
$NS->alter = (function($__L_ref, $__L_f, ...$__L_args) { ($NS->ensure_in_transaction_BANG_)();
$__L_old_val = ($P->_deref)($__L_ref);
$__L_new_val = ($NS->apply)($__L_f, $__L_old_val, $__L_args);
return ($NS->ref_set)($__L_ref, $__L_new_val);});
$NS->commute = (function($__L_ref, $__L_f, ...$__L_args) { ($NS->ensure_in_transaction_BANG_)();
$__L_old_val = ($P->_deref)($__L_ref);
$__L_new_val = ($NS->apply)($__L_f, $__L_old_val, $__L_args);
($NS->swap_BANG_)($NS->_STAR_current_transaction_STAR_->commutes, $NS->update, $__L_ref, ($NS->fnil)($NS->conj, \Clojure\Php\vec()), \Clojure\Php\vec($__L_f, $__L_args));
($NS->swap_BANG_)($NS->_STAR_current_transaction_STAR_->values, $NS->assoc, $__L_ref, $__L_new_val);
return $__L_new_val;});
$NS->ensure = (function($__L_ref) { ($NS->ensure_in_transaction_BANG_)();
($NS->swap_BANG_)($NS->_STAR_current_transaction_STAR_->ensures, $NS->conj, $__L_ref);
return ($Clojure_Core->deref)($__L_ref);});
$NS->commit_transaction_BANG_ = (function($__L_txn) {  while(true) { $__L_s3077 = \Clojure\Php\seq(($Clojure_Core->deref)($__L_txn->sets));
 while(true) { if ($__L_s3077) { $__L_ref = \Clojure\Php\first($__L_s3077);
$__L_new_val = \Clojure\Php\get_(($Clojure_Core->deref)($__L_txn->values), $__L_ref);
$__L_when_let__3078 = $__L_ref->validator;
if ($__L_when_let__3078) { $__L_validator = $__L_when_let__3078;
if (call_user_func($__L_validator, $__L_new_val)) { null;
} else { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L_new_val));
}
}
$__recur_0 = \Clojure\Php\next_($__L_s3077); $__L_s3077 = $__recur_0; continue;}
 break; }
$__L_s3079 = \Clojure\Php\seq(($Clojure_Core->deref)($__L_txn->commutes));
 while(true) { if ($__L_s3079) { $__L___dest_16 = \Clojure\Php\first($__L_s3079);
$__L_ref = \Clojure\Php\nth($__L___dest_16, 0);
$__L_commute_ops = \Clojure\Php\nth($__L___dest_16, 1);
$__L_base_val = ((function() { $__L_or__3080 = \Clojure\Php\get_(($Clojure_Core->deref)($__L_txn->values), $__L_ref); if ($__L_or__3080) { return $__L_or__3080;} else { return $__L_ref->val;} })());
$__L_final_val = ($NS->reduce)((function($__L_v, $__L___dest_17) use (&$__L_s3079, &$__L_ref, &$__L_txn, &$__L___dest_16, &$__L_commute_ops, &$__L_base_val) { $__L___dest_18 = $__L___dest_17;
$__L_f = \Clojure\Php\nth($__L___dest_18, 0);
$__L_args = \Clojure\Php\nth($__L___dest_18, 1);
return ($NS->apply)($__L_f, $__L_v, $__L_args);}), $__L_base_val, $__L_commute_ops);
($NS->swap_BANG_)($__L_txn->values, $NS->assoc, $__L_ref, $__L_final_val);
$__recur_0 = \Clojure\Php\next_($__L_s3079); $__L_s3079 = $__recur_0; continue;}
 break; }
$__L_s3081 = \Clojure\Php\seq(($Clojure_Core->deref)($__L_txn->values));
 while(true) { if ($__L_s3081) { $__L___dest_19 = \Clojure\Php\first($__L_s3081);
$__L_ref = \Clojure\Php\nth($__L___dest_19, 0);
$__L_new_val = \Clojure\Php\nth($__L___dest_19, 1);
$__L_old_val = $__L_ref->val;
$__L_ref->val = $__L_new_val;
if (\Clojure\Php\equals($__L_old_val, $__L_new_val)) { null;
} else { ($P->_notify_watches)($__L_ref, $__L_old_val, $__L_new_val);
}
$__recur_0 = \Clojure\Php\next_($__L_s3081); $__L_s3081 = $__recur_0; continue;} break; }
 break; }});
$NS->run_in_transaction = (function($__L_f) { if ($NS->_STAR_current_transaction_STAR_) { return call_user_func($__L_f);} else { $__L_txn = ($NS->make_transaction)();
try { return ($NS->binding)(\Clojure\Php\vec($NS->_STAR_current_transaction_STAR_, $__L_txn), ((function() { $__L_result = call_user_func($__L_f); ($NS->commit_transaction_BANG_)($__L_txn);
return $__L_result; })())); } catch (\Throwable $__L_e) { throw $__L_e;
 }
}});
$NS->ref_history_count = (function($__L_ref) { return 0;});
$NS->ref_min_history = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_ref = $__args[0]; $__L_n = $__args[1]; $__L_ref->min_history = $__L_n;
return $__L_ref; } else if ($__n == 1) { $__L_ref = $__args[0]; return $__L_ref->min_history; } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->ref_max_history = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_ref = $__args[0]; $__L_n = $__args[1]; $__L_ref->max_history = $__L_n;
return $__L_ref; } else if ($__n == 1) { $__L_ref = $__args[0]; return $__L_ref->max_history; } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.agent'), 'Agent - Asynchronous reference type.

   Agents provide independent, asynchronous updates to shared state.
   Actions sent to agents are queued and executed asynchronously.

   In environments without true concurrency (PHP), agents execute
   actions in order but still provide the same API for portability.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
$NS->agent_queue = ($NS->atom)(\Clojure\Php\vec());
$NS->_STAR_agent_STAR_ = null;
call_user_func('\Clojure\Php\defType', 'Agent', array('val', 'error', 'error-handler', 'error-mode', 'validator', 'watches', '_meta', 'action-queue'), array('val', 'error', 'error-handler', 'error-mode', 'validator', 'watches', 'action-queue'), array('-deref', (function($__L__) { if ($__L__->error) { throw $__L__->error;
}
return $__L__->val;}), '-set-validator!', (function($__L__, $__L_vf) { if (($__L_vf ? (!call_user_func($__L_vf, $__L__->val)) : false)) { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L__->val));
}
return $__L__->validator = $__L_vf;}), '-get-validator', (function($__L__) { return $__L__->validator;}), '-add-watch', (function($__L_this, $__L_k, $__L_f) { $__L_this->watches = \Clojure\Php\assoc($__L_this->watches, $__L_k, $__L_f);
return $__L_this;}), '-remove-watch', (function($__L_this, $__L_k) { $__L_this->watches = \Clojure\Php\dissoc($__L_this->watches, $__L_k);
return $__L_this;}), '-notify-watches', (function($__L_this, $__L_old_val, $__L_new_val) {  while(true) { $__L_s3082 = \Clojure\Php\seq($__L_this->watches);
 while(true) { if ($__L_s3082) { $__L___dest_20 = \Clojure\Php\first($__L_s3082);
$__L_k = \Clojure\Php\nth($__L___dest_20, 0);
$__L_f = \Clojure\Php\nth($__L___dest_20, 1);
call_user_func($__L_f, $__L_k, $__L_this, $__L_old_val, $__L_new_val);
$__recur_0 = \Clojure\Php\next_($__L_s3082); $__L_s3082 = $__recur_0; continue;} break; }
 break; }}), '-meta', (function($__L__) { return $__L__->_meta;}), '-with-meta', (function($__L__, $__L_m) { throw ($NS->ex_info)('Agents don\'t support with-meta directly', \Clojure\Php\hashMap());
}), '-equiv', (function($__L_this, $__L_other) { return ($__L_this === $__L_other);}), '-hash', (function($__L_this) { return System::identityHashCode($__L_this);})));
$NS->__GT_Agent = (function($__L_val, $__L_error, $__L_error_handler, $__L_error_mode, $__L_validator, $__L_watches, $__L__meta, $__L_action_queue) { return call_user_func('\Clojure\Php\createType', 'Agent', $__L_val, $__L_error, $__L_error_handler, $__L_error_mode, $__L_validator, $__L_watches, $__L__meta, $__L_action_queue);});
$NS->agent = (function(...$__args) { $__n = count($__args); if ($__n == 1) { $__L_val = $__args[0]; return ($NS->__GT_Agent)($__L_val, null, null, \Clojure\Php\Kw::create('fail'), null, \Clojure\Php\hashMap(), null, ($NS->atom)(\Clojure\Php\vec())); } else if ($__n >= 1) { $__L_val = $__args[0]; $__L_options = array_slice($__args, 1); $__L_opts = ($NS->apply)($NS->hash_map, $__L_options);
$__L_meta = \Clojure\Php\get_($__L_opts, \Clojure\Php\Kw::create('meta'));
$__L_validator = \Clojure\Php\get_($__L_opts, \Clojure\Php\Kw::create('validator'));
$__L_error_handler = \Clojure\Php\get_($__L_opts, \Clojure\Php\Kw::create('error-handler'));
$__L_error_mode = \Clojure\Php\get_($__L_opts, \Clojure\Php\Kw::create('error-mode'), \Clojure\Php\Kw::create('fail'));
if (($__L_validator ? (!call_user_func($__L_validator, $__L_val)) : false)) { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L_val));
}
return ($NS->__GT_Agent)($__L_val, null, $__L_error_handler, $__L_error_mode, $__L_validator, \Clojure\Php\hashMap(), $__L_meta, ($NS->atom)(\Clojure\Php\vec())); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->agent_QMARK_ = (function($__L_x) { return ($NS->instance_QMARK_)($NS->Agent, $__L_x);});
$NS->agent_error = (function($__L_a) { return $__L_a->error;});
$NS->restart_agent = (function($__L_a, $__L_new_state, ...$__L_options) { $__L_opts = ($NS->apply)($NS->hash_map, $__L_options);
$__L_clear_actions_QMARK_ = \Clojure\Php\get_($__L_opts, \Clojure\Php\Kw::create('clear-actions'), false);
if ($__L_a->error) { null;
} else { throw ($NS->ex_info)('Agent does not have an error', \Clojure\Php\hashMap());
}
$__L_when_let__3083 = $__L_a->validator;
if ($__L_when_let__3083) { $__L_validator = $__L_when_let__3083;
if (call_user_func($__L_validator, $__L_new_state)) { null;
} else { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L_new_state));
}
}
$__L_a->val = $__L_new_state;
$__L_a->error = null;
if ($__L_clear_actions_QMARK_) { ($NS->reset_BANG_)($__L_a->action_queue, \Clojure\Php\vec());
}
($NS->process_agent_queue_BANG_)($__L_a);
return $__L_new_state;});
$NS->set_error_handler_BANG_ = (function($__L_a, $__L_f) { $__L_a->error_handler = $__L_f;
return $__L_a;});
$NS->error_handler = (function($__L_a) { return $__L_a->error_handler;});
$NS->set_error_mode_BANG_ = (function($__L_a, $__L_mode) { if (\Clojure\Php\contains(\Clojure\Php\hashSet(\Clojure\Php\Kw::create('continue'), \Clojure\Php\Kw::create('fail')), $__L_mode)) { null;
} else { throw ($NS->ex_info)('Invalid error mode', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('mode'), $__L_mode));
}
$__L_a->error_mode = $__L_mode;
return $__L_a;});
$NS->error_mode = (function($__L_a) { return $__L_a->error_mode;});
$NS->execute_action_BANG_ = (function($__L_a, $__L_f, $__L_args) { try { $__L_old_val = $__L_a->val;
$__L_new_val = ($NS->binding)(\Clojure\Php\vec($NS->_STAR_agent_STAR_, $__L_a), ($NS->apply)($__L_f, $__L_old_val, $__L_args));
$__L_when_let__3084 = $__L_a->validator;
if ($__L_when_let__3084) { $__L_validator = $__L_when_let__3084;
if (call_user_func($__L_validator, $__L_new_val)) { null;
} else { throw ($NS->ex_info)('Invalid reference state', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('val'), $__L_new_val));
}
}
$__L_a->val = $__L_new_val;
return ($P->_notify_watches)($__L_a, $__L_old_val, $__L_new_val); } catch (\Throwable $__L_e) { if (\Clojure\Php\equals(\Clojure\Php\Kw::create('continue'), $__L_a->error_mode)) { $__L_when_let__3085 = $__L_a->error_handler;
if ($__L_when_let__3085) { $__L_handler = $__L_when_let__3085;
try { return call_user_func($__L_handler, $__L_a, $__L_e); } catch (\Throwable $__L__) { return null; }
}} else { return $__L_a->error = $__L_e;} }
});
$NS->process_agent_queue_BANG_ = (function($__L_a) {  while(true) { if ($__L_a->error) { return null;} else {  while(true) { $__L_when_let__3086 = \Clojure\Php\seq(($Clojure_Core->deref)($__L_a->action_queue));
if ($__L_when_let__3086) { $__L_actions = $__L_when_let__3086;
$__L___dest_21 = \Clojure\Php\first($__L_actions);
$__L_f = \Clojure\Php\nth($__L___dest_21, 0);
$__L_args = \Clojure\Php\nth($__L___dest_21, 1);
($NS->swap_BANG_)($__L_a->action_queue, ($NS->comp)($NS->vec, $NS->rest));
($NS->execute_action_BANG_)($__L_a, $__L_f, $__L_args);
if ($__L_a->error) { return null;} else { continue;}} break; }
} break; }});
$NS->send = (function($__L_a, $__L_f, ...$__L_args) { if ($__L_a->error) { throw ($NS->ex_info)('Agent has errors', \Clojure\Php\hashMap(\Clojure\Php\Kw::create('agent'), $__L_a, \Clojure\Php\Kw::create('error'), $__L_a->error));
}
($NS->swap_BANG_)($__L_a->action_queue, $NS->conj, \Clojure\Php\vec($__L_f, $__L_args));
($NS->process_agent_queue_BANG_)($__L_a);
return $__L_a;});
$NS->send_off = (function($__L_a, $__L_f, ...$__L_args) { return ($NS->apply)($NS->send, $__L_a, $__L_f, $__L_args);});
$NS->send_via = (function($__L_executor, $__L_a, $__L_f, ...$__L_args) { return ($NS->apply)($NS->send, $__L_a, $__L_f, $__L_args);});
$NS->await = (function(...$__L_agents) { return null;});
$NS->await_for = (function($__L_timeout_ms, ...$__L_agents) { return true;});
$NS->release_pending_sends = (function() { return null;});
$NS->_STAR_agent_STAR__fn = (function() { return $NS->_STAR_agent_STAR_;});
$NS->shutdown_agents = (function() { return null;});
($NS->ns)(\Clojure\Php\Sym::create('clojure.lang.ex'), 'Exception types and utilities.

   Clojure uses ex-info/ex-data for rich exception information.
   This provides the portable implementation.', \Clojure\Php\plist(\Clojure\Php\Kw::create('require'), [\Clojure\Php\Sym::create('clojure.lang.protocols'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('p')], [\Clojure\Php\Sym::create('clojure.lang.kernel'), \Clojure\Php\Kw::create('as'), \Clojure\Php\Sym::create('k')]));
call_user_func('\Clojure\Php\defType', 'ExceptionInfo', array('message', 'data', 'cause'), array(), array('-data', (function($__L__) { return $__L__->data;}), '-message', (function($__L__) { return $__L__->message;}), '-cause', (function($__L__) { return $__L__->cause;}), '-lookup', (function($__L__, $__L_k) { return \Clojure\Php\get_($__L__->data, $__L_k);}), '-lookup', (function($__L__, $__L_k, $__L_not_found) { return \Clojure\Php\get_($__L__->data, $__L_k, $__L_not_found);})));
$NS->__GT_ExceptionInfo = (function($__L_message, $__L_data, $__L_cause) { return call_user_func('\Clojure\Php\createType', 'ExceptionInfo', $__L_message, $__L_data, $__L_cause);});
$NS->ex_info = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_msg = $__args[0]; $__L_data = $__args[1]; $__L_cause = $__args[2]; return ($NS->__GT_ExceptionInfo)($__L_msg, $__L_data, $__L_cause); } else if ($__n == 2) { $__L_msg = $__args[0]; $__L_data = $__args[1]; return ($NS->__GT_ExceptionInfo)($__L_msg, $__L_data, null); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->ex_data = (function($__L_ex) { if (($NS->instance_QMARK_)($NS->ExceptionInfo, $__L_ex)) { return $__L_ex->data;}});
$NS->ex_message = (function($__L_ex) { if (($NS->instance_QMARK_)($NS->ExceptionInfo, $__L_ex)) { return $__L_ex->message;} else { return $__L_ex->getMessage();}});
$NS->ex_cause = (function($__L_ex) { if (($NS->instance_QMARK_)($NS->ExceptionInfo, $__L_ex)) { return $__L_ex->cause;} else { return $__L_ex->getCause();}});
$NS->ex_info_QMARK_ = (function($__L_x) { return ($NS->instance_QMARK_)($NS->ExceptionInfo, $__L_x);});
$NS->_STAR_assert_STAR_ = true;
$NS->print_stack_trace = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_tr = $__args[0]; $__L_out = $__args[1]; return $__L_tr->printStackTrace($__L_out); } else if ($__n == 1) { $__L_tr = $__args[0]; return ($NS->print_stack_trace)($__L_tr, $NS->_STAR_out_STAR_); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->get_stack_trace = (function($__L_ex) { return \Clojure\Php\seq($__L_ex->getStackTrace());});
$NS->stack_element_str = (function($__L_el) { return ((string)$__L_el);});
$NS->root_cause = (function($__L_ex) {  while(true) { $__L_cause = $__L_ex;
 while(true) { $__L_if_let__3087 = ($NS->ex_cause)($__L_cause);
if ($__L_if_let__3087) { $__L_c = $__L_if_let__3087;
$__recur_0 = $__L_c; $__L_cause = $__recur_0; continue;} else { return $__L_cause;} break; }
 break; }});
$NS->exception_chain = (function($__L_ex) { return ($NS->lazy_seq)(($__L_ex ? \Clojure\Php\cons($__L_ex, ($NS->exception_chain)(($NS->ex_cause)($__L_ex))) : null));});
$NS->get_suppressed = (function($__L_ex) { try { return \Clojure\Php\seq($__L_ex->getSuppressed()); } catch (\Throwable $__L__) { return null; }
});
$NS->add_suppressed = (function($__L_ex, $__L_suppressed) { try { return $__L_ex->addSuppressed($__L_suppressed); } catch (\Throwable $__L__) { return null; }
return $__L_ex;});
$NS->throw_arity = (function($__L_name, $__L_n) { throw ($NS->ex_info)(\Clojure\Php\str_('Wrong number of args (', $__L_n, ') passed to: ', $__L_name), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('arity-exception'), \Clojure\Php\Kw::create('name'), $__L_name, \Clojure\Php\Kw::create('arity'), $__L_n));
});
$NS->throw_illegal_arg = (function(...$__L_msg) { throw ($NS->ex_info)(($NS->apply)($NS->str, $__L_msg), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('illegal-argument')));
});
$NS->throw_illegal_state = (function(...$__L_msg) { throw ($NS->ex_info)(($NS->apply)($NS->str, $__L_msg), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('illegal-state')));
});
$NS->throw_unsupported = (function(...$__L_msg) { throw ($NS->ex_info)(($NS->apply)($NS->str, $__L_msg), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('unsupported-operation')));
});
$NS->throw_index_out_of_bounds = (function($__L_n, $__L_count) { throw ($NS->ex_info)(\Clojure\Php\str_('Index ', $__L_n, ' out of bounds for count ', $__L_count), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('index-out-of-bounds'), \Clojure\Php\Kw::create('index'), $__L_n, \Clojure\Php\Kw::create('count'), $__L_count));
});
$NS->throw_key_not_found = (function($__L_key) { throw ($NS->ex_info)(\Clojure\Php\str_('Key not found: ', $__L_key), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('key-not-found'), \Clojure\Php\Kw::create('key'), $__L_key));
});
$NS->throw_class_cast = (function($__L_from, $__L_to) { throw ($NS->ex_info)(\Clojure\Php\str_('Cannot cast ', $__L_from, ' to ', $__L_to), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('class-cast'), \Clojure\Php\Kw::create('from'), $__L_from, \Clojure\Php\Kw::create('to'), $__L_to));
});
$NS->throw_null_pointer = (function(...$__L_msg) { throw ($NS->ex_info)(((function() { $__L_or__3088 = \Clojure\Php\first($__L_msg); if ($__L_or__3088) { return $__L_or__3088;} else { return 'Null pointer exception';} })()), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), \Clojure\Php\Kw::create('null-pointer')));
});
$NS->inc = (function($__L_x) { return ($__L_x + 1);});
$NS->dec = (function($__L_x) { return ($__L_x - 1);});
$NS->identity = (function($__L_x) { return $__L_x;});
$NS->not = (function($__L_x) { if ($__L_x) { return false;} else { return true;}});
$NS->nil_QMARK_ = (function($__L_x) { return is_null($__L_x);});
$NS->apply = (function($__L_f, ...$__L_args) { $__L_flat_args = (\Clojure\Php\next_($__L_args) ? array_merge(array_slice($__L_args, 0, -1), ($NS->last)($__L_args)) : \Clojure\Php\first($__L_args));
return call_user_func_array($__L_f, call_user_func('\Clojure\Php\intoArray', $__L_flat_args));});
$NS->comp = (function(...$__L_fs) { $__L_fs = array_reverse($__L_fs);
return (function(...$__L_args) use (&$__L_fs) {  while(true) { $__L_ret = ($NS->apply)(\Clojure\Php\first($__L_fs), $__L_args);
$__L_fs = \Clojure\Php\next_($__L_fs);
 while(true) { if ($__L_fs) { $__recur_0 = call_user_func(\Clojure\Php\first($__L_fs), $__L_ret); $__recur_1 = \Clojure\Php\next_($__L_fs); $__L_ret = $__recur_0; $__L_fs = $__recur_1; continue;} else { return $__L_ret;} break; }
 break; }});});
$NS->partial = (function($__L_f, ...$__L_args) { return (function(...$__L_more) use (&$__L_args, &$__L_f) { return ($NS->apply)($__L_f, array_merge($__L_args, $__L_more));});});
$NS->count = (function($__L_coll) { if (($__L_coll === null)) { return 0;} else { return count($__L_coll);}});
$NS->seq = (function($__L_coll) { if (($__L_coll === null)) { return null;} else { if (is_array($__L_coll)) { if ((0 === count($__L_coll))) { return null;} else { return $__L_coll;}} else { if (method_exists($__L_coll, 'seq')) { return $__L_coll->seq();} else { return $__L_coll;}}}});
$NS->first = (function($__L_coll) { if (($__L_coll === null)) { return null;} else { if (is_array($__L_coll)) { if ((0 === count($__L_coll))) { return null;} else { return reset($__L_coll);}} else { return $__L_coll->first();}}});
$NS->rest = (function($__L_coll) { if (($__L_coll === null)) { return \Clojure\Php\vec();} else { if (is_array($__L_coll)) { return array_slice($__L_coll, 1);} else { return $__L_coll->rest();}}});
$NS->next = (function($__L_coll) { if (($__L_coll === null)) { return null;} else { $__L_r = \Clojure\Php\rest($__L_coll);
return \Clojure\Php\seq($__L_r);}});
$NS->cons = (function($__L_x, $__L_coll) { return call_user_func('\Clojure\Php\cons', $__L_x, $__L_coll);});
$NS->vector = (function(...$__L_args) { return call_user_func_array('\Clojure\Php\vec', $__L_args);});
$NS->conj = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_coll = $__args[0]; $__L_x = $__args[1]; return call_user_func('\Clojure\Php\conj', $__L_coll, $__L_x); } else if ($__n >= 2) { $__L_coll = $__args[0]; $__L_x = $__args[1]; $__L_xs = array_slice($__args, 2); return ($NS->reduce)($NS->conj, \Clojure\Php\conj($__L_coll, $__L_x), $__L_xs); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->reduced = (function($__L_x) { return call_user_func('\Clojure\Php\reduced', $__L_x);});
$NS->reduced_QMARK_ = (function($__L_x) { return call_user_func('\Clojure\Php\isReduced', $__L_x);});
$NS->unreduced = (function($__L_x) { if (($NS->reduced_QMARK_)($__L_x)) { return call_user_func('\Clojure\Php\unreduced', $__L_x);} else { return $__L_x;}});
$NS->reduce = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_f = $__args[0]; $__L_val = $__args[1]; $__L_coll = $__args[2];  while(true) { $__L_val = $__L_val;
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_ret = call_user_func($__L_f, $__L_val, \Clojure\Php\first($__L_coll));
if (($NS->reduced_QMARK_)($__L_ret)) { return ($NS->unreduced)($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_val = $__recur_0; $__L_coll = $__recur_1; continue;}} else { return $__L_val;} break; }
 break; } } else if ($__n == 2) { $__L_f = $__args[0]; $__L_coll = $__args[1]; if (\Clojure\Php\seq($__L_coll)) { return ($NS->reduce)($__L_f, \Clojure\Php\first($__L_coll), \Clojure\Php\next_($__L_coll));} else { return call_user_func($__L_f);} } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->reduce_kv = (function($__L_f, $__L_init, $__L_coll) {  while(true) { if (($__L_coll === null)) { return $__L_init;} else { $__L_acc = $__L_init;
$__L_ks = ($NS->keys)($__L_coll);
 while(true) { if (\Clojure\Php\seq($__L_ks)) { $__L_k = \Clojure\Php\first($__L_ks);
$__L_ret = call_user_func($__L_f, $__L_acc, $__L_k, \Clojure\Php\get_($__L_coll, $__L_k));
if (($NS->reduced_QMARK_)($__L_ret)) { return ($NS->unreduced)($__L_ret);} else { $__recur_0 = $__L_ret; $__recur_1 = \Clojure\Php\next_($__L_ks); $__L_acc = $__recur_0; $__L_ks = $__recur_1; continue;}} else { return $__L_acc;} break; }
} break; }});
$NS->last = (function($__L_coll) {  while(true) { $__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\next_($__L_coll)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__L_coll = $__recur_0; continue;} else { return \Clojure\Php\first($__L_coll);} break; }
 break; }});
$NS->second = (function($__L_coll) { return \Clojure\Php\first(\Clojure\Php\next_($__L_coll));});
$NS->map = (function($__L_f, $__L_coll) { return (new \Clojure\Php\LazySeq((function() use (&$__L_coll, &$__L_f) { if (\Clojure\Php\seq($__L_coll)) { return \Clojure\Php\cons(call_user_func($__L_f, \Clojure\Php\first($__L_coll)), ($NS->map)($__L_f, \Clojure\Php\next_($__L_coll)));}})));});
$NS->filter = (function($__L_pred, $__L_coll) { return (new \Clojure\Php\LazySeq((function() use (&$__L_pred, &$__L_coll) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
if (call_user_func($__L_pred, $__L_x)) { return \Clojure\Php\cons($__L_x, ($NS->filter)($__L_pred, \Clojure\Php\next_($__L_coll)));} else { return ($NS->filter)($__L_pred, \Clojure\Php\next_($__L_coll));}}})));});
$NS->mapv = (function($__L_f, $__L_coll) {  while(true) { 'Eager version of map - returns a vector';
$__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__recur_0 = \Clojure\Php\conj($__L_result, call_user_func($__L_f, \Clojure\Php\first($__L_coll))); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_result;} break; }
 break; }});
$NS->filterv = (function($__L_pred, $__L_coll) {  while(true) { 'Eager version of filter - returns a vector';
$__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
$__recur_0 = (call_user_func($__L_pred, $__L_x) ? \Clojure\Php\conj($__L_result, $__L_x) : $__L_result); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_result;} break; }
 break; }});
$NS->remove = (function($__L_pred, $__L_coll) { return ($NS->filter)((function($__L_x) use (&$__L_pred, &$__L_coll) { return (!call_user_func($__L_pred, $__L_x));}), $__L_coll);});
$NS->mapcat = (function($__L_f, $__L_coll) { return ($NS->apply)($NS->concat, ($NS->map)($__L_f, $__L_coll));});
$NS->map_indexed = (function($__L_f, $__L_coll) {  while(true) { $__L_result = \Clojure\Php\vec();
$__L_idx = 0;
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__recur_0 = \Clojure\Php\conj($__L_result, call_user_func($__L_f, $__L_idx, \Clojure\Php\first($__L_coll))); $__recur_1 = ($__L_idx + 1); $__recur_2 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_idx = $__recur_1; $__L_coll = $__recur_2; continue;} else { return $__L_result;} break; }
 break; }});
$NS->keep = (function($__L_f, $__L_coll) {  while(true) { $__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_v = call_user_func($__L_f, \Clojure\Php\first($__L_coll));
$__recur_0 = (($__L_v === null) ? $__L_result : \Clojure\Php\conj($__L_result, $__L_v)); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_result;} break; }
 break; }});
$NS->keep_indexed = (function($__L_f, $__L_coll) {  while(true) { $__L_result = \Clojure\Php\vec();
$__L_idx = 0;
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_v = call_user_func($__L_f, $__L_idx, \Clojure\Php\first($__L_coll));
$__recur_0 = (($__L_v === null) ? $__L_result : \Clojure\Php\conj($__L_result, $__L_v)); $__recur_1 = ($__L_idx + 1); $__recur_2 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_idx = $__recur_1; $__L_coll = $__recur_2; continue;} else { return $__L_result;} break; }
 break; }});
$NS->flatten = (function($__L_coll) {  while(true) { $__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
if ((is_array($__L_x) ? is_array($__L_x) : ($NS->coll_QMARK_)($__L_x))) { $__recur_0 = ($NS->into)($__L_result, ($NS->flatten)($__L_x)); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;} else { $__recur_0 = \Clojure\Php\conj($__L_result, $__L_x); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;}} else { return $__L_result;} break; }
 break; }});
$NS->interleave = (function($__L_c1, $__L_c2) {  while(true) { $__L_result = \Clojure\Php\vec();
$__L_s1 = $__L_c1;
$__L_s2 = $__L_c2;
 while(true) { if ((\Clojure\Php\seq($__L_s1) ? \Clojure\Php\seq($__L_s2) : false)) { $__recur_0 = \Clojure\Php\conj(\Clojure\Php\conj($__L_result, \Clojure\Php\first($__L_s1)), \Clojure\Php\first($__L_s2)); $__recur_1 = \Clojure\Php\next_($__L_s1); $__recur_2 = \Clojure\Php\next_($__L_s2); $__L_result = $__recur_0; $__L_s1 = $__recur_1; $__L_s2 = $__recur_2; continue;} else { return $__L_result;} break; }
 break; }});
$NS->interpose = (function($__L_sep, $__L_coll) {  while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_result = \Clojure\Php\vec(\Clojure\Php\first($__L_coll));
$__L_coll = \Clojure\Php\next_($__L_coll);
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__recur_0 = \Clojure\Php\conj(\Clojure\Php\conj($__L_result, $__L_sep), \Clojure\Php\first($__L_coll)); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_result = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_result;} break; }
} else { return \Clojure\Php\vec();} break; }});
$NS->_EQ_ = (function(...$__L_args) {  while(true) { if ((count($__L_args) < 2)) { return true;} else { $__L_first_arg = array_shift($__L_args);
$__L_args = $__L_args;
 while(true) { if (\Clojure\Php\seq($__L_args)) { if (call_user_func('\Clojure\Php\equals', $__L_first_arg, \Clojure\Php\first($__L_args))) { $__recur_0 = \Clojure\Php\next_($__L_args); $__L_args = $__recur_0; continue;} else { return false;}} else { return true;} break; }
} break; }});
$NS->not_EQ_ = (function(...$__L_args) { return (!($NS->apply)($NS->_EQ_, $__L_args));});
$NS->atom = (function($__L_x) { return (new \Clojure\Php\Atom($__L_x));});
$NS->deref = (function($__L_x) { return $__L_x->deref();});
$NS->reset_BANG_ = (function($__L_x, $__L_newval) { $__L_x->reset($__L_newval);
return $__L_newval;});
$NS->swap_BANG_ = (function($__L_x, $__L_f, ...$__L_args) { $__L_old = $__L_x->deref();
$__L_new = ($NS->apply)($__L_f, $__L_old, $__L_args);
$__L_x->reset($__L_new);
return $__L_new;});
$NS->assoc = (function($__L_map, $__L_key, $__L_val, ...$__L_kvs) { $__L_m = call_user_func('\Clojure\Php\assoc', $__L_map, $__L_key, $__L_val);
if ($__L_kvs) { return ($NS->apply)($NS->assoc, $__L_m, $__L_kvs);} else { return $__L_m;}});
$NS->update = (function($__L_m, $__L_k, $__L_f, ...$__L_args) { return \Clojure\Php\assoc($__L_m, $__L_k, ($NS->apply)($__L_f, \Clojure\Php\get_($__L_m, $__L_k), $__L_args));});
$NS->get = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_m = $__args[0]; $__L_k = $__args[1]; $__L_not_found = $__args[2]; $__L_val = call_user_func('\Clojure\Php\get', $__L_m, $__L_k);
if (($__L_val === null)) { return $__L_not_found;} else { return $__L_val;} } else if ($__n == 2) { $__L_m = $__args[0]; $__L_k = $__args[1]; return call_user_func('\Clojure\Php\get', $__L_m, $__L_k); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->str = (function(...$__L_args) { return implode('', $__L_args);});
$NS->print = (function(...$__L_more) { return print(($NS->apply)($NS->str, $__L_more));});
$NS->println = (function(...$__L_more) { print(($NS->apply)($NS->str, $__L_more));
return print(PHP_EOL);});
$NS->pr_str = (function(...$__L_xs) { 'Returns a string containing the printed representation of xs (EDN format)';
return implode(' ', call_user_func('\Clojure\Php\toArray', ($NS->map)((function($__L_x) use (&$__L_xs) { return call_user_func('\Clojure\Php\prStr', $__L_x);}), $__L_xs)));});
$NS->prn_str = (function(...$__L_xs) { 'Same as pr-str but with a trailing newline';
return \Clojure\Php\str_(($NS->apply)($NS->pr_str, $__L_xs), PHP_EOL);});
$NS->pr = (function(...$__L_xs) { 'Prints the object(s) in a machine-readable form';
return print(($NS->apply)($NS->pr_str, $__L_xs));});
$NS->prn = (function(...$__L_xs) { 'Same as pr followed by newline';
print(($NS->apply)($NS->pr_str, $__L_xs));
return print(PHP_EOL);});
$NS->print_str = (function(...$__L_xs) { 'Returns a string of the values printed by print';
return ($NS->apply)($NS->str, $__L_xs);});
$NS->_PLUS_ = (function(...$__L_xs) { return ($NS->reduce)((function($__L_a, $__L_b) use (&$__L_xs) { return ($__L_a + $__L_b);}), 0, $__L_xs);});
$NS->_ = (function($__L_x, ...$__L_xs) { if ($__L_xs) { return ($NS->reduce)((function($__L_a, $__L_b) use (&$__L_x, &$__L_xs) { return ($__L_a - $__L_b);}), $__L_x, $__L_xs);} else { return (0 - $__L_x);}});
$NS->_STAR_ = (function(...$__L_xs) { return ($NS->reduce)((function($__L_a, $__L_b) use (&$__L_xs) { return ($__L_a * $__L_b);}), 1, $__L_xs);});
$NS->_SLASH_ = (function($__L_x, ...$__L_xs) { if ($__L_xs) { return ($NS->reduce)((function($__L_a, $__L_b) use (&$__L_x, &$__L_xs) { return ($__L_a / $__L_b);}), $__L_x, $__L_xs);} else { return (1 / $__L_x);}});
$NS->mod = (function($__L_num, $__L_div) { $__L_m = ($__L_num % $__L_div);
if (((0 === $__L_m) ? (0 === $__L_m) : (($__L_num > 0) === ($__L_div > 0)))) { return $__L_m;} else { return ($__L_m + $__L_div);}});
$NS->rem = (function($__L_num, $__L_div) { return ($__L_num % $__L_div);});
$NS->quot = (function($__L_num, $__L_div) { return intdiv($__L_num, $__L_div);});
$NS->max = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; if (($__L_x > $__L_y)) { return $__L_x;} else { return $__L_y;} } else if ($__n == 1) { $__L_x = $__args[0]; return $__L_x; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->max, ($NS->max)($__L_x, $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->min = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; if (($__L_x < $__L_y)) { return $__L_x;} else { return $__L_y;} } else if ($__n == 1) { $__L_x = $__args[0]; return $__L_x; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return ($NS->reduce)($NS->min, ($NS->min)($__L_x, $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->abs = (function($__L_a) { return abs($__L_a);});
$NS->zero_QMARK_ = (function($__L_x) { return (0 === $__L_x);});
$NS->pos_QMARK_ = (function($__L_x) { return ($__L_x > 0);});
$NS->neg_QMARK_ = (function($__L_x) { return ($__L_x < 0);});
$NS->even_QMARK_ = (function($__L_n) { return (0 === ($__L_n % 2));});
$NS->odd_QMARK_ = (function($__L_n) { return (1 === abs(($__L_n % 2)));});
$NS->string_QMARK_ = (function($__L_x) { return is_string($__L_x);});
$NS->int_QMARK_ = (function($__L_x) { return is_int($__L_x);});
$NS->integer_QMARK_ = (function($__L_x) { return is_int($__L_x);});
$NS->float_QMARK_ = (function($__L_x) { return is_float($__L_x);});
$NS->number_QMARK_ = (function($__L_x) { return is_numeric($__L_x);});
$NS->boolean_QMARK_ = (function($__L_x) { return is_bool($__L_x);});
$NS->true_QMARK_ = (function($__L_x) { return ($__L_x === true);});
$NS->false_QMARK_ = (function($__L_x) { return ($__L_x === false);});
$NS->array_QMARK_ = (function($__L_x) { return is_array($__L_x);});
$NS->fn_QMARK_ = (function($__L_x) { return is_callable($__L_x);});
$NS->symbol_QMARK_ = (function($__L_x) { return is_string($__L_x);});
$NS->keyword_QMARK_ = (function($__L_x) { return ($__L_x instanceof \Clojure\Php\Kw);});
$NS->vector_QMARK_ = (function($__L_x) { return ($__L_x instanceof \Clojure\Php\Vec);});
$NS->map_QMARK_ = (function($__L_x) { return ($__L_x instanceof \Clojure\Php\Map);});
$NS->set_QMARK_ = (function($__L_x) { return ($__L_x instanceof \Clojure\Php\Set);});
$NS->list_QMARK_ = (function($__L_x) { return ($__L_x instanceof \Clojure\Php\PList);});
$NS->coll_QMARK_ = (function($__L_x) { if (($__L_x instanceof \Clojure\Php\Vec)) { return ($__L_x instanceof \Clojure\Php\Vec);} else { if (($__L_x instanceof \Clojure\Php\Map)) { return ($__L_x instanceof \Clojure\Php\Map);} else { if (($__L_x instanceof \Clojure\Php\Set)) { return ($__L_x instanceof \Clojure\Php\Set);} else { if (($__L_x instanceof \Clojure\Php\PList)) { return ($__L_x instanceof \Clojure\Php\PList);} else { return is_array($__L_x);}}}}});
$NS->seq_QMARK_ = (function($__L_x) { if (($__L_x instanceof \Clojure\Php\Vec)) { return ($__L_x instanceof \Clojure\Php\Vec);} else { return ($__L_x instanceof \Clojure\Php\PList);}});
$NS->empty_QMARK_ = (function($__L_x) { return (0 === \Clojure\Php\count_($__L_x));});
$NS->some_QMARK_ = (function($__L_x) { return (!($__L_x === null));});
$NS->keys = (function($__L_m) { if (($__L_m === null)) { return null;} else { return call_user_func('\Clojure\Php\keys_', $__L_m);}});
$NS->vals = (function($__L_m) { if (($__L_m === null)) { return null;} else { return call_user_func('\Clojure\Php\vals', $__L_m);}});
$NS->contains_QMARK_ = (function($__L_coll, $__L_key) { if (($__L_coll === null)) { return false;} else { return call_user_func('\Clojure\Php\contains', $__L_coll, $__L_key);}});
$NS->take = (function($__L_n, $__L_coll) {  while(true) { $__L_n = $__L_n;
$__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if ((($__L_n > 0) ? \Clojure\Php\seq($__L_coll) : false)) { $__L_x = \Clojure\Php\first($__L_coll);
$__recur_0 = ($__L_n - 1); $__recur_1 = \Clojure\Php\next_($__L_coll); $__recur_2 = \Clojure\Php\conj($__L_result, $__L_x); $__L_n = $__recur_0; $__L_coll = $__recur_1; $__L_result = $__recur_2; continue;} else { return $__L_result;} break; }
 break; }});
$NS->drop = (function($__L_n, $__L_coll) {  while(true) { $__L_n = $__L_n;
$__L_coll = $__L_coll;
 while(true) { if ((($__L_n > 0) ? \Clojure\Php\seq($__L_coll) : false)) { $__recur_0 = ($__L_n - 1); $__recur_1 = \Clojure\Php\next_($__L_coll); $__L_n = $__recur_0; $__L_coll = $__recur_1; continue;} else { return $__L_coll;} break; }
 break; }});
$NS->take_while = (function($__L_pred, $__L_coll) {  while(true) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
if (call_user_func($__L_pred, $__L_x)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_x); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;}} else { return $__L_result;} break; }
 break; }});
$NS->drop_while = (function($__L_pred, $__L_coll) {  while(true) { $__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { if (call_user_func($__L_pred, \Clojure\Php\first($__L_coll))) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__L_coll = $__recur_0; continue;} else { return $__L_coll;}} else { return $__L_coll;} break; }
 break; }});
$NS->concat = (function(...$__L_colls) { return ($NS->reduce)((function($__L_acc, $__L_c) use (&$__L_colls) {  while(true) { $__L_acc = $__L_acc;
$__L_c = $__L_c;
 while(true) { if (\Clojure\Php\seq($__L_c)) { $__recur_0 = \Clojure\Php\conj($__L_acc, \Clojure\Php\first($__L_c)); $__recur_1 = \Clojure\Php\next_($__L_c); $__L_acc = $__recur_0; $__L_c = $__recur_1; continue;} else { return $__L_acc;} break; }
 break; }}), \Clojure\Php\vec(), $__L_colls);});
$NS->reverse = (function($__L_coll) { $__L_arr = call_user_func('\Clojure\Php\intoArray', $__L_coll);
return array_reverse($__L_arr);});
$NS->nth = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_coll = $__args[0]; $__L_n = $__args[1]; $__L_not_found = $__args[2];  while(true) { if (($__L_coll instanceof \Clojure\Php\Vec)) { if ((($__L_n >= 0) ? ($__L_n < \Clojure\Php\count_($__L_coll)) : false)) { return call_user_func('\Clojure\Php\get', $__L_coll, $__L_n);} else { return $__L_not_found;}} else { $__L_coll = $__L_coll;
$__L_n = $__L_n;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { if (($__L_n === 0)) { return \Clojure\Php\first($__L_coll);} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = ($__L_n - 1); $__L_coll = $__recur_0; $__L_n = $__recur_1; continue;}} else { return $__L_not_found;} break; }
} break; } } else if ($__n == 2) { $__L_coll = $__args[0]; $__L_n = $__args[1]; return \Clojure\Php\nth($__L_coll, $__L_n); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->range3 = (function($__L_start, $__L_end, $__L_step) {  while(true) { $__L_n = $__L_start;
$__L_result = \Clojure\Php\vec();
 while(true) { if (($__L_n < $__L_end)) { $__L_current = $__L_n;
$__recur_0 = ($__L_n + $__L_step); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_current); $__L_n = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }
 break; }});
$NS->range = (function($__L_end_or_start, ...$__L_args) { if ((\Clojure\Php\first($__L_args) === null)) { return ($NS->range3)(0, $__L_end_or_start, 1);} else { $__L_end = \Clojure\Php\first($__L_args);
$__L_step = (\Clojure\Php\second($__L_args) ? \Clojure\Php\second($__L_args) : 1);
return ($NS->range3)($__L_end_or_start, $__L_end, $__L_step);}});
$NS->repeat = (function($__L_n, $__L_x) {  while(true) { $__L_i = $__L_n;
$__L_result = \Clojure\Php\vec();
 while(true) { if (($__L_i > 0)) { $__recur_0 = ($__L_i - 1); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_x); $__L_i = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }
 break; }});
$NS->into = (function($__L_to, $__L_from) { return ($NS->reduce)($NS->conj, $__L_to, $__L_from);});
$NS->zipmap = (function($__L_keys, $__L_vals) {  while(true) { $__L_m = \Clojure\Php\hashMap();
$__L_ks = $__L_keys;
$__L_vs = $__L_vals;
 while(true) { if ((\Clojure\Php\seq($__L_ks) ? \Clojure\Php\seq($__L_vs) : false)) { $__recur_0 = \Clojure\Php\assoc($__L_m, \Clojure\Php\first($__L_ks), \Clojure\Php\first($__L_vs)); $__recur_1 = \Clojure\Php\next_($__L_ks); $__recur_2 = \Clojure\Php\next_($__L_vs); $__L_m = $__recur_0; $__L_ks = $__recur_1; $__L_vs = $__recur_2; continue;} else { return $__L_m;} break; }
 break; }});
$NS->partition = (function(...$__args) { $__n = count($__args); if ($__n == 4) { $__L_n = $__args[0]; $__L_step = $__args[1]; $__L_pad = $__args[2]; $__L_coll = $__args[3];  while(true) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { $__L_chunk = ($NS->take)($__L_n, $__L_coll);
if (($__L_n === \Clojure\Php\count_($__L_chunk))) { $__recur_0 = ($NS->drop)($__L_step, $__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_chunk); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;} else { if (\Clojure\Php\seq($__L_chunk)) { return \Clojure\Php\conj($__L_result, ($NS->take)($__L_n, ($NS->concat)($__L_chunk, $__L_pad)));} else { return $__L_result;}} break; }
 break; } } else if ($__n == 3) { $__L_n = $__args[0]; $__L_step = $__args[1]; $__L_coll = $__args[2];  while(true) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { $__L_chunk = ($NS->take)($__L_n, $__L_coll);
if (($__L_n === \Clojure\Php\count_($__L_chunk))) { $__recur_0 = ($NS->drop)($__L_step, $__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_chunk); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }
 break; } } else if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; return ($NS->partition)($__L_n, $__L_n, $__L_coll); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->distinct = (function($__L_coll) {  while(true) { $__L_coll = $__L_coll;
$__L_seen = \Clojure\Php\hashSet();
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
if (\Clojure\Php\contains($__L_seen, $__L_x)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = $__L_seen; $__recur_2 = $__L_result; $__L_coll = $__recur_0; $__L_seen = $__recur_1; $__L_result = $__recur_2; continue;} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_seen, $__L_x); $__recur_2 = \Clojure\Php\conj($__L_result, $__L_x); $__L_coll = $__recur_0; $__L_seen = $__recur_1; $__L_result = $__recur_2; continue;}} else { return $__L_result;} break; }
 break; }});
$NS->group_by = (function($__L_f, $__L_coll) { return ($NS->reduce)((function($__L_m, $__L_x) use (&$__L_coll, &$__L_f) { $__L_k = call_user_func($__L_f, $__L_x);
return ($NS->update)($__L_m, $__L_k, (function($__L_v) use (&$__L_x, &$__L_m, &$__L_coll, &$__L_k, &$__L_f) { return \Clojure\Php\conj(($__L_v ? $__L_v : \Clojure\Php\vec()), $__L_x);}));}), \Clojure\Php\hashMap(), $__L_coll);});
$NS->frequencies = (function($__L_coll) { return ($NS->reduce)((function($__L_m, $__L_x) use (&$__L_coll) { return ($NS->update)($__L_m, $__L_x, (function($__L_v) use (&$__L_x, &$__L_m, &$__L_coll) { return (($__L_v ? $__L_v : 0) + 1);}));}), \Clojure\Php\hashMap(), $__L_coll);});
$NS->sort = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_comp = $__args[0]; $__L_coll = $__args[1]; $__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
usort($__L_arr, $__L_comp);
return call_user_func('\Clojure\Php\vector', $__L_arr); } else if ($__n == 1) { $__L_coll = $__args[0]; $__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
sort($__L_arr);
return call_user_func('\Clojure\Php\vector', $__L_arr); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->sort_by = (function($__L_keyfn, $__L_coll) { $__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
usort($__L_arr, (function($__L_a, $__L_b) use (&$__L_arr, &$__L_coll, &$__L_keyfn) { $__L_ka = call_user_func($__L_keyfn, $__L_a);
$__L_kb = call_user_func($__L_keyfn, $__L_b);
if (($__L_ka < $__L_kb)) { return -1;} else { if (($__L_ka > $__L_kb)) { return 1;} else { return 0;}}}));
return call_user_func('\Clojure\Php\vector', $__L_arr);});
$NS->merge = (function(...$__L_maps) { return ($NS->reduce)((function($__L_m1, $__L_m2) use (&$__L_maps) { if ($__L_m2) { return ($NS->reduce)((function($__L_m, $__L_kv) use (&$__L_m1, &$__L_maps, &$__L_m2) { return \Clojure\Php\assoc($__L_m, \Clojure\Php\first($__L_kv), \Clojure\Php\second($__L_kv));}), $__L_m1, \Clojure\Php\seq($__L_m2));} else { return $__L_m1;}}), \Clojure\Php\hashMap(), $__L_maps);});
$NS->get_in = (function($__L_m, $__L_ks) { return ($NS->reduce)($NS->get, $__L_m, $__L_ks);});
$NS->assoc_in = (function($__L_m, $__L_ks, $__L_v) { $__L_k = \Clojure\Php\first($__L_ks);
$__L_ks = \Clojure\Php\next_($__L_ks);
if ($__L_ks) { return \Clojure\Php\assoc($__L_m, $__L_k, ($NS->assoc_in)(\Clojure\Php\get_($__L_m, $__L_k), $__L_ks, $__L_v));} else { return \Clojure\Php\assoc($__L_m, $__L_k, $__L_v);}});
$NS->update_in = (function($__L_m, $__L_ks, $__L_f, ...$__L_args) { $__L_k = \Clojure\Php\first($__L_ks);
$__L_ks = \Clojure\Php\next_($__L_ks);
if ($__L_ks) { return \Clojure\Php\assoc($__L_m, $__L_k, ($NS->apply)($NS->update_in, \Clojure\Php\get_($__L_m, $__L_k), $__L_ks, $__L_f, $__L_args));} else { return \Clojure\Php\assoc($__L_m, $__L_k, ($NS->apply)($__L_f, \Clojure\Php\get_($__L_m, $__L_k), $__L_args));}});
$NS->dissoc = (function($__L_m, ...$__L_ks) { return ($NS->reduce)((function($__L_m, $__L_k) use (&$__L_ks) { return call_user_func('\Clojure\Php\dissoc', $__L_m, $__L_k);}), $__L_m, $__L_ks);});
$NS->select_keys = (function($__L_m, $__L_ks) { return ($NS->reduce)((function($__L_result, $__L_k) use (&$__L_ks, &$__L_m) { if (\Clojure\Php\contains($__L_m, $__L_k)) { return \Clojure\Php\assoc($__L_result, $__L_k, \Clojure\Php\get_($__L_m, $__L_k));} else { return $__L_result;}}), \Clojure\Php\hashMap(), $__L_ks);});
$NS->every_QMARK_ = (function($__L_pred, $__L_coll) {  while(true) { $__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { if (call_user_func($__L_pred, \Clojure\Php\first($__L_coll))) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__L_coll = $__recur_0; continue;} else { return false;}} else { return true;} break; }
 break; }});
$NS->some = (function($__L_pred, $__L_coll) {  while(true) { $__L_coll = $__L_coll;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_result = call_user_func($__L_pred, \Clojure\Php\first($__L_coll));
if ($__L_result) { return $__L_result;} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__L_coll = $__recur_0; continue;}} break; }
 break; }});
$NS->not_every_QMARK_ = (function($__L_pred, $__L_coll) { return (!($NS->every_QMARK_)($__L_pred, $__L_coll));});
$NS->not_any_QMARK_ = (function($__L_pred, $__L_coll) { return (!($NS->some)($__L_pred, $__L_coll));});
$NS->constantly = (function($__L_x) { return (function(...$__L_args) use (&$__L_x) { return $__L_x;});});
$NS->complement = (function($__L_f) { return (function(...$__L_args) use (&$__L_f) { return (!($NS->apply)($__L_f, $__L_args));});});
$NS->juxt = (function(...$__L_fs) { return (function(...$__L_args) use (&$__L_fs) { return ($NS->mapv)((function($__L_f) use (&$__L_fs, &$__L_args) { return ($NS->apply)($__L_f, $__L_args);}), $__L_fs);});});
$NS->fnil = (function($__L_f, $__L_x) { return (function($__L_arg, ...$__L_args) use (&$__L_x, &$__L_f) { return ($NS->apply)($__L_f, (($__L_arg === null) ? $__L_x : $__L_arg), $__L_args);});});
$NS->memoize = (function($__L_f) { $__L_cache = ($NS->atom)(\Clojure\Php\hashMap());
return (function(...$__L_args) use (&$__L_cache, &$__L_f) { $__L_key = ($NS->apply)($NS->vector, $__L_args);
$__L_cached = \Clojure\Php\get_(($NS->deref)($__L_cache), $__L_key);
if (($__L_cached !== null)) { return $__L_cached;} else { $__L_result = ($NS->apply)($__L_f, $__L_args);
($NS->swap_BANG_)($__L_cache, $NS->assoc, $__L_key, $__L_result);
return $__L_result;}});});
$NS->compare = (function($__L_x, $__L_y) { if (($__L_x < $__L_y)) { return -1;} else { if (($__L_x > $__L_y)) { return 1;} else { if (\Clojure\Php\Kw::create('else')) { return 0;} else { return null;}}}});
$NS->identical_QMARK_ = (function($__L_x, $__L_y) { return ($__L_x === $__L_y);});
$NS->name = (function($__L_x) { if (($__L_x instanceof \Clojure\Php\Kw)) { return $__L_x->getName();} else { if (($__L_x instanceof \Clojure\Php\Sym)) { return strval($__L_x);} else { if (is_string($__L_x)) { return $__L_x;} else { if (\Clojure\Php\Kw::create('else')) { return strval($__L_x);} else { return null;}}}}});
$NS->namespace = (function($__L_x) { if (($__L_x instanceof \Clojure\Php\Kw)) { return $__L_x->getNamespace();}});
$NS->subs = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_s = $__args[0]; $__L_start = $__args[1]; $__L_end = $__args[2]; return substr($__L_s, $__L_start, ($__L_end - $__L_start)); } else if ($__n == 2) { $__L_s = $__args[0]; $__L_start = $__args[1]; return substr($__L_s, $__L_start); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->interleave = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_c1 = $__args[0]; $__L_c2 = $__args[1];  while(true) { $__L_c1 = $__L_c1;
$__L_c2 = $__L_c2;
$__L_result = \Clojure\Php\vec();
 while(true) { if ((\Clojure\Php\seq($__L_c1) ? \Clojure\Php\seq($__L_c2) : false)) { $__recur_0 = \Clojure\Php\next_($__L_c1); $__recur_1 = \Clojure\Php\next_($__L_c2); $__recur_2 = \Clojure\Php\conj(\Clojure\Php\conj($__L_result, \Clojure\Php\first($__L_c1)), \Clojure\Php\first($__L_c2)); $__L_c1 = $__recur_0; $__L_c2 = $__recur_1; $__L_result = $__recur_2; continue;} else { return $__L_result;} break; }
 break; } } else if ($__n >= 2) { $__L_c1 = $__args[0]; $__L_c2 = $__args[1]; $__L_colls = array_slice($__args, 2);  while(true) { $__L_colls = \Clojure\Php\cons($__L_c1, \Clojure\Php\cons($__L_c2, $__L_colls));
$__L_result = \Clojure\Php\vec();
 while(true) { if (($NS->every_QMARK_)($NS->seq, $__L_colls)) { $__recur_0 = ($NS->map)($NS->next, $__L_colls); $__recur_1 = ($NS->into)($__L_result, ($NS->map)($NS->first, $__L_colls)); $__L_colls = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }
 break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->interpose = (function($__L_sep, $__L_coll) {  while(true) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
$__L_first_QMARK_ = true;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { if ($__L_first_QMARK_) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, \Clojure\Php\first($__L_coll)); $__recur_2 = false; $__L_coll = $__recur_0; $__L_result = $__recur_1; $__L_first_QMARK_ = $__recur_2; continue;} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj(\Clojure\Php\conj($__L_result, $__L_sep), \Clojure\Php\first($__L_coll)); $__recur_2 = false; $__L_coll = $__recur_0; $__L_result = $__recur_1; $__L_first_QMARK_ = $__recur_2; continue;}} else { return $__L_result;} break; }
 break; }});
$NS->flatten = (function($__L_coll) {  while(true) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
if (($NS->coll_QMARK_)($__L_x)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = ($NS->into)($__L_result, ($NS->flatten)($__L_x)); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_x); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;}} else { return $__L_result;} break; }
 break; }});
$NS->mapcat = (function($__L_f, $__L_coll) { return ($NS->apply)($NS->concat, ($NS->map)($__L_f, $__L_coll));});
$NS->keep = (function($__L_f, $__L_coll) {  while(true) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_v = call_user_func($__L_f, \Clojure\Php\first($__L_coll));
$__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = (($__L_v === null) ? $__L_result : \Clojure\Php\conj($__L_result, $__L_v)); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }
 break; }});
$NS->keep_indexed = (function($__L_f, $__L_coll) {  while(true) { $__L_coll = $__L_coll;
$__L_idx = 0;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_v = call_user_func($__L_f, $__L_idx, \Clojure\Php\first($__L_coll));
$__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = ($__L_idx + 1); $__recur_2 = (($__L_v === null) ? $__L_result : \Clojure\Php\conj($__L_result, $__L_v)); $__L_coll = $__recur_0; $__L_idx = $__recur_1; $__L_result = $__recur_2; continue;} else { return $__L_result;} break; }
 break; }});
$NS->map_indexed = (function($__L_f, $__L_coll) {  while(true) { $__L_coll = $__L_coll;
$__L_idx = 0;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = ($__L_idx + 1); $__recur_2 = \Clojure\Php\conj($__L_result, call_user_func($__L_f, $__L_idx, \Clojure\Php\first($__L_coll))); $__L_coll = $__recur_0; $__L_idx = $__recur_1; $__L_result = $__recur_2; continue;} else { return $__L_result;} break; }
 break; }});
$NS->partition_by = (function($__L_f, $__L_coll) {  while(true) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
$__L_current = \Clojure\Php\vec();
$__L_current_val = null;
 while(true) { if (\Clojure\Php\seq($__L_coll)) { $__L_x = \Clojure\Php\first($__L_coll);
$__L_v = call_user_func($__L_f, $__L_x);
if ((($__L_current_val === null) ? ($__L_current_val === null) : \Clojure\Php\equals($__L_v, $__L_current_val))) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = $__L_result; $__recur_2 = \Clojure\Php\conj($__L_current, $__L_x); $__recur_3 = $__L_v; $__L_coll = $__recur_0; $__L_result = $__recur_1; $__L_current = $__recur_2; $__L_current_val = $__recur_3; continue;} else { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, $__L_current); $__recur_2 = \Clojure\Php\vec($__L_x); $__recur_3 = $__L_v; $__L_coll = $__recur_0; $__L_result = $__recur_1; $__L_current = $__recur_2; $__L_current_val = $__recur_3; continue;}} else { if (\Clojure\Php\seq($__L_current)) { return \Clojure\Php\conj($__L_result, $__L_current);} else { return $__L_result;}} break; }
 break; }});
$NS->split_at = (function($__L_n, $__L_coll) { return \Clojure\Php\vec(($NS->take)($__L_n, $__L_coll), ($NS->drop)($__L_n, $__L_coll));});
$NS->split_with = (function($__L_pred, $__L_coll) { return \Clojure\Php\vec(($NS->take_while)($__L_pred, $__L_coll), ($NS->drop_while)($__L_pred, $__L_coll));});
$NS->butlast = (function($__L_coll) {  while(true) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (\Clojure\Php\next_($__L_coll)) { $__recur_0 = \Clojure\Php\next_($__L_coll); $__recur_1 = \Clojure\Php\conj($__L_result, \Clojure\Php\first($__L_coll)); $__L_coll = $__recur_0; $__L_result = $__recur_1; continue;} else { return $__L_result;} break; }
 break; }});
$NS->shuffle = (function($__L_coll) { $__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
shuffle($__L_arr);
return call_user_func('\Clojure\Php\vector', $__L_arr);});
$NS->union = (function($__L_s1, $__L_s2) { return ($NS->reduce)($NS->conj, $__L_s1, $__L_s2);});
$NS->intersection = (function($__L_s1, $__L_s2) { return ($NS->reduce)((function($__L_result, $__L_x) use (&$__L_s1, &$__L_s2) { if (\Clojure\Php\contains($__L_s2, $__L_x)) { return \Clojure\Php\conj($__L_result, $__L_x);} else { return $__L_result;}}), \Clojure\Php\hashSet(), $__L_s1);});
$NS->difference = (function($__L_s1, $__L_s2) { return ($NS->reduce)((function($__L_result, $__L_x) use (&$__L_s1, &$__L_s2) { if (\Clojure\Php\contains($__L_s2, $__L_x)) { return $__L_result;} else { return \Clojure\Php\conj($__L_result, $__L_x);}}), \Clojure\Php\hashSet(), $__L_s1);});
$NS->subset_QMARK_ = (function($__L_s1, $__L_s2) { return ($NS->every_QMARK_)((function($__L_x) use (&$__L_s1, &$__L_s2) { return \Clojure\Php\contains($__L_s2, $__L_x);}), $__L_s1);});
$NS->superset_QMARK_ = (function($__L_s1, $__L_s2) { return ($NS->subset_QMARK_)($__L_s2, $__L_s1);});
$NS->max = (function(...$__L_xs) { return ($NS->reduce)((function($__L_a, $__L_b) use (&$__L_xs) { if (($__L_a > $__L_b)) { return $__L_a;} else { return $__L_b;}}), \Clojure\Php\first($__L_xs), \Clojure\Php\next_($__L_xs));});
$NS->min = (function(...$__L_xs) { return ($NS->reduce)((function($__L_a, $__L_b) use (&$__L_xs) { if (($__L_a < $__L_b)) { return $__L_a;} else { return $__L_b;}}), \Clojure\Php\first($__L_xs), \Clojure\Php\next_($__L_xs));});
$NS->abs = (function($__L_x) { if (($__L_x < 0)) { return (-$__L_x);} else { return $__L_x;}});
$NS->mod = (function($__L_n, $__L_d) { return ($__L_n % $__L_d);});
$NS->quot = (function($__L_n, $__L_d) { return intdiv($__L_n, $__L_d);});
$NS->rem = (function($__L_n, $__L_d) { return ($__L_n % $__L_d);});
$NS->even_QMARK_ = (function($__L_n) { return (0 === ($__L_n % 2));});
$NS->odd_QMARK_ = (function($__L_n) { return (!(($__L_n & 1) === 0));});
$NS->pos_QMARK_ = (function($__L_n) { return ($__L_n > 0);});
$NS->neg_QMARK_ = (function($__L_n) { return ($__L_n < 0);});
$NS->zero_QMARK_ = (function($__L_n) { return ($__L_n === 0);});
$NS->rand = (function() { return (rand() / getrandmax());});
$NS->rand_int = (function($__L_n) { return rand(0, ($__L_n - 1));});
$NS->rand_nth = (function($__L_coll) { return \Clojure\Php\nth($__L_coll, ($NS->rand_int)(\Clojure\Php\count_($__L_coll)));});
$NS->read_string = (function($__L_s) { 'Parse an EDN string. Currently uses PHP\'s json_decode for JSON-compatible values.';
return json_decode($__L_s, true);});
$NS->ex_info = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_msg = $__args[0]; $__L_data = $__args[1]; $__L_cause = $__args[2]; return call_user_func('\Clojure\Php\exInfo', $__L_msg, $__L_data, $__L_cause); } else if ($__n == 2) { $__L_msg = $__args[0]; $__L_data = $__args[1]; return call_user_func('\Clojure\Php\exInfo', $__L_msg, $__L_data); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->ex_data = (function($__L_ex) { return call_user_func('\Clojure\Php\exData', $__L_ex);});
$NS->ex_message = (function($__L_ex) { return call_user_func('\Clojure\Php\exMessage', $__L_ex);});
$NS->ex_cause = (function($__L_ex) { return call_user_func('\Clojure\Php\exCause', $__L_ex);});
$NS->Throwable__GT_map = (function($__L_ex) { if (($__L_ex === null)) { return null;} else { $__L_data = ($NS->ex_data)($__L_ex);
$__L_msg = ($NS->ex_message)($__L_ex);
$__L_cause = ($NS->ex_cause)($__L_ex);
$__L_class_name = get_class($__L_ex);
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), $__L_class_name, \Clojure\Php\Kw::create('message'), $__L_msg, \Clojure\Php\Kw::create('data'), $__L_data, \Clojure\Php\Kw::create('cause'), ($__L_cause ? ($NS->Throwable__GT_map)($__L_cause) : null));}});
$NS->ex_triage = (function($__L_throwable_map) { $__L___dest_22 = $__L_throwable_map;
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
return \Clojure\Php\hashMap(\Clojure\Php\Kw::createNs('cljp.error', 'file'), $__L_file, \Clojure\Php\Kw::createNs('cljp.error', 'type'), $__L_err_type, \Clojure\Php\Kw::createNs('cljp.error', 'column'), $__L_col, \Clojure\Php\Kw::createNs('cljp.error', 'message'), $__L_message, \Clojure\Php\Kw::createNs('cljp.error', 'line'), $__L_line, \Clojure\Php\Kw::createNs('cljp.error', 'phase'), $__L_phase, \Clojure\Php\Kw::createNs('cljp.error', 'hint'), $__L_hint, \Clojure\Php\Kw::createNs('cljp.error', 'class'), $__L_type, \Clojure\Php\Kw::createNs('cljp.error', 'cause'), ($__L_cause ? ($NS->ex_message)(\Clojure\Php\hashMap(\Clojure\Php\Kw::create('message'), call_user_func(\Clojure\Php\Kw::create('message'), $__L_cause))) : null), \Clojure\Php\Kw::createNs('cljp.error', 'symbol'), $__L_sym);});
$NS->ex_str = (function($__L_triage_data) { $__L_phase = \Clojure\Php\get_($__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'phase'));
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
Hint: ', $__L_hint) : ''));});
$NS->err__GT_msg = (function($__L_ex) { return ($NS->ex_str)(($NS->ex_triage)(($NS->Throwable__GT_map)($__L_ex)));});
$NS->_STAR_1 = null;
$NS->_STAR_2 = null;
$NS->_STAR_3 = null;
$NS->_STAR_e = null;
$NS->_STAR_ns_STAR_ = 'user';
$NS->_STAR_print_length_STAR_ = null;
$NS->_STAR_print_level_STAR_ = null;
$NS->_STAR_print_meta_STAR_ = null;
$NS->_push_repl_result = (function($__L_value) { 'Internal: Update REPL result history.';
return $__L_value;});
$NS->symbol = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_ns = $__args[0]; $__L_name = $__args[1]; return call_user_func('\Clojure\Php\sym', $__L_name, $__L_ns); } else if ($__n == 1) { $__L_name = $__args[0]; return call_user_func('\Clojure\Php\sym', $__L_name); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->format = (function($__L_fmt, ...$__L_args) { return call_user_func_array('sprintf', \Clojure\Php\cons($__L_fmt, $__L_args));});
$NS->realized_QMARK_ = (function($__L_x) { if (method_exists($__L_x, 'isRealized')) { return $__L_x->isRealized();} else { return true;}});
$NS->doall = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1];  while(true) { if ($__L_n) { $__L_i = $__L_n;
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ((($__L_i > 0) ? $__L_s : false)) { $__recur_0 = ($__L_i - 1); $__recur_1 = \Clojure\Php\next_($__L_s); $__L_i = $__recur_0; $__L_s = $__recur_1; continue;}
 break; }
} else { $__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ($__L_s) { $__recur_0 = \Clojure\Php\next_($__L_s); $__L_s = $__recur_0; continue;}
 break; }
}
return $__L_coll; break; } } else if ($__n == 1) { $__L_coll = $__args[0]; return ($NS->doall)($__L_coll, null); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->dorun = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1];  while(true) { $__L_i = $__L_n;
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ((($__L_i > 0) ? $__L_s : false)) { $__recur_0 = ($__L_i - 1); $__recur_1 = \Clojure\Php\next_($__L_s); $__L_i = $__recur_0; $__L_s = $__recur_1; continue;}
 break; }
return null; break; } } else if ($__n == 1) { $__L_coll = $__args[0];  while(true) { $__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if ($__L_s) { $__recur_0 = \Clojure\Php\next_($__L_s); $__L_s = $__recur_0; continue;}
 break; }
return null; break; } } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->re_pattern = (function($__L_s) { if (is_string($__L_s)) { return $__L_s;} else { throw (new \InvalidArgumentException('re-pattern requires a string'));
}});
$NS->re_matches = (function($__L_re, $__L_s) { $__L_result = array();
$__L_matched = preg_match(\Clojure\Php\str_('^', $__L_re, '$'), $__L_s, $__L_result);
if (($__L_matched > 0)) { if ((1 === count($__L_result))) { return \Clojure\Php\first($__L_result);} else { return call_user_func('\Clojure\Php\vec', $__L_result);}}});
$NS->re_find = (function($__L_re, $__L_s) { $__L_result = array();
$__L_matched = preg_match($__L_re, $__L_s, $__L_result);
if (($__L_matched > 0)) { if ((1 === count($__L_result))) { return \Clojure\Php\first($__L_result);} else { return call_user_func('\Clojure\Php\vec', $__L_result);}}});
$NS->re_seq = (function($__L_re, $__L_s) { $__L_result = array();
$__L__ = preg_match_all($__L_re, $__L_s, $__L_result);
if ((count(\Clojure\Php\first($__L_result)) > 0)) { return call_user_func('\Clojure\Php\vec', \Clojure\Php\first($__L_result));}});
$NS->re_groups = (function($__L_m) { if (is_string($__L_m)) { return $__L_m;} else { if ((\Clojure\Php\count_($__L_m) > 1)) { return $__L_m;} else { return \Clojure\Php\first($__L_m);}}});
$NS->line_seq = (function($__L_rdr) { return (new \Clojure\Php\LazySeq((function() use (&$__L_rdr) { $__L_line = fgets($__L_rdr);
if (($__L_line !== false)) { return \Clojure\Php\cons(rtrim($__L_line, PHP_EOL), ($NS->line_seq)($__L_rdr));}})));});
$NS->tree_seq = (function($__L_branch_QMARK_, $__L_children, $__L_root) { $__L_walk = (function($__L_node) use (&$__L_branch_QMARK_, &$__L_children, &$__L_root) { return (new \Clojure\Php\LazySeq((function() use (&$__L_branch_QMARK_, &$__L_children, &$__L_node, &$__L_root) { return \Clojure\Php\cons($__L_node, (call_user_func($__L_branch_QMARK_, $__L_node) ? ($NS->mapcat)($NS->walk, call_user_func($__L_children, $__L_node)) : null));})));});
return call_user_func($__L_walk, $__L_root);});
$NS->file_seq = (function($__L_dir) { return ($NS->tree_seq)((function($__L_f) use (&$__L_dir) { return is_dir($__L_f);}), (function($__L_d) use (&$__L_dir) { $__L_files = scandir($__L_d);
return ($NS->filter)((function($__L_f) use (&$__L_dir, &$__L_files, &$__L_d) { return (!(\Clojure\Php\equals($__L_f, '.') ? \Clojure\Php\equals($__L_f, '.') : \Clojure\Php\equals($__L_f, '..')));}), ($NS->map)((function($__L_f) use (&$__L_dir, &$__L_files, &$__L_d) { return \Clojure\Php\str_($__L_d, '/', $__L_f);}), $__L_files));}), $__L_dir);});
$NS->xml_seq = (function($__L_root) { return ($NS->tree_seq)((function($__L_node) use (&$__L_root) { if (($__L_node instanceof \Clojure\Php\Map)) { return \Clojure\Php\contains($__L_node, \Clojure\Php\Kw::create('content'));} else { return false;}}), (function($__L_node) use (&$__L_root) { return \Clojure\Php\get_($__L_node, \Clojure\Php\Kw::create('content'));}), $__L_root);});
$NS->bit_not = (function($__L_x) { return $__L_x;});
$NS->bit_test = (function($__L_x, $__L_n) { return (0 !== ($__L_x & (1 << $__L_n)));});
$NS->bit_set = (function($__L_x, $__L_n) { return ($__L_x | (1 << $__L_n));});
$NS->bit_clear = (function($__L_x, $__L_n) { return ($__L_x & (1 << $__L_n));});
$NS->bit_flip = (function($__L_x, $__L_n) { return ($__L_x ^ (1 << $__L_n));});
$NS->unsigned_bit_shift_right = (function($__L_x, $__L_n) { if (($__L_x >= 0)) { return ($__L_x >> $__L_n);} else { return (($__L_x + (1 << ((8 * PHP_INT_SIZE) - 1))) >> ($__L_n - 1));}});
$NS->add_watch = (function($__L_ref, $__L_key, $__L_fn) { $__L_ref->addWatch($__L_key, $__L_fn);
return $__L_ref;});
$NS->remove_watch = (function($__L_ref, $__L_key) { $__L_ref->removeWatch($__L_key);
return $__L_ref;});
$NS->set_validator_BANG_ = (function($__L_ref, $__L_validator_fn) { $__L_ref->setValidator($__L_validator_fn);
return null;});
$NS->get_validator = (function($__L_ref) { return $__L_ref->getValidator();});
$NS->compare_and_set_BANG_ = (function($__L_atom, $__L_oldval, $__L_newval) { return $__L_atom->compareAndSet($__L_oldval, $__L_newval);});
$NS->run_BANG_ = (function($__L_proc, $__L_coll) { ($NS->reduce)((function($__L__, $__L_x) use (&$__L_proc, &$__L_coll) { call_user_func($__L_proc, $__L_x);
return null;}), null, $__L_coll);
return null;});
$NS->transient = (function($__L_coll) { if (method_exists($__L_coll, 'asTransient')) { return $__L_coll->asTransient();} else { throw (new \InvalidArgumentException('transient not supported'));
}});
$NS->persistent_BANG_ = (function($__L_tcoll) { if (method_exists($__L_tcoll, 'persistent')) { return $__L_tcoll->persistent();} else { throw (new \InvalidArgumentException('persistent! not supported'));
}});
$NS->conj_BANG_ = (function($__L_tcoll, $__L_val) { if (method_exists($__L_tcoll, 'conj')) { return $__L_tcoll->conj($__L_val);} else { throw (new \InvalidArgumentException('conj! not supported'));
}});
$NS->assoc_BANG_ = (function($__L_tcoll, $__L_key, $__L_val) { if (method_exists($__L_tcoll, 'assoc')) { return $__L_tcoll->assoc($__L_key, $__L_val);} else { throw (new \InvalidArgumentException('assoc! not supported'));
}});
$NS->dissoc_BANG_ = (function($__L_tcoll, $__L_key) { if (method_exists($__L_tcoll, 'dissoc')) { return $__L_tcoll->dissoc($__L_key);} else { throw (new \InvalidArgumentException('dissoc! not supported'));
}});
$NS->pop_BANG_ = (function($__L_tcoll) { if (method_exists($__L_tcoll, 'pop')) { return $__L_tcoll->pop();} else { throw (new \InvalidArgumentException('pop! not supported'));
}});
$NS->disj_BANG_ = (function($__L_tset, $__L_val) { if (method_exists($__L_tset, 'disj')) { return $__L_tset->disj($__L_val);} else { throw (new \InvalidArgumentException('disj! not supported'));
}});
$NS->select = (function($__L_pred, $__L_xset) { return ($NS->reduce)((function($__L_s, $__L_k) use (&$__L_pred, &$__L_xset) { if (call_user_func($__L_pred, $__L_k)) { return $__L_s;} else { return ($NS->disj)($__L_s, $__L_k);}}), $__L_xset, $__L_xset);});
$NS->project = (function($__L_xrel, $__L_ks) { return ($NS->into)(\Clojure\Php\hashSet(), ($NS->map)((function($__L_m) use (&$__L_ks, &$__L_xrel) { return ($NS->select_keys)($__L_m, $__L_ks);}), $__L_xrel));});
$NS->rename_keys = (function($__L_map, $__L_kmap) { return ($NS->reduce)((function($__L_m, $__L___dest_23) use (&$__L_map, &$__L_kmap) { $__L___dest_24 = $__L___dest_23;
$__L_old = \Clojure\Php\nth($__L___dest_24, 0);
$__L_new = \Clojure\Php\nth($__L___dest_24, 1);
if (\Clojure\Php\contains($__L_m, $__L_old)) { return \Clojure\Php\assoc(\Clojure\Php\dissoc($__L_m, $__L_old), $__L_new, \Clojure\Php\get_($__L_m, $__L_old));} else { return $__L_m;}}), $__L_map, $__L_kmap);});
$NS->rename = (function($__L_xrel, $__L_kmap) { return ($NS->into)(\Clojure\Php\hashSet(), ($NS->map)((function($__L_m) use (&$__L_xrel, &$__L_kmap) { return ($NS->rename_keys)($__L_m, $__L_kmap);}), $__L_xrel));});
$NS->index = (function($__L_xrel, $__L_ks) { return ($NS->reduce)((function($__L_m, $__L_x) use (&$__L_ks, &$__L_xrel) { $__L_ik = ($NS->select_keys)($__L_x, $__L_ks);
return ($NS->update)($__L_m, $__L_ik, (function($__L_is) use (&$__L_x, &$__L_ks, &$__L_xrel, &$__L_m, &$__L_ik) { return \Clojure\Php\conj(($__L_is ? $__L_is : \Clojure\Php\hashSet()), $__L_x);}));}), \Clojure\Php\hashMap(), $__L_xrel);});
$NS->map_invert = (function($__L_m) { return ($NS->reduce)((function($__L_result, $__L___dest_25) use (&$__L_m) { $__L___dest_26 = $__L___dest_25;
$__L_k = \Clojure\Php\nth($__L___dest_26, 0);
$__L_v = \Clojure\Php\nth($__L___dest_26, 1);
return \Clojure\Php\assoc($__L_result, $__L_v, $__L_k);}), \Clojure\Php\hashMap(), $__L_m);});
$NS->join = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_xrel = $__args[0]; $__L_yrel = $__args[1]; $__L_km = $__args[2]; $__L___dest_28 = ((\Clojure\Php\count_($__L_xrel) <= \Clojure\Php\count_($__L_yrel)) ? \Clojure\Php\vec($__L_xrel, $__L_yrel, ($NS->map_invert)($__L_km)) : \Clojure\Php\vec($__L_yrel, $__L_xrel, $__L_km));
$__L_r = \Clojure\Php\nth($__L___dest_28, 0);
$__L_s = \Clojure\Php\nth($__L___dest_28, 1);
$__L_k = \Clojure\Php\nth($__L___dest_28, 2);
$__L_idx = ($NS->index)($__L_r, ($NS->vals)($__L_k));
return ($NS->reduce)((function($__L_ret, $__L_x) use (&$__L_idx, &$__L_r, &$__L___dest_28, &$__L_xrel, &$__L_s, &$__L_k, &$__L_yrel, &$__L_km) { $__L_found = \Clojure\Php\get_($__L_idx, ($NS->rename_keys)(($NS->select_keys)($__L_x, ($NS->keys)($__L_k)), $__L_k));
if ($__L_found) { return ($NS->reduce)((function($__L_ret, $__L_y) use (&$__L_idx, &$__L_x, &$__L_r, &$__L___dest_28, &$__L_xrel, &$__L_s, &$__L_k, &$__L_yrel, &$__L_found, &$__L_km) { return \Clojure\Php\conj($__L_ret, ($NS->merge)($__L_y, $__L_x));}), $__L_ret, $__L_found);} else { return $__L_ret;}}), \Clojure\Php\hashSet(), $__L_s); } else if ($__n == 2) { $__L_xrel = $__args[0]; $__L_yrel = $__args[1]; if ((\Clojure\Php\seq($__L_xrel) ? \Clojure\Php\seq($__L_yrel) : false)) { $__L_ks = ($NS->intersection)(($NS->into)(\Clojure\Php\hashSet(), ($NS->keys)(\Clojure\Php\first($__L_xrel))), ($NS->into)(\Clojure\Php\hashSet(), ($NS->keys)(\Clojure\Php\first($__L_yrel))));
$__L___dest_27 = ((\Clojure\Php\count_($__L_xrel) <= \Clojure\Php\count_($__L_yrel)) ? \Clojure\Php\vec($__L_xrel, $__L_yrel) : \Clojure\Php\vec($__L_yrel, $__L_xrel));
$__L_r = \Clojure\Php\nth($__L___dest_27, 0);
$__L_s = \Clojure\Php\nth($__L___dest_27, 1);
$__L_idx = ($NS->index)($__L_r, $__L_ks);
return ($NS->reduce)((function($__L_ret, $__L_x) use (&$__L_idx, &$__L_r, &$__L_ks, &$__L_xrel, &$__L_s, &$__L___dest_27, &$__L_yrel) { $__L_found = \Clojure\Php\get_($__L_idx, ($NS->select_keys)($__L_x, $__L_ks));
if ($__L_found) { return ($NS->reduce)((function($__L_ret, $__L_y) use (&$__L_idx, &$__L_x, &$__L_r, &$__L_ks, &$__L_xrel, &$__L_s, &$__L___dest_27, &$__L_yrel, &$__L_found) { return \Clojure\Php\conj($__L_ret, ($NS->merge)($__L_y, $__L_x));}), $__L_ret, $__L_found);} else { return $__L_ret;}}), \Clojure\Php\hashSet(), $__L_s);} else { return \Clojure\Php\hashSet();} } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->gensym = (function(...$__args) { $__n = count($__args); if ($__n == 1) { $__L_prefix = $__args[0]; return (\Clojure\Php\str_($__L_prefix, uniqid()) instanceof \Clojure\Php\Sym ? \Clojure\Php\str_($__L_prefix, uniqid()) : \Clojure\Php\Sym::create((string)\Clojure\Php\str_($__L_prefix, uniqid()))); } else if ($__n == 0) { return ($NS->gensym)('G__'); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$NS->with_meta = (function($__L_obj, $__L_m) { if (method_exists($__L_obj, 'withMeta')) { return $__L_obj->withMeta($__L_m);} else { return $__L_obj;}});
$NS->meta = (function($__L_obj) { if (method_exists($__L_obj, 'meta')) { return $__L_obj->meta();}});
$NS->vary_meta = (function($__L_obj, $__L_f, ...$__L_args) { return ($NS->with_meta)($__L_obj, ($NS->apply)($__L_f, ($NS->meta)($__L_obj), $__L_args));});
$NS->alter_meta_BANG_ = (function($__L_ref, $__L_f, ...$__L_args) { if (method_exists($__L_ref, 'alterMeta')) { return $__L_ref->alterMeta($__L_f, $__L_args);} else { throw (new \InvalidArgumentException('alter-meta! not supported'));
}});
$NS->reset_meta_BANG_ = (function($__L_ref, $__L_m) { if (method_exists($__L_ref, 'resetMeta')) { return $__L_ref->resetMeta($__L_m);} else { throw (new \InvalidArgumentException('reset-meta! not supported'));
}});
$NS->bounded_count = (function($__L_n, $__L_coll) {  while(true) { if (($__L_coll instanceof \Countable)) { return ($NS->min)($__L_n, \Clojure\Php\count_($__L_coll));} else { $__L_i = 0;
$__L_s = \Clojure\Php\seq($__L_coll);
 while(true) { if (($__L_s ? ($__L_i < $__L_n) : false)) { $__recur_0 = ($__L_i + 1); $__recur_1 = \Clojure\Php\next_($__L_s); $__L_i = $__recur_0; $__L_s = $__recur_1; continue;} else { return $__L_i;} break; }
} break; }});
$NS->not_empty = (function($__L_coll) { if (\Clojure\Php\seq($__L_coll)) { return $__L_coll;}});
$NS->int = (function($__L_x) { return intval($__L_x);});
$NS->long = (function($__L_x) { return intval($__L_x);});
$NS->float = (function($__L_x) { return floatval($__L_x);});
$NS->double = (function($__L_x) { return floatval($__L_x);});
$NS->char = (function($__L_x) { if (is_int($__L_x)) { return chr($__L_x);} else { if (is_string($__L_x)) { return \Clojure\Php\first($__L_x);} else { throw (new \InvalidArgumentException('Cannot coerce to char'));
}}});
$NS->boolean = (function($__L_x) { return boolval($__L_x);});
$NS->byte = (function($__L_x) { return (intval($__L_x) & 255);});
$NS->short = (function($__L_x) { return intval($__L_x);});
$NS->object_array = (function($__L_n) { return array_fill(0, $__L_n, null);});
$NS->num = (function($__L_x) { if ((is_int($__L_x) || is_float($__L_x))) { return $__L_x;} else { throw (new \InvalidArgumentException('Cannot coerce to num'));
}});
$NS->bigint = (function($__L_x) { return intval($__L_x);});
$NS->bigdec = (function($__L_x) { return floatval($__L_x);});
$NS->rationalize = (function($__L_x) { return floatval($__L_x);});
echo(strval((1 + ((function() { $__L_x = 2; return $__L_x; })()))));
echo('
');
echo(strval((((function() { $__L_a = 1; return $__L_a; })()) + ((function() { $__L_b = 2; return $__L_b; })()))));
echo('
');
echo(strval((10 + ((function() { $NS->temp = 5;
return $NS->temp; })()))));
echo('
');
echo('Done
');
