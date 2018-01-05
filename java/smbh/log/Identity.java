package smbh.log;

/**
 * The clojure compiler "optimizes" Integer, Long and Double literals (cf. Compiler#NumberExpr) for unboxed access and it
 * interfere with the method matching during macro expansion. We add a stupid "identity" function that will get inlined
 * during JIT. The impact should be minimal.
 */
public class Identity {

    public static Object identity(Object x) {
        return x;
    }

    public static Throwable throwable(Throwable t) {
        return t;
    }

}
