//# java.awt.geom
//# java.awt.geom.Rectangle2D
//# test
public static FieldsConfig gf(java.awt.geom.Rectangle2D sypet_arg0, double sypet_arg1, double sypet_arg2) {
    //#Sypet
}
// # //
public static boolean test() throws Throwable {
java.awt.geom.Rectangle2D rec = new java.awt.geom.Rectangle2D.Double(10, 20, 10, 2);
java.awt.geom.Rectangle2D target = new java.awt.geom.Rectangle2D.Double(20, 60, 20, 6);
java.awt.geom.Rectangle2D result = gf(rec, 2, 3); return (target.equals(result));
}
// # //
