public boolean test() throws Throwable {
	java.awt.geom.Rectangle2D rec = new java.awt.geom.Rectangle2D.Double(10, 20, 10, 2);
	java.awt.geom.Rectangle2D target = new java.awt.geom.Rectangle2D.Double(16, 21, 10, 2);
	java.awt.geom.Rectangle2D result = translate(rec, 6, 1);
	return (target.equals(result));
}