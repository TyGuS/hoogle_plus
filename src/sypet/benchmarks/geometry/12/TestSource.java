public boolean test() throws Throwable {
	java.awt.geom.Rectangle2D rec = new java.awt.geom.Rectangle2D.Double(10, 20, 10, 2);
	java.awt.geom.Rectangle2D target = new java.awt.geom.Rectangle2D.Double(-22, 10, 2, 10);
	java.awt.geom.Rectangle2D result = rotateQuadrant(rec, 1);
	return (target.equals(result));
}
