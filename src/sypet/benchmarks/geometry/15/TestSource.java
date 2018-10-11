public boolean test() throws Throwable {
	java.awt.geom.Rectangle2D rec = new java.awt.geom.Rectangle2D.Double(10, 20, 10, 2);
	java.awt.geom.Ellipse2D circ = new java.awt.geom.Ellipse2D.Double(9, 19, 2, 2);
	java.awt.geom.Rectangle2D target = new java.awt.geom.Rectangle2D.Double(10, 20, 1, 1);
	java.awt.geom.Rectangle2D result = getIntersection(rec, circ);
	return (target.equals(result));
}