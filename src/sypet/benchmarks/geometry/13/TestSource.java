public boolean test() throws Throwable {
	return test0() && test1();
}

public boolean test0() throws Throwable {
    java.awt.geom.Area a1 = new java.awt.geom.Area(new java.awt.geom.Rectangle2D.Double(0, 0, 10, 2));
    java.awt.geom.Area a2 = new java.awt.geom.Area(new java.awt.geom.Rectangle2D.Double(-2, 0, 2, 10));
    java.awt.geom.Point2D p = new java.awt.geom.Point2D.Double(0, 0);
    java.awt.geom.Area a3 = rotate(a1, p, Math.PI / 2);
    return a2.equals(a3);
}

public boolean test1() throws Throwable {
    java.awt.geom.Area a1 = new java.awt.geom.Area(new java.awt.geom.Rectangle2D.Double(10, 20, 10, 2));
    java.awt.geom.Area a2 = new java.awt.geom.Area(new java.awt.geom.Rectangle2D.Double(8, 20, 2, 10));
    java.awt.geom.Point2D p = new java.awt.geom.Point2D.Double(10, 20);
    java.awt.geom.Area a3 = rotate(a1, p, Math.PI / 2);
    return a2.equals(a3);
}