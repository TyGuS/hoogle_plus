public boolean test() throws Throwable {
    java.awt.geom.Rectangle2D area = new java.awt.geom.Rectangle2D.Double(10, 20, 10, 10);
    java.awt.geom.Rectangle2D target = new java.awt.geom.Rectangle2D.Double(20, 24, 15, 14);
    java.awt.geom.Rectangle2D result = shear(area, 0.5, 0.4);
    return (target.equals(result));
}
