import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;

public class Solution {

    public static Rectangle2D getIntersection(Rectangle2D arg0, Ellipse2D arg1) {
        Area v1 = new Area(arg1);
        Rectangle2D v2 = v1.getBounds2D();
        Rectangle2D v3 = v2.createIntersection(arg0);
        return v3;
    }
}
