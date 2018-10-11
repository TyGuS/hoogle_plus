import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Point2D;

public class Solution {

    public static Area rotate(Area arg0, Point2D arg1, double arg2) {
        double v1 = arg1.getX();
        double v2 = arg1.getY();
        AffineTransform v3 = AffineTransform.getRotateInstance(arg2, v1, v2);
        Area v4 = arg0.createTransformedArea(v3);
        return v4;
    }

}
