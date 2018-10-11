import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;

public class Solution {

    public static Rectangle2D shear(Rectangle2D arg0, double arg1, double arg2) {
        Area v1 = new Area(arg0);
        AffineTransform v2 = AffineTransform.getShearInstance(arg1, arg2);
        Area v3 = v1.createTransformedArea(v2);
        Rectangle2D v4 = v3.getBounds2D();
        return v4;
    }

}
