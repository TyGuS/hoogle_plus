public boolean delta(double v1, double v2, double error) {
	if (java.lang.Math.abs(v1-v2) <= error)
		return true;
	else
		return false;
}

public boolean test() throws Throwable {
	cmu.symonster.MyPoint mp = new cmu.symonster.MyPoint(1.5, 2.2, 3.3);
	cmu.symonster.Point p = convert(mp);
	
	return delta(1.5,p.getPitch(),0.001);
}
