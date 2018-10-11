public boolean test() throws Throwable {
	cmu.symonster.MyPoint p1 = new cmu.symonster.MyPoint(0,0);
	cmu.symonster.MyPoint p2 = new cmu.symonster.MyPoint(3,0);
	cmu.symonster.MyPoint p3 = new cmu.symonster.MyPoint(0,3);
	cmu.symonster.Triangle tr = new cmu.symonster.Triangle(p1, p2, p3);
	java.util.Vector<cmu.symonster.MyPoint> res = coordinates(tr);

	boolean ok = false;
	if (res.size() == 3){
		if (res.get(0).getX() == 0 && res.get(0).getY() == 0 &&
			res.get(1).getX() == 3 && res.get(1).getY() == 0 &&
			res.get(2).getX() == 0 && res.get(2).getY() == 3)
			ok = true;
	}

	return ok;
}
