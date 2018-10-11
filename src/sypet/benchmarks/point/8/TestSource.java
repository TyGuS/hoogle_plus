public boolean test() throws Throwable {
	cmu.symonster.MyPoint mp = new cmu.symonster.MyPoint(1,2);
	cmu.symonster.Point p = convert(mp);

	return (p.getY() == 2 && p.getX() == 1);
}
