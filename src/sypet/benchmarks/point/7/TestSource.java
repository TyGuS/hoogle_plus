public boolean test() throws Throwable {
	cmu.symonster.MyPoint mp = new cmu.symonster.MyPoint(1,2);
	mp.setName("foo");
	cmu.symonster.Point p = convert(mp);

	return (p.getName().equals("foo") && p.getX() == 1);
}
