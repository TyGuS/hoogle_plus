public boolean test() throws Throwable {
	cmu.symonster.MyPoint mp = new cmu.symonster.MyPoint("foo");
	cmu.symonster.Point p = convert(mp);

	return (p.getName().equals("foo"));
}
