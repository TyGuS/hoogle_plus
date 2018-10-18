package cmu.edu.codeformer;

public class FormerResult {
    private String code;
    private Integer[] satList;
    private Integer loc;
    public String getCode() { return code; }
    public void setCode(String c) { code = c;}
    public Integer[] getSatList() { return satList; }
    public void setSatList(Integer[] s) { satList = s.clone(); }
    public Integer getLoc() { return loc; }
    public void setLoc(Integer i) { loc = i; }
}