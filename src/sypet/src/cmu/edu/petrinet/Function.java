package cmu.edu.petrinet;

import java.util.List;

public class Function {
	private List<String> funParams;
	private List<Function> hoParams;
	private String funReturn;
	private String funName;

	public Function(List<String> p, List<Function> hp, String r, String n) {
		funParams = p;
		hoParams = hp;
		funReturn = r;
		funName = n;
	}

	public List<String> getFunParams() {
		return funParams;
	}

	public void setFunParams(List<String> params) {
		funParams = params;
	}

	public List<Function> getHoParams() {
		return hoParams;
	}

	public void setHoParams(List<Function> ho) {
		hoParams = ho;
	}

	public String getFunReturn() {
		return funReturn;
	}

	public void setFunReturn(String ret) {
		funReturn = ret;
	}

	public String getFunName() {
		return funName;
	}

	public void setFunName(String name) {
		funName = name;
	}

	public void print() {
		System.err.print(funName + "::");
        for(String t : funParams) {
            System.err.print(t + "->");
        }
        System.err.println(funReturn);
        for(Function f : hoParams) {
        	f.print();
        }
	}
}