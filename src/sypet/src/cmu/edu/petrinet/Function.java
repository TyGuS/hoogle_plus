package cmu.edu.petrinet;

import java.util.List;

public class Function {
	private List<String> funParams;
	private String funReturn;
	private String funName;

	public List<String> getFunParams() {
		return funParams;
	}

	public void setFunParams(List<String> params) {
		funParams = params;
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