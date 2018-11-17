/**
 * BSD 3-Clause License
 *	
 *	
 *	Copyright (c) 2018, SyPet 2.0 - Ruben Martins, Yu Feng, Isil Dillig
 *	All rights reserved.
 *	
 *	Redistribution and use in source and binary forms, with or without
 *	modification, are permitted provided that the following conditions are met:
 *	
 *	* Redistributions of source code must retain the above copyright notice, this
 *	  list of conditions and the following disclaimer.
 *	
 *	* Redistributions in binary form must reproduce the above copyright notice,
 *	  this list of conditions and the following disclaimer in the documentation
 *	  and/or other materials provided with the distribution.
 *	
 *	* Neither the name of the copyright holder nor the names of its
 *	  contributors may be used to endorse or promote products derived from
 *	  this software without specific prior written permission.
 *	
 *	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 *	FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *	DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *	SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *	CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *	OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package cmu.edu.reachability;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.sat4j.core.VecInt;
//import org.sat4j.minisat.SolverFactory;
//import org.sat4j.specs.ISolver;
import org.sat4j.pb.IPBSolver;
import org.sat4j.maxsat.WeightedMaxSatDecorator;
import org.sat4j.pb.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.TimeoutException;

import cmu.edu.reachability.SATSolver.ConstraintType;

public class SATSolver {
	
	private IPBSolver pbSolver = null;
	private WeightedMaxSatDecorator solver = null;
	private boolean unsat = false;
	private VecInt assumptions;
	
	enum ConstraintType { LTE, EQ, GTE; }
	
	// Maps the variable id to transition
	public HashMap<Integer,Variable> id2variable = new HashMap<>();
	
	private int nbVariables = 0;
	public VecInt loc_variables;
	
	public SATSolver(){
		pbSolver = SolverFactory.newDefault();
		solver = new WeightedMaxSatDecorator(pbSolver);
		assumptions = new VecInt();
		loc_variables = new VecInt();
	}
	
	public void reset(){
		pbSolver = SolverFactory.newDefault();
		solver = new WeightedMaxSatDecorator(pbSolver);
		unsat = false;
		id2variable.clear();
		nbVariables = 0;
	}
	
	public int getNbConstraints(){
		return solver.nConstraints();
	}
	
	public void setNbVariables(int vars){
		
		 // version for additional variables
		// for (int i = vars+1; i <= vars+100; i++)
		// 	loc_variables.push(i);
		// nbVariables = vars+100;
		// solver.newVar(nbVariables+100);
		
		// dummy constraints for the additional variables
		// each variable much appear at least once in the solver
		// for (int i = vars+1; i <= 100; i++) {
		// 	try {
		// 		solver.addAtLeast(new VecInt(new int[] {i}), 1);
		// 	} catch (ContradictionException e) {
		// 		assert(false);
		// 	}
		// }
		
		nbVariables = vars;
		solver.newVar(nbVariables);
	}
	
	public int getNbVariables(){
		return nbVariables;
	}
	
	public void addClause(VecInt constraint) {
		try {
			solver.addHardClause(constraint);
		} catch (ContradictionException e) {
			unsat = false;
		}
	}

	public void addSoftClause(int weight, VecInt constraint) {
		try {
			solver.addSoftClause(weight, constraint);
		} catch (ContradictionException e) {
			unsat = false;
		}
	}
	
	public void addConstraint(VecInt constraint, VecInt coeffs, ConstraintType ct, int k){ 
		try {
			switch(ct){
				case LTE:
					solver.addAtMost(constraint, coeffs, k);
					break;
				case EQ:
					solver.addExactly(constraint, coeffs, k);
					break;
				case GTE:
					solver.addAtLeast(constraint, coeffs, k);
					break;
				default:
					assert(false);
			}
		} catch (ContradictionException e) {
			unsat = true;
		}
	}
	
	public void addConstraint(VecInt constraint, ConstraintType ct, int k){ 
		try {
			switch(ct){
				case LTE:
					solver.addAtMost(constraint, k);
					break;
				case EQ:
					solver.addExactly(constraint, k);
					break;
				case GTE:
					solver.addAtLeast(constraint, k);
					break;
				default:
					assert(false);
			}
		} catch (ContradictionException e) {
			unsat = true;
		}
	}
	
	public void setAssumption(int v) {
		assumptions.push(v);
	}
		
	public void setTrue(int v){
		try{
			VecInt clause = new VecInt(new int[] {v});
			solver.addHardClause(clause);
		} catch (ContradictionException e) {
			unsat = true;
		}
	}

	public void setFalse(int v){
		try{
			VecInt clause = new VecInt(new int[] {-v});
			solver.addHardClause(clause);
		} catch (ContradictionException e) {
			unsat = true;
		}
	}
	
	public List<Variable> findPath(int loc){
		// System.out.println("Finding path for length " + loc);
		ArrayList<Variable> res = new ArrayList<>();
		// TODO: what happens when loc -> loc+1
		// 1) initial state can be encoded as constraints
		// clear the assumptions: 1) final state, 2) blocking of models
		// set a new final state
		// set the previous state as true (you can use constraints -> setTrue)
		// incrementally increase the encoding to loc+1
		try {
			// comment the below assert when using assumptions
			assert(assumptions.isEmpty());
			if(!unsat && solver.isSatisfiable(assumptions)){
				int [] model = solver.model();  
				assert (model.length == nbVariables);
				VecInt block = new VecInt();
				for (Integer id : id2variable.keySet()){
					if (model[id-1] > 0){
						block.push(-id);
						res.add(id2variable.get(id));
					}
				}

				// block model
				try {
					// ~getX(loc=1) OR ~setX(loc=2) OR ~setY(loc=3)
					// ~getX(loc=1) OR ~setX(loc=2) OR ~setY(loc=3) OR L1
					//block.push(loc_variables.get(loc-1));
					//assumptions.push(-loc_variables.get(loc-1));
					solver.addHardClause(block);
				}
				catch (ContradictionException e) {
					unsat = true;
				}

			}
		} catch (TimeoutException e) {
			// consider as it did not find a solution
			unsat = true;
		}
		
		// sort transitions by increasing time step
		Collections.sort(res);
		
		return res;
	}
}
