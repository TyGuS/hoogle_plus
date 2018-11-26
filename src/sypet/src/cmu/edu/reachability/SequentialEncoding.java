/**
 * BSD 3-Clause License
 *  
 *  
 *  Copyright (c) 2018, SyPet 2.0 - Ruben Martins, Yu Feng, Isil Dillig
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *  
 *  * Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *  
 *  * Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *  
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package cmu.edu.reachability;

import java.util.*;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.ImmutableTriple;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
import org.sat4j.core.VecInt;

import cmu.edu.reachability.SATSolver.ConstraintType;
import cmu.edu.reachability.Variable.Type;
import uniol.apt.adt.pn.Flow;
import uniol.apt.adt.pn.PetriNet;
import uniol.apt.adt.pn.Place;
import uniol.apt.adt.pn.Transition;

public class SequentialEncoding implements Encoding {

    int loc = 1;
    PetriNet pnet = null;
    int nbVariables = 1;
    int nbConstraints = 0;
    List<String> mustTransitions = new ArrayList<>();

    public SequentialEncoding(PetriNet pnet, int loc) {
        this.pnet = pnet;
        this.loc = loc;

        // clean the data structures before creating a new encoding
        place2variable.clear();
        transition2variable.clear();
        solver.reset();

        createVariables();
        createConstraints();
        // System.out.println("#constraints = " + solver.getNbConstraints());
        // System.out.println("#variables = " + nbVariables);
        // System.out.println("#variables in solver = " + solver.getNbVariables());
    }

    public SequentialEncoding(PetriNet pnet, int loc, List<String> hoArgs) {
        this.pnet = pnet;
        this.loc = loc;

        // clean the data structures before creating a new encoding
        place2variable.clear();
        transition2variable.clear();
        solver.reset();

        mustTransitions = hoArgs;

        createVariables();
        createConstraints();
    }

    public void setAtLeastK(List<Pair<String, Integer>> atLeastK) {
        for (Pair<String, Integer> p : atLeastK) {
            atLeastK(p.getRight(), p.getLeft());
        }
    }

    public void atMostK(int k) {
        for (Transition tr : pnet.getTransitions()) {
            VecInt constraint = new VecInt();
            for (int t = 0; t < loc; t++) {
                // create a variable with <place in the petri-net, timestamp, value>
                Pair<Transition, Integer> pair = new ImmutablePair<Transition, Integer>(tr, t);
                Variable var = transition2variable.get(pair);
                constraint.push(var.getId());
            }
            solver.addConstraint(constraint, ConstraintType.LTE, k);
        }
    }

    public void setHints(List<String> hints) {

        if (solver.loc_variables.isEmpty())
            return;

        // System.out.println("#variables = " + solver.getNbVariables());
        for (String s : hints) {
            
            VecInt aux = new VecInt();
            
            for (Transition tr : pnet.getTransitions()) {

                if (tr.getId().contains(s)) {

                    if (solver.loc_variables.isEmpty())
                        return;
                    int v = solver.loc_variables.last();
                    solver.loc_variables.pop();
                    aux.push(v);
                    
                    VecInt constraint = new VecInt();
                    constraint.push(-v);
                    
                    for (int t = 0; t < loc; t++) {
                        //System.out.println("tr = " + tr);
                        // create a variable with <place in the petri-net, timestamp, value>
                        Pair<Transition, Integer> pair = new ImmutablePair<Transition, Integer>(tr, t);
                        Variable var = transition2variable.get(pair);
                        constraint.push(var.getId());

                        VecInt c = new VecInt();
                        c.push(-var.getId());
                        c.push(v);
                        solver.addClause(c);
                    }
                    solver.addClause(constraint);
                }
            }
            solver.addClause(aux);
        }
    }

    public void atLeastK(int k, String transition) {

        // TODO: support multiple methods with the same name with auxiliary variables
        for (Transition tr : pnet.getTransitions()) {

            if (tr.getId().equals(transition)) {
                VecInt constraint = new VecInt();
                for (int t = 0; t < loc; t++) {
                    // create a variable with <place in the petri-net, timestamp, value>
                    Pair<Transition, Integer> pair = new ImmutablePair<Transition, Integer>(tr, t);
                    Variable var = transition2variable.get(pair);
                    constraint.push(var.getId());
                }
                solver.addClause(constraint);
                // solver.addConstraint(constraint, ConstraintType.GTE, k);
                break;
            }
        }
    }

    // Exactly one transition f is fired at each time step t
    private void sequentialTransitions() {

        // loop for each time step t
        for (int t = 0; t < loc; t++) {
            // loop for each transition
            VecInt constraint = new VecInt();
            for (Transition tr : pnet.getTransitions()) {
                Pair<Transition, Integer> pair = new ImmutablePair<Transition, Integer>(tr, t);
                Variable var = transition2variable.get(pair);
                constraint.push(var.getId());
            }

            // add constraints to the solver
            // exactly one transition is going to be fired
            solver.addConstraint(constraint, ConstraintType.EQ, 1);
        }

        // = , <=, >= RHS
    }

    private void postConditionsTransitions() {
        // loop for each time step t
        for (int t = 0; t < loc; t++) {
            // loop for each transition
            for (Transition tr : pnet.getTransitions()) {

                // collect all places that will have their marking changed
                HashMap<Place, Integer> places_to_be_changed = new HashMap<>();
                for (Flow f : tr.getPostsetEdges()) {
                    Place p = f.getPlace();
                    if (!places_to_be_changed.containsKey(p))
                        places_to_be_changed.put(p, f.getWeight());
                    else
                        places_to_be_changed.put(p, places_to_be_changed.get(p) + f.getWeight());
                }

                for (Flow f : tr.getPresetEdges()) {
                    Place p = f.getPlace();
                    if (!places_to_be_changed.containsKey(p))
                        places_to_be_changed.put(p, -f.getWeight());
                    else
                        places_to_be_changed.put(p, places_to_be_changed.get(p) - f.getWeight());
                }

                Pair<Transition, Integer> transition = new ImmutablePair<Transition, Integer>(tr, t);
                Variable fireTr = transition2variable.get(transition);

                for (Place p : places_to_be_changed.keySet()) {
                    for (int w = 0; w <= p.getMaxToken(); w++) {

                        int diff = places_to_be_changed.get(p);
                        // void always remains the same
                        if (p.getId().equals("void"))
                            diff = 0;

                        if (w + places_to_be_changed.get(p) < 0 || w + places_to_be_changed.get(p) > p.getMaxToken())
                            continue;

                        Triple<Place, Integer, Integer> placeBefore = new ImmutableTriple<Place, Integer, Integer>(p, t,
                                w);
                        Triple<Place, Integer, Integer> placeAfter = new ImmutableTriple<Place, Integer, Integer>(p,
                                t + 1, w + diff);

                        Variable previousState = place2variable.get(placeBefore);
                        Variable nextState = place2variable.get(placeAfter);
                        VecInt state = new VecInt(
                                new int[] { -fireTr.getId(), -previousState.getId(), nextState.getId() });
                        solver.addClause(state);

                        // f AND a => b
                        // clause: ~f OR ~a OR B
                        // a OR b => a + b >= 1
                        // ~a = (1-a)+(1-b)+c >= 1 => -a -b +c >= -1
                        // a AND b => c

                        // \sum t1 t2 t3 = s
                        // t1 + t2 + t3 + s_1 = 0
                        // t1 + t2 + t3 - s_2 = 0
                        // t1 + t2 + t3 - s_3 = 0

                    }
                }
            }
        }
    }

    private void preConditionsTransitions() {
        // loop for each time step t
        for (int t = 0; t < loc; t++) {
            // loop for each transition
            for (Transition tr : pnet.getTransitions()) {
                List<VecInt> preconditions = new ArrayList<VecInt>();
                for (Flow f : tr.getPresetEdges()) {
                    VecInt pre = new VecInt();
                    Place p = f.getPlace();
                    int weight = f.getWeight();
                    for (int w = weight; w <= p.getMaxToken(); w++) {
                        Triple<Place, Integer, Integer> triple = new ImmutableTriple<Place, Integer, Integer>(p, t, w);
                        Variable v = place2variable.get(triple);
                        pre.push(v.getId());
                    }
                    preconditions.add(pre);
                }

                Pair<Transition, Integer> pair = new ImmutablePair<Transition, Integer>(tr, t);
                Variable fireTr = transition2variable.get(pair);

                // if f is fired then there are enough resources to fire it
                for (VecInt pc : preconditions) {
                    pc.push(-fireTr.getId());
                    solver.addClause(pc);
                }

                // we cannot fire a transition if we are at max capacity
                // Exception: if the overall difference is zero
                for (Flow f : tr.getPostsetEdges()) {
                    Place p = f.getPlace();
                    int w1 = f.getWeight();
                    Triple<Place, Integer, Integer> triple = new ImmutableTriple<Place, Integer, Integer>(p, t,
                            p.getMaxToken());
                    Variable v = place2variable.get(triple);
                    boolean ok = true;
                    if (p.getId().equals("void"))
                        ok = false;

                    for (Flow o : tr.getPresetEdges()) {
                        Place c = o.getPlace();
                        if (p == c) {
                            int w2 = o.getWeight();
                            // same source as target
                            int diff = w1 - w2;
                            if (diff == 0) {
                                ok = false;
                                break;
                            }
                        }
                    }

                    if (ok) {
                        VecInt clause = new VecInt(new int[] { -v.getId(), -fireTr.getId() });
                        solver.addClause(clause);
                    }
                }
            }
        }
    }

    private void tokenRestrictions() {

        // loop for each time step t
        for (int t = 0; t <= loc; t++) {
            // loop for each place
            for (Place p : pnet.getPlaces()) {
                VecInt amo = new VecInt();
                // loop for each number of tokens
                for (int w = 0; w <= p.getMaxToken(); w++) {
                    Triple<Place, Integer, Integer> triple = new ImmutableTriple<Place, Integer, Integer>(p, t, w);
                    Variable v = place2variable.get(triple);
                    amo.push(v.getId());
                }
                // enforce token restrictions
                solver.addConstraint(amo, ConstraintType.EQ, 1);
            }
        }

    }

    private void noTransitionTokens() {

        // loop for each time step t
        for (int t = 0; t < loc; t++) {
            // loop for each place
            for (Place p : pnet.getPlaces()) {
                Set<Transition> transitions = new HashSet<Transition>();
                transitions.addAll(p.getPostset());
                transitions.addAll(p.getPreset());
                VecInt transitionsConstr = new VecInt();
                for (Transition tr : transitions) {
                    Pair<Transition, Integer> pair = new ImmutablePair<Transition, Integer>(tr, t);
                    transitionsConstr.push(transition2variable.get(pair).getId());
                }

                for (int w = 0; w <= p.getMaxToken(); w++) {
                    Triple<Place, Integer, Integer> current = new ImmutableTriple<Place, Integer, Integer>(p, t, w);
                    Triple<Place, Integer, Integer> next = new ImmutableTriple<Place, Integer, Integer>(p, t + 1, w);

                    VecInt clause = new VecInt();
                    transitionsConstr.copyTo(clause);
                    clause.push(-place2variable.get(current).getId());
                    clause.push(place2variable.get(next).getId());
                    solver.addClause(clause);
                }
            }
        }
    }

    private void dummyConstraints() {

        for (int v = 1; v <= nbVariables; v++) {
            VecInt constraint = new VecInt();
            constraint.push(v);
            solver.addConstraint(constraint, ConstraintType.LTE, 1);
        }

    }

    private void mustFireTransitions() {
        for(String name : mustTransitions) {
            Transition tr = pnet.getTransition(name);
            VecInt clause = new VecInt();
            for(int t = 0; t < loc; t++) {
                Pair<Transition, Integer> pair = new ImmutablePair<Transition, Integer>(tr, t);
                Variable var = transition2variable.get(pair);
                clause.push(var.getId());
            }

            // at least one transition is going to be fired
            solver.addClause(clause);
        }
    }

    private void weightedTransitions() {
        for(Transition tr : pnet.getTransitions()){
            for(int t = 0; t < loc; t++) {
                Pair<Transition, Integer> pair = new ImmutablePair<Transition, Integer>(tr, t);
                Variable var = transition2variable.get(pair);
                VecInt clause = new VecInt();
                clause.push(-var.getId());
                if(tr.getId().contains("intersection")) {
                    solver.addSoftClause(1, clause);
                } else {
                    solver.addSoftClause(100, clause);
                }
            }
        }
    }

    @Override
    public void createVariables() {
        assert (pnet != null);

        // Place -> Nodes (Types)
        // e.g. MyPoint(line#, #tokens)
        // e.g. LOC = 1; MaxTokens = 2; MyPoint(0,0), MyPoint(0,1), MyPoint(0,2),
        // MyPoint(1,0), MyPoint(1,1), MyPoint(1,2)
        // MyPoint(0,b1,c), MyPoint(0,b2,c) ; LOC=2,
        // MyPoint(0,b1,0),MyPoint(0,b1,1),MyPoint(0,b1,2) ...

        for (Place p : pnet.getPlaces()) {
            for (int t = 0; t <= loc; t++) {
                for (int v = 0; v <= p.getMaxToken(); v++) {
                    // create a variable with <place in the petri-net, timestamp, value>
                    Triple<Place, Integer, Integer> triple = new ImmutableTriple<Place, Integer, Integer>(p, t, v);
                    Variable var = new Variable(nbVariables, p.getId(), Type.PLACE, t, v);
                    place2variable.put(triple, var);
                    // solver.id2variable.put(nbVariables, var);
                    // each variable is associated with an id (starts at 1)
                    nbVariables++;
                }
            }
        }

        for (Transition tr : pnet.getTransitions()) {
            for (int t = 0; t < loc; t++) {
                // create a variable with <transition in the petri-net,timestamp>
                Pair<Transition, Integer> pair = new ImmutablePair<Transition, Integer>(tr, t);
                Variable var = new Variable(nbVariables, tr.getLabel(), Type.TRANSITION, t);
                transition2variable.put(pair, var);
                solver.id2variable.put(nbVariables, var);
                // each variable is associated with an id (starts at 1)
                nbVariables++;
            }
        }

        // set number of variables in the solver
        solver.setNbVariables(nbVariables);
        assert (solver.getNbVariables() > 0);
    }

    @Override
    public void createConstraints() {

        // for (Transition tr : pnet.getTransitions()) {
        // if (tr.getId().contains("java.net.Socket"))
        // System.out.println("tr = " + tr);
        // }

        // atLeastK(1, "java.net.Socket(Constructor)(java.lang.String int
        // )java.net.Socket");

        // All variables must be used in some constraint
        // These dummy constraints would not be necessary if the above invariant
        // is maintained
        dummyConstraints();

        // Exactly one transition f is fired at each time step t
        sequentialTransitions();

        // A place can only have 0, 1, 2, ..., n tokens. Example: if a place has
        // 2 tokens then it cannot have 3 tokens
        tokenRestrictions();

        // Pre-conditions for firing f
        preConditionsTransitions();

        // Post-conditions for firing f
        postConditionsTransitions();

        // if no transitions were fired that used the place p then the marking
        // of p remains the same from times step t to t+1
        noTransitionTokens();

        mustFireTransitions();

        weightedTransitions();

    }

    @Override
    public void setState(Set<Pair<Place, Integer>> state, int timestep) {

        Set<Place> visited = new HashSet<Place>();
        for (Pair<Place, Integer> p : state) {
            Triple<Place, Integer, Integer> place = new ImmutableTriple<Place, Integer, Integer>(p.getLeft(), timestep,
                    p.getRight());
            int v = place2variable.get(place).getId();
            solver.setTrue(v);
            visited.add(p.getLeft());
        }
    }

}
