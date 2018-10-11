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

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import uniol.apt.adt.pn.PetriNet;
import uniol.apt.adt.pn.Place;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class EncodingUtil {

    /*
     * Given petrinet and input, create a set of <Place, Integer> pair that
     * represents the initial state
     */
    public static Set<Pair<Place, Integer>> setInitialState(PetriNet pnet, List<String> inputs){
        // Initial state
        HashSet<Pair<Place,Integer>> initial = new HashSet<>();
        HashMap<Place, Integer> count = new HashMap<Place, Integer>();
        // Count the number of inputs
        for (String input : inputs) {
            Place p;
            p = pnet.getPlace(input);
            if (count.containsKey(p)) {
                count.put(p, count.get(p) + 1);
            } else {
                count.put(p, 1);
            }
        }
        // Add inputs into initial state
        for(Place key : count.keySet()) {
            initial.add(new ImmutablePair<Place, Integer>(key, count.get(key)));
        }

        //Add non-input places into initial states
        Set<Place> ps = pnet.getPlaces();
        for (Place p : ps) {
            boolean isInput = false;
            for (String input : inputs) {
                if (p.getId().equals(input)) {
                    isInput = true;
                }
            }
            if(p.getId().equals("void")) {
                initial.add(new ImmutablePair<Place, Integer>(p, 1));
            }
            else if(!isInput) {
                initial.add(new ImmutablePair<Place, Integer>(p, 0));
            }
        }
        return initial;
    }

    /*
     * Given petrinet and output , create a set of <Place, Integer> pair that
     * represents the goal state
     */
    public static Set<Pair<Place, Integer>> setGoalState(PetriNet pnet, String retType){

        // Final state
        HashSet<Pair<Place,Integer>> initial = new HashSet<>();
        Set<Place> pl = pnet.getPlaces();
        for(Place p : pl){
            if(p.getId().equals("void")) {
                continue;
            } else if (p.getId().equals(retType)) {
                initial.add(new ImmutablePair<Place, Integer>(p, 1));
            } else {
                initial.add(new ImmutablePair<Place, Integer>(p, 0));
            }
        }
        return initial;
    }
}
