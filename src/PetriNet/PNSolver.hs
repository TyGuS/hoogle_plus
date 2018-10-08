{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}

module PetriNet.PNSolver where

import Data.String (fromString)
import Foreign.JNI (withJVM)
import Language.Java.Inline

test :: IO ()
test = do
    withJVM [ fromString ("-Djava.class.path=src/sypet/sypet.jar:"                 ++ 
                                            "src/sypet/lib/sat4j-pb.jar:"          ++
                                            "src/sypet/lib/commons-lang3-3.4.jar:" ++
                                            "src/sypet/lib/gson-2.8.5.jar:"        ++
                                            "src/sypet/lib/apt.jar")
            ] 
       [java| {
            cmu.edu.petrinet.BuildNet buildNet = new cmu.edu.petrinet.BuildNet(new java.util.ArrayList<>());
            uniol.apt.adt.pn.PetriNet net = buildNet.build();
            int loc = 1;
            boolean solution = false;
            int paths = 0;
            int programs = 0;
            java.util.List<String> srcTypes = new java.util.ArrayList<>();
            srcTypes.add("T");
            srcTypes.add("List (Maybe (T))");
            buildNet.setMaxTokens(srcTypes);
            String tgtType = "T";

            while (!solution && loc <= 3) {
                //System.out.println("loc = " + loc);
                // create a formula that has the same semantics as the petri-net
                cmu.edu.reachability.Encoding encoding = new cmu.edu.reachability.SequentialEncoding(net, loc);
                // set initial state and final state
                encoding.setState(cmu.edu.reachability.EncodingUtil.setInitialState(net, srcTypes), 0);
                encoding.setState(cmu.edu.reachability.EncodingUtil.setGoalState(net, tgtType), loc);

                // 4. Perform reachability analysis

                // for each loc find all possible programs
                java.util.List<cmu.edu.reachability.Variable> result = cmu.edu.reachability.Encoding.solver.findPath(loc);
                while (!result.isEmpty() && !solution) {
                    System.out.println(result.toString());
                    result = cmu.edu.reachability.Encoding.solver.findPath(loc);
                }
                loc++;
            }
         } |]