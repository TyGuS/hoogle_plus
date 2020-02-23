import React from "react";
import FactTable from "./FactTable";

export const TopBar = () => (
    <div>
    <h1 className="hplus_font">
          <a href="." className="nav-link" id="home">Hoogle+</a>
          <br></br>
          </h1>
    <h5> Welcome to the Hoogle+ Demo </h5>
    <p> {flavorText} </p>
    <FactTable/>
    </div>
);

const flavorText = `
Hoogle+ is a type-driven synthesis engine for Haskell - like Hoogle but able to find compositions of functions. Given a Haskell type, Hoogle+ generates terms that inhabit this type by composing library components. It supports polymorphism, type classes, and higher-order functions. Available library components are listed in the side bar.
`;