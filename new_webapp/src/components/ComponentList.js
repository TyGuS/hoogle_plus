import React from "react";
import { connect } from "react-redux";
// import CandidateList from "./CandidateList";

const mapStateToProps = state => {
    return {
        library: state.library
    };
}

const ComponentListBase = ({library}) => (
    <div class="card"><div class="card-header" role="tab" id="headingmid0"><a role="button" data-toggle="collapse" data-parent="#accordionEx" data-target="#mid0" href="#mid0" aria-expanded="false" aria-controls="mid0" class="collapsed"><h5 class="mb-0"> Data.Bool<svg class="svg-inline--fa fa-angle-down fa-w-10 rotate-icon" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="angle-down" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512" data-fa-i2svg=""><path fill="currentColor" d="M143 352.3L7 216.3c-9.4-9.4-9.4-24.6 0-33.9l22.6-22.6c9.4-9.4 24.6-9.4 33.9 0l96.4 96.4 96.4-96.4c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9l-136 136c-9.2 9.4-24.4 9.4-33.8 0z"></path></svg>
    </h5>
    </a>
    </div>
    <div class="collapse" id="mid0" role="tabpanel" aria-labelledby="headingmid0" data-parent="#accordionEx" style={{}}><div class="card-body"><pre><code class="haskell hljs properties"><span><span class="hljs-attr">Data.Bool</span></span>
    <span><span class="hljs-attr">data</span> <code class="hljs-string language-haskell">Bool -&gt; Bool -&gt; Bool</code></span>
    <span><span class="hljs-attr">False</span> :<span class="hljs-string">: Bool</span></span>
    <span><span class="hljs-attr">True</span> :<span class="hljs-string">: Bool</span></span>
    <span><span class="hljs-meta">(&amp;&amp;)</span> :<span class="hljs-string">: Bool -&gt; Bool -&gt; Bool</span></span>
    <span><span class="hljs-attr">infixr</span> <span class="hljs-string">3 &amp;&amp;</span></span>
    <span><span class="hljs-meta">(||)</span> :<span class="hljs-string">: Bool -&gt; Bool -&gt; Bool</span></span>
    <span><span class="hljs-attr">infixr</span> <span class="hljs-string">2 ||</span></span>
    <span><span class="hljs-attr">not</span> :<span class="hljs-string">: Bool -&gt; Bool</span></span>
    <span><span class="hljs-attr">otherwise</span> :<span class="hljs-string">: Bool</span></span>
    <span><span class="hljs-attr">bool</span> :<span class="hljs-string">: a -&gt; a -&gt; Bool -&gt; a</span></span>
    </code>
    </pre>
    </div>
    </div>
    </div>
);

const ComponentList = connect(mapStateToProps)(ComponentListBase);

export default ComponentList;