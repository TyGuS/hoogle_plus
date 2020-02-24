import React from "react";

export const SampleQueries = () => (
    <div class="card" id="demo">
        <div class="card-header"><h5 class="card-title">Sample queries</h5>
        </div>
        <div class="card-body"><span> Apply pair:&nbsp;
            <a class="sample"><pre><code class="language-haskell">foo :: (a -&gt; b, a) -&gt; b</code></pre>
            </a>
            </span>
                <br></br>
            <span> Look up element by key:&nbsp;<a class="sample"><code>Eq a =&gt; xs: [(a, b)] -&gt; k: a -&gt; b</code>
                </a>
            </span>
            <br></br>
            <span> Both:&nbsp;
                <a class="sample"><code>f: (a -&gt; b) -&gt; p: (a, a) -&gt; (b, b)</code>
                </a>
            </span>
                <br></br>
                <span> Resolve either:&nbsp;
                    <a class="sample"><code>Either a b -&gt; (a -&gt; b) -&gt; b</code>
                    </a>
                </span>
                <br></br>
        </div>
    </div>
);

export default SampleQueries;
