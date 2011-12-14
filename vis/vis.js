function shareSeriesData(share) {
  var start = pv.min(share.req, function(v) { return v.start; });
  var end = pv.max(share.req, function(v) { return v.end; });
  var flows = pv.uniq(share.req, function(v) { return JSON.stringify(v.flows); });
  var labels = [];
  var rows = pv.range(flows.length).map(function(flowIndex) {
    labels[flowIndex] = flows[flowIndex];
    
    var flowResvs = share.req.filter(function(v) { 
      return v.data.reserve && 
             JSON.stringify(v.flows) === flows[flowIndex];
    });

    return pv.range(start, end).map(function(t) {
      var timeResvs = flowResvs.filter(function(v) { 
        return v.start <= t && v.end >= t; 
      });
      return pv.sum(timeResvs, function(v) { return v.data.reserve; });
    });
  });

  return {
    name: share.name,
    labels: labels, 
    rows: rows,
    m: end - start,
    n: labels.length
  };
}

function sharesSeries(shareTree, dict) {
  var v = shareSeriesData(shareTree.item);
  dict[v.name] = v;
  shareTree.children.forEach(function(tree) { sharesSeries(tree, dict); });
}

var allShares = { };
sharesSeries(instant.shares, allShares);

function makeSharesTree(state) {
  function f(v) {
    var childNames = v.children.map(function(c) { return c.item.name; });
    return pv.dict(childNames, function() { return f(v.children[this.index]); });
  }

  return f(state.shares);
}

var w = document.body.clientWidth,
    h = document.body.clientHeight;


var mainVis = new pv.Panel();
mainVis.width(w)
   .height(h);

mainVis.def("series", { rows: [], labels: [], m: 0, n: 0 });

var stack, hRule, vRule;
function setupVis(vis) {
   x = pv.Scale.linear(0, 1).range(0, Math.floor(w * 0.8)),
    y = pv.Scale.linear(0, 1).range(0, h);
    stack = vis.add(pv.Layout.Stack);
    stack
    .layers(function() { return mainVis.series().rows; })
    .x(x.by(pv.index))
    .y(y)
    .layer.add(pv.Area)
    .anchor("left").add(pv.Label)
    .def("max", function(d) { return pv.max.index(d); })
    .visible(function() {return this.index == this.max(); })
    .font('10pt')
    .text(function(d, p) {
          // TODO: Hacked to just show flow user
     return JSON.parse(mainVis.series().labels[this.parent.index]).srcUser[0]; });

  hRule = vis.add(pv.Rule);
  hRule.data(x.ticks())
    .left(x)
    .lineWidth(1)
    .strokeStyle(function(d)  { return d ? "#eee" : "#000"; })
     .anchor("center").add(pv.Label)
    .text(x.tickFormat);

  vRule = vis.add(pv.Rule);
  vRule.data(y.ticks())
       .bottom(y)
       .strokeStyle(function(d) { return d ? "#eee" : "#000"; })
       .anchor("center").add(pv.Label)
                        .text(y.tickFormat);

};


function setupShareTree(vis) {
  var sharesTree = pv.dom(makeSharesTree(instant));
  
  
  var sharesLayout = vis.add(pv.Layout.Indent)
     .nodes(sharesTree.root("Shares").nodes())
  
  sharesLayout.link.add(pv.Line);

  var selectedNode = null;
  
  sharesLayout.node.add(pv.Dot)
      .fillStyle(function(n) { return n === selectedNode ? "#f00" : "#fff" })
      .events("all").event("click", function(n) {
        selectedNode = n;
        var share = allShares[n.nodeName];
        mainVis.series(share);
        var x = pv.Scale.linear(0, share.m - 1).range(0, w);
        var y = pv.Scale.linear(0, 100 * share.n).range(0, h);
        hRule.data(x.ticks());
        hRule.left(x);
        vRule.data(y.ticks());
        vRule.bottom(y);
        stack.x(x.by(pv.index));
        stack.y(y);
        mainVis.render();
       });
  
  sharesLayout.label.add(pv.Label);
}

var treePanel = mainVis.add(pv.Panel).width(w * 0.2).left(0);
setupShareTree(treePanel);


setupVis(mainVis.add(pv.Panel).width(Math.floor(w * 0.8)).left(w * 0.2));

mainVis.render();


