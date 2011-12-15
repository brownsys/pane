var gridColor = 'rgba(14,14,14,0.1)';

var emptyInstant = { shares: { item: { name: 'rootShare', req: [] }, children: [] } };

function shareSeriesData(share) {
  var start = pv.min(share.req, function(v) { return v.start; });
  var end = pv.max(share.req, function(v) { return v.end; });
  if (share.req.some(function(v) { return v.end === null; })) {
    end = end + 10;
  }
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
        return v.start <= t && (v.end === null || v.end >= t); 
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
var instant = emptyInstant;
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

var emptySeries = { rows: [], labels: [], m: 0, n: 0 };
mainVis.def("series", emptySeries);

var stack, hRule, vRule;
function setupVis(vis) {
    var x = pv.Scale.linear(0, 1).range(0, Math.floor(w * 0.7)),
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
    .strokeStyle(function(d)  { return d ? gridColor : "#000"; })
     .anchor("center").add(pv.Label)
    .text(x.tickFormat);

  vRule = vis.add(pv.Rule);
  vRule.data(y.ticks())
       .bottom(y)
       .strokeStyle(function(d) { return d ? gridColor : "#000"; })
       .anchor("center").add(pv.Label)
                        .text(y.tickFormat);

};


var sharesLayout;
function setupShareTree(vis) {
  sharesLayout = vis.add(pv.Layout.Indent);
  sharesLayout.nodes(pv.dom(makeSharesTree(instant)).root("Shares").nodes());
  
  sharesLayout.link.add(pv.Line);

  var selectedNode = null;
  
  sharesLayout.node.add(pv.Dot)
      .fillStyle(function(n) { return n === selectedNode ? "#f00" : "#fff" })
      .events("all").event("click", function(n) {
        selectedNode = n;
        var share = allShares[n.nodeName];
        mainVis.series(share);
        var x = pv.Scale.linear(0, share.m - 1).range(0, w * 0.7);
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


var srcVis = mainVis.add(pv.Panel)
  .left(0)
  .width(w * 0.1);
 
srcVis
  .events('all')
  .event('mousedown', pv.Behavior.pan())
  .event('pan',  function() {
    this.transform().x = 0;
    srcVis.render();
  });
  
var srcGrid = srcVis.add(pv.Layout.Grid);
srcGrid.rows(sources.map(function(v) { return [v]; })).cell
  .height(20)
  .add(pv.Label)
  .events('all')
  .event('click', function(v) {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', "/" + v, true);
    xhr.onreadystatechange = function(evt) {
      if (xhr.readyState === 4) {
        if (xhr.status === 200) {
          allShares = { };
          window.instant = emptyInstant;
          allShares = { };
          eval(xhr.responseText); // bad karma
          window.instant = instant;
          sharesSeries(instant.shares, allShares);
          mainVis.series(emptySeries);
          sharesLayout.reset();
          sharesLayout.nodes(pv.dom(makeSharesTree(instant)).root(v).nodes());
          mainVis.render();
        }
        else {
          debugger;
        }
      }
    };
    xhr.send(null);
  });


var treePanel = mainVis.add(pv.Panel).width(w * 0.2).left(w * 0.1);
setupShareTree(treePanel);



setupVis(mainVis.add(pv.Panel).width(Math.floor(w * 0.7)).left(w * 0.3));

mainVis.render();


