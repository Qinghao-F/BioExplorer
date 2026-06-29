/* global Shiny, d3, r2d3 */

// Leaves: top line = species, second line = common (if different); colors match map.
// Shapes: Animalia/Fungi = diamond, others = circle; auto-highlight "newly appeared leaves" only; selecting a new leaf clears old highlights.

r2d3.onRender(function (data, svg, width, height, options) {
  const cfg = Object.assign(
    {
      link_dist: 18,
      node_r_root: 10,
      node_r_internal: 6,
      node_r_leaf_big: 12,
      node_r_collapsed: 7,
      zoom_min: 0.4,
      zoom_max: 3,
      levels: [],
      initial_view: "kingdoms",
      start_color: "#dcefebff",
      end_color: "#003d35",
      depth_gamma: 0.85,
      t_min_nonkingdom: 0.2,

      // Shiny input binding for leaf clicks (e.g., ns("rtree_click_from_js"))
      leaf_click_input: null,

      // highlight style
      highlight_color: "#ffd700",
      highlight_width: 3,

      // hide leaves with no month
      hide_no_month: true,

      // legend scale (visual size multiplier)
      legend_scale: 1.1,
    },
    options || {}
  );

  const MONTH_PALETTE = [
    "#e41a1c",
    "#ff7f00",
    "#be4dea",
    "#55ca51",
    "#377eb8",
    "#984ea3",
    "#a65628",
    "#f781bf",
    "#1b9e77",
    "#7570b3",
    "#66a61e",
    "#e6ab02",
  ];
  const NEUTRAL_LEAF = "#9e9e9e";

  const TERM_TIPS = {
    "Biodiversity (Root)":
      "The starting point of the tree — not a real taxonomic rank.",
    Kingdom: "The broadest bucket, like “Animals” or “Plants”.",
    Taxa: "A flexible grouping between Kingdom and Phylum (e.g., “Fauna”/“Flora”).",
    Phylum: "A big subgroup inside a kingdom.",
    Class: "A subgroup inside a phylum.",
    Order: "A subgroup inside a class.",
    Family: "A subgroup inside an order.",
    Genus: "A tight group inside a family.",
    Species: "A single kind of organism.",
  };

  // ---------- helpers ----------
  function deepClone(o) {
    return JSON.parse(JSON.stringify(o));
  }
  function validMonth(m) {
    return Number.isFinite(+m) && +m >= 1 && +m <= 12;
  }

  function copyMeta(dst, src) {
    const s = src && src.data ? src.data : src || {};
    [
      "uid",
      "uids",
      "month",
      "commonname",
      "kingdom",
      "taxa",
      "phylum",
      "class",
      "order",
      "family",
      "genus",
      "species",
    ].forEach((k) => {
      if (s[k] !== undefined) dst[k] = s[k];
    });
  }

  function pruneNoMonthRaw(n) {
    if (!n) return null;
    const kids = Array.isArray(n.children) ? n.children : [];
    if (!kids.length) return validMonth(n.month) ? deepClone(n) : null;
    const pruned = kids.map(pruneNoMonthRaw).filter(Boolean);
    if (!pruned.length) return null;
    const out = deepClone(n);
    out.children = pruned;
    return out;
  }

  // avoid double-render with same key
  const key =
    options && options.render_key ? String(options.render_key) : "__once__";
  const host = svg.node();
  if (host.__lastKey === key) return;
  host.__lastKey = key;

  // canvas
  svg.selectAll("*").remove();
  svg
    .attr("width", width)
    .attr("height", height)
    .attr("viewBox", [0, 0, width, height])
    .style("background", "none");

  // layered groups
  const legend = svg.append("g").attr("class", "legend"); // legend box (items + bg)
  const legendReset = svg.append("g").attr("class", "legend-reset"); // reset button (outside box, above)
  const legendTips = svg.append("g").attr("class", "legend-tips"); // tips (outside box, below)
  const ui = svg.append("g").attr("class", "ui-layer"); // tooltips, callouts

  // persistent popup near node
  let currentTransform = d3.zoomIdentity;
  let pinnedNode = null;

  const callout = ui
    .append("g")
    .attr("class", "node-callout")
    .style("display", "none");
  const callRect = callout
    .append("rect")
    .attr("rx", 8)
    .attr("ry", 8)
    .attr("fill", "rgba(255,255,255,0.98)")
    .attr("stroke", "#e0e7ea")
    .attr("stroke-width", 0.8);
  const callText = callout
    .append("text")
    .attr("x", 10)
    .attr("y", 16)
    .attr("fill", "#334349")
    .attr("font-size", 12)
    .style(
      "font-family",
      "system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif"
    );

  function updateCalloutPosition(d) {
    if (!d) return;
    const sx = currentTransform.applyX(d.x);
    const sy = currentTransform.applyY(d.y);
    callout.attr("transform", `translate(${sx + 12}, ${sy + 12})`);
  }
  function showNodePopup(d) {
    pinnedNode = d;
    const speciesName = String(d?.data?.name || "");
    const common = String(d?.data?.commonname || "");
    callText.selectAll("tspan").remove();
    callText.append("tspan").text(speciesName).attr("font-weight", 600);
    if (common && common.toLowerCase() !== speciesName.toLowerCase()) {
      callText
        .append("tspan")
        .attr("x", 10)
        .attr("dy", "1.4em")
        .text("Common: " + common)
        .attr("font-size", 11)
        .attr("fill", "#607d8b");
    }
    const bbox = callText.node().getBBox();
    callRect
      .attr("x", 0)
      .attr("y", 0)
      .attr("width", Math.ceil(bbox.width + 20))
      .attr("height", Math.ceil(bbox.height + 12));
    updateCalloutPosition(d);
    callout.style("display", null);
    ui.raise();
  }
  function hideNodePopup() {
    callout.style("display", "none");
    pinnedNode = null;
  }

  // legend tooltip
  let tipPinned = false;
  const tip = ui
    .append("g")
    .attr("class", "legend-tooltip")
    .style("pointer-events", "none")
    .style("display", "none");
  const tipPadX = 10,
    tipPadY = 8;
  let tipMaxW = 260 * (cfg.legend_scale || 1.0);
  const tipRect = tip
    .append("rect")
    .attr("rx", 8)
    .attr("ry", 8)
    .attr("fill", "rgba(0,0,0,0.85)")
    .attr("stroke", "#e0e7ea")
    .attr("stroke-width", 0.6);
  const tipText = tip
    .append("text")
    .attr("x", tipPadX)
    .attr("y", tipPadY + 11)
    .attr("fill", "#fff")
    .attr("font-size", Math.round(12 * (cfg.legend_scale || 1.0)))
    .style(
      "font-family",
      "system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif"
    );

  function wrapSVGText(textSel, raw, maxWidth) {
    textSel.selectAll("tspan").remove();
    const words = String(raw).split(/\s+/);
    let line = [],
      lineNumber = 0;
    const lineHeight = Math.round(16 * (cfg.legend_scale || 1.0));
    let tspan = textSel
      .append("tspan")
      .attr("x", tipPadX)
      .attr("dy", 0)
      .text("");
    for (let i = 0; i < words.length; i++) {
      line.push(words[i]);
      tspan.text(line.join(" "));
      if (tspan.node().getComputedTextLength() > maxWidth) {
        line.pop();
        tspan.text(line.join(" "));
        line = [words[i]];
        tspan = textSel
          .append("tspan")
          .attr("x", tipPadX)
          .attr("dy", lineNumber === 0 ? lineHeight : lineHeight)
          .text(words[i]);
        lineNumber++;
      }
    }
    return (lineNumber + 1) * lineHeight + tipPadY * 2;
  }
  function moveTipToPointer(event) {
    const [mx, my] = d3.pointer(event, svg.node());
    const offset = 12;
    let x = mx + offset,
      y = my + offset;
    const bbox = tip.node().getBBox(),
      margin = 8;
    if (x + bbox.width + margin > width)
      x = Math.max(margin, width - bbox.width - margin);
    if (y + bbox.height + margin > height)
      y = Math.max(margin, height - bbox.height - margin);
    tip.attr("transform", `translate(${x},${y})`);
  }
  function showLegendTip(text, event) {
    if (!text) return;
    tip.style("display", null);
    const h = wrapSVGText(tipText, text, tipMaxW);
    const bbox = tipText.node().getBBox();
    tipRect
      .attr("x", 0)
      .attr("y", 0)
      .attr("width", Math.ceil(Math.max(bbox.width + tipPadX * 2, 80)))
      .attr("height", Math.ceil(h));
    ui.raise();
    tip.raise();
    moveTipToPointer(event);
  }
  function hideLegendTip() {
    if (!tipPinned) tip.style("display", "none");
  }

  // levels & colors
  const levelsArr = Array.isArray(cfg.levels)
    ? cfg.levels.map((s) => String(s).toLowerCase())
    : [];
  const ORDERED_LEVELS = levelsArr.length
    ? levelsArr
    : ["kingdom", "phylum", "class", "order", "family", "genus", "species"];
  const Pretty = (s) => (s ? s.charAt(0).toUpperCase() + s.slice(1) : s);
  const C0 = d3.rgb(cfg.start_color),
    C1 = d3.rgb(cfg.end_color);
  const interp = d3.interpolateRgb(C0, C1);

  const nodeName = (n) => (n && (n.name ?? n.data?.name)) || "";
  function compressUnaryChains(node, parentPathParts, levelIdxBase) {
    const out = { name: nodeName(node) };
    copyMeta(out, node);
    out._levelIdx = levelIdxBase || 0;
    out._pathParts = (parentPathParts || []).concat([out.name]);

    const kids = Array.isArray(node.children) ? node.children : [];
    if (kids.length) {
      const nextBasePath = out._pathParts.slice();
      const children = kids
        .map((ch) => {
          let names = [nodeName(ch)],
            cur = ch,
            hops = 1;
          while (cur.children && cur.children.length === 1) {
            cur = cur.children[0];
            names.push(nodeName(cur));
            hops += 1;
          }
          const leafLevelIx = Math.min(
            ORDERED_LEVELS.length,
            (levelIdxBase || 0) + hops
          );
          const childOut = {
            name: names[names.length - 1],
            _pathParts: nextBasePath.concat(names),
            _fullJoined: names.join(" / "),
            _levelIdx: leafLevelIx,
          };
          copyMeta(childOut, cur);
          const branchKids =
            cur.children && cur.children.length > 1 ? cur.children : [];
          if (branchKids.length) {
            childOut.children = branchKids
              .map((grand) =>
                compressUnaryChains(grand, childOut._pathParts, leafLevelIx)
              )
              .filter(Boolean);
          }
          return childOut;
        })
        .filter(Boolean);
      if (children.length) out.children = children;
    }
    return out;
  }

  const sourceData = cfg.hide_no_month
    ? (() => {
        const c = deepClone(data);
        c.children = (c.children || []).map(pruneNoMonthRaw).filter(Boolean);
        return c;
      })()
    : data;

  const dataCompressed = compressUnaryChains(sourceData, [], 0) || {
    name: "Biodiversity",
  };
  const root = d3.hierarchy(dataCompressed);

  // max depth
  const maxDepth = (() => {
    let m = 0;
    root.each((d) => {
      if (d.depth > m) m = d.depth;
    });
    return Math.max(m, 2);
  })();

  function levelColorByDepth(depth) {
    if (depth <= 0) return d3.color(C0).formatHex();
    if (depth === 1)
      return d3.color(d3.interpolateRgb(C0, C1)(0.1)).formatHex();
    const rel = (depth - 2) / Math.max(1, maxDepth - 2);
    const t =
      cfg.t_min_nonkingdom +
      (1 - cfg.t_min_nonkingdom) *
        Math.pow(Math.max(0, Math.min(1, rel)), cfg.depth_gamma);
    return d3.color(interp(t)).formatHex();
  }
  const getMonth = (d) => {
    const m = +d?.data?.month;
    return Number.isFinite(m) && m >= 1 && m <= 12 ? m : null;
  };
  const isLeaf = (d) => !d.children && !d._children;

  // leaf shape by top kingdom
  const topKingdom = (d) => {
    if (d?.data && Array.isArray(d.data._pathParts))
      return String(d.data._pathParts[1] || "").toLowerCase();
    const parts = d
      .ancestors()
      .map((x) => x.data?.name || "")
      .reverse();
    return String(parts[1] || "").toLowerCase();
  };
  const leafColor = (d) => {
    const m = getMonth(d);
    return m ? MONTH_PALETTE[m - 1] : NEUTRAL_LEAF;
  };

  function collapse(d) {
    if (d.children) {
      d._children = d.children;
      d.children = null;
    }
  }
  function collapseAll(d) {
    collapse(d);
    (d._children || []).forEach(collapseAll);
  }
  function expandAll(d) {
    if (d._children) {
      d.children = d._children;
      d._children = null;
    }
    (d.children || []).forEach(expandAll);
  }
  function showAllKingdomsCollapsed() {
    collapseAll(root);
    if (root._children) {
      root.children = root._children;
      root._children = null;
    }
  }

  if (cfg.initial_view === "all") expandAll(root);
  else if (cfg.initial_view === "animalia") {
    collapseAll(root);
    if (root._children && root._children.length) {
      const idx = root._children.findIndex((d) =>
        /animalia?/i.test((d.data && d.data.name) || "")
      );
      const pick =
        idx >= 0 ? root._children.splice(idx, 1)[0] : root._children[0];
      collapseAll(pick);
      root.children = [pick];
      root._children = [];
    }
  } else showAllKingdomsCollapsed();

  const scene = svg
    .append("g")
    .attr("class", "scene")
    .style("pointer-events", "all");

  // zoom
  const zoom = d3
    .zoom()
    .scaleExtent([cfg.zoom_min, cfg.zoom_max])
    .on("zoom", (e) => {
      scene.attr("transform", e.transform);
      currentTransform = e.transform;
      if (pinnedNode) updateCalloutPosition(pinnedNode);
    });
  svg.call(zoom).on("dblclick.zoom", null);
  svg.call(
    zoom.transform,
    d3.zoomIdentity.translate(width / 2, height / 2).scale(2)
  );
  currentTransform = d3.zoomIdentity.translate(width / 2, height / 2).scale(2);

  const linkGroup = scene.append("g").attr("stroke-opacity", 0.95);
  const nodeGroup = scene.append("g");
  let nodes = flatten(root);
  let links = root.links();

  const simulation = d3
    .forceSimulation(nodes)
    .force("link", d3.forceLink(links).distance(cfg.link_dist).strength(1.0))
    .force("charge", d3.forceManyBody().strength(-110))
    .force(
      "collision",
      d3.forceCollide().radius((d) => collideRadius(d) + 1.5)
    )
    .force("center", d3.forceCenter(0, 0))
    .alpha(0.6)
    .alphaDecay(0.12);

  // highlight bookkeeping
  let firstRenderDone = false;
  let prevNodeKeySet = null;
  let highlightedKeySet = new Set();

  const nodeKey = (d) =>
    d.data?._fullJoined ||
    d
      .ancestors()
      .map((a) => a.data.name)
      .reverse()
      .join("/");

  function highlightByKeySet(keySet) {
    nodeGroup.selectAll("g").each(function (d) {
      const g = d3.select(this);
      const key = nodeKey(d);
      const on = keySet.has(key);
      const shape = nodeShape(d);

      g.select(".dot-circle")
        .attr("stroke", on ? cfg.highlight_color : "#fff")
        .attr("stroke-width", on ? cfg.highlight_width : 1.2);
      g.select(".dot-rect")
        .attr("stroke", on ? cfg.highlight_color : "#fff")
        .attr("stroke-width", on ? cfg.highlight_width : 1.2);

      g.select(".dot-halo-circle").style(
        "opacity",
        on && shape === "circle" ? 1 : 0
      );
      g.select(".dot-halo-rect").style(
        "opacity",
        on && shape !== "circle" ? 1 : 0
      );
    });
  }
  function setHighlightKeys(keys) {
    highlightedKeySet = new Set(keys);
    highlightByKeySet(highlightedKeySet);
  }
  function setSingleHighlight(d) {
    setHighlightKeys([nodeKey(d)]);
  }

  function applyAutoHighlightNew() {
    const currMap = new Map(nodes.map((nd) => [nodeKey(nd), nd]));
    const currKeys = new Set(currMap.keys());
    if (!firstRenderDone || prevNodeKeySet === null) {
      prevNodeKeySet = new Set(currKeys);
      firstRenderDone = true;
      return;
    }
    const newLeafKeys = [];
    currMap.forEach((nd, key) => {
      if (!prevNodeKeySet.has(key) && isLeaf(nd)) newLeafKeys.push(key);
    });
    if (newLeafKeys.length) setHighlightKeys(newLeafKeys);
    prevNodeKeySet = new Set(currKeys);
  }

  render();
  drawLegend(); // will also draw Reset (above) and Tips (below), then position them
  ui.raise();

  prevNodeKeySet = new Set(nodes.map(nodeKey));
  firstRenderDone = true;

  function nodeShape(d) {
    if (isLeaf(d))
      return /^(animalia|fungi)$/.test(topKingdom(d)) ? "diamond" : "circle";
    if (d.depth === 0) return "circle";
    if (d.depth === 1)
      return /^(animalia|fungi)$/.test(String(d.data?.name || "").toLowerCase())
        ? "diamond"
        : "square";
    return "square";
  }
  function circleRadius(d) {
    if (d.depth === 0) return cfg.node_r_root;
    if (isLeaf(d)) return cfg.node_r_leaf_big;
    if (d.depth === 1)
      return Math.max(cfg.node_r_leaf_big, cfg.node_r_internal + 2);
    return cfg.node_r_internal;
  }
  function squareSide(d) {
    if (isLeaf(d)) return Math.max(14, cfg.node_r_leaf_big * 2.2);
    if (d.depth === 1) return Math.max(10, cfg.node_r_internal * 2.4 * 0.9);
    return Math.max(8, cfg.node_r_internal * 2.2 * 0.85);
  }
  function collideRadius(d) {
    return nodeShape(d) === "circle"
      ? circleRadius(d)
      : (squareSide(d) * Math.SQRT2) / 2;
  }
  function labelOffsetX(d, shape) {
    return shape === "circle" ? circleRadius(d) + 6 : squareSide(d) / 2 + 6;
  }

  function render() {
    const link = linkGroup
      .selectAll("line")
      .data(
        links,
        (d) =>
          (d.source.data._fullJoined || d.source.data.name) +
          "→" +
          (d.target.data._fullJoined || d.target.data.name)
      );
    link.exit().remove();
    link
      .enter()
      .append("line")
      .attr("stroke", "#2f3b43")
      .attr("stroke-width", 1.2);

    const node = nodeGroup.selectAll("g").data(
      nodes,
      (d) =>
        d.data._fullJoined ||
        d
          .ancestors()
          .map((x) => x.data.name)
          .reverse()
          .join("/")
    );
    node.exit().remove();

    const nodeEnter = node
      .enter()
      .append("g")
      .attr("cursor", "pointer")
      .call(
        d3
          .drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended)
      )
      .on("click", click)
      .on("mouseover", (e, d) => setFill(d3.select(e.currentTarget), d, true))
      .on("mouseout", (e, d) => setFill(d3.select(e.currentTarget), d, false));

    nodeEnter
      .append("circle")
      .attr("class", "dot-halo-circle")
      .attr("fill", "none")
      .attr("stroke", cfg.highlight_color)
      .attr("stroke-width", cfg.highlight_width)
      .style("opacity", 0)
      .style("pointer-events", "none");
    nodeEnter
      .append("rect")
      .attr("class", "dot-halo-rect")
      .attr("fill", "none")
      .attr("stroke", cfg.highlight_color)
      .attr("stroke-width", cfg.highlight_width)
      .attr("rx", 2)
      .attr("ry", 2)
      .style("opacity", 0)
      .style("pointer-events", "none");

    nodeEnter
      .append("circle")
      .attr("class", "dot-circle")
      .attr("stroke", "#fff")
      .attr("stroke-width", 1.2);
    nodeEnter
      .append("rect")
      .attr("class", "dot-rect")
      .attr("stroke", "#fff")
      .attr("stroke-width", 1.2)
      .attr("rx", 2)
      .attr("ry", 2);

    nodeEnter
      .append("text")
      .attr("class", "node-label")
      .style("font-size", "10px")
      .style(
        "font-family",
        "system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif"
      )
      .style("paint-order", "stroke")
      .style("stroke", "rgba(255,255,255,0.9)")
      .style("stroke-width", 2)
      .style("fill", "#263238");
    nodeEnter.append("title");

    const nodeAll = nodeEnter.merge(node);
    nodeAll.each(function (d) {
      const g = d3.select(this);
      const shape = nodeShape(d);
      const fill = isLeaf(d) ? leafColor(d) : levelColorByDepth(d.depth);

      g.select("circle.dot-circle").style("display", "none");
      g.select("rect.dot-rect")
        .style("display", "none")
        .attr("transform", null);

      if (shape === "circle") {
        const r = circleRadius(d);
        g.select("circle.dot-circle")
          .style("display", null)
          .attr("r", r)
          .attr("fill", fill);
      } else {
        const side = squareSide(d),
          half = side / 2;
        g.select("rect.dot-rect")
          .style("display", null)
          .attr("x", -half)
          .attr("y", -half)
          .attr("width", side)
          .attr("height", side)
          .attr("fill", fill)
          .attr("transform", shape === "diamond" ? "rotate(45)" : null);
      }

      const tx = labelOffsetX(d, shape);
      const label = g.select("text");
      label.selectAll("tspan").remove();
      label.text(null);

      const speciesName = String(d.data.name || "").trim();
      const commonRaw =
        d.data && d.data.commonname ? String(d.data.commonname).trim() : "";
      const hasCommon =
        isLeaf(d) &&
        commonRaw &&
        commonRaw.toLowerCase() !== speciesName.toLowerCase();

      if (hasCommon) {
        label.attr("x", tx).attr("y", 0);
        label
          .append("tspan")
          .attr("x", tx)
          .attr("dy", "0.4em")
          .text(speciesName);
        label
          .append("tspan")
          .attr("x", tx)
          .attr("dy", "1.2em")
          .text(commonRaw)
          .attr("font-size", "9px")
          .attr("fill", "#607d8b");
      } else {
        label.text(speciesName).attr("x", tx).attr("y", 3);
      }

      g.select("title").text(
        hasCommon
          ? `${d.data._fullJoined || speciesName}\nCommon: ${commonRaw}`
          : d.data._fullJoined || speciesName
      );

      if (shape === "circle") {
        const r = circleRadius(d) + 4;
        g.select(".dot-halo-circle").attr("r", r).style("opacity", 0);
        g.select(".dot-halo-rect").style("opacity", 0);
      } else {
        const s = squareSide(d) + 8,
          half2 = s / 2;
        g.select(".dot-halo-rect")
          .attr("x", -half2)
          .attr("y", -half2)
          .attr("width", s)
          .attr("height", s)
          .attr("transform", "rotate(45)")
          .style("opacity", 0);
        g.select(".dot-halo-circle").style("opacity", 0);
      }
    });

    simulation.nodes(nodes).on("tick", ticked);
    simulation.force("link").links(links);
    simulation.alpha(0.9).restart();

    function ticked() {
      linkGroup
        .selectAll("line")
        .attr("x1", (d) => d.source.x)
        .attr("y1", (d) => d.source.y)
        .attr("x2", (d) => d.target.x)
        .attr("y2", (d) => d.target.y);
      nodeGroup
        .selectAll("g")
        .attr("transform", (d) => `translate(${d.x},${d.y})`);
      if (pinnedNode) updateCalloutPosition(pinnedNode);
    }

    // highlight only new leaves after render
    applyAutoHighlightNew();
  }

  function nodeFill(d) {
    return isLeaf(d) ? leafColor(d) : levelColorByDepth(d.depth);
  }
  function setFill(sel, d, hover) {
    const base = nodeFill(d);
    const fill = hover ? d3.color(base).brighter(0.7).formatHex() : base;
    sel.select("circle.dot-circle").attr("fill", fill);
    sel.select("rect.dot-rect").attr("fill", fill);
  }

  const joinPath = (arr) =>
    (arr || []).map((s) => String(s || "").trim()).join(" / ");
  function expandToNode(d) {
    d.ancestors().forEach((a) => {
      if (a._children) {
        a.children = a._children;
        a._children = null;
      }
    });
  }
  function findByPath(pathParts) {
    if (!pathParts || !pathParts.length) return null;
    const target = joinPath(pathParts);
    let hit =
      nodes.find(
        (nd) =>
          Array.isArray(nd.data._pathParts) &&
          joinPath(nd.data._pathParts) === target
      ) || null;
    if (hit) return hit;
    hit =
      nodes.find(
        (nd) =>
          joinPath(
            nd
              .ancestors()
              .map((a) => a.data.name)
              .reverse()
          ) === target
      ) || null;
    return hit;
  }
  function findByUID(uid) {
    if (!uid) return null;
    const s = String(uid);
    return (
      nodes.find((nd) => {
        const d = nd?.data || {};
        if (d.uid && String(d.uid) === s) return true;
        if (Array.isArray(d.uids) && d.uids.map(String).includes(s))
          return true;
        return false;
      }) || null
    );
  }

  function clearHighlight() {
    nodeGroup.selectAll("g").each(function () {
      const g = d3.select(this);
      g.select(".dot-halo-circle").style("opacity", 0);
      g.select(".dot-halo-rect").style("opacity", 0);
      g.select(".dot-circle").attr("stroke", "#fff").attr("stroke-width", 1.2);
      g.select(".dot-rect").attr("stroke", "#fff").attr("stroke-width", 1.2);
    });
  }

  function refresh() {
    simulation.stop();
    nodes = flatten(root);
    links = root.links();
    render(); // applyAutoHighlightNew() is called inside render()
  }

  // map → tree (expand path, then center & popup)
  function expandHighlightByPath(pathParts, fallbackName, meta) {
    const centerAndPopup = (picked) => {
      if (!picked) return;
      requestAnimationFrame(() => {
        setSingleHighlight(picked);
        try {
          const t = d3.zoomTransform(svg.node());
          const k = Math.max(1, Math.min(2.2, t.k));
          const x = width / 2 - picked.x * k;
          const y = height / 2 - picked.y * k;
          svg
            .transition()
            .duration(260)
            .call(zoom.transform, d3.zoomIdentity.translate(x, y).scale(k));
        } catch (e) {}
        showNodePopup(picked);
      });
    };

    if (meta && meta.uid) {
      let target = findByUID(meta.uid);
      if (!target && Array.isArray(pathParts) && pathParts.length >= 2) {
        for (let k = 2; k <= pathParts.length; k++) {
          const partial = pathParts.slice(0, k);
          let mid = findByPath(partial);
          if (!mid && k === 2) {
            expandToNode(root);
            refresh();
            mid = findByPath(partial);
          }
          if (mid) {
            expandToNode(mid);
            refresh();
          }
        }
        target = findByUID(meta.uid);
      }
      if (target) {
        if (meta.commonname) target.data.commonname = String(meta.commonname);
        if (Number.isFinite(+meta.month)) target.data.month = +meta.month;
        refresh();
        const again = findByUID(meta.uid) || target;
        centerAndPopup(again);
        return;
      }
      return;
    }

    if (!pathParts || pathParts.length < 2) return;
    for (let k = 2; k <= pathParts.length; k++) {
      const partial = pathParts.slice(0, k);
      let mid = findByPath(partial);
      if (!mid && k === 2) {
        expandToNode(root);
        refresh();
        mid = findByPath(partial);
      }
      if (mid) {
        expandToNode(mid);
        refresh();
      }
    }
    let target = findByPath(pathParts);
    if (!target && fallbackName) {
      const matches = nodes.filter((nd) => nd?.data?.name === fallbackName);
      if (matches.length === 1) target = matches[0];
    }
    centerAndPopup(target);
  }

  // click handlers
  function click(e, d) {
    if (e.defaultPrevented) return;
    if (d.children) {
      d._children = d.children;
      d.children = null;
    } else if (d._children) {
      d.children = d._children;
      d._children = null;
    }
    refresh();
    if (isLeaf(d)) {
      setSingleHighlight(d);
      showNodePopup(d);
      if (typeof Shiny !== "undefined" && d.data && d.data.name) {
        const id =
          (options && options.leaf_click_input) ||
          cfg.leaf_click_input ||
          "rtree_click_from_js";
        Shiny.setInputValue(id, d.data.name, { priority: "event" });
      }
    } else {
      hideNodePopup();
    }
  }
  function flatten(root) {
    const out = [];
    root.eachBefore((d) => out.push(d));
    return out;
  }
  function dragstarted(e, d) {
    if (!e.active) simulation.alphaTarget(0.25).restart();
    d.fx = d.x;
    d.fy = d.y;
  }
  function dragged(e, d) {
    d.fx = e.x;
    d.fy = e.y;
    if (pinnedNode === d) updateCalloutPosition(d);
  }
  function dragended(e, d) {
    if (!e.active) simulation.alphaTarget(0);
    d.fx = null;
    d.fy = null;
  }

  // -------- Legend (box only) + external Reset (above) + external Tips (below) --------
  function buildLegendItems() {
    const items = [
      {
        label: "Biodiversity (Root)",
        color: levelColorByDepth(0),
        shape: "circle",
      },
      { label: "Kingdom", color: levelColorByDepth(1), shape: "square" },
    ];
    for (let i = 2; i <= ORDERED_LEVELS.length; i++) {
      const lbl = Pretty(ORDERED_LEVELS[i - 1]);
      if (lbl === "Species")
        items.push({ label: "Species", color: "#ff9800", shape: "circle" });
      else
        items.push({
          label: lbl,
          color: levelColorByDepth(i),
          shape: "square",
        });
    }
    return items.map((it) => ({ ...it, tip: TERM_TIPS[it.label] || "" }));
  }

  function drawLegend() {
    legend.selectAll("*").remove();

    const scale = cfg.legend_scale || 1.0;
    const pad = Math.round(14 * scale);
    const sw = Math.round(14 * scale);
    const gapY = Math.round(8 * scale);
    const txtOff = Math.round(8 * scale);
    const fontPx = Math.round(13 * scale);

    const inner = legend.append("g").attr("class", "legend-inner");

    const items = buildLegendItems();
    const row = inner
      .selectAll("g.item")
      .data(items)
      .enter()
      .append("g")
      .attr("class", "item")
      .attr("transform", (d, i) => `translate(0, ${i * (sw + gapY)})`)
      .attr("tabindex", 0)
      .attr("aria-label", (d) => d.tip || d.label)
      .on("mouseenter", (e, d) => {
        tipPinned = false;
        if (d.tip) showLegendTip(d.tip, e);
      })
      .on("mousemove", (e, d) => {
        if (d.tip) moveTipToPointer(e);
      })
      .on("mouseleave", () => {
        hideLegendTip();
      })
      .on("click", (e, d) => {
        if (!d.tip) return;
        tipPinned = !tipPinned;
        if (tipPinned) showLegendTip(d.tip, e);
        else hideLegendTip();
      })
      .on("focus", (e, d) => {
        tipPinned = false;
        if (d.tip) showLegendTip(d.tip, e);
      })
      .on("blur", () => {
        hideLegendTip();
      });

    row.each(function (d) {
      const g = d3.select(this);
      if (d.shape === "circle") {
        g.append("circle")
          .attr("cx", sw / 2)
          .attr("cy", sw / 2)
          .attr("r", sw / 2)
          .attr("fill", d.color)
          .attr("stroke", "#2f3b43")
          .attr("stroke-width", 0.8);
      } else {
        g.append("rect")
          .attr("x", 0)
          .attr("y", 0)
          .attr("width", sw)
          .attr("height", sw)
          .attr("fill", d.color)
          .attr("stroke", "#2f3b43")
          .attr("stroke-width", 0.8);
      }
      g.append("text")
        .attr("x", sw + txtOff)
        .attr("y", sw / 2 + Math.round(4 * scale))
        .text(d.label)
        .attr("font-size", fontPx)
        .attr("fill", "#334349")
        .style(
          "font-family",
          "system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif"
        );
    });

    // background only around legend box
    const bbox = inner.node().getBBox();
    legend
      .insert("rect", ".legend-inner")
      .attr("class", "legend-bg")
      .attr("x", bbox.x - pad)
      .attr("y", bbox.y - pad)
      .attr("width", bbox.width + 2 * pad)
      .attr("height", bbox.height + 2 * pad)
      .attr("rx", Math.round(10 * scale))
      .attr("ry", Math.round(10 * scale))
      .attr("fill", "rgba(255,255,255,0.95)")
      .attr("stroke", "#e0e7ea")
      .attr("stroke-width", Math.max(0.8, 0.8 * scale));

    // draw external reset & tips (not inside legend)
    drawResetAndTips();

    // finally, position legend + external elements together
    positionLegend();
  }

  function drawResetAndTips() {
    legendReset.selectAll("*").remove();
    legendTips.selectAll("*").remove();

    const scale = cfg.legend_scale || 1.0;

    // --- Reset button (outside legend box, above) ---
    const btnW = Math.round(96 * scale);
    const btnH = Math.round(24 * scale);

    const resetG = legendReset
      .append("g")
      .attr("class", "legend-reset-btn")
      .style("cursor", "pointer")
      .on("click", resetTree)
      .on("mouseenter", function () {
        d3.select(this).select("rect").attr("fill", "#ffffffee");
      })
      .on("mouseleave", function () {
        d3.select(this).select("rect").attr("fill", "#ffffffcc");
      });

    resetG
      .append("rect")
      .attr("x", 0)
      .attr("y", 0)
      .attr("rx", Math.round(10 * scale))
      .attr("ry", Math.round(10 * scale))
      .attr("width", btnW)
      .attr("height", btnH)
      .attr("fill", "#ffffffcc")
      .attr("stroke", "#e0e7ea")
      .attr("stroke-width", Math.max(0.8, 0.8 * scale));

    resetG
      .append("text")
      .attr("x", Math.round(10 * scale))
      .attr("y", Math.round(16 * scale))
      .text("Reset Tree")
      .attr("font-size", Math.round(12 * scale))
      .attr("fill", "#334349")
      .style(
        "font-family",
        "system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif"
      );

    // --- English tips (outside legend box, below, using <tspan> multiline) ---
    const defaultTips = [
      "• Drag the panel edges to resize the window.",
      "• Click Reset Tree to return to the initial state.",
      "• Click a point on the map to expand the corresponding species in the tree.",
      "• Click a species in the tree to highlight and show it on the map.",
      "• Try interacting with the tree!",
    ];

    // If you want to pass a long string containing \n from options in the future, also supported:
    // options.tips_str = "line1\nline2\n..."
    const tips =
      options && Array.isArray(options.tips)
        ? options.tips
        : options && typeof options.tips_str === "string"
        ? options.tips_str.split(/\r?\n/)
        : defaultTips;

    const tipsFont = Math.round(12 * scale);
    const tipsLineGap = Math.round(4 * scale);

    const textBlock = legendTips
      .append("text")
      .attr("class", "legend-tips-text")
      .attr("x", 0)
      .attr("y", 0)
      .attr("font-size", tipsFont)
      .attr("fill", "#455a64")
      .style(
        "font-family",
        "system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif"
      );

    tips.forEach((t, i) => {
      textBlock
        .append("tspan")
        .attr("x", 0)
        .attr("dy", i === 0 ? 0 : tipsFont + tipsLineGap)
        .text(String(t));
    });
  }

  function positionLegend() {
    const scale = cfg.legend_scale || 1.0;
    const margin = Math.round(12 * scale);
    const hSvg = +svg.attr("height") || height;

    // Get legend dimensions
    const bg = legend.select(".legend-bg").node();
    if (!bg) return;
    const legendBox = bg.getBBox();
    const tipsBBox = legendTips.node()
      ? legendTips.node().getBBox()
      : { height: 0, width: 0 };

    const btnH = Math.round(24 * scale);
    const gapTop = Math.round(6 * scale); // Reset ↔ legend
    const gapBetween = Math.round(8 * scale); // legend ↔ tips

    // 1) tips at bottom
    const tipsX = margin;
    const tipsY = hSvg - margin - tipsBBox.height;
    legendTips.attr("transform", `translate(${tipsX}, ${tipsY})`);

    // 2) legend right above tips
    const legendX = margin;
    const legendY = Math.max(margin, tipsY - gapBetween - legendBox.height);
    legend.attr("transform", `translate(${legendX}, ${legendY})`);

    // 3) Reset above legend
    const resetX = margin;
    const resetY = Math.max(margin, legendY - (btnH + gapTop));
    legendReset.attr("transform", `translate(${resetX}, ${resetY})`);
  }

  r2d3.onResize(function () {
    positionLegend();
    ui.raise();
    if (pinnedNode) updateCalloutPosition(pinnedNode);
  });

  // Shiny bridge (map → tree)
  if (typeof Shiny !== "undefined") {
    Shiny.addCustomMessageHandler("bio_tree_highlight_path", function (msg) {
      const path = msg && msg.path;
      const fallbackName = msg && msg.name;
      const meta = {
        month: msg && msg.month,
        commonname: msg && msg.commonname,
        uid: msg && msg.uid,
      };
      expandHighlightByPath(path, fallbackName, meta);
    });
  }

  // Reset logic
  function resetTree() {
    highlightedKeySet = new Set();
    clearHighlight();
    showAllKingdomsCollapsed();
    hideNodePopup();
    refresh();
    svg.call(
      zoom.transform,
      d3.zoomIdentity.translate(width / 2, height / 2).scale(2)
    );
    currentTransform = d3.zoomIdentity
      .translate(width / 2, height / 2)
      .scale(2);
    prevNodeKeySet = new Set(nodes.map(nodeKey)); // avoid treating all as "new" after reset
    ui.raise();
  }
});
