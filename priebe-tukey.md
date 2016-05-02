Priebe's Tukey-for-Graphs:

(for simple graph, no attributes)

0. plot the graph -- default igraph layout?
    NB: this might should be skipped for large graph.

1. spy adjacency matrix --
    let hclust order vertices, where we hclust using hamming distance.
    for big graph, you may hafta be computationally clever?

2. plot ASE & nL scree plots (with ZG 1,2,3 annotated).

3. image Phat, where Phat is (H)SBM estimate via mclust o ase.
    for big graph, replace mclust with kmeans.

 3.1. go back to 1, ordering & coloring via (H)SBM.

4. pairs plot of embedding (perhaps of kde or mclust of embedding).

---carey
