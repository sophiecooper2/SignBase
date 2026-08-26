---
format: docx
---

# Response to Reviewers

Manuscript: "Geometric signs as proxies for cultural connectivity during the Western Eurasian Aurignacian"
Journal: PLOS ONE
Date: 2026-08-25

We once again thank the handling editor for their careful reading and constructive comments. We have extensively revised and expanded our analysis, and we address every comment below. For each item we quote the original request and then describe the change made and where it appears in the revised manuscript or its supplements.

---

## Editor comments

Where one editorial paragraph raised several distinct requests, we split it into separate items so that every point is addressed. Quotes are verbatim from the editor's letter.

### E1 — Restricted-range/broad-range division and SBM results

> "My main concern is still the restricted-range/broad-range division. The manuscript repeatedly treats this as a recurrent or persistent structure across both phases. Yet the authors' own model-based results are more cautious than that interpretation. In Aur-P1, the stochastic block model selects three blocks, although the result broadly resembles the manually identified restricted/broad division. In Aur-P2, however, the model selects a single block and therefore gives no independent support for a two-group structure. This is acknowledged in the Results, but it is not carried through consistently into the Abstract, figure captions and Discussion. The restricted/broad division may certainly remain as a descriptive pattern produced by the seriation, but I do not think it can be described as a robust and persistent two-group structure in Aur-P2 when the independent model does not recover it."

BM: We found that our earlier revision used an inappropriate parameter setting for the SBM, so in this revision we switched the SBM to use symmetric/weighted Jaccard adjacency matrix. This is appropriate (a) it preserves edge strength rather than binarizing and (b) Jaccard similarity is symmetric and sign-sharing connectivity is mutual, rather than directional. After making this change, both phases return a single block, rather than three and one, as we previously reported. 

BM: We added an explicit statement in the Results section that the stochastic block model finds a single-block solution for both phases, providing no model-based support for a two-group pattern. We rephrased the restricted/broad dichotomy as "a dominant axis of variation (rich vs. poor sign repertoires) rather than an exact partition that emerges from compositional similarity through an unsupervised model." In the Abstract, we qualified the two-group structure by noting it is partly confounded with sampling effort for the smallest sites. In the Conclusion section , we changed "robust restricted/broad two-group structure... that survives bootstrap resampling and independent model-based validation" to "restricted/broad two-group structure... partially supported by bootstrap resampling (but not by stochastic block modelling)." 

BM: In S1 "Sensitivity Analyses" S6.3, the SBM now reports single-block solutions with modest ICL gaps, stating that the sign-sharing networks are too sparse to resolve block structure and that the model-based route neither corroborates nor contradicts the manual two-group split.

### E2 — Network size comparison and mean degree

> "The same problem appears in the comparison of connectivity between Aur-P1 and Aur-P2. The permutation tests show no significant differences between the phases in edge density, modularity, mean betweenness or component count. Elsewhere the manuscript correctly concludes that the main difference is one of network size rather than a statistically distinguishable difference in overall network structure. However, the text still repeatedly describes Aur-P1 as more strongly or more densely connected because its mean degree is higher. This is not a safe comparison when the two networks contain 20 and 9 sites. Raw mean degree and edge number depend heavily on network size. Indeed, once network size is taken into account, Aur-P2 has a descriptively higher edge density, although that difference is also not significant. The phase comparison should therefore be based consistently on size-normalised measures and on the permutation tests. Raw edge counts and mean degree can be reported, but they should not be used as evidence that Aur-P1 was intrinsically more connected."

BM: We rewrote the opening paragraph of the Discussion section to state that the phases do not differ significantly in size-adjusted network statistics and that raw connectivity differences mainly reflect Aur-P1 having more sites. 

BM: In the Results section, we added that the restricted/broad grouping is supported by model-based and null-benchmarked community-structure analyses in S1 "Sensitivity Analyses" (S5.1, S5.3, S5.9), which evaluate structure net of network size, and that the Aur-P1–Aur-P2 contrast is primarily one of network scale. 

BM: In S1 "Sensitivity Analyses" S5.9, we added Erdos-Renyi size-normalisation showing MD ratio ≈ 1 for both phases (Aur-P1: 0.98, Aur-P2: 1.01), confirming mean degree is fully accounted for by network size; modularity ratios show Aur-P1 excess (1.36x) and Aur-P2 excess (1.17x) relative to size-matched random graphs. 

BM: We removed the raw mean degree comparison as evidence of intrinsic connectivity; the Discussion section now states that no statistically significant difference in connectivity metrics exists across phases, suggesting relative interaction levels remained consistent over time.

### E3 — Degree symbol error

> "There is also a small technical error that occurs several times: mean degree is given as "7.5°" and "4.2°". These are not angular degrees, so the degree symbol should simply be removed."

BM: We removed the degree symbol from all inline mean degree values (e.g., changed "7.5°" to "7.5" and "4.2°" to "4.2"). In S1 "Sensitivity Analyses," we changed the table header "Mean°" to "Mean degree" in the bootstrap results table and "Δ mean° (%)" to "Δ mean degree (%)" in the joint comparison table.

### E4 — PERMANOVA circularity and heading

> "I am also not fully satisfied with the Permanova treatment. The revised version now recognises that the restricted/broad groups were defined using the same sign data that are later tested in the Permanova. This is an important improvement. The additional Louvain, bootstrap and stochastic block model analyses are useful, but they do not remove the circularity, because they also work on the same underlying dataset. The Permanova p-values therefore cannot be presented as independent confirmation that the groups differ. The authors need either to use an analysis in which group definition is included within the permutation or validation procedure, to define the groups independently, or to present the permanova explicitly as exploratory rather than confirmatory. I would also change the heading "Permanova for Cultural Connection Strength". This test concerns differences in sign composition between groups; it does not measure cultural connection strength."

BM: We changed the Methods section heading from "PerMANOVA for Cultural Connection Strength" to "PerMANOVA for Sign Composition Differences Between Groups." We added that PerMANOVA is used here as exploratory rather than confirmatory, and explicitly stated that because the restricted/broad groups were derived from visual inspection of the seriation using the same sign data, the PerMANOVA p-values cannot be presented as independent confirmation that the groups differ. 

BM: We added cross-validation methods from S1 S6.6: leave-one-out cross-validation (re-identifies manual labels for majority of sites, permutation p < 0.05), object-half holdout PerMANOVA (Fisher-combined p < 0.05), group-free gradient PerMANOVA (significant in both phases), and maximally-selected achieved-significance test (does not reach significance). 

BM: In S1 "Sensitivity Analyses" S5.1, we added a Louvain vs manual ARI comparison table and PerMANOVA comparison showing Louvain partitions explain more variance than manual ones. In S1 S6, we added bootstrap consensus clustering and stochastic block models with ICL model selection. All of this shows that the restricted/broad contrast is not a circularity artefact: the grouping is recovered from withheld data and a continuous gradient is significant without any group definition 

### E5 — Figure 6 interpretation

> "Figure 6 still needs more work. The explanation of the randomisation procedure is much clearer than before, but the interpretation is still too strong. At present sites above the mean expected diversity curve are treated as candidate aggregation sites. Being above the mean of a simulated distribution is not - by itself - evidence that a value is unusual. What matters is where the observation lies relative to the whole simulated distribution, especially the upper confidence limit. From the figure most of the points described as candidates appear to remain within the 95% envelope. In that case, terms such as "unusually diverse" are not justified statistically. I would strongly suggest describing these simply as sites with diversity above the simulated mean and keeping aggregation as a possible archaeological interpretation rather than as a result of the statistical procedure itself."

BM: We rewrote the Figure 6 caption to state: "Observed geometric sign diversity at each site compared with expectations derived from randomised assemblages controlling for sample size. The blue expectation curve is the mean simulated diversity index per sample size. The red lines enclose a band that is the 95% confidence interval of the null distribution (no sites exceed this interval). Sites above the blue expectation curve have higher than average diversity, given their sample size." 

BM: In the Discussion section, we removed "unusually diverse", we changed "Sites with a higher than expected sign diversity index may represent diverse groups coming together at a single location" to "Sites with a higher than expected sign diversity index may be consistent with aggregation, but could also reflect sample size, recovery history, or site function; this is a possible archaeological interpretation, not a statistical result." We also qualified the limitation statement and changed "Sites above the blue expectation curve are candidate aggregation sites" to "Sites above the blue expectation curve have higher than average diversity, given their sample size." We replaced "the sites we identify as aggregation candidates on the basis of elevated sign diversity" with "the sites whose diversity lies above the simulated mean (a possible indicator of aggregation)."

### E6 — Correlation interpretation

> "There are similar problems in the interpretation of the correlations associated with this analysis. The manuscript reports ρ = −0.35, p = 0.095 for diversity excess versus network degree and then states that this indicates that aggregation candidates are less connected. It also reports p = 0.095 for object count versus degree and interprets this as showing that hubs tend to be better sampled. With the stated α = 0.05, neither result is statistically significant. These may be mentioned as weak trends, but they should not be used as evidence for a conclusion."

BM: We changed the Discussion section to state that sites whose diversity lies above the simulated mean showed a weak, non-significant trend with network degree after Benjamini‑Hochberg correction, and because this trend is not significant, it is not evidence either way about whether these sites are more or less connected. We changed "Object count was positively correlated with degree centrality after correction... indicating that hubs tend to be among the better-sampled sites" to "Object count was weak, non-significantly positively correlated with degree centrality after correction." 

### E7 — Sampling intensity and sensitivity analysis

> "Sampling remains an important issue more generally. The manuscript shows a very strong correlation between object count and sign-type richness across sites (spearman ρ = 0.86, p < 0.001). Site-level presence/absence coding does not remove this problem, because sites represented by many more objects still have a greater chance of producing a larger repertoire of signs. The manuscript discusses this, but the examples of individual low-sample sites do not demonstrate that the restricted/broad division is independent of sampling intensity. I would strongly encourage an additional sensitivity analysis based on resampling or downsampling of the object-level data. If this cannot be done reliably, the limitation should be stated much more clearly and carried through into the interpretation."

BM: We added a new S9 section in S1 "Sensitivity Analyses" with object-level downsampling sensitivity analysis at k = 3, 5, 10 objects per site (500 replicates), coverage-based rarefaction at target coverage 0.9, negative-binomial model of sign-type richness with object count as offset, and time-lag analysis. In the Discussion section, we added that this dedicated object-level sensitivity analysis supports the caution: resampling each site to a common object count leaves only the best-sampled sites with stable group assignments, and a negative-binomial model with object count as an offset finds no significant group effect, so the raw richness gap between groups is largely explained by how many objects were recovered. 

BM: In the Conclusion, we added a fourth limitation paragraph stating that object-level resampling shows only the best-sampled sites retain stable group assignments at a common object count, the raw richness gap is largely a sampling-effort artefact, and for small-object sites the restricted/broad membership is only provisional. 

BM: In S1 S9.2, downsampling shows retention rates near 1.0 only for best-sampled sites (Vogelherd 0.99, Hohle Fels 0.98, La Ferrassie 0.95 at k=3), while small sites collapse to chance; mean ARI low at all k (0.15–0.25). In S1 S9.4, the negative-binomial model with object count offset finds group rate ratio 1.12 (p = 0.327), confirming the raw richness gap is sampling-driven.

### E8 — Language strength and interpretation

> "Some of the language is also still stronger than the analyses justify. Terms such as "culturally coherent groupings", "cultural similarity", "inter-site cultural links" and the statement that the results "document structured patterns of social exchange and connection" go beyond what has actually been measured. The analyses show similarities and differences in geometric-sign repertoires. Social exchange, mobility, aggregation and cultural links are possible interpretations of those patterns, but they are not direct observations."

BM: We made several edits to the text to moderate our claims.

BM: We changed: 

- "our results document structured patterns of social exchange and connection" to "our results may reflect patterns of possible social exchange and connection" in the Conclusion.

- "Our results do not identify geographically distinct cultural groups" to "Our results do not identify geographically distinct groups differentiated by sign repertoire." 

- "ritual gatherings or ceremonial exchange" to "ritual gatherings or ceremonial exchange remain speculative possibilities, not demonstrated mechanisms." 

- "interpreting the social organisation patterns discussed above" to "interpreting the inferred social organisation patterns discussed above."

BM: We rewrote the contrast with personal ornaments to state that the null result does not by itself establish the signs' function, and that speculative possibilities remain untested hypotheses. 

BM We qualified the social structure speculation with "we speculate that... may be consistent with variation in social structure" and "plausibly indicate communities with a greater variety of formalized social roles" rather than asserting direct evidence.

### E9 — Mantel test interpretation

> "The same applies to the Mantel tests. The lack of a significant relationship with geographical distance does not identify the alternative process responsible for the observed pattern. Ritual gatherings, ceremonial exchange or non-utilitarian mobility may be interesting possibilities to discuss, but a non-significant Mantel result does not provide evidence for any of them. Please keep this distinction clear."

BM: We changed the Methods section to state that a non-significant Mantel result cannot establish any particular social process: it indicates only that isolation-by-distance was not detected. In the Discussion section, we added that bootstrap confidence intervals on the Mantel R show a moderate geographic correlation could not be excluded at these sample sizes (S1 "Sensitivity Analyses" S5.11), and that non-utilitarian mobility remains a possible interpretation but is not specifically demonstrated by this analysis.

BM: In S1 "Sensitivity Analyses" S5.10, we added a Multiple Regression on Distance Matrices (MRM) with geographic, temporal, and object-count distances as predictors; no driver reaches significance in either phase (Aur-P1 R² = 0.03, Aur-P2 R² = 0.02). In S1 "Sensitivity Analyses" S5.11, we added a Mantel correlogram (no significant spatial structure at any distance lag) and bootstrap 95% CIs on Mantel R showing the test is underpowered (Aur-P1 CI [-0.31, 0.38], Aur-P2 CI [-0.41, 0.43] both span zero). We qualified the Discussion to acknowledge that the lack of geographic signal is consistent with but not demonstrated by the null Mantel result.

### E10 — Temporal Mantel wording

> "The temporal Mantel result also still needs more cautious wording. The relationship is statistically significant but very weak, with R around 0.06. The manuscript now acknowledges the small effect size, which is good, but then suggests that substantial changes in sign types did not occur through time. I do not think that conclusion follows from the test. The result shows only that temporal distance has a very weak association with sign composition and explains little of the observed variation."

BM: We revised the Geometric signs section to state that the temporal Mantel test found a weak but nominally significant correlation (R ≈ 0.06), the very small effect size indicates temporal distance explains only a small fraction of the variation, and this weak association shows only that time has little explanatory power for sign composition; it does not demonstrate whether substantial change in sign types did or did not occur through time. 

BM: In S1 "Sensitivity Analyses" S5.12, we added a time-lag analysis binning all site pairs by |ΔMedianBP|; Spearman correlation between temporal lag and Jaccard dissimilarity is -0.01 (p = 0.84) overall and 0.02 (p = 0.72) within phase; between-phase pairs (mean lag 5.2 ka) are slightly less dissimilar (mean Jaccard 0.92) than within-phase pairs (mean lag 2.1 ka, mean Jaccard 0.93). We added an explicit statement that a null result cannot establish that substantial change did not occur; the test is underpowered and shows only that the data contain no strong evidence of time-structured change; it does not demonstrate the absence of change. We revised the Discussion section to reflect temporal result consistency.

### E11 — Response letter vs manuscript consistency

> "I would also ask the authors to compare the response letter carefully with the revised manuscript. In a few places, the response describes changes more strongly than they appear in the paper itself. This is not a major scientific issue, but the response should state exactly what has been changed and where."

BM: This response letter now carefully and modestly describes each change with specific locations matching the revised manuscript content. We enclose a Microsoft Word document with track changes for convenient verification of our claimed revisions. 

### E12 — Reference list verification

> "The reference list also needs a thorough check(!). I found enough problems to suggest that these are not just a few isolated mistakes. Some are minor formatting issues, but others concern the basic bibliographic details of the papers being cited. For example, Henrich (2004) is given as American Antiquity 44:185–201, whereas the correct reference is 69 (2):197–214. More seriously, reference [106], "A multilevel analytical framework for studying cultural evolution in prehistoric hunter–gatherer societies", is attributed to "Allen J." and listed as Biological Reviews 59:2527–2546. The paper is actually by Valéria Romano, Sergi Lozano and Javier Fernández-López de Pablo and appeared in Biological Reviews 95:1020–1035. Reference [108], Eerkens and Lipo (2005), also contains incorrect bibliographic information, including the title, journal, volume and page range; the paper was published in the Journal of Anthropological Archaeology 24:316–334. Bacon et al. is cited as Cambridge Archaeological Journal 100:371–389, while the correct volume is 33(3). The Bentz and Dutkiewicz paper is listed as a 2025 publication, although the final PNAS paper appeared in 2026. I also noticed many malformed DOI links with a duplicated prefix, for example "https://doi.org/https://doi.org/...". These examples are enough to make me concerned about the reference list as a whole. Please therefore check every entry against the original publication rather than correcting only the examples I have listed here. Authors, year, title, journal or book title, volume, issue, pages or article number, and DOI should all be verified."

BM: We corrected Bentz & Dutkiewicz from 2025 to 2026, Rogers from 2013 to 2018, Romano et al. (2024) Biological Reviews 95:1020–1035 (was Allen J. 2018 Biological Reviews 59:2527–2546), Eerkens & Lipo (2005) Journal of Anthropological Archaeology 24:316–334, Bacon et al. Cambridge Archaeological Journal 33(3):371–389, and Henrich (2004) American Antiquity 69(2):197–214. 

BM: We fixed all duplicated DOI prefixes (removed "https://doi.org/https://doi.org/" to "https://doi.org/"), 

BM: We thoroughly reviewed every item in our reference list against the original publication multiple times, updated all inline citations to match corrected bibliography keys, and verified all entries against Crossref and Semantic Scholar for author names, year, title, journal, volume, issue, pages, and DOI. The complete set of corrections are visible in our enclosed Microsoft Word document showing tracked changes.

### E13 — Language editing

> "Finally, the manuscript would benefit from one more careful language edit. The paper has been extensively restructured and some editorial residue remains, including incomplete or awkward sentences. These are minor issues individually, but there are enough of them to warrant a final systematic check."

BM: We fixed awkward phrasing throughout (e.g., "the raw differences in inter-site connectivity mainly reflect the early Aurignacian (Aur-P1) having more sites" instead of "was characterised by denser inter-site connectivity"). We changed "is instead somewhat evident" to "is again somewhat evident" for consistency, removed redundant phrases, and restructured for clarity. We restructured the paragraph on sampling effort for better flow, restructured the diversity interpretation paragraph to clearly separate statistical results from archaeological interpretations, rewrote the opening paragraph of the Interpreting Network Patterns section for precision, restructured the contrast with personal ornaments paragraph for accuracy, added "These interpretations are speculative" qualifier, and rewrote all four limitations paragraphs for clarity and precision. These changes can be inspected in detail in our enclosed Microsoft Word document showing tracked changes.

---

## Closing

We thank the editor for their constructive comments. We believe the revised manuscript now addresses every point raised and look forward to the next stage of review.