---
format: docx
---

# Response to Reviewers

Manuscript: "Geometric signs as proxies for cultural connectivity during the Western Eurasian Aurignacian"
Journal: PLOS ONE
Date: 2026-08-05

We thank the handling editor, both reviewers, and Dr Peresani for their careful reading and constructive comments. We have treated the archaeological concerns raised by Reviewer 2, and the editor's and Dr. Peresani's guidance on those same points, as the primary basis for the revision, and we address every comment below. For each item we quote the original request and then describe the change made and where it appears in the revised manuscript or its supplements.

## Summary of major revisions

1. Two-phase framework: Replaced the four-phase scheme (including the erroneous "Transitional" phase) with the two-phase Aur-P1/Aur-P2 baseline of Shao et al. (2021): Aur-P1 = Proto + Early (43–37 ka BP), Aur-P2 = Evolved + Late (37–32 ka BP).
2. Technology-based site assignment: Sites are assigned to phases from specialist technological attributions in the published literature, not from radiocarbon dates alone; Spy and El Castillo are excluded as too mixed to phase, and Gatzarria is retained in the Proto-Aurignacian with justification.
3. Corrected Table 1 markers: The material-culture markers for the Aurignacian and for the Early and Evolved phases were corrected per Reviewer 2.
4. Moderated claims: The title, Abstract, and Conclusions were rewritten to the editor's cautious formulations; strong terms ("we demonstrate", "reveals peak inter-group connectivity", aggregation claims) were replaced.
5. New sensitivity analyses: Alternative thresholds (0.1/0.3), alternative metrics (Sørensen–Dice, Simpson), a continuous threshold sweep, bootstrap CIs, a figurine-exclusion test (Vogelherd), and a date-based reclassification alternative were added.
6. Reproducibility: The supplements now carry the processed data, excluded-site list, per-site phase assignments, matrices, and all R code; the site-assignment supplement is generated directly from the pipeline.

---

## Editor comments

Where one editorial paragraph raised several distinct requests, we split it into separate items so that every point is addressed. Quotes are verbatim from the editor's letter.

### E1 — Moderate over-strong claims

> "First, some of the main claims are expressed too strongly in relation to the evidence presented. The manuscript repeatedly moves from patterns in the presence or absence of geometric signs to conclusions about cultural groups, social organisation, aggregation sites and inter-group connectivity. These interpretations may be plausible, but they are not demonstrated directly by the analyses. The wording should therefore be moderated throughout the manuscript, especially in the Abstract and Conclusions."

BM: We moderated four specific over-strong claims in the manuscript so that findings are presented as cautious inferences from geometric-sign distributions rather than as direct evidence of social structures:

1. Abstract: "hub-centred network" → "network centred on key sites" (removes the strong "hub" framing).

2. Figure 4 caption (the group-map figure): it now reads "Geographic distribution of sign-use groups (Restricted-range and Broader range) in the two Aurignacian phases…" — the caption uses the descriptive "sign-use groups" label instead of the interpretive "cultural groups" claim (Figure 1's caption, line 346, never used "cultural groups"; the earlier wording was on the group-map figure, which is Figure 4).

3. Results - Group Dynamics: "each phase can be divided into two main cultural groups" → "each phase can be divided into two main sign-use groups" (the Results now describe the seriation output with the same descriptive label).

4. Discussion limitations: "interpreting the social organisation patterns discussed above" → "interpreting the inferred social organisation patterns discussed above" (explicitly marks the social-organisation interpretation as inferential).

No remaining sentence presents aggregation, social organisation, or inter-group connectivity as demonstrated by the analyses. 

### E2 — "Approximately a millennium" inconsistency

> "In particular, the statement that the results push back the emergence of complex inter-group networks by 'approximately a millennium' appears inconsistent with the later comparison to c. 30 ka BP; if the proposed date is c. 41 ka BP, this is not a difference of one millennium."

BM: We corrected this numerical inconsistency by removing the claim and the entire "~41 ka emergence" framing. The revised manuscript no longer states that complex inter-group networks emerged around c. 41 ka BP, nor that the results push back their emergence by "approximately a millennium". The temporally ordered "densest network / ~41 ka emergence" narrative was replaced by a comparative two-phase account that does not rest on any single emergence date.

### E3 — Mantel null result phrasing

> "The absence of a statistically significant Mantel correlation with geographic distance should not be presented as proof that sign distributions are culturally structured; it only indicates that this relationship was not detected in the present dataset."

BM: We rephrased every statement about the Mantel results so that the absence of a significant geographic correlation is reported as "not detected in the present dataset" rather than as evidence that sign distributions must be culturally structured. The Abstract no longer frames the result as showing that sign distribution "reflects cultural grouping rather than geographic isolation"; it now states that geographic distance did not account for sign distribution in either phase and that this null result should be interpreted cautiously given the small sample sizes. The Discussion repeats the "not detected in this dataset" qualifier and directs readers to the multiple-testing supplement (S5.8).

### E4 — Temporal Mantel effect size

> "Similarly, the temporal Mantel result is statistically significant but has a very small effect size, and should not be described as confirming meaningful temporal change without further qualification."

BM: We rewrote the sentence discussing the temporal Mantel test so that it reports the relationship as weak and only nominally statistically significant (R ≈ 0.06), explaining only a small fraction of the variance, and we explicitly state that it does not confirm meaningful temporal change. The "substantial changes did not occur" framing is retained but qualified by the small effect size, so the passage no longer overclaims a temporal signal.

### E5 — Jaccard similarity vs dissimilarity

> "The network analysis also needs clearer explanation, especially the use of Jaccard dissimilarity versus similarity. ... The manuscript should state unambiguously whether the edges represent Jaccard similarity or Jaccard dissimilarity, as these have opposite meanings for interpreting edge strength and the 0.2 threshold."

BM: The Methods section now states unambiguously that "each edge weight is the Jaccard similarity (1 − Jaccard dissimilarity), with an edge drawn only if two sites share ≥ 20% Jaccard similarity." We also corrected the Figure caption that described the network edges as "based on Jaccard dissimilarity … edges connect pairs with similarity ≥0.2", which was internally contradictory; the caption now says the edges are based on Jaccard similarity. The phrase "Jaccard dissimilarity matrix" is retained only where it names the legitimate input to the Mantel test (Methods §Mantel and Supplement S2), which is correct usage.

### E6 — Sensitivity check for the 0.2 threshold

> "At present, this threshold appears to be justified mainly by graphical clarity, whereas it may materially affect the resulting network structure. A sensitivity check using alternative thresholds would be appropriate."

BM: We added a full sensitivity programme for the 0.2 threshold. The supplements now report network statistics at thresholds 0.1, 0.2, and 0.3 (S1); alternative similarity metrics (Sørensen–Dice and Simpson; S2); a continuous threshold sweep from 0.05 to 0.50 that preserves the Aur-P1 > Aur-P2 mean-degree ordering at every step (S5.6); edge-set and adjacency-matrix agreement across threshold pairs (S5.2); and bootstrap confidence intervals on the network statistics (S5.7). The Methods text now justifies the baseline threshold as the value that keeps both phases sufficiently connected and cites these supplements.

### E7 — perMANOVA: scope, circularity, dispersion

> "In my opinion, the perMANOVA results should be interpreted more cautiously. The analysis appears to test differences between restricted-range and broad-range groups within phases, rather than changes between phases themselves. Moreover, because these groups are defined using the same sign data that are then tested statistically, the risk of circularity should be addressed. The authors should also report whether differences in dispersion between groups were assessed."

BM: The Results now state explicitly that the perMANOVA tests whether the restricted-range and broad-range groups *within* each phase differ in sign composition — not whether the phases differ from each other. To address circularity, we validate the manual restricted/broad groups against data-driven Louvain community detection and report the agreement (ARI 0.787/0.798/0.581 for Aur-P1 across thresholds; Supplement S5.1), and we test the two-group structure against a stochastic block model that was never shown the seriation solution (S6), which recovers the restricted/broad split for all but one Aur-P1 site and selects a single block in the small Aur-P2 sample. Dispersion was assessed: betadisper results are reported in the main text (Aur-P1 p = 0.124, Aur-P2 p = 0.771), and we present the contrast as a difference in group centroids (composition) rather than a difference in dispersion. The main text explicitly flags the shared-data circularity risk and explains how the independent validations address it.

### E8 — Reproducibility (data, exclusions, assignments, matrices, code)

> "More generally, the revised manuscript should provide a clearer account of the processed dataset, site exclusions, phase assignments, matrices and R code used in the analyses, so that the results are fully reproducible."

BM: We made the analysis reproducible end-to-end using R and Quarto. The "Site Phase Assignments" supplement now documents the processed dataset, the excluded-site list, and the per-site phase assignment for every site, and it is generated directly from the pipeline code, so the tables cannot drift from the data. The Methods section describes the full pipeline (raw SignBase → cleaning → date calibration → phase assignment), and the analytical matrices and all R scripts are provided with the revision. A reproducibility statement accompanies the submission (see also E23, F1, F2).

### E9 — Incorrect Table 3 reference in Figure 4 caption

> "Several presentation errors should also be corrected, including the incorrect reference to Table 3 in the caption to Figure 4 ..."

BM: The Figure 4 caption now correctly references the Mantel results table (Table 2). The only remaining "Table 3" references in the manuscript are the legitimate perMANOVA table. We confirmed this is correct in the current render.

### E10 — Figure 2 panel order vs caption

> "... the mismatch between the panel order and caption of Figure 2 ..."

BM: Figure 2 was re-generated as a two-phase figure (Aur-P1 panel A, Aur-P2 panel B), and the caption now describes the panels in the order they appear. The previous mismatch (where the figure showed "Transitional" first while the caption listed Protoaurignacian as panel A) is gone because the "Transitional" panel no longer exists.

### E11 — Standardise chronological terminology

> "In addition, the revised manuscript should be checked carefully for technical consistency. The chronological terminology should be standardised throughout, particularly the use of 'ka BP', 'k BP', 'cal BP' and 'BP', since the analyses rely on calibrated age ranges and phase assignments."

BM: We standardised the chronological terminology so that calibrated values are reported with a single, consistent unit across the manuscript and both supplements. The body text uses "ka BP" for calibrated ages throughout (43–37 ka, 37–32 ka, and related values), and we have aligned the table headers that used "k cal BP" to the same convention. 

### E12 — Seriation procedure (Brower–Kile vs correspondence analysis)

> "The description of the seriation procedure also needs to be made consistent between the Methods section and the figure captions. At present, the Methods refer to the Brower-Kile seriation algorithm, whereas the caption to Figure 2 describes the result as 'correspondence analysis-based seriation'."

BM: The Methods section and the Figure 2 caption now both describe the same procedure, the Brower–Kile seriation algorithm. The phrase "correspondence analysis-based seriation" no longer appears in the Figure 2 caption, so the reader sees a single, consistent description of the method.

### E13 — Title

> "The title should also be reconsidered. In its present form, 'Geometric signs reveal changes in social structures and networks during the Western Eurasian Aurignacian' overstates what the data can demonstrate. ... A more cautious title would better reflect the evidential basis of the study. I therefore recommend that the title be revised to a more cautious formulation, such as: 'Geometric signs as proxies for cultural connectivity during the Western Eurasian Aurignacian'."

BM: We adopted the editor's suggested cautious title verbatim: "Geometric signs as proxies for cultural connectivity during the Western Eurasian Aurignacian." The revised title no longer claims that the signs "reveal changes in social structures and networks".

### E14 — Keywords

> "The keywords should also be revised. Several of the current terms repeat words already present in the title, especially 'Aurignacian' and 'Geometric signs', and therefore add little to the discoverability of the article. I recommend replacing some of these repeated or very broad terms with more specific keywords that reflect the methods, material and interpretative framework of the study, for example: mobile art, symbolic behaviour, cultural connectivity, seriation, network analysis, cultural transmission, Upper Palaeolithic, Western Eurasia, and Late Pleistocene."

BM: We revised the keywords to the editor's recommended list, removing the terms that duplicated the title and adding terms that reflect the methods and framework: mobile art, symbolic behaviour, cultural connectivity, seriation, network analysis, cultural transmission, Upper Palaeolithic, Western Eurasia, and Late Pleistocene.

### E15 — Abstract

> "The Abstract should be substantially revised because several claims are too strong. Terms such as 'we demonstrate', 'reflects cultural grouping rather than geographic isolation', and 'reveals peak inter-group connectivity' should be replaced with more cautious wording. The interpretation of the 'Transitional phase' and phase-specific connectivity must also be reconsidered after the chronological framework has been corrected. The statement that the results push back complex inter-group networks to c. 41 ka BP by 'approximately a millennium' is numerically inconsistent and should be corrected. Overall, the Abstract should present the results as cautious inferences from geometric sign distributions, not as direct evidence of social structures."

BM: We substantially rewrote the Abstract. It no longer uses "we demonstrate", "reflects cultural grouping rather than geographic isolation", or "reveals peak inter-group connectivity"; instead it reports the quantitative results (two-phase framework; restricted/broad structure with bootstrap and SBM validation; mean degree 7.5 vs 4.2; Mantel nulls; permutation-test results) as cautious inferences. The "Transitional phase" discussion and the "millennium" inconsistency were removed. The Abstract now presents the separation of restricted- and broad-range sign use as a persistent feature of the data that is robust to the checks we ran, without claiming direct proof of social structures.

### E16 — Figure 1: scale bar and label legibility

> "Figure 1 is useful, but the map panels require improvement. A scale bar should be added, as spatial distance is important for the interpretation of site distribution and connectivity. The authors should also improve the legibility of site labels and make the colour coding between panels A and B clearer."

BM: We added a scale bar to the map panels of Figure 1 so that spatial scale is interpretable at a glance, improved the legibility of the site labels, and made the phase colour coding consistent between panel A (site map) and panels B and C (signs over time and object counts) by using a shared legend and the same two phase colours throughout.

### E17 — Figure 2 label size

> "Figure 2 should also be revised for clarity and consistency. ... The labels are small and should be improved for readability."

BM: We enlarged the row labels in Figure 2 and re-rendered the figure so that the site names are legible at the published output scale. (The panel-order issue in the same comment is handled under E10, and the seriation-wording issue under E12.)

### E18 — Figure 3 label overlap

> "Figure 3 – several site labels overlap with network lines or nodes, making them difficult to read. The authors should reposition the labels, or use label repulsion, so that names are not obscured by edges and remain legible in all four panels."

BM: We repositioned the site labels in Figure 3 using label repulsion so that names are no longer obscured by network nodes and edges and remain legible in all panels. We re-checked the rendered figure after the change.

### E19 — Figure 4 consistency with Figure 1

> "Figure 4 should be made graphically consistent with Figure 1. The authors should use the same mapping style, symbols and colour scheme where possible, and add a scale bar to the map panels. The site labels should also be checked for legibility."

BM: We rebuilt Figure 5 (the second map, the figure numbering changed from 4 to 5 during revisions) using the same mapping style, symbols, and colour scheme as Figure 1, added a scale bar to its map panels, and checked the site labels for legibility. (The Figure 5 caption's Table reference was corrected under E9.)

### E20 — Figure 5/6 (diversity vs sample size): explanation and moderation

> "Figure 5 – the idea of comparing observed sign diversity with an expected diversity for a given sample size is useful, but the current description is not sufficient to assess the robustness of the result. The authors should explain exactly how the randomised expectations and the 95% confidence intervals were generated, what 'sample size' refers to, how many randomisations were used, and whether the procedure was based on objects, sign types or sign occurrences. More importantly, sites with elevated sign diversity should not be directly interpreted as aggregation sites without additional archaeological support. Higher diversity may reflect aggregation, but it may also result from sample size, recovery history, research intensity, preservation, site function or the structure of the SignBase data. The interpretation of Figure 5 should therefore be considerably moderated."

BM: We expanded the Methods description of the diversity analysis so that it now states explicitly how the randomised expectations and 95% confidence intervals were generated (expected diversity for each site is the mean Shannon–Weaver diversity of randomised assemblages of the same sample size; the band is the 95% interval of those randomisations; 1000 randomisations were used; the procedure operates on objects and their sign occurrences). We also moderated the interpretation: sites above the expectation curve are now described as having higher diversity than expected for their sample size and as *candidate* aggregation sites, with an explicit note that elevated diversity may also reflect sample size, recovery history, research intensity, preservation, site function, or the structure of the SignBase data. The figure caption no longer states that such sites "may be interpreted as aggregation sites" without that qualification.

### E21 — Expand the specialist literature

> "The literature cited in the manuscript is relevant in several respects, particularly for SignBase, symbolic material culture, seriation and network analysis. However, the bibliographic basis is not sufficient for the part of the manuscript on which the whole analysis depends most strongly: the chronological and cultural attribution of the assemblages. The authors should expand and critically reassess the specialist archaeological literature on Aurignacian periodisation, diagnostic markers and site-specific contexts."

BM: We added the key specialist references that support the phase attributions and the periodisation: Bataille & Conard (2018, 2019) on Hohle Fels; Schürch et al. (2025) and Schürch & Conard (2026) on Vogelherd; Talamo et al. (2020) on La Ferrassie; Chiotti (2005) and Higham et al. (2011) on Abri Pataud; Oliva (2006) on Mladeč; Barshay-Szmidt et al. (2012) on Gatzarria; Falcucci et al. (2017, 2020) on Fumane; and Shao et al. (2021) and Schmidt & Zimmermann (2019) on the two-phase periodisation. Each technology-based assignment is justified with its specialist literature in the "Site Phase Assignments" supplement, which lists the basis citation for every site.

### E22 — Archaeological content, not dates alone

> "The revised manuscript should not rely on radiocarbon age ranges alone, but should demonstrate that each phase attribution is supported by the archaeological content and by current specialist literature."

BM: We changed the phase-assignment procedure from a date-driven one to a technology-driven one. Each site is assigned to a phase on the basis of its material-culture content (the markers in Table 1) using specialist attributions from the published literature, and only where no technological attribution exists is the calibrated date used as the default. The "Site Phase Assignments" supplement gives the literature basis for every technology-assigned site and states the principle, echoing Reviewer 2, that a radiocarbon date alone cannot assign a layer to a phase.

### E23 — Point-by-point response and substantial revision

> "The revised manuscript should also moderate its main claims and make clear which conclusions are directly supported by the analyses and which remain interpretative hypotheses."

BM: Throughout the revised manuscript we mark conclusions that are directly supported by the analyses (network statistics, permutation and Mantel tests) separately from those that remain interpretive hypotheses (cultural grouping, aggregation, non-utilitarian mobility), and we say so explicitly in the Abstract and Conclusions.

### E24 — Correct markers/site assignments and rerun the analyses

> "In particular, they must either justify or revise the chronological and cultural framework used to structure the dataset, correct the archaeological markers and site assignments where necessary, and rerun the analyses if any changes to phase attribution or site inclusion affect the dataset."

BM: We revised the framework (removing "Transitional"; adopting Aur-P1/Aur-P2), corrected the Table 1 markers (R2e–g), corrected the site assignments (R2h, R2i, P4), and reran every analysis on the resulting dataset (29 sites / 409 objects; Aur-P1 20 sites / 346 objects; Aur-P2 9 sites / 63 objects). We also ran the analyses under two additional conditions so that the conclusions do not depend on a single attribution decision: a date-based reclassification alternative (S8) and an analysis excluding Vogelherd (S7/S8 Analysis B). The main results are robust under all three conditions.

---

## Reviewer #1

### R1a — Recent Aurignacian discoveries (Stajnia Cave)

> "One should at least mention the most recent Aurignacian artefacts discovered after the period during which the base on which the authors relied was established. I am thinking, for example, of the discovery of the Stajnia Cave in Poland."

BM: We thank the reviewer for this suggestion. We now cite the recent Aurignacian discoveries that post-date the establishment of the SignBase corpus, including Stajnia Cave (Poland) and its ~41.5 ka decorated ivory pendant, in the Introduction and Discussion. This places the study in the context of the most current evidence without altering the dataset itself.

### R1b — Capitalisation of proper nouns

> "All proper nouns such as 'Paleolithic', 'Europe', etc., which are written with a lower-case letter, should be corrected."

BM: We checked the body text for the capitalisation issues you raised and found the prose already uses the proper nouns consistently and capitalised: "Palaeolithic/Upper Palaeolithic" and "Europe" throughout, with no lowercase "paleolithic" or "europe" in the text. We preserved the original (lower-case where it occurs) spelling inside verbatim bibliographic titles, as is conventional. The body is therefore consistent with the request.

---

## Reviewer #2

We thank Reviewer 2 for the careful, expert review of the chronological and cultural framework. We have treated these points as the central basis of the revision.

### R2a — The Châtelperronian precedes the Aurignacian

> "Minor point: I disagree with what is written on line 24, which states that the Aurignacian marks the starting point of the Upper Paleolithic. It is well known, particularly in Western Europe (France and Spain), that the Aurignacian is preceded, both stratigraphically and chronologically, by the Châtelperronian, which is distinct in terms of material culture and strictly earlier."

BM: We removed the sentence claiming that the Aurignacian marks the start of the Upper Palaeolithic. In its place, the periodisation section now notes that in parts of Western Europe (particularly France and Spain) the Aurignacian is preceded by the Châtelperronian, which is distinct in material culture and strictly earlier, and that the Aurignacian is therefore not the starting point of the Upper Palaeolithic. This answers the point directly.

### R2b — "Small-flake tools" is wrong

> "On line 73, I do not understand what the authors are referring to when they mention 'small-flake tools' as characteristic of the Aurignacian as a whole, when in fact the Aurignacian is defined precisely by the opposite: the quasi-exclusive production of blades and bladelets."

BM: The revised Table 1 no longer lists small-flake tools as a general Aurignacian marker. Both Aur-P1 and Aur-P2 are now described by their blade and bladelet production sequences (a single continuous blade-and-bladelet reduction sequence for Aur-P1; separate, intensified carinated/nosed-scraper bladelet-core production for Aur-P2), consistent with the quasi-exclusive production of blades and bladelets that defines the technocomplex.

### R2c — Neanderthal-to-modern-human "evolutionary shift" claim

> "I also question the basis on which the authors claim that the Aurignacian is characterized by an ongoing evolutionary shift from Neanderthals to early Modern Humans. On what basis?"

BM: We removed the claim that the Aurignacian is characterised by an ongoing evolutionary shift from Neanderthals to early modern humans, which had no stated basis. The word "Neanderthal" no longer appears in the manuscript; the "modern humans" references that remain concern demographic expansion into Europe during the Aurignacian, which is supported by the cited population-reconstruction literature, and are not framed as an evolutionary transition.

### R2d — The "Transitional" phase does not exist

> "However, while Banks et al. begin the Aurignacian with the Protoaurignacian phase, the authors introduce a new phase here: 'Transitional.' What does this correspond to? There is no transitional phase except in archaeological layers known for decades to be mixed and contaminated by older Mousterian elements."

BM: We removed the "Transitional" phase entirely. The revised framework is the two-phase Aur-P1/Aur-P2 scheme (Aur-P1 = Proto + Early; Aur-P2 = Evolved/Late), which contains no "Transitional" category. The word "Transitional" appears nowhere in the manuscript or supplements, and the analyses no longer rest on that phase. Sites that had been placed in "Transitional" were reassigned on technological grounds (see R2h).

### R2e — "Transitional" cultural markers were erroneous

> "Moreover, the cultural markers (see Table 1) for this so-called transitional phase are erroneous! They include side-scrapers (which, when present, are contaminants from the Mousterian) and split-base points, which are characteristic neither of a transitional phase nor of the Protoaurignacian, but of the Early Aurignacian. This is a well-documented certainty ... Furthermore, I can guarantee that there is no flake production scheme in the Aurignacian as a whole, nor any Mousterian influence!"

BM: With the removal of the "Transitional" phase, its erroneous markers are gone from Table 1. Side-scrapers are no longer listed as an Aurignacian marker (they are described, where relevant, as Mousterian contaminants), and split-base points are correctly placed as a marker of the Early Aurignacian (Aur-P1) rather than of a transitional or Proto stage. No flake-production scheme is attributed to the Aurignacian in the revised Table 1.

### R2f — Early Aurignacian definition (split-base points; twisted bladelets)

> "The definition of Early Aurignacian characteristics is entirely incorrect: it omits the indisputable and characteristic element (split-base points), and twisted bladelets are not characteristic of the Early Aurignacian but of the Evolved Aurignacian."

BM: The revised Table 1 definition of the Early Aurignacian (within Aur-P1) now includes split-based antler/bone points as a characteristic marker, and the "twisted bladelets" marker has been moved to the Evolved Aurignacian (Aur-P2), where it belongs. The Early Aurignacian row now describes separate blade and bladelet reduction sequences, carinated/nosed end-scraper bladelet cores, thick blades with invasive Aurignacian retouch, and split-based points, with twisted, Roc-de-Combe-subtype bladelets listed under Aur-P2.

### R2g — "Backed microliths" are not an Evolved marker

> "Finally, there are no backed microliths in the Evolved Aurignacian."

BM: We removed "backed microliths" from the Evolved Aurignacian markers in Table 1. The Evolved Aurignacian row now lists the busked burins and Vachons-type burins, the smaller twisted Roc-de-Combe bladelets, Font-Yves/Krems bladelets, and the sequence of lozenge- and biconical-section bone points, with no backed microliths.

### R2h — Sites in the (non-existent) "Transitional" phase

> "In the transitional phase, everything is wrong because this phase does not exist. The authors include Geissenklösterle (Early Aurignacian), Fumane (Protoaurignacian), Hohle Fels (Early Aurignacian), Labeko Koba (Protoaurignacian), and El Castillo (contaminated and mixed older layers)."

BM: We removed the "Transitional" phase and reassigned every site the reviewer listed: Geissenklösterle is assigned to the Early Aurignacian (Aur-P1), and Hohle Fels is assigned at the layer level (its lower Aurignacian horizons, AH V, in Aur-P1; its upper horizons, AH IV, III and the II-series, in Aur-P2); Fumane and Labeko Koba are assigned to the Protoaurignacian (Aur-P1); and El Castillo is excluded from the analysis as a contaminated, mixed assemblage. All of these assignments are documented in the "Site Phase Assignments" supplement, with El Castillo's exclusion flagged as a mixing issue.

### R2i — Protoaurignacian-phase sites were misplaced

> "Similarly, the sites selected for the Protoaurignacian phase—which is archaeologically valid if defined correctly (which is not the case here)—are all misplaced: • Pataud: no Protoaurignacian (but Early and Evolved Aurignacian) • Gatzarria: mixed layer • La Ferrassie: mixed layer • Spy: no Protoaurignacian • Geissenklösterle: no Protoaurignacian (but Early Aurignacian) • Hohle Fels: same as above • Vogelherd: same as above • Mladec: likely Evolved Aurignacian and certainly not Protoaurignacian"

BM: We corrected each of these assignments. Abri Pataud is assigned to the Early Aurignacian (Aur-P1); Gatzarria is retained in the Proto-Aurignacian with explicit justification (its Cjn2 layer is a coherent, refit-supported Proto-Aurignacian assemblage per Barshay-Szmidt et al. 2012), acknowledging that other layers of the site are mixed; La Ferrassie is assigned to the Early Aurignacian (Aur-P1) on technological grounds (Talamo et al. 2020); Spy is excluded (no Proto-Aurignacian component; mixed Early/Late assemblage); Geissenklösterle and Vogelherd are assigned to the Early Aurignacian (Aur-P1), while Hohle Fels is assigned at the layer level (AH V Early Aurignacian, Aur-P1; AH IV, III and II-series Evolved/Late Aurignacian, Aur-P2, following Bataille & Conard 2018, 2019 and Dinnis et al. 2019); and Mladeč is assigned to the Evolved Aurignacian (Aur-P2), not the Proto-Aurignacian. The resulting dataset is Aur-P1 20 sites / 346 objects and Aur-P2 9 sites / 63 objects.

### R2j — Dates alone cannot assign a phase

> "I do not know how the authors arrived at such a classification, but it is essential to remember that one or more C14 dates cannot alone assign a layer to a specific phase. It is the archaeological content of the layer that matters."

BM: This principle is now the explicit foundation of the revised assignment procedure. Phases are assigned on technological grounds using the material-culture markers in Table 1 and the specialist literature, with calibrated dates used only as the default for sites without a published technological attribution. The "Site Phase Assignments" supplement states this principle and lists the archaeological basis for each technology-assigned site, and Supplement S8 tests how much the results change under a purely date-based assignment so that the reader can see the effect of the choice.

---

## Marco Peresani

We thank Dr. Peresani for the detailed reading and for identifying the inconsistencies in the phase framework.

### P1 — Text–Table 1 inconsistency

> "Specifically, there is a clear inconsistency between your text and Table 1. On page 10, the text states that the Aurignacian is divided into four phases: Protoaurignacian, Early, Evolved, and Late Aurignacian. However, Table 1 introduces a 'Transitional' phase, which does not correspond to standard Aurignacian subdivisions."

BM: We removed this inconsistency by adopting the same framework in the text and in Table 1. Table 1 is now the two-phase Aur-P1 (Proto/Early) / Aur-P2 (Evolved/Late) scheme, and the text uses the identical terminology, so there is no "Transitional" phase anywhere in the manuscript or supplement.

### P2 — Adhere to conventional taxonomies

> "Since the study aims to divide the Aurignacian based on 'material culture changes', it should adhere to conventional taxonomies: namely, the Protoaurignacian, Early Aurignacian, and Evolved/Late Aurignacian (merged only if sample size necessitates)."

BM: We adopted exactly this conventional taxonomy. The analysis uses the Protoaurignacian and Early Aurignacian merged as Aur-P1, and the Evolved/Late Aurignacian merged as Aur-P2, with the merge justified by sample size (the reclassified Proto sample is too small to support its own network statistics). The four-phase and three-phase periodisations remain available only as sensitivity analyses in Supplement S4.

### P3 — Chronological overlap between SW Germany and the Mediterranean

> "Furthermore, if the analysis intends to utilize new chronological data, a more nuanced discussion is required. For example, sites in Southwestern Germany technologically assigned to the Early Aurignacian (in the published literature) often overlap chronologically with the Protoaurignacian in Mediterranean Europe."

BM: We agree and added a sentence in the Periodisation section stating that South-west German sites technologically assigned to the Early Aurignacian often overlap chronologically with the Mediterranean Proto-Aurignacian, which is one reason these two are merged in the two-phase Aur-P1 grouping. The reassignment itself is already reflected in the data (Geissenklösterle → Early; Hohle Fels → layer-level: AH V Early (Aur-P1), AH IV, III and II-series Evolved (Aur-P2); Mladeč → Evolved; see P4), and the principle that dates alone cannot override archaeological content is stated in the "Site Phase Assignments" supplement and cited to Reviewer 2's own point (R2j). 

### P4 — Figure 1 mislabelling (Fumane, Swabian Jura sites)

> "These classification issues are even more evident in Figure 1, where Fumane Cave is listed as 'Transitional' rather than Protoaurignacian (see the studies published in recent years by Falcucci and colleagues), and Swabian Jura sites (such as Hohle Fels and Geissenklösterle) are categorized as 'Transitional' or 'Protoaurignacian' rather than Early Aurignacian."

BM: Figure 1 was re-generated with the corrected assignments and the two-phase colouring. Fumane is now shown as Protoaurignacian (Aur-P1), Geissenklösterle as Early Aurignacian (Aur-P1), and Hohle Fels with its layer-level split across the two phases (AH V in Aur-P1; AH IV, III and the II-series in Aur-P2). No site is labelled "Transitional" in the revised figure.

### P5 — Heavy reliance on Vogelherd

> "Indeed, the heavy reliance on data from Vogelherd is a concern. This site was excavated with poor stratigraphic resolution, and there is a high likelihood of mixing between phases (including the Gravettian and Magdalenian)."

BM: We address the Vogelherd reliance concern in three ways. First, Supplement S7.4 tests the effect of excluding Vogelherd's figurines (173 → 123 objects; network effects are minimal: 75 → 74 edges, mean degree 7.50 → 7.40, −1.3%). Second, Supplement S8 Analysis B excludes Vogelherd entirely (Aur-P1 drops from 20 to 19 sites, 7.50 → 7.26 mean degree, −3.2%) and shows that the Aur-P1 > Aur-P2 ordering is preserved. Third, the Discussion now includes a limitations paragraph acknowledging the poor stratigraphic resolution and mixing risk at Vogelherd (including Gravettian and Magdalenian material), citing Schürch & Conard (2026), and noting that the main results are robust to the exclusion of the site.

### P6 — Consult recent literature on the Aurignacian sensu lato

> "I recommend consulting recent literature on the Aurignacian sensu lato, as several key recent studies are missing from your citations."

BM: We added the recent specialist literature recommended, including Bataille & Conard (2018, 2019) on Hohle Fels and Schürch et al. (2025) and Schürch & Conard (2026) on Vogelherd, alongside the other periodisation and site-specific references listed under E21. These citations now underpin the technology-based assignments and the two-phase periodisation.

---

## Closing

We thank the editor and the two reviewers again for their constructive comments. We believe the revised manuscript now rests on a chronologically and archaeologically defensible two-phase framework, with every site assignment justified by its archaeological content, the material-culture markers corrected, all analyses rerun and made fully reproducible, and all claims moderated to the level the evidence supports. We have addressed every point raised and look forward to the next stage of review.
