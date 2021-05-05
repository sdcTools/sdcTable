#include <Rcpp.h>
using namespace Rcpp;

Environment base("package:base");
Function do_unique = base["unique"];

// returns various information about current sub-problem, relating to a specific constraint
List info(CharacterVector sdc, NumericVector freqs, IntegerVector indices) {
  int nr_primsupps = 0;
  int nr_secondsupps = 0;
  IntegerVector ind_poss_s;
  IntegerVector ind_poss_z;
  IntegerVector ind_poss_w;
  IntegerVector ind_poss_s_or_z;

  IntegerVector ind_primsupps;

  int nr_singletons = 0;
  double amount_supped = 0;
  double amount_available_s = 0;
  double amount_available_z = 0;
  double amount_available_w = 0;

  if (indices.size() != sdc.size()) {
    stop("inputs `sdc` and `indices` do not match in size");
  }
  if (freqs.size() != sdc.size()) {
    stop("inputs `sdc` and `freqs` do not match in size");
  }

  for (int i = 0; i < sdc.size(); i++) {
    if (sdc[i] == "u") {
      amount_supped = amount_supped + freqs[i];
      nr_primsupps = nr_primsupps + 1;
      if (freqs[i] == 1) {
        nr_singletons = nr_singletons + 1;
      }
      ind_primsupps.push_back(indices[i]);
    }
    if (sdc[i] == "x") {
      amount_supped = amount_supped + freqs[i];
      nr_secondsupps = nr_secondsupps + 1;
      if (freqs[i] == 1) {
        nr_singletons = nr_singletons + 1;
      }
    }

    if (sdc[i] == "s") {
      ind_poss_s.push_back(indices[i]);
      ind_poss_s_or_z.push_back(indices[i]);
      amount_available_s = amount_available_s + freqs[i];
    }
    if (sdc[i] == "z") {
      ind_poss_z.push_back(indices[i]);
      ind_poss_s_or_z.push_back(indices[i]);
      amount_available_z = amount_available_z + freqs[i];
    }
    if (sdc[i] == "w") {
      ind_poss_w.push_back(indices[i]);
      amount_supped = amount_supped + freqs[i];
      amount_available_w = amount_available_w + freqs[i];
    }
  }
  // cells that are not w
  int nr_supps = nr_primsupps + nr_secondsupps;
  int nr_non_w = indices.size() - ind_poss_w.size();
  bool fully_supped = nr_supps == nr_non_w;
  return Rcpp::List::create(
    Named("nr_primsupps") = nr_primsupps,
    Named("ind_primsupps") = ind_primsupps,
    Named("nr_secondsupps") = nr_secondsupps,
    Named("nr_supps") = nr_supps,
    Named("fully_supped") = fully_supped,
    Named("ind_poss_s") = ind_poss_s,
    Named("nr_s") = ind_poss_s.size(),
    Named("ind_poss_z") = ind_poss_z,
    Named("nr_z") = ind_poss_z.size(),
    Named("ind_poss_w") = ind_poss_w,
    Named("nr_w") = ind_poss_w.size(),
    Named("nr_non_w") = nr_non_w,
    Named("ind_poss_s_or_z") = ind_poss_s_or_z,
    Named("nr_sz") = ind_poss_s_or_z.size(),
    Named("nr_singletons") = nr_singletons,
    Named("amount_available_s") = amount_available_s,
    Named("amount_available_z") = amount_available_z,
    Named("amount_available_w") = amount_available_w,
    Named("amount_supped") = amount_supped
  );
}

// uses base::order to order in increasing order by frequencies and ids
IntegerVector order_(NumericVector x, CharacterVector y) {
  Function order_r("order");
  NumericVector x_orig = clone(x);
  CharacterVector y_orig = clone(y);

  IntegerVector o = order_r(x_orig, y_orig, Rcpp::_["decreasing"]  = false);
  o = o - 1; // c-indices
  return(o);
}

// sorts an IntegerVector using std::sort()
IntegerVector stl_sort(IntegerVector x) {
  IntegerVector y = clone(x);
  std::sort(y.begin(), y.end());
  return y;
}

// compares two constraints and checks, if there are only two overlapping common cells;
// in this case an additional constraint is automatically generated
List compare_constraints(IntegerVector idx_a, IntegerVector idx_b, LogicalVector is_common_cell) {
  bool debug = true;
  List out = List::create();

  // compute common_cells
  IntegerVector common_cells = intersect(idx_a, idx_b);

  // no common cells -> nothing todo
  if (common_cells.size() == 0) {
    return(List::create());
  }

  // compute non-overlapping common cells;
  // this must be done in both directions and we have to remove non-common cells first

  LogicalVector log_a = is_common_cell[idx_a];
  IntegerVector common_a = idx_a[log_a];

  LogicalVector log_b = is_common_cell[idx_b];
  IntegerVector common_b = idx_b[log_b];

  // check if two cells overlap
  IntegerVector overlap_a = setdiff(common_a, common_cells);
  IntegerVector overlap_b = setdiff(common_b, common_cells);

  overlap_a = stl_sort(overlap_a);
  overlap_b = stl_sort(overlap_b);

  if (overlap_a.size() == 2) {
    if (debug == true) {
      Rcout << "problem (a)" << std::endl;
      Rcout << "common cells without totals in a: " << common_a << std::endl;
      Rcout << "overlapping cells in a: " << overlap_a << std::endl;
    }
    out.push_back(overlap_a, "idx");
  }

  if (overlap_b.size() == 2) {
    if (debug == true) {
      Rcout << "problem (b)" << std::endl;
      Rcout << "common cells without totals in b: " << common_b << std::endl;
      Rcout << "overlapping cells in b: " << overlap_b << std::endl;
    }
    out.push_back(overlap_b, "idx");
  }
  return(out);
}

/*
 * creates a simple-triplet input (provided as list) to c-style indices
 * if find_overlaps is true; all constraints are compared if they have exactly two overlapping common cells
 * and possible new constraints are generated that are dealt with in suppConstraints()
*/
List simple_triplet_to_indices(List m, LogicalVector is_common_cell, bool find_overlaps) {
  List added_constraints;

  // looping over the constraint matrix"
  IntegerVector vals_i = m["i"]; // row-indices
  IntegerVector vals_j = m["j"]; // column-indices
  IntegerVector vals_v = m["v"]; // -1: total; 1: contributing value
  int nr_constraints = max(vals_i);

  List ll(nr_constraints); //pre-allocate

  LogicalVector col_ind;
  IntegerVector mat_cols;
  IntegerVector vals;

  for (int i = 1; i <= nr_constraints; i++) {
    col_ind = vals_i == i;
    mat_cols = vals_j[col_ind];
    mat_cols = mat_cols - 1; // cpp-index
    vals = vals_v[col_ind];
    List tmplist = List::create(
      Rcpp::Named("idx") = mat_cols,
      Rcpp::Named("vals") = vals
    );
    ll[i - 1] = tmplist;
  }

  // todo: improve computation-time
  if (find_overlaps == true) {
    Rcout << "finding overlaps" << std::endl;
    List a, b;
    bool debug = true;

    int nr_constraints = ll.size();
    for (int i = 0; i < (nr_constraints - 1); i++) {
      a = ll[i];
      IntegerVector ind_a = a["idx"];
      LogicalVector log_a = is_common_cell[ind_a];
      IntegerVector common_a = ind_a[log_a];
      for (int j = i + 1; j < nr_constraints; j++) {
        b = ll[j];
        IntegerVector ind_b = b["idx"];
        // new: compare constraints directly in the function to avoid overhead/copying
        // compute common_cells
        IntegerVector common_cells = intersect(ind_a, ind_b);

        List tmpres = compare_constraints(ind_a, ind_b, is_common_cell);
        if (tmpres.size() > 0) {
          if (debug == true) {
            Rcout << "--> additional constraint generated. i = " << i << " | j = " << j << std::endl;
          }
          added_constraints.push_back(tmpres);
        }
      }
    }
    Rcout << "finished finding " << added_constraints.size() << " overlaps" << std::endl;
    added_constraints = do_unique(added_constraints);
  }
  return Rcpp::List::create(
    Rcpp::Named("constraint_mat") = ll,
    Rcpp::Named("additional_constraints") = added_constraints
  );
}

/*
 * This function computes an additional suppression and returns an updated
 * set of measures like the number of suppressed cells together with its amount and a
 * an updated suppression-pattern of the given constraint
 */
List perform_suppression(IntegerVector full_ids, CharacterVector sdc, NumericVector freqs, NumericVector weights, CharacterVector ids, IntegerVector ind_poss, List measures) {
  if (ind_poss.size() == 0) {
    Rcout << "full_ids: " << full_ids << std::endl;
    Rcout << "freqs: " << freqs << std::endl;
    Rcout << "sdc: " << sdc << std::endl;
    stop("we cannot find a solution; no cells left to suppress");
  }

  IntegerVector additional_supps = measures["additional_supps"];
  int nr_supps = measures["nr_supps"];
  int nr_s = measures["nr_s"];
  int nr_non_w = measures["nr_non_w"];
  double amount_avail = measures["amount_avail"];
  double amount_supped = measures["amount_supped"];

  // we need to _order/sort and use the first index to be suppressed
  IntegerVector tmporder = order_(weights[ind_poss], ids[ind_poss]);
  IntegerVector final_index = ind_poss[tmporder];
  final_index = final_index[0];
  ind_poss = setdiff(ind_poss, final_index);
  int idx = final_index[0];
  int idx_full = full_ids[idx];

  // we update the indices that need to be suppressed
  // and that are relative to the entire problem
  additional_supps.push_back(idx_full);

  // update (sub)problem (relative to current constraint)
  sdc[idx] = "x";
  nr_supps = nr_supps + 1;
  nr_s = nr_s - 1;
  bool fully_supped = nr_supps == nr_non_w;
  amount_avail = amount_avail - freqs[idx];
  amount_supped = amount_supped + freqs[idx];
  return List::create(
    Named("final_index") = idx_full,
    Named("sdc") = sdc,
    Named("additional_supps") = additional_supps,
    Named("ind_poss") = ind_poss,
    Named("nr_supps") = nr_supps,
    Named("nr_s") = nr_s,
    Named("fully_supped") = fully_supped,
    Named("amount_avail") = amount_avail,
    Named("amount_supped") = amount_supped
  );
}

/*
 * makes sure that for each constraint at least 2 cells are suppressed
 * if do_singletons is specified possibly additional suppressions are done if one ore two primary singleton suppressions are part of the pattern
 * if threshold is larger > 0 we suppress additional cells until the threshold is reached of the row is completely suppressed
 */
IntegerVector supp_single_constraint(IntegerVector full_ids, CharacterVector str_ids, CharacterVector sdc, NumericVector weights, NumericVector freqs, bool do_singletons, double threshold) {
  bool debug = false;
  int nr_elements = sdc.size();
  bool run_singleton = false;

  if (do_singletons == true or threshold > 0) {
    run_singleton = true;
  }

  IntegerVector additional_supps;
  IntegerVector final_index;

  // these indices are relative only the the current constraint
  IntegerVector indices = seq(0, nr_elements - 1);

  // compute information
  List suppinfo = info(sdc, freqs, indices);

  int nr_primsupps = suppinfo["nr_primsupps"];
  int nr_supps = suppinfo["nr_supps"];
  int nr_s = suppinfo["nr_s"];
  int nr_non_w = suppinfo["nr_non_w"];
  int nr_singletons = suppinfo["nr_singletons"];
  IntegerVector ind_poss_s = suppinfo["ind_poss_s"];
  IntegerVector ind_primsupps = suppinfo["ind_primsupps"];

  bool fully_supped = suppinfo["fully_supped"];
  double amount_avail = suppinfo["amount_available_s"];
  double amount_supped = suppinfo["amount_supped"];
  double amount_w = suppinfo["amount_available_w"];

  IntegerVector cur_indices;

  // no "open" cells left; we have nothing todo
  if (fully_supped == true) {
    return(additional_supps.sort());
  }

  // default-case: we have only a single suppression in the row/column
  if ((nr_supps == 1) and (fully_supped == false) and (amount_w <= 0)) {
    // we need to find an additional suppression
    if (debug) {
      Rcout << "we have a single suppression and "
            << nr_s << " s-cells with "
            << amount_avail << " frequencies to suppress" << std::endl;
    }

    // default case: a single supp and n
    List measures = List::create(
      Named("nr_supps") = nr_supps,
      Named("nr_s") = nr_s,
      Named("nr_non_w") = nr_non_w,
      Named("additional_supps") = additional_supps,
      Named("fully_supped") = fully_supped,
      Named("amount_avail") = amount_avail,
      Named("amount_supped") = amount_supped
    );

    List res = perform_suppression(full_ids, sdc, freqs, weights, str_ids, ind_poss_s, measures);
    additional_supps = res["additional_supps"];
    sdc = res["sdc"];
    nr_supps = res["nr_supps"];
    nr_s = res["nr_s"];
    ind_poss_s = res["ind_poss"];
    fully_supped = res["fully_supped"];
    amount_avail = res["amount_avail"];
    amount_supped =  res["amount_supped"];
  }

  if (run_singleton == true) {
    if (do_singletons == true and nr_singletons > 0 and nr_supps == 2 and fully_supped == false) {
      if (debug) {
        Rcout << "we have exactly two supps and " << nr_singletons << " singleton(s) --> additional supps must be found" << std::endl;
      }

      List measures = List::create(
        Named("nr_supps") = nr_supps,
        Named("nr_s") = nr_s,
        Named("nr_non_w") = nr_non_w,
        Named("additional_supps") = additional_supps,
        Named("fully_supped") = fully_supped,
        Named("amount_avail") = amount_avail,
        Named("amount_supped") = amount_supped
      );

      List res = perform_suppression(full_ids, sdc, freqs, weights, str_ids, ind_poss_s, measures);
      additional_supps = res["additional_supps"];
      sdc = res["sdc"];
      nr_supps = res["nr_supps"];
      nr_s = res["nr_s"];
      ind_poss_s = res["ind_poss"];
      fully_supped = res["fully_supped"];
      amount_avail = res["amount_avail"];
      amount_supped =  res["amount_supped"];
    }

    // if a frequency rule is used, it could happen that two cells on a row/column are
    // primary unsafe, but the sum of the two cells could still be unsafe. In that case
    // it should be prevented that these two cells protect each other.
    if ((do_singletons == true) and (nr_supps == 3) and (nr_primsupps == 3) and (fully_supped == false)) {
      Rcout << "case detected!" << std::endl;
      if (sdc[0] == "u") { // the total is a primary suppressed cell
        List measures = List::create(
          Named("nr_supps") = nr_supps,
          Named("nr_s") = nr_s,
          Named("nr_non_w") = nr_non_w,
          Named("additional_supps") = additional_supps,
          Named("fully_supped") = fully_supped,
          Named("amount_avail") = amount_avail,
          Named("amount_supped") = amount_supped
        );

        List res = perform_suppression(full_ids, sdc, freqs, weights, str_ids, ind_poss_s, measures);
        additional_supps = res["additional_supps"];
        sdc = res["sdc"];
        nr_supps = res["nr_supps"];
        nr_s = res["nr_s"];
        ind_poss_s = res["ind_poss"];
        fully_supped = res["fully_supped"];
        amount_avail = res["amount_avail"];
        amount_supped =  res["amount_supped"];
      }
    }

    // we want to make sure that a given amount (threshold) is primary suppressed
    if ((threshold > 0) and amount_supped < threshold) {
      if (debug) {
        Rcout << " --> the required threshold " << threshold
              << " is not reached with amount_supped = " << amount_supped
              << " --> additional supps must be found" << std::endl;
      }
      while ((amount_supped < threshold) and (fully_supped == false)) {
        List measures = List::create(
          Named("nr_supps") = nr_supps,
          Named("nr_s") = nr_s,
          Named("nr_non_w") = nr_non_w,
          Named("additional_supps") = additional_supps,
          Named("fully_supped") = fully_supped,
          Named("amount_avail") = amount_avail,
          Named("amount_supped") = amount_supped
        );

        List res = perform_suppression(full_ids, sdc, freqs, weights, str_ids, ind_poss_s, measures);
        additional_supps = res["additional_supps"];
        sdc = res["sdc"];
        nr_supps = res["nr_supps"];
        nr_s = res["nr_s"];
        ind_poss_s = res["ind_poss"];
        fully_supped = res["fully_supped"];
        amount_avail = res["amount_avail"];
        amount_supped =  res["amount_supped"];
      }
    }
  }
  return(additional_supps.sort());
}

// [[Rcpp::export]]
List suppConstraints(DataFrame dat, List m, List params) {
  Function cpp_print("print");
  String wname = params["wname"];
  String idname = params["idname"];
  String sdcname = params["sdcname"];
  String freqname = params["freqname"];
  LogicalVector is_common_cell = params["is_common_cell"];
  bool find_overlaps = params["find_overlaps"];
  bool verbose = params["verbose"];
  bool do_singletons = params["do_singletons"];
  double threshold = params["threshold"];

  // convert simple-triplet-input matrix to indices and values
  if (verbose == true) {
    Rcout << "converting simple-triplet matrix to indices (this could take a while ...)" << std::endl;
  }
  List tmpres = simple_triplet_to_indices(m, is_common_cell, find_overlaps);
  m = tmpres["constraint_mat"];
  List additional_constraints = tmpres["additional_constraints"];
  int nr_additional_constraints = additional_constraints.size();

  if (verbose == true) {
    Rcout << nr_additional_constraints << " additional constraints have been created" << std::endl;
  }

  if (do_singletons == true or threshold > 0) {
    if (verbose == true) {
      Rcout << "procedure checks for singletons" << std::endl;
    }
  }

  // start protection
  int nr_vars = dat.nrow();
  int nr_constraints = m.size();
  if (verbose == true) {
    Rcout << "we protect a dataset with " << nr_vars << " variables"
    " and " << nr_constraints << " linear constraints" << std::endl;
  }

  // "global" variables
  int idx_weightcol = dat.findName(wname);
  NumericVector weights = dat[idx_weightcol];

  int idx_sdccol = dat.findName(sdcname);
  CharacterVector tmpsdc = dat[idx_sdccol];
  CharacterVector sdc_status = clone(tmpsdc);

  int idx_idcol = dat.findName(idname);
  CharacterVector tmpids = dat[idx_idcol];
  CharacterVector strids = clone(tmpids);

  int idx_freqcol = dat.findName(freqname);
  NumericVector freqs = dat[idx_freqcol];

  // variables used for each constraint
  IntegerVector indices;
  NumericVector cur_w;
  CharacterVector cur_sdc;
  IntegerVector cur_indices;
  NumericVector cur_freqs;
  CharacterVector cur_ids;

  List suppinfo;
  List tmplist;

  bool finished = false;
  bool supps_added;
  int nr_additional_supps = 0;
  int counter = 0;
  while (!finished) {
    counter += 1;
    if (counter > 1) {
      // we only need to run singleton-detection procedure once (if it is required at all)
      do_singletons = false;
      threshold = -1;
    }
    supps_added = false;
    nr_additional_supps = 0;

    // we start by dealing with additionally created constraints
    // that would allow differencing attacks in constraints that
    // differ by exactly two common cells
    // we make sure that in such cases; either none or both common cells
    // are suppressed
    if (nr_additional_constraints > 0) {
      IntegerVector tmpind;
      int to_supp;
      for (int i = 0; i < nr_additional_constraints; i++) {
        List tmpindlist = additional_constraints[i];
        tmpind = tmpindlist["idx"];
        cur_sdc = sdc_status[tmpind];
        cur_freqs = freqs[tmpind];
        List add_suppinfo = info(cur_sdc, cur_freqs, tmpind);

        bool is_fully_supped = add_suppinfo["fully_supped"];
        int nr_tmp_supps = add_suppinfo["nr_supps"];
        if ((is_fully_supped == false) and (nr_tmp_supps == 1)) {
          // suppress the other remaining cell to make sure, both cells are suppressed
          IntegerVector tmp_poss_s = add_suppinfo["ind_poss_s"];
          if (tmp_poss_s.size() > 0) {
            to_supp = add_suppinfo["ind_poss_s"];
          } else {
            // also if a cell is known to be 0 it could be used to compute other cell-values
            to_supp = add_suppinfo["ind_poss_s_or_z"];
          }
          sdc_status[to_supp] = "x";
          supps_added = true;
          nr_additional_supps = nr_additional_supps + 1;
        }
      }

      if (verbose == true) {
        Rcout << "run " << counter << " | additional supps after additional constraints): " << nr_additional_supps << std::endl;
      }
    }

    // we now check all the "regular" constraints
    for (int i = 0; i < nr_constraints; i++) {
      tmplist = m[i];
      indices = tmplist["idx"];

      // get weights + sdc_status (integer) for cells in this constraint
      cur_w = weights[indices];
      cur_sdc = sdc_status[indices];
      cur_freqs = freqs[indices];
      cur_ids = strids[indices];

      // indices: contains the full indices relevant to this constraint within the larger problem
      IntegerVector new_supps = supp_single_constraint(indices, cur_ids, cur_sdc, cur_w, cur_freqs, do_singletons, threshold);
      if (new_supps.size() > 0) {
        sdc_status[new_supps] = "x";
        supps_added = true;
        nr_additional_supps = nr_additional_supps + 1;
      }
    }

    if (verbose == true) {
      Rcout << "run " << counter << " | additional supps: " << nr_additional_supps << std::endl;
    }
    if (supps_added == false) {
      finished = true;
    } else {
      if (counter >= 10) {
        stop("no solution after 10 counts!");
      }
    }
  }
  return Rcpp::List::create(
    Rcpp::Named("dat") = dat,
    Rcpp::Named("m") = m,
    Rcpp::Named("additional_constraints") = additional_constraints,
    Rcpp::Named("sdc_status") = sdc_status
  );
}
